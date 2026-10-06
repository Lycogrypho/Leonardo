package it.grypho.scala.leonardo
package matrix

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0054 — the Cholesky factorisation `chol(A)`, `A = L·Lᵀ`.
 *
 *  The acceptance values are MPC requirement T1-02's.  Beyond them the suite pins the two
 *  things a Cholesky kernel gets wrong quietly: its **refusals** (a factor of an indefinite or
 *  asymmetric matrix is a confident wrong answer, and the failure is the convexity test F_0053
 *  will rely on), and that both refusal thresholds are **relative**, so rescaling `A` never
 *  moves a verdict (the units rule in CLAUDE.md).
 */
class CholeskyTest extends AnyFlatSpec:

  private val env = new Environment()

  private def dense(rows: Int, cols: Int, elems: Double*): _MatrixValue =
    _MatrixValue(rows, cols, elems.toArray)

  private def parse(s: String, exact: Option[Int] = None): _Expression =
    Parser.parse(s, exact) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for \"$s\": $other")

  private def factor(m: _MatrixValue): _MatrixValue =
    m.cholesky.getOrElse(fail(s"expected a Cholesky factor of $m"))

  private def assertClose(a: _MatrixValue, b: _MatrixValue, tol: Double = 1e-12): Unit =
    assert(a.rows == b.rows && a.cols == b.cols, s"shape: $a vs $b")
    for i <- 0 until a.rows; j <- 0 until a.cols do
      assert(math.abs(a(i, j) - b(i, j)) <= tol * math.max(1.0, math.abs(b(i, j))),
        s"($i,$j): expected ${b(i, j)} but got ${a(i, j)}")

  // --- the dense kernel ---

  "cholesky" should "factor the acceptance matrix as [[2, 0], [1, √2]]" in
  {
    assertClose(factor(dense(2, 2, 4, 2, 2, 3)), dense(2, 2, 2, 0, 1, math.sqrt(2)))
  }

  it should "reconstruct a larger SPD matrix as L·Lᵀ, L lower triangular with a positive diagonal" in
  {
    // M·Mᵀ + I is symmetric positive definite for any M.
    val m = dense(4, 4, 1, 2, 0, -1, 3, 1, 2, 0, -2, 0, 1, 4, 1, 1, 1, 1)
    val a = m.multiply(m.transpose).add(_MatrixValue.identity(4))
    val l = factor(a)
    for i <- 0 until 4; j <- i + 1 until 4 do assert(l(i, j) == 0.0, s"L($i,$j) = ${l(i, j)}")
    for i <- 0 until 4 do assert(l(i, i) > 0.0)
    assertClose(l.multiply(l.transpose), a)
  }

  it should "decline an indefinite, a negative-definite and a singular semidefinite matrix" in
  {
    assert(dense(2, 2, 1, 2, 2, 1).cholesky.isEmpty, "indefinite")
    assert(dense(2, 2, -4, 0, 0, -1).cholesky.isEmpty, "negative definite")
    assert(dense(2, 2, 1, 1, 1, 1).cholesky.isEmpty, "singular PSD: a zero pivot")
  }

  it should "decline a non-square or an asymmetric matrix rather than read one triangle" in
  {
    assert(dense(2, 3, 1, 0, 0, 0, 1, 0).cholesky.isEmpty, "non-square")
    // Reading only the lower triangle would factor this as though it were [[4, 2], [2, 3]].
    assert(dense(2, 2, 4, 1, 2, 3).cholesky.isEmpty, "asymmetric")
  }

  it should "accept rounding-level asymmetry, relative to the matrix's size" in
  {
    assert(dense(2, 2, 4, 2, 2 + 1e-15, 3).cholesky.isDefined)
    assert(dense(2, 2, 4e8, 2e8, 2e8 + 1e-7, 3e8).cholesky.isDefined)
  }

  it should "give the same verdict at every scale (no absolute threshold)" in
  {
    for k <- List(1e-12, 1e-6, 1.0, 1e6, 1e12) do
      val spd = dense(2, 2, 4 * k, 2 * k, 2 * k, 3 * k)
      assertClose(factor(spd), dense(2, 2, 2, 0, 1, math.sqrt(2)).scale(math.sqrt(k)))
      assert(dense(2, 2, k, k, k, k).cholesky.isEmpty, s"singular PSD at scale $k")
      assert(dense(2, 2, 4 * k, k, 2 * k, 3 * k).cholesky.isEmpty, s"asymmetric at scale $k")
  }

  // --- the language ---

  "chol(A)" should "evaluate a numeric matrix to its factor" in
  {
    parse("chol([[4, 2], [2, 3]])").eval(env) match
      case Right(l: _MatrixValue) => assertClose(l, dense(2, 2, 2, 0, 1, math.sqrt(2)))
      case other                  => fail(s"expected a factor, got $other")
  }

  it should "stay unevaluated on an indefinite matrix, and on a symbolic one" in
  {
    assert(parse("chol([[1, 2], [2, 1]])").eval(env).isLeft)
    parse("chol([[a, 0], [0, 1]])").eval(env) match
      case Left(_: _Cholesky) => succeed
      case other              => fail(s"expected chol to stay symbolic, got $other")
  }

  it should "be matrix-shaped, so chol(A) * transpose(chol(A)) multiplies back to A" in
  {
    parse("chol([[4, 2], [2, 3]]) * transpose(chol([[4, 2], [2, 3]]))").eval(env) match
      case Right(a: _MatrixValue) => assertClose(a, dense(2, 2, 4, 2, 2, 3))
      case other                  => fail(s"expected the product, got $other")
  }

  it should "round-trip through toString, and reserve its name" in
  {
    val e = parse("chol([[4, 2], [2, 3]])")
    assert(parse(e.toString) == e)
    assert(Parser.ReservedWords.contains("chol"))
  }

  // --- the exact tier ---

  private def exactFactor(s: String): Vector[_Expression] =
    parse(s, Some(30)).eval(env) match
      case Left(m: _Matrix) => m.elems
      case other            => fail(s"expected an exact symbolic matrix, got $other")

  "chol in exact mode" should "keep a rational factor exact" in
  {
    val cells = exactFactor("chol([[4, 2], [2, 2]])")
    assert(cells.forall(_.isInstanceOf[_Rational]), s"not exact: $cells")
    assert(cells.map { case _Number(d) => d; case other => fail(s"$other") } == Vector(2.0, 0.0, 1.0, 1.0))
  }

  it should "take an irrational pivot at the working precision" in
  {
    val cells = exactFactor("chol([[4, 2], [2, 3]])")
    assert(cells.forall(_.isInstanceOf[_Rational]), s"not exact: $cells")
    val two = _Rational.One.add(_Rational.One)
    cells(3) match
      case r: _Rational => r.multiply(r).subtract(two).abs match
        case _Number(err) => assert(err < 1e-25, s"√2 is only good to $err")
      case other => fail(s"$other")
  }

  it should "decline an exact indefinite or asymmetric matrix by exact comparison" in
  {
    // A successful exact factor is a Left too (a symbolic _Matrix), so match the node itself.
    for s <- List("chol([[1, 2], [2, 1]])", "chol([[4, 1], [2, 3]])") do
      parse(s, Some(30)).eval(env) match
        case Left(_: _Cholesky) => succeed
        case other              => fail(s"expected a refusal for $s, got $other")
  }

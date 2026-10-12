package it.grypho.scala.leonardo
package matrix

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random


/** F_0087 — `nullspace(A)` and its sibling `orth(A)`: orthonormal bases of the null space and the
 *  range, read off the SVD at `rank`'s tolerance.
 *
 *  A basis is unique only up to rotation, so nothing is compared entry by entry: the suite checks
 *  what defines one — `A·N = 0`, orthonormal columns, the right dimension (rank–nullity), and for
 *  the range that projecting `A` onto it changes nothing.  The rank is `rank`'s, so the units rule
 *  carries over: rescaling `A` never changes a dimension.
 */
class NullspaceTest extends AnyFlatSpec:

  private val env = new Environment()

  private def parse(s: String, exact: Option[Int] = None): _Expression =
    Parser.parse(s, exact) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for \"$s\": $other")

  private def evaluated(s: String, exact: Option[Int] = None): Either[_Expression, _Value] =
    parse(s, exact).eval(env)

  private def basis(s: String, exact: Option[Int] = None): _MatrixValue = evaluated(s, exact) match
    case Right(m: _MatrixValue) => m
    case other                  => fail(s"$s: expected a dense basis, got $other")

  private def literal(a: _MatrixValue): String =
    (0 until a.rows).map(i => (0 until a.cols).map(j => _Number.render(a(i, j))).mkString("[", ", ", "]")).mkString("[", ", ", "]")

  private def random(rows: Int, cols: Int, seed: Long): _MatrixValue =
    val rng = new Random(seed)
    _MatrixValue(rows, cols, Array.fill(rows * cols)(rng.nextDouble() * 2 - 1))

  private def norm(a: _MatrixValue): Double = math.sqrt(a.toVector.map(x => x * x).sum)

  private def assertOrthonormal(q: _MatrixValue): Unit =
    val g = q.transpose.multiply(q)
    for i <- 0 until g.rows; j <- 0 until g.cols do
      assert(math.abs(g(i, j) - (if i == j then 1.0 else 0.0)) < 1e-10, s"QᵀQ($i, $j) = ${g(i, j)}")

  private def assertAnnihilates(a: _MatrixValue, n: _MatrixValue): Unit =
    val an = a.multiply(n)
    assert(norm(an) <= 1e-10 * math.max(1.0, norm(a)), s"‖A·N‖ = ${norm(an)}")

  "nullspace" should "give an orthonormal basis that A annihilates, of dimension n - rank" in
  {
    val a = "[[1, 2, 3], [2, 4, 6], [1, 0, 1]]"            // rank 2
    val n = basis(s"nullspace($a)")
    assert(n.rows == 3 && n.cols == 1)
    assertOrthonormal(n)
    assertAnnihilates(basis(a), n)
  }

  it should "satisfy rank–nullity on wide and rank-deficient matrices, with orth" in
  {
    for (rows, cols, k, seed) <- List((2, 4, 2, 1L), (5, 3, 2, 2L), (4, 6, 3, 3L), (6, 6, 4, 4L)) do
      // A rank-k matrix as a product of random rows×k and k×cols factors.
      val a = random(rows, k, seed).multiply(random(k, cols, seed + 100))
      val n = basis(s"nullspace(${literal(a)})")
      val o = basis(s"orth(${literal(a)})")
      assert(n.rows == cols && n.cols == cols - k, s"null: ${n.rows}x${n.cols} for rank $k")
      assert(o.rows == rows && o.cols == k, s"orth: ${o.rows}x${o.cols} for rank $k")
      assertOrthonormal(n)
      assertOrthonormal(o)
      assertAnnihilates(a, n)
      // The range: projecting A onto span(O) leaves it unchanged.
      val p = o.multiply(o.transpose).multiply(a)
      assert(norm(p.add(a.scale(-1))) <= 1e-10 * norm(a))
  }

  it should "answer false for a trivial null space, and the whole space for the zero matrix" in
  {
    assert(evaluated("nullspace([[1, 0], [0, 1], [1, 1]])") == Right(_Bool(false)))
    val n = basis("nullspace([[0, 0, 0], [0, 0, 0]])")
    assert(n.rows == 3 && n.cols == 3)
    assertOrthonormal(n)
    assert(evaluated("orth([[0, 0, 0], [0, 0, 0]])") == Right(_Bool(false)))
  }

  it should "decide the rank as rank does: relative, so the units never move a dimension" in
  {
    val a = "[[1, 2, 3], [2, 4, 6], [1, 0, 1]]"
    for k <- List("1e-12", "1", "1e12") do
      assert(basis(s"nullspace($k * $a)").cols == 1, s"scale $k")
      assert(basis(s"orth($k * $a)").cols == 2, s"scale $k")
  }

  it should "take rank's optional tolerance: a tiny singular value counts as zero only above it" in
  {
    val d = "[[1, 0], [0, 1e-8]]"
    assert(basis(s"nullspace($d)").cols == 1)                       // 1e-8 ≤ 0.5·10⁻⁵ at precision 5
    assert(evaluated(s"nullspace($d, 1e-10)") == Right(_Bool(false)))
    assert(basis(s"orth($d, 1e-10)").cols == 2)
  }

  it should "demote an exact matrix and leave a symbolic one as written" in
  {
    val n = basis("nullspace([[1, 2], [2, 4]])", exact = Some(30))
    assert(n.rows == 2 && n.cols == 1)
    val sym = parse("nullspace([[a, 1], [0, 1]])")
    assert(sym.eval(env).isLeft)
  }

  it should "refuse a tolerance outside [0, 1), saying why" in
  {
    for src <- List("nullspace([[1, 2], [2, 4]], 2)", "orth([[1, 2], [2, 4]], -0.5)") do
      val e = parse(src)
      assert(e.eval(env) == Left(e), src)
      e match
        case n: _NullSpace => assert(n.declineReason(env).exists(_.contains("[0, 1)")), src)
        case other         => fail(s"$src parsed as $other")
  }

  it should "round-trip through toString and reserve its names" in
  {
    // toString writes a number at DefaultPrecision (by design: it is the fixed canonical form),
    // so a tolerance like 1e-9 reads back as 0.0 -- the same holds for rank(A, 1e-9). Tolerances
    // here survive five decimals.
    for src <- List("nullspace(A)", "nullspace(A, 0.001)", "orth(A)", "orth(A, 0.5)") do
      val e = parse(src)
      assert(parse(e.toString) == e, src)
    assert(Parser.ReservedWords.contains("nullspace") && Parser.ReservedWords.contains("orth"))
  }

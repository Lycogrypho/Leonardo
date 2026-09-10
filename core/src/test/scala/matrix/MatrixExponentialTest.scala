package it.grypho.scala.leonardo
package matrix

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** Issue 6.39 — the matrix exponential `expm(A)`.
 *
 *  The suite leans on **identities rather than tabulated numbers**, because they hold for
 *  every input and so catch a wrong algorithm rather than a wrong transcription.  The
 *  defective case is the one that matters most: it is what the rejected eigen route would get
 *  silently wrong rather than refuse.
 */
class MatrixExponentialTest extends AnyFlatSpec:

  val env: Environment = new Environment()

  def dense(rows: Int, cols: Int, elems: Double*): _MatrixValue =
    _MatrixValue(rows, cols, elems.toArray)

  def expmOf(m: _MatrixValue): _MatrixValue =
    m.expm.getOrElse(fail(s"expected an exponential for $m"))

  def assertClose(a: _MatrixValue, b: _MatrixValue, tol: Double = 1e-9): Unit =
    assert(a.rows == b.rows && a.cols == b.cols, s"shape: $a vs $b")
    for i <- 0 until a.rows; j <- 0 until a.cols do
      assert(math.abs(a(i, j) - b(i, j)) < tol * math.max(1.0, math.abs(b(i, j))),
        s"($i,$j): expected ${b(i, j)} but got ${a(i, j)}\n  actual=$a\n  expected=$b")

  // --- the identities ---

  "expm(0)" should "be the identity matrix" in
  {
    assertClose(expmOf(dense(3, 3, 0,0,0, 0,0,0, 0,0,0)), _MatrixValue.identity(3))
  }

  "expm(A) * expm(-A)" should "be the identity" in
  {
    // Holds for every A, since A and -A commute -- a strong check that needs no known answer.
    val a = dense(3, 3, 1, 2, 0, -1, 3, 1, 0, 1, 2)
    assertClose(expmOf(a).multiply(expmOf(a.scale(-1))), _MatrixValue.identity(3), 1e-8)
  }

  "expm of a diagonal matrix" should "exponentiate the diagonal" in
  {
    val d = dense(2, 2, 2.0, 0, 0, -1.5)
    assertClose(expmOf(d), dense(2, 2, math.exp(2.0), 0, 0, math.exp(-1.5)))
  }

  "expm(I)" should "be e times the identity" in
  {
    assertClose(expmOf(_MatrixValue.identity(2)), _MatrixValue.identity(2).scale(math.E))
  }

  "d/dt expm(A*t)" should "equal A * expm(A*t)" in
  {
    // The defining property, checked by a central difference at t = 0.5.
    val a  = dense(2, 2, 0.5, 1.0, -0.3, 0.2)
    val h  = 1e-6
    val hi = expmOf(a.scale(0.5 + h))
    val lo = expmOf(a.scale(0.5 - h))
    val approx = hi.add(lo.scale(-1)).scale(1.0 / (2 * h))
    assertClose(approx, a.multiply(expmOf(a.scale(0.5))), 1e-6)
  }

  // --- the case the eigen route would get wrong ---

  "expm of a DEFECTIVE matrix (a Jordan block)" should "use the closed form, not an eigenbasis" in
  {
    // [[l,1],[0,l]] has a single eigenvalue with one eigenvector, so V*diag(e^l)*V^-1 has no
    // valid V. The true answer is e^l * [[1,1],[0,1]]. This is why 6.39 Decision D chose
    // scaling-and-squaring over the eigen route, and why the case is pinned here.
    val l = 0.7
    assertClose(expmOf(dense(2, 2, l, 1, 0, l)),
                dense(2, 2, math.exp(l), math.exp(l), 0, math.exp(l)))
  }

  "expm of a nilpotent matrix" should "terminate the series exactly" in
  {
    // N^2 = 0, so expm(N) = I + N exactly.
    val nMat = dense(3, 3, 0,1,0, 0,0,0, 0,0,0)
    assertClose(expmOf(nMat), _MatrixValue.identity(3).add(nMat))
  }

  "expm of a rotation generator" should "give the rotation matrix" in
  {
    // [[0,-t],[t,0]] exponentiates to [[cos t, -sin t],[sin t, cos t]].
    val t = 0.9
    assertClose(expmOf(dense(2, 2, 0, -t, t, 0)),
                dense(2, 2, math.cos(t), -math.sin(t), math.sin(t), math.cos(t)))
  }

  "expm of a badly scaled matrix" should "stay accurate through scaling and squaring" in
  {
    // Norm well past the Pade radius, so the scaling branch is exercised rather than skipped.
    val a = dense(2, 2, 10.0, 4.0, 0.0, 10.0)
    assertClose(expmOf(a), dense(2, 2, math.exp(10.0), 4 * math.exp(10.0), 0, math.exp(10.0)), 1e-8)
  }

  // --- refusals ---

  "expm of a non-square matrix" should "be undefined (None)" in
  {
    assert(dense(2, 3, 1,2,3, 4,5,6).expm.isEmpty)
  }

  // --- the node, parser and round-trip ---

  "expm(A) as a node" should "reduce a dense operand" in
  {
    val e = _MatrixExponential(dense(2, 2, 0, 0, 0, 0))
    e.eval(env) match
      case Right(m: _MatrixValue) => assertClose(m, _MatrixValue.identity(2))
      case other                  => fail(s"expected a dense matrix but got: $other")
  }

  "expm of a symbolic matrix" should "stay symbolic" in
  {
    val e = _MatrixExponential(_Matrix(2, 2, Vector(_Variable("a"), _Number(0), _Number(0), _Number(1))))
    assert(e.eval(env).isLeft)
  }

  "expm(...) toString" should "round-trip through the parser" in
  {
    // A _Matrix LITERAL, not a dense _MatrixValue: a dense value's toString re-parses to a
    // literal, so the two nodes would compare unequal for a reason that is not about expm.
    // `det`/`inv` round-trip the same way.
    val e = _MatrixExponential(_Matrix(2, 2, Vector(_Variable("a"), _Number(0), _Number(0), _Variable("b"))))
    val reparsed = Parser.parse(e.toString)
    assert(reparsed.successful, s"round-trip parse failed: ${e.toString}")
    assert(reparsed.get == e)
  }

  "bare 'expm'" should "be a reserved word" in
  {
    assert(!Parser.parse("expm").successful)
  }

  "expm(A) parsed and evaluated" should "match the kernel" in
  {
    val parsed = Parser.parse("expm([[0, 1], [0, 0]])")
    assert(parsed.successful, s"parse failed: $parsed")
    parsed.get.eval(env) match
      case Right(m: _MatrixValue) => assertClose(m, dense(2, 2, 1, 1, 0, 1))
      case other                  => fail(s"expected a dense matrix but got: $other")
  }

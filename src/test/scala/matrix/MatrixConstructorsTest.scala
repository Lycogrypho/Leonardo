package it.grypho.scala.leonardo
package matrix

import core.*
import matrix.*
import org.scalatest.flatspec.AnyFlatSpec


// Feature 4.1 â€” matrix constructors: eye(n) and zeros(r, c) / zeros(n).
class MatrixConstructorsTest extends AnyFlatSpec:

  val env: Environment = new Environment()

  def dense(rows: Int, cols: Int, elems: Double*): _MatrixValue =
    _MatrixValue(rows, cols, elems.toArray)

  def evalMatrix(e: _Expression): _MatrixValue =
    e.eval(env) match
      case Right(m: _MatrixValue) => m
      case other                  => fail(s"expected a dense matrix but got: $other")

  // --- eye(n) ---

  "eye(1)" should "be the 1Ã—1 identity" in
  {
    assert(evalMatrix(IdentityMatrix(_Number(1))) == dense(1, 1, 1.0))
  }

  "eye(2)" should "be [[1,0],[0,1]]" in
  {
    assert(evalMatrix(IdentityMatrix(_Number(2))) == dense(2, 2, 1, 0, 0, 1))
  }

  "eye(3)" should "have 1s on the diagonal only" in
  {
    val m = evalMatrix(IdentityMatrix(_Number(3)))
    assert(m.rows == 3 && m.cols == 3)
    for i <- 0 until 3; j <- 0 until 3 do
      assert(m(i, j) == (if i == j then 1.0 else 0.0), s"($i,$j)")
  }

  "eye with a non-integer dimension" should "stay symbolic" in
  {
    val e = IdentityMatrix(_Number(2.5))
    assert(e.eval(env) == Left(e))
  }

  "eye with a non-positive dimension" should "stay symbolic" in
  {
    val e = IdentityMatrix(_Number(0))
    assert(e.eval(env) == Left(e))
  }

  "eye with a free variable" should "reduce the variable and stay symbolic while unbound" in
  {
    val e = IdentityMatrix(_Variable("n"))
    assert(e.eval(env).isLeft)
    val bound = env.withBinding("n", _Number(2))
    assert(evalMatrix(IdentityMatrix(_Variable("n")).eval(bound).toExpression) == dense(2, 2, 1, 0, 0, 1))
  }

  "eye(n) toString" should "round-trip through the parser" in
  {
    val e = IdentityMatrix(_Number(3))
    val reparsed = parser.Parser.parse(e.toString)
    assert(reparsed.successful, s"round-trip parse failed: ${e.toString}")
    assert(reparsed.get == e)
  }

  "bare 'eye'" should "be a reserved word" in
  {
    assert(!parser.Parser.parse("eye").successful)
  }

  "eye(3) parsed" should "evaluate to the 3Ã—3 identity" in
  {
    val result = parser.Parser.parse("eye(3)").get.eval(env)
    assert(result == Right(dense(3, 3, 1,0,0, 0,1,0, 0,0,1)))
  }

  "eye(n) is matrix-shaped" should "combine with + to build MatSum" in
  {
    val e = parser.Parser.parse("eye(2) + eye(2)").get
    assert(e.isInstanceOf[matrix.MatSum])
    assert(evalMatrix(e) == dense(2, 2, 2, 0, 0, 2))
  }

  // --- zeros(r, c) and zeros(n) ---

  "zeros(2, 3)" should "be a 2Ã—3 zero matrix" in
  {
    val m = evalMatrix(ZeroMatrix(_Number(2), _Number(3)))
    assert(m.rows == 2 && m.cols == 3)
    for i <- 0 until 2; j <- 0 until 3 do assert(m(i, j) == 0.0)
  }

  "zeros(n) â€” square form" should "be nÃ—n zeros" in
  {
    val parsed = parser.Parser.parse("zeros(3)").get
    val m = evalMatrix(parsed)
    assert(m.rows == 3 && m.cols == 3)
    for i <- 0 until 3; j <- 0 until 3 do assert(m(i, j) == 0.0)
  }

  "zeros(2, 3) toString" should "round-trip through the parser" in
  {
    val e = ZeroMatrix(_Number(2), _Number(3))
    val reparsed = parser.Parser.parse(e.toString)
    assert(reparsed.successful, s"round-trip failed: ${e.toString}")
    assert(reparsed.get == e)
  }

  "bare 'zeros'" should "be a reserved word" in
  {
    assert(!parser.Parser.parse("zeros").successful)
  }

  "zeros with non-positive dimension" should "stay symbolic" in
  {
    val e = ZeroMatrix(_Number(0), _Number(2))
    assert(e.eval(env) == Left(e))
  }

  "eye(n) + zeros(n, n)" should "equal eye(n)" in
  {
    val n    = 3
    val iMat = evalMatrix(IdentityMatrix(_Number(n)))
    val zMat = evalMatrix(ZeroMatrix(_Number(n), _Number(n)))
    assert(iMat.add(zMat) == iMat)
  }

  // --- REPL integration ---

  "the REPL" should "evaluate eye and zeros expressions" in
  {
    val s = cli.Session()
    assert(s.execute("eye(2)")    == "[[1.0, 0.0], [0.0, 1.0]]")
    assert(s.execute("zeros(2)")  == "[[0.0, 0.0], [0.0, 0.0]]")
    assert(s.execute("zeros(2, 3)") == "[[0.0, 0.0, 0.0], [0.0, 0.0, 0.0]]")
  }

  "the REPL" should "compute A + eye(n) correctly" in
  {
    val s = cli.Session()
    s.execute("A := [[2, 1], [0, 3]]")
    assert(s.execute("A + eye(2)") == "[[3.0, 1.0], [0.0, 4.0]]")
  }

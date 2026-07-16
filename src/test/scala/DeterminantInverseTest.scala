package it.grypho.scala.leonardo

import core.*
import scalar.*
import matrix.*
import org.scalatest.flatspec.AnyFlatSpec


// Feature 4.1 (determinant, det) + 4.2 (inverse, inv / 1÷A) from ToDo.md.
class DeterminantInverseTest extends AnyFlatSpec:

  val a = _Variable("a")
  val b = _Variable("b")
  val c = _Variable("c")
  val d = _Variable("d")

  def envWith(bindings: (String, Double)*): Environment =
    bindings.foldLeft(new Environment())((e, kv) => e.withBinding(kv._1, _Number(kv._2)))

  def evalMatrix(e: _Expression, env: Environment = new Environment()): _MatrixValue =
    e.eval(env) match
      case Right(m: _MatrixValue) => m
      case other                  => fail(s"expected a dense matrix value but got: $other")

  def dense(rows: Int, cols: Int, elems: Double*): _MatrixValue =
    _MatrixValue(rows, cols, elems.toArray)

  def literal(rows: Int, cols: Int, elems: _Expression*): _Matrix =
    _Matrix(rows, cols, elems.toVector)

  // --- 4.1: dense determinant kernel ---

  "determinant of a 2×2" should "be ad − bc" in
  {
    // det([[1, 2], [1, 3]]) = 1*3 − 2*1 = 1  (the reported example)
    assert(dense(2, 2, 1, 2, 1, 3).determinant.contains(1.0))
  }

  "determinant of a 3×3" should "match cofactor expansion" in
  {
    // det([[6,1,1],[4,-2,5],[2,8,7]]) = -306
    dense(3, 3, 6, 1, 1, 4, -2, 5, 2, 8, 7).determinant match
      case Some(v) => assert(math.abs(v - -306.0) < 1e-9)
      case None    => fail("expected a determinant")
  }

  "determinant of a singular matrix" should "be 0" in
  {
    // rows [1,2] and [2,4] are linearly dependent
    dense(2, 2, 1, 2, 2, 4).determinant match
      case Some(v) => assert(math.abs(v) < 1e-9)
      case None    => fail("expected 0.0")
  }

  "determinant of a non-square matrix" should "be undefined (None)" in
  {
    assert(dense(2, 3, 1, 2, 3, 4, 5, 6).determinant.isEmpty)
  }

  // --- 4.1: Determinant node ---

  "Determinant of a dense matrix" should "reduce to a number" in
  {
    assert(Determinant(dense(2, 2, 1, 2, 1, 3)).eval(new Environment()) == Right(_Number(1.0)))
  }

  "Determinant of a non-square matrix" should "stay symbolic" in
  {
    val e = Determinant(dense(2, 3, 1, 2, 3, 4, 5, 6))
    assert(e.eval(new Environment()) == Left(e))
  }

  "Determinant of a symbolic 2×2" should "expand by cofactors and fold when bound" in
  {
    val m = literal(2, 2, a, b, c, d)
    // det([[a,b],[c,d]]) stays symbolic, then folds to 1*3 − 2*1 = 1 when bound
    assert(Determinant(m).eval(new Environment()).isLeft)
    assert(Determinant(m).eval(envWith("a" -> 1, "b" -> 2, "c" -> 1, "d" -> 3)) == Right(_Number(1.0)))
  }

  "Determinant" should "not be matrix-shaped: 2 * det(A) is a scalar product" in
  {
    // If det were matrix-shaped the parser would build MatScale and eval would fail.
    val e = parser.Parser.parse("2 * det([[1, 2], [1, 3]])")
    assert(e.successful, s"parse failed: $e")
    assert(e.get.eval(new Environment()) == Right(_Number(2.0)))
  }

  // --- 4.2: dense inverse kernel ---

  "inverse of a 2×2" should "give the adjugate over the determinant" in
  {
    // inv([[1,2],[1,3]]) = [[3,-2],[-1,1]] since det = 1
    assert(dense(2, 2, 1, 2, 1, 3).inverse.contains(dense(2, 2, 3, -2, -1, 1)))
  }

  "a matrix times its inverse" should "be the identity" in
  {
    val m  = dense(3, 3, 2, 1, 1, 1, 3, 2, 1, 0, 0)
    val mi = m.inverse.getOrElse(fail("expected an inverse"))
    val id = m.multiply(mi)
    for i <- 0 until 3; j <- 0 until 3 do
      val expected = if i == j then 1.0 else 0.0
      assert(math.abs(id(i, j) - expected) < 1e-9, s"($i,$j) = ${id(i, j)}")
  }

  "inverse of a singular matrix" should "be undefined (None)" in
  {
    assert(dense(2, 2, 1, 2, 2, 4).inverse.isEmpty)
  }

  "inverse of a non-square matrix" should "be undefined (None)" in
  {
    assert(dense(2, 3, 1, 2, 3, 4, 5, 6).inverse.isEmpty)
  }

  // --- 4.2: Inverse node ---

  "Inverse of a dense matrix" should "reduce to the dense inverse" in
  {
    assert(evalMatrix(Inverse(dense(2, 2, 1, 2, 1, 3))) == dense(2, 2, 3, -2, -1, 1))
  }

  "Inverse of a singular matrix" should "stay symbolic" in
  {
    val e = Inverse(dense(2, 2, 1, 2, 2, 4))
    assert(e.eval(new Environment()) == Left(e))
  }

  "Inverse of a symbolic 2×2" should "build adjugate/det and fold when bound" in
  {
    val m = literal(2, 2, a, b, c, d)
    assert(Inverse(m).eval(new Environment()).isLeft)
    assert(evalMatrix(Inverse(m), envWith("a" -> 1, "b" -> 2, "c" -> 1, "d" -> 3)) == dense(2, 2, 3, -2, -1, 1))
  }

  "Inverse of a symbolic 1×1" should "be the reciprocal" in
  {
    assert(evalMatrix(Inverse(literal(1, 1, a)), envWith("a" -> 4.0)) == dense(1, 1, 0.25))
  }

  // --- 4.2: 1 / A and M / N through scalar Ratio ---

  "1 / A" should "evaluate to the matrix inverse" in
  {
    val env = new Environment().withBinding("A", dense(2, 2, 1, 2, 1, 3))
    assert(evalMatrix(Ratio(_Number(1), _Variable("A")), env) == dense(2, 2, 3, -2, -1, 1))
  }

  "k / A" should "scale the inverse" in
  {
    val env = new Environment().withBinding("A", dense(2, 2, 1, 2, 1, 3))
    assert(evalMatrix(Ratio(_Number(2), _Variable("A")), env) == dense(2, 2, 6, -4, -2, 2))
  }

  "M / N" should "be M times the inverse of N" in
  {
    // A / A = I
    val a2 = dense(2, 2, 1, 2, 1, 3)
    assert(evalMatrix(Ratio(a2, a2)) == dense(2, 2, 1, 0, 0, 1))
  }

  "1 / A with a singular A" should "stay symbolic" in
  {
    assert(Ratio(_Number(1), dense(2, 2, 1, 2, 2, 4)).eval(new Environment()).isLeft)
  }

  // --- parser + reserved words ---

  "bare 'det' and 'inv'" should "be reserved words (parse errors)" in
  {
    assert(!parser.Parser.parse("det").successful)
    assert(!parser.Parser.parse("inv").successful)
  }

  "det(...) and inv(...)" should "parse to the right nodes" in
  {
    assert(parser.Parser.parse("det([[1, 2], [1, 3]])").get.isInstanceOf[Determinant])
    assert(parser.Parser.parse("inv([[1, 2], [1, 3]])").get.isInstanceOf[Inverse])
  }

  "det/inv toString" should "round-trip through the parser" in
  {
    for e <- List(Determinant(literal(2, 2, a, b, c, d)), Inverse(literal(2, 2, a, b, c, d))) do
      val reparsed = parser.Parser.parse(e.toString)
      assert(reparsed.successful, s"round-trip parse failed for: ${e.toString}")
      assert(reparsed.get == e)
  }

  // --- REPL end to end (the reported session) ---

  "the REPL" should "compute det(A), inv(A) and 1/A on a bound matrix" in
  {
    val s = cli.Session()
    s.execute("A := [[1, 2], [1, 3]]")
    assert(s.execute("det(A)") == "1.0")
    assert(s.execute("inv(A)") == "[[3.0, -2.0], [-1.0, 1.0]]")
    assert(s.execute("1 / A") == "[[3.0, -2.0], [-1.0, 1.0]]")
    assert(s.execute("eval inv(A)") == "[[3.0, -2.0], [-1.0, 1.0]]")
    assert(s.execute("simplify det(A)") == "1.0")
  }

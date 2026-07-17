package it.grypho.scala.leonardo

import core.*
import matrix.*
import org.scalatest.flatspec.AnyFlatSpec


// Feature 4.2 — matrix eigenvalue decomposition: eigen(A) → [[λ₁, λ₂, …, λₙ]].
class EigenvalueTest extends AnyFlatSpec:

  val env: Environment = new Environment()
  val tol: Double      = 1e-6

  def dense(rows: Int, cols: Int, elems: Double*): _MatrixValue =
    _MatrixValue(rows, cols, elems.toArray)

  def eigenvalues(m: _MatrixValue): Vector[_Value] =
    m.eigenDecompose.getOrElse(fail(s"eigenDecompose returned None for $m"))

  def approxReal(v: _Value, expected: Double): Boolean = v match
    case _Number(d) => math.abs(d - expected) < tol
    case _          => false

  def approxComplex(v: _Value, re: Double, im: Double): Boolean = v match
    case c: _Complex => math.abs(c.re - re) < tol && math.abs(c.im - im) < tol
    case _Number(d)  => math.abs(d - re) < tol && math.abs(im) < tol
    case _           => false

  // --- _MatrixValue.eigenDecompose ---

  "eigenDecompose of a 1×1" should "return the single element" in
  {
    val a = dense(1, 1, 7.0)
    assert(eigenvalues(a) == Vector(_Number(7.0)))
  }

  "eigenDecompose of a 2×2 symmetric" should "return the two real eigenvalues" in
  {
    // [[4, 1], [1, 3]]: tr=7, det=11, disc=5 → eigenvalues (7 ± √5)/2
    val a    = dense(2, 2, 4, 1, 1, 3)
    val eigs = eigenvalues(a)
    assert(eigs.size == 2)
    val sorted = eigs.collect { case _Number(d) => d }.sorted
    assert(sorted.size == 2, "expected 2 real eigenvalues")
    assert(math.abs(sorted.head - (7 - math.sqrt(5)) / 2) < tol)
    assert(math.abs(sorted.last - (7 + math.sqrt(5)) / 2) < tol)
  }

  "eigenDecompose of a 2×2 rotation" should "return ±i" in
  {
    // [[0, -1], [1, 0]]: tr=0, det=1, disc=-4 → eigenvalues ±i
    val a    = dense(2, 2, 0, -1, 1, 0)
    val eigs = eigenvalues(a)
    assert(eigs.size == 2)
    assert(eigs.exists(approxComplex(_, 0, 1)),  s"expected +i in $eigs")
    assert(eigs.exists(approxComplex(_, 0, -1)), s"expected -i in $eigs")
  }

  "eigenDecompose of a 3×3 diagonal" should "return the three diagonal elements" in
  {
    val a    = dense(3, 3, 2, 0, 0, 0, 3, 0, 0, 0, 5)
    val eigs = eigenvalues(a)
    val sorted = eigs.collect { case _Number(d) => d }.sorted
    assert(sorted == Vector(2.0, 3.0, 5.0))
  }

  "eigenDecompose of a 3×3 identity" should "return three 1s" in
  {
    val a    = dense(3, 3, 1, 0, 0, 0, 1, 0, 0, 0, 1)
    val eigs = eigenvalues(a)
    assert(eigs.size == 3)
    assert(eigs.forall(approxReal(_, 1.0)), s"expected all 1s, got $eigs")
  }

  "eigenDecompose of a 3×3 symmetric" should "return 2, 2+√2, 2−√2" in
  {
    // [[2, 1, 0], [1, 2, 1], [0, 1, 2]]: char poly = (2−λ)[(2−λ)²−2]=0 → λ∈{2, 2±√2}
    val a      = dense(3, 3, 2, 1, 0, 1, 2, 1, 0, 1, 2)
    val eigs   = eigenvalues(a)
    assert(eigs.size == 3)
    val sorted = eigs.collect { case _Number(d) => d }.sorted
    assert(sorted.size == 3, "expected 3 real eigenvalues")
    assert(math.abs(sorted(0) - (2 - math.sqrt(2))) < tol)
    assert(math.abs(sorted(1) - 2.0)                < tol)
    assert(math.abs(sorted(2) - (2 + math.sqrt(2))) < tol)
  }

  "eigenDecompose of a non-square matrix" should "return None" in
  {
    assert(dense(2, 3, 1, 2, 3, 4, 5, 6).eigenDecompose.isEmpty)
  }

  "eigenDecompose of a 3×3 with a complex pair" should "have one real and two complex eigenvalues" in
  {
    // [[1, -1, 0], [1, 1, 0], [0, 0, 3]]: sub-diagonal (2,1)=0 → deflate 3,
    // then 2×2 [[1,-1],[1,1]]: tr=2, det=2, disc=-4 → 1±i
    val a    = dense(3, 3, 1, -1, 0, 1, 1, 0, 0, 0, 3)
    val eigs = eigenvalues(a)
    assert(eigs.size == 3)
    assert(eigs.exists(approxReal(_, 3.0)),       s"expected 3 in $eigs")
    assert(eigs.exists(approxComplex(_, 1.0, 1.0)),  s"expected 1+i in $eigs")
    assert(eigs.exists(approxComplex(_, 1.0, -1.0)), s"expected 1-i in $eigs")
  }

  "eigenDecompose of a general 4×4" should "converge and return 4 eigenvalues" in
  {
    // Companion matrix of x^4 - 10x^3 + 35x^2 - 50x + 24 = (x-1)(x-2)(x-3)(x-4)
    val a    = dense(4, 4,
      0, 0, 0, -24,
      1, 0, 0,  50,
      0, 1, 0, -35,
      0, 0, 1,  10)
    val eigs = eigenvalues(a)
    assert(eigs.size == 4)
    val sorted = eigs.collect { case _Number(d) => d }.sorted
    assert(sorted.size == 4, "expected 4 real eigenvalues")
    for (expected, got) <- Seq(1.0, 2.0, 3.0, 4.0).zip(sorted) do
      assert(math.abs(got - expected) < 1e-4, s"expected $expected, got $got")
  }

  // --- _EigenDecomposition AST node ---

  "_EigenDecomposition" should "evaluate to Left(_Matrix(1,2,...)) for a 2×2 matrix" in
  {
    val a   = dense(2, 2, 4, 1, 1, 3)
    val res = _EigenDecomposition(a).eval(env)
    res match
      case Left(m: _Matrix) =>
        assert(m.rows == 1 && m.cols == 2, s"expected 1×2 result, got ${m.rows}×${m.cols}")
      case other => fail(s"expected Left(_Matrix(1,2,...)) but got: $other")
  }

  "_EigenDecomposition on a non-matrix argument" should "stay symbolic" in
  {
    val e = _EigenDecomposition(_Number(5))
    assert(e.eval(env) == Left(e))
  }

  "_EigenDecomposition on a non-square matrix" should "stay symbolic" in
  {
    val e = _EigenDecomposition(dense(2, 3, 1, 2, 3, 4, 5, 6))
    assert(e.eval(env) == Left(e))
  }

  "eigen toString" should "round-trip through the parser" in
  {
    val e        = _EigenDecomposition(_Variable("A"))
    val reparsed = parser.Parser.parse(e.toString)
    assert(reparsed.successful, s"round-trip failed: ${e.toString}")
    assert(reparsed.get == e)
  }

  "bare 'eigen'" should "be a reserved word" in
  {
    assert(!parser.Parser.parse("eigen").successful)
  }

  // --- REPL integration ---

  "the REPL" should "compute eigen([[4, 1], [1, 3]]) without error" in
  {
    val s   = cli.Session()
    val out = s.execute("eigen([[4, 1], [1, 3]])")
    assert(out.nonEmpty,                     "expected non-empty output")
    assert(!out.startsWith("parse error"),   s"unexpected parse error: $out")
  }

  "the REPL" should "compute eigen(A) for a bound matrix" in
  {
    val s = cli.Session()
    s.execute("A := [[2, 0], [0, 3]]")
    val out = s.execute("eigen(A)")
    assert(out.nonEmpty,                   "expected non-empty output")
    assert(!out.startsWith("parse error"), s"unexpected parse error: $out")
  }

  "at(eigen(A), 1, k)" should "extract individual eigenvalues" in
  {
    val s = cli.Session()
    s.execute("A := [[0, -1], [1, 0]]")
    // eigen returns [[i, -i]] or [[-i, i]]; at(…, 1, 1) and at(…, 1, 2) must be non-error
    val e1 = s.execute("at(eigen(A), 1, 1)")
    val e2 = s.execute("at(eigen(A), 1, 2)")
    assert(!e1.startsWith("parse error"), s"at eigen 1 failed: $e1")
    assert(!e2.startsWith("parse error"), s"at eigen 2 failed: $e2")
  }

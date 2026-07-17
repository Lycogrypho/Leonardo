package it.grypho.scala.leonardo

import core.*
import matrix.*
import org.scalatest.flatspec.AnyFlatSpec


// Feature 4.2 — matrix decompositions: lu(A) and qr(A).
class MatrixDecompositionTest extends AnyFlatSpec:

  val env: Environment = new Environment()

  def dense(rows: Int, cols: Int, elems: Double*): _MatrixValue =
    _MatrixValue(rows, cols, elems.toArray)

  def evalMatrix(e: _Expression): _MatrixValue =
    e.eval(env) match
      case Right(m: _MatrixValue) => m
      case other                  => fail(s"expected a dense matrix but got: $other")

  def approxEq(a: _MatrixValue, b: _MatrixValue, tol: Double = 1e-9): Boolean =
    a.rows == b.rows && a.cols == b.cols &&
      (0 until a.rows).forall(i => (0 until a.cols).forall(j => math.abs(a(i, j) - b(i, j)) <= tol))

  // Extract the k-th element (1-based) from a 1×n result matrix.
  def extractMatrix(result: Either[_Expression, _Value], k: Int): _MatrixValue =
    result match
      case Left(m: _Matrix) if k >= 1 && k <= m.cols =>
        m(0, k - 1) match
          case mv: _MatrixValue => mv
          case other            => fail(s"element $k is not a _MatrixValue: $other")
      case other => fail(s"expected Left(_Matrix(1, n, ...)) but got: $other")

  // --- _MatrixValue.luDecompose ---

  "luDecompose of a 2×2" should "satisfy P·A = L·U" in
  {
    val a = dense(2, 2, 1, 2, 3, 4)
    a.luDecompose match
      case None         => fail("expected a decomposition")
      case Some((l, u, p)) =>
        assert(approxEq(p.multiply(a), l.multiply(u)))
  }

  "luDecompose of a 3×3" should "satisfy P·A = L·U" in
  {
    val a = dense(3, 3, 2, 1, 1, 4, 3, 3, 8, 7, 9)
    a.luDecompose match
      case None         => fail("expected a decomposition")
      case Some((l, u, p)) =>
        assert(approxEq(p.multiply(a), l.multiply(u)))
        // L must be unit lower triangular
        for i <- 0 until 3; j <- 0 until 3 do
          if j > i  then assert(math.abs(l(i, j)) < 1e-12, s"L[$i,$j] should be 0")
          if j == i then assert(math.abs(l(i, j) - 1.0) < 1e-12, s"L[$i,$j] should be 1")
        // U must be upper triangular
        for i <- 0 until 3; j <- 0 until 3 do
          if j < i then assert(math.abs(u(i, j)) < 1e-12, s"U[$i,$j] should be 0")
  }

  "luDecompose of a singular matrix" should "return None" in
  {
    assert(dense(2, 2, 1, 2, 2, 4).luDecompose.isEmpty)
  }

  "luDecompose of a non-square matrix" should "return None" in
  {
    assert(dense(2, 3, 1, 2, 3, 4, 5, 6).luDecompose.isEmpty)
  }

  // --- _LUDecomposition AST node ---

  "lu(A) AST node" should "evaluate to Left([[L, U, P]]) for a dense matrix" in
  {
    val a   = dense(2, 2, 1, 2, 3, 4)
    val res = _LUDecomposition(a).eval(env)
    res match
      case Left(m: _Matrix) =>
        assert(m.rows == 1 && m.cols == 3, s"expected 1×3 result, got ${m.rows}×${m.cols}")
        val l = extractMatrix(res, 1)
        val u = extractMatrix(res, 2)
        val p = extractMatrix(res, 3)
        assert(approxEq(p.multiply(a), l.multiply(u)))
      case other => fail(s"expected Left(_Matrix(1,3,…)) but got: $other")
  }

  "lu(A) on a singular matrix" should "stay symbolic" in
  {
    val e = _LUDecomposition(dense(2, 2, 1, 2, 2, 4))
    assert(e.eval(env) == Left(e))
  }

  "lu(A) on a non-matrix value" should "stay symbolic" in
  {
    val e = _LUDecomposition(_Number(5))
    assert(e.eval(env) == Left(e))
  }

  "lu toString" should "round-trip through the parser" in
  {
    val e        = _LUDecomposition(_Variable("A"))
    val reparsed = parser.Parser.parse(e.toString)
    assert(reparsed.successful, s"round-trip failed: ${e.toString}")
    assert(reparsed.get == e)
  }

  "bare 'lu'" should "be a reserved word" in
  {
    assert(!parser.Parser.parse("lu").successful)
  }

  "at(lu(A), 1, k)" should "extract L, U, P individually" in
  {
    val s = cli.Session()
    s.execute("A := [[1, 2], [3, 4]]")
    val res = _LUDecomposition(_Variable("A")).eval(
      new Environment().withBinding("A", dense(2, 2, 1, 2, 3, 4))
    )
    val l = extractMatrix(res, 1)
    val u = extractMatrix(res, 2)
    val p = extractMatrix(res, 3)
    // P·A = L·U
    val a = dense(2, 2, 1, 2, 3, 4)
    assert(approxEq(p.multiply(a), l.multiply(u)))
  }

  // --- _MatrixValue.qrDecompose ---

  "qrDecompose of a 2×2" should "satisfy A = Q·R with orthogonal Q" in
  {
    val a = dense(2, 2, 1, 2, 3, 4)
    a.qrDecompose match
      case None         => fail("expected a decomposition")
      case Some((q, r)) =>
        // A = Q·R
        assert(approxEq(a, q.multiply(r)))
        // Q is orthogonal: Q^T · Q = I
        val qtq = q.transpose.multiply(q)
        for i <- 0 until 2; j <- 0 until 2 do
          assert(math.abs(qtq(i, j) - (if i == j then 1.0 else 0.0)) < 1e-9, s"Q^T·Q[$i,$j]")
        // R is upper triangular
        assert(math.abs(r(1, 0)) < 1e-12, "R[1,0] should be 0")
  }

  "qrDecompose of a 3×3" should "satisfy A = Q·R with orthogonal Q and upper-triangular R" in
  {
    val a = dense(3, 3, 12, -51, 4, 6, 167, -68, -4, 24, -41)
    a.qrDecompose match
      case None         => fail("expected a decomposition")
      case Some((q, r)) =>
        assert(approxEq(a, q.multiply(r), tol = 1e-8))
        val qtq = q.transpose.multiply(q)
        for i <- 0 until 3; j <- 0 until 3 do
          assert(math.abs(qtq(i, j) - (if i == j then 1.0 else 0.0)) < 1e-8, s"Q^T·Q[$i,$j]")
        for i <- 0 until 3; j <- 0 until 3 do
          if j < i then assert(math.abs(r(i, j)) < 1e-8, s"R[$i,$j] should be 0")
  }

  "qrDecompose of a non-square matrix (m > n)" should "satisfy A = Q·R" in
  {
    val a = dense(3, 2, 1, 2, 3, 4, 5, 6)
    a.qrDecompose match
      case None         => fail("expected a decomposition")
      case Some((q, r)) =>
        assert(q.rows == 3 && q.cols == 2)
        assert(r.rows == 2 && r.cols == 2)
        assert(approxEq(a, q.multiply(r)))
  }

  "qrDecompose when rows < cols" should "return None" in
  {
    assert(dense(2, 3, 1, 2, 3, 4, 5, 6).qrDecompose.isEmpty)
  }

  "qrDecompose of a rank-deficient matrix" should "return None" in
  {
    assert(dense(2, 2, 1, 2, 2, 4).qrDecompose.isEmpty)
  }

  // --- _QRDecomposition AST node ---

  "qr(A) AST node" should "evaluate to Left([[Q, R]]) for a dense matrix" in
  {
    val a   = dense(2, 2, 1, 2, 3, 4)
    val res = _QRDecomposition(a).eval(env)
    res match
      case Left(m: _Matrix) =>
        assert(m.rows == 1 && m.cols == 2, s"expected 1×2 result, got ${m.rows}×${m.cols}")
        val q = extractMatrix(res, 1)
        val r = extractMatrix(res, 2)
        assert(approxEq(a, q.multiply(r)))
      case other => fail(s"expected Left(_Matrix(1,2,…)) but got: $other")
  }

  "qr(A) on a rank-deficient matrix" should "stay symbolic" in
  {
    val e = _QRDecomposition(dense(2, 2, 1, 2, 2, 4))
    assert(e.eval(env) == Left(e))
  }

  "qr toString" should "round-trip through the parser" in
  {
    val e        = _QRDecomposition(_Variable("A"))
    val reparsed = parser.Parser.parse(e.toString)
    assert(reparsed.successful, s"round-trip failed: ${e.toString}")
    assert(reparsed.get == e)
  }

  "bare 'qr'" should "be a reserved word" in
  {
    assert(!parser.Parser.parse("qr").successful)
  }

  // --- REPL integration ---

  "the REPL" should "compute lu(A) and display [[L, U, P]]" in
  {
    val s = cli.Session()
    s.execute("A := [[1, 2], [3, 4]]")
    val out = s.execute("lu(A)")
    // Result is a 1×3 matrix: [[L, U, P]] displayed as a _Matrix of _MatrixValue strings
    assert(out.nonEmpty, "expected non-empty output from lu(A)")
    assert(!out.startsWith("parse error"))
  }

  "the REPL" should "compute qr(A) and display [[Q, R]]" in
  {
    val s = cli.Session()
    s.execute("A := [[1, 2], [3, 4]]")
    val out = s.execute("qr(A)")
    assert(out.nonEmpty, "expected non-empty output from qr(A)")
    assert(!out.startsWith("parse error"))
  }

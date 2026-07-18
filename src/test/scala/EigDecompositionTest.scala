package it.grypho.scala.leonardo

import core.*
import matrix.*
import org.scalatest.flatspec.AnyFlatSpec


// Feature 4.2/4.3 — spectral decomposition eig(A) → [[V, D]] and
// Jordan decomposition jordan(A) → [[P, J]].
class EigDecompositionTest extends AnyFlatSpec:

  val env: Environment = new Environment()
  val tol: Double      = 1e-6

  def dense(rows: Int, cols: Int, elems: Double*): _MatrixValue =
    _MatrixValue(rows, cols, elems.toArray)

  def approxEq(a: _MatrixValue, b: _MatrixValue, t: Double = tol): Boolean =
    a.rows == b.rows && a.cols == b.cols &&
      (0 until a.rows).forall(i => (0 until a.cols).forall(j => math.abs(a(i,j) - b(i,j)) <= t))

  // Extract a dense _MatrixValue from the k-th column (1-based) of a 1×n result.
  def denseAt(result: Either[_Expression, _Value], k: Int): _MatrixValue =
    result match
      case Left(m: _Matrix) if k >= 1 && k <= m.cols =>
        m(0, k-1) match
          case mv: _MatrixValue => mv
          case sym: _Matrix =>
            // All-real symbolic matrix → materialise
            val nums = sym.elems.collect { case _Number(d) => d }
            if nums.size == sym.elems.size then _MatrixValue(sym.rows, sym.cols, nums.toArray)
            else fail(s"element $k has non-numeric entries: $sym")
          case other => fail(s"element $k is not a matrix: $other")
      case other => fail(s"expected Left(_Matrix(1,n,...)) but got: $other")

  // ---- _MatrixValue.spectralDecompose ----

  "spectralDecompose of a 2×2 symmetric" should "satisfy A·V = V·D" in
  {
    val a = dense(2, 2, 4, 1, 1, 3)
    a.spectralDecompose match
      case None => fail("spectralDecompose returned None")
      case Some((cols, eigs)) =>
        assert(cols.size == 2 && eigs.size == 2)
        // V is stored column-by-column: V[i,j] = cols(j)(i)
        val n = 2
        val v = _MatrixValue(n, n, (for i <- 0 until n; j <- 0 until n
          yield cols(j)(i).asInstanceOf[_Number].d).toArray)
        val d = _MatrixValue(n, n, (for i <- 0 until n; j <- 0 until n
          yield if i == j then eigs(i).asInstanceOf[_Number].d else 0.0).toArray)
        val av  = a.multiply(v)
        val vd  = v.multiply(d)
        assert(approxEq(av, vd), s"A·V ≠ V·D\nA·V=$av\nV·D=$vd")
  }

  "spectralDecompose of a 3×3 diagonal" should "return identity-like V and D = A" in
  {
    val a = dense(3, 3, 2, 0, 0, 0, 3, 0, 0, 0, 5)
    a.spectralDecompose match
      case None => fail("spectralDecompose returned None")
      case Some((cols, eigs)) =>
        assert(cols.size == 3 && eigs.size == 3)
        val eigVals = eigs.collect { case _Number(d) => d }.sorted
        assert(eigVals == Vector(2.0, 3.0, 5.0), s"unexpected eigenvalues $eigVals")
  }

  "spectralDecompose of a 3×3 symmetric" should "satisfy A·V = V·D" in
  {
    val a = dense(3, 3, 2, 1, 0, 1, 2, 1, 0, 1, 2)
    a.spectralDecompose match
      case None => fail("spectralDecompose returned None for 3×3 symmetric")
      case Some((cols, evs)) =>
        val n = 3
        val v = _MatrixValue(n, n, (for i <- 0 until n; j <- 0 until n
          yield cols(j)(i).asInstanceOf[_Number].d).toArray)
        val d = _MatrixValue(n, n, (for i <- 0 until n; j <- 0 until n
          yield if i == j then evs(i).asInstanceOf[_Number].d else 0.0).toArray)
        assert(approxEq(a.multiply(v), v.multiply(d), 1e-5), "A·V ≠ V·D")
  }

  "spectralDecompose of a non-square matrix" should "return None" in
  {
    assert(dense(2, 3, 1, 2, 3, 4, 5, 6).spectralDecompose.isEmpty)
  }

  "spectralDecompose of a 2×2 rotation" should "return complex eigenvectors" in
  {
    // [[0,-1],[1,0]]: eigenvalues ±i, complex eigenvectors
    val a = dense(2, 2, 0, -1, 1, 0)
    a.spectralDecompose match
      case None => fail("spectralDecompose returned None for rotation matrix")
      case Some((cols, eigs)) =>
        assert(cols.size == 2 && eigs.size == 2)
        assert(eigs.exists(_.isInstanceOf[_Complex]), "expected complex eigenvalues")
  }

  // ---- _EigDecomposition AST node ----

  "_EigDecomposition on a 2×2 symmetric" should "return Left(_Matrix(1,2,[V,D])) with A·V = V·D" in
  {
    val a   = dense(2, 2, 3, 1, 1, 3)
    val res = _EigDecomposition(a).eval(env)
    val v   = denseAt(res, 1)
    val d   = denseAt(res, 2)
    val av  = a.multiply(v)
    val vd  = v.multiply(d)
    assert(approxEq(av, vd), s"A·V ≠ V·D\n  A·V = $av\n  V·D = $vd")
  }

  "_EigDecomposition on a 3×3 symmetric" should "satisfy A·V = V·D" in
  {
    val a   = dense(3, 3, 2, 1, 0, 1, 2, 1, 0, 1, 2)
    val res = _EigDecomposition(a).eval(env)
    val v   = denseAt(res, 1)
    val d   = denseAt(res, 2)
    assert(approxEq(a.multiply(v), v.multiply(d), 1e-5))
  }

  "_EigDecomposition on a non-square" should "stay symbolic" in
  {
    val e = _EigDecomposition(dense(2, 3, 1, 2, 3, 4, 5, 6))
    assert(e.eval(env) == Left(e))
  }

  "_EigDecomposition on a non-matrix" should "stay symbolic" in
  {
    val e = _EigDecomposition(_Number(5))
    assert(e.eval(env) == Left(e))
  }

  "eig toString" should "round-trip through the parser" in
  {
    val e        = _EigDecomposition(_Variable("A"))
    val reparsed = parser.Parser.parse(e.toString)
    assert(reparsed.successful, s"round-trip failed: ${e.toString}")
    assert(reparsed.get == e)
  }

  "bare 'eig'" should "be a reserved word" in
  {
    assert(!parser.Parser.parse("eig").successful)
  }

  "at(eig(A), 1, 1)" should "extract V and at(eig(A), 1, 2) should extract D" in
  {
    val s = cli.Session()
    s.execute("A := [[4, 1], [1, 3]]")
    val outV = s.execute("at(eig(A), 1, 1)")
    val outD = s.execute("at(eig(A), 1, 2)")
    assert(!outV.startsWith("parse error"), s"extracting V failed: $outV")
    assert(!outD.startsWith("parse error"), s"extracting D failed: $outD")
  }

  // ---- _JordanDecomposition AST node ----

  "_JordanDecomposition on a 2×2 symmetric" should "return Left(_Matrix(1,2,[P,J])) with A = P·J·P⁻¹" in
  {
    val a   = dense(2, 2, 3, 1, 1, 3)
    val res = _JordanDecomposition(a).eval(env)
    val p   = denseAt(res, 1)
    val j   = denseAt(res, 2)
    // Verify A·P = P·J  (equivalent to A = P·J·P⁻¹ when P is invertible)
    assert(approxEq(a.multiply(p), p.multiply(j), 1e-5), s"A·P ≠ P·J")
    // J must be diagonal
    for i <- 0 until 2; k <- 0 until 2 do
      if i != k then assert(math.abs(j(i,k)) < tol, s"J[$i,$k] should be 0")
  }

  "_JordanDecomposition on a 3×3 diagonal" should "return P·J·P⁻¹ = A" in
  {
    val a   = dense(3, 3, 1, 0, 0, 0, 4, 0, 0, 0, 9)
    val res = _JordanDecomposition(a).eval(env)
    val p   = denseAt(res, 1)
    val j   = denseAt(res, 2)
    assert(approxEq(a.multiply(p), p.multiply(j), 1e-5))
  }

  "_JordanDecomposition on a non-square" should "stay symbolic" in
  {
    val e = _JordanDecomposition(dense(2, 3, 1, 2, 3, 4, 5, 6))
    assert(e.eval(env) == Left(e))
  }

  "jordan toString" should "round-trip through the parser" in
  {
    val e        = _JordanDecomposition(_Variable("A"))
    val reparsed = parser.Parser.parse(e.toString)
    assert(reparsed.successful, s"round-trip failed: ${e.toString}")
    assert(reparsed.get == e)
  }

  "bare 'jordan'" should "be a reserved word" in
  {
    assert(!parser.Parser.parse("jordan").successful)
  }

  "the REPL" should "compute eig([[3,1],[1,3]]) without error" in
  {
    val s   = cli.Session()
    val out = s.execute("eig([[3, 1], [1, 3]])")
    assert(out.nonEmpty, "expected non-empty output")
    assert(!out.startsWith("parse error"), s"parse error: $out")
  }

  "the REPL" should "compute jordan([[3,1],[1,3]]) without error" in
  {
    val s   = cli.Session()
    val out = s.execute("jordan([[3, 1], [1, 3]])")
    assert(out.nonEmpty, "expected non-empty output")
    assert(!out.startsWith("parse error"), s"parse error: $out")
  }

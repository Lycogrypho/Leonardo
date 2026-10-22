package it.grypho.scala.leonardo
package matrix

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

  "spectralDecompose of a matrix in tiny units" should "find the eigenvectors it finds at unit scale (F_0125)" in
  {
    // 1e-14·[[1, 1], [0, 2]]: the pivot floor 1e-12 sat above every entry of A − λI, and both
    // eigenvectors came back (1, 0).
    for k <- Seq(1.0, 1e-14) do
      val a = dense(2, 2, k, k, 0.0, 2 * k)
      a.spectralDecompose match
        case None => fail(s"declined at scale $k")
        case Some((vectors, values)) =>
          for (v, lambda) <- vectors.zip(values) do
            val (vs, l) = (v.collect { case _Number(d) => d }, lambda match { case _Number(d) => d; case o => fail(s"$o") })
            val av = Vector(a(0, 0) * vs(0) + a(0, 1) * vs(1), a(1, 0) * vs(0) + a(1, 1) * vs(1))
            assert(av.zip(vs).forall((x, y) => math.abs(x - l * y) <= 1e-9 * math.abs(l)), s"scale $k: A·v ≠ λv for $vs, $l")
  }

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

  "_EigDecomposition on a defective matrix" should "stay symbolic rather than return a singular V" in
  {
    // [[1, 1], [-1, 3]] and the Jordan block [[2, 1], [0, 2]] have one eigenvalue of
    // multiplicity 2 and a one-dimensional eigenspace: no basis of eigenvectors exists, so a V
    // whose two columns are the same vector is not a decomposition (found by LA 06).
    for a <- List(dense(2, 2, 1, 1, -1, 3), dense(2, 2, 2, 1, 0, 2)) do
      val e = _EigDecomposition(a)
      assert(e.eval(env) == Left(e), s"eig($a) should stay unevaluated")
      val j = _JordanDecomposition(a)
      assert(j.eval(env) == Left(j), s"jordan($a) should stay unevaluated")
  }

  "_EigDecomposition on a repeated but diagonalizable eigenvalue" should "return independent eigenvectors" in
  {
    // -I, and Austin's Activity 4.2.18(c), whose eigenvalue -2 has multiplicity 2 and a plane of
    // eigenvectors: V must be a basis, not the same direction twice.
    for a <- List(dense(2, 2, -1, 0, 0, -1), dense(3, 3, -1, 0, 2, -2, -2, -4, 0, 0, -2)) do
      val res = _EigDecomposition(a).eval(env)
      val v   = denseAt(res, 1)
      assert(approxEq(a.multiply(v), v.multiply(denseAt(res, 2))))
      assert(isJordanInvertible(v), s"V = $v is not a basis")
  }

  "_EigDecomposition on a rotation and on a nearly repeated eigenvalue" should "still answer" in
  {
    // The rotation's V is complex and a basis; [[1, 2], [0, 1.000001]] is diagonalizable, its
    // V's relative determinant 5e-7 well above the defective case's 5.3e-9.
    for a <- List(dense(2, 2, 0, -1, 1, 0), dense(2, 2, 1, 2, 0, 1.000001)) do
      _EigDecomposition(a).eval(env) match
        case Left(m: _Matrix) => assert(m.rows == 1 && m.cols == 2, s"eig($a) = $m")
        case other            => fail(s"eig($a) should answer, got $other")
  }

  "_EigDecomposition on Austin's defective 3x3" should "stay symbolic" in
  {
    // Its double root comes out of the QR iteration as -2 ± 3e-8 i, so V is complex and nearly
    // singular: the complex path must be tested too.
    // Activity 4.2.18(d): the same characteristic polynomial as (c), one eigenvector for -2.
    val e = _EigDecomposition(dense(3, 3, -5, -2, -6, -2, -2, -4, 2, 1, 2))
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

  // Indexing into eig's result from the REPL lives in matrix/EigDecompositionReplTest.scala
  // (issue 5.2 phase 1.1).

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

  // ---- issue 2.4: scale-relative Jordan invertibility test ----

  "isJordanInvertible on an orthogonal V" should "accept it" in
  {
    assert(isJordanInvertible(dense(2, 2, 1, 0, 0, 1)))
  }

  "isJordanInvertible on a well-conditioned but tiny-magnitude V" should "accept it" in
  {
    // Columns (1e-6, 0) and (0, 1e-6): perfectly orthogonal, so genuinely invertible.
    // |det| = 1e-12 fails the OLD absolute `|det| > 1e-10` test (wrongly "defective");
    // the scale-relative ratio |det| / ∏‖colⱼ‖ = 1e-12 / 1e-12 = 1 correctly accepts it.
    assert(isJordanInvertible(dense(2, 2, 1e-6, 0, 0, 1e-6)))
  }

  "isJordanInvertible on a well-conditioned but huge-magnitude V" should "accept it" in
  {
    assert(isJordanInvertible(dense(2, 2, 1e6, 0, 0, 1e6)))
  }

  "isJordanInvertible on near-parallel columns" should "reject it" in
  {
    // Columns (1, 0) and (1, 1e-12): almost linearly dependent → ratio ≈ 1e-12 < tol.
    assert(!isJordanInvertible(dense(2, 2, 1, 1, 0, 1e-12)))
  }

  "isJordanInvertible on identical columns" should "reject it (exactly singular)" in
  {
    assert(!isJordanInvertible(dense(2, 2, 1, 1, 0, 0)))
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


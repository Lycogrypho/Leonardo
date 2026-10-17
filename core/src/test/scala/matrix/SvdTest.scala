package it.grypho.scala.leonardo
package matrix

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random


/** F_0061 — the singular value decomposition `svd(A)` and the three answers read off it:
 *  `pinv`, `rank`, `cond`.
 *
 *  An SVD has a sign (and, for repeated values, a basis) ambiguity, so the factors are never
 *  compared entry by entry: the suite checks what defines them — the reconstruction
 *  `U·S·Vᵀ = A`, orthonormal `U` and `V`, singular values non-negative and descending. Beyond
 *  that it pins Decision B's tolerance, **relative** to the largest singular value so rescaling
 *  never moves a rank (the units rule), and the Moore–Penrose identities, which are what make
 *  a pseudo-inverse the pseudo-inverse rather than some generalised inverse.
 */
class SvdTest extends AnyFlatSpec:

  private val env = new Environment()

  private def dense(rows: Int, cols: Int, elems: Double*): _MatrixValue =
    _MatrixValue(rows, cols, elems.toArray)

  private def random(rows: Int, cols: Int, seed: Long): _MatrixValue =
    val rng = new Random(seed)
    _MatrixValue(rows, cols, Array.fill(rows * cols)(rng.nextDouble() * 2 - 1))

  /** `GᵀG` for an `r×n` matrix `G` of small integers: symmetric, rank `r` (almost surely). */
  private def lowRank(r: Int, n: Int, seed: Long): _MatrixValue =
    val rng = new Random(seed)
    val g   = Array.fill(r, n)((rng.nextInt(5) - 2).toDouble)
    _MatrixValue(n, n, Array.tabulate(n * n)(idx => (0 until r).map(t => g(t)(idx / n) * g(t)(idx % n)).sum))

  "svd" should "decompose a rank-deficient matrix whose dead directions outnumber what an unpivoted completion finds (F_0117)" in {
    // Rank 20 of 40: U has 20 live columns and must be completed by 20 more. The completion took
    // unit vectors in order with a fixed 0.5 threshold and ran out of candidates, so svd declined.
    for (r, n, seed) <- Seq((20, 40, 2L), (15, 30, 1L), (30, 60, 5L), (1, 50, 7L)) do
      val a = lowRank(r, n, seed)
      a.svd match
        case Some((u, s, v)) =>
          assert(s.count(_ > 1e-9 * s.head) == r, s"rank $r of $n: $s")
          assert(u.rows == n && u.cols == n && v.cols == n)
        case None => fail(s"rank $r of $n declined")
  }

  "completeBasis" should "always complete an orthonormal base, by pivoting on the largest residual" in {
    for (r, n, seed) <- Seq((20, 40, 2L), (1, 7, 3L), (63, 64, 4L)) do
      // An orthonormal base of r directions spread over every coordinate: Q from qr of a random matrix.
      val base = random(n, r, seed).qrDecompose.map(_._1).getOrElse(fail("qr"))
      val cols = Vector.tabulate(r)(j => Array.tabulate(n)(i => base(i, j)))
      val full = _MatrixValue.completeBasis(cols, n, n).getOrElse(fail(s"$r of $n not completed"))
      assert(full.size == n)
      for p <- 0 until n; q <- p until n do
        val d = (0 until n).map(i => full(p)(i) * full(q)(i)).sum
        assert(math.abs(d - (if p == q then 1.0 else 0.0)) <= 1e-12, s"$r of $n: columns $p, $q dot $d")
  }

  private def parse(s: String, exact: Option[Int] = None): _Expression =
    Parser.parse(s, exact) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for \"$s\": $other")

  private def evaluated(s: String, e: Environment = env, exact: Option[Int] = None): Either[_Expression, _Value] =
    parse(s, exact).eval(e)

  private def number(s: String, e: Environment = env): Double = evaluated(s, e) match
    case Right(_Number(d)) => d
    case other             => fail(s"$s: expected a number, got $other")

  private def matrix(s: String, e: Environment = env): _MatrixValue = evaluated(s, e) match
    case Right(m: _MatrixValue) => m
    case other                  => fail(s"$s: expected a dense matrix, got $other")

  private def factors(s: String): (_MatrixValue, _MatrixValue, _MatrixValue) = evaluated(s) match
    case Left(_Matrix(1, 3, Vector(u: _MatrixValue, sm: _MatrixValue, v: _MatrixValue))) => (u, sm, v)
    case other => fail(s"$s: expected [[U, S, V]], got $other")

  private def decompose(a: _MatrixValue): (_MatrixValue, Vector[Double], _MatrixValue) =
    a.svd.getOrElse(fail(s"expected an SVD of $a"))

  private def norm(a: _MatrixValue): Double = math.sqrt(a.toVector.map(x => x * x).sum)

  /** A matrix as a literal the parser reads back at full precision. */
  private def literalOf(a: _MatrixValue): String =
    (0 until a.rows).map(i => (0 until a.cols).map(j => _Number.render(a(i, j))).mkString("[", ", ", "]")).mkString("[", ", ", "]")

  private def assertClose(a: _MatrixValue, b: _MatrixValue, tol: Double = 1e-10): Unit =
    assert(a.rows == b.rows && a.cols == b.cols, s"shape: ${a.rows}x${a.cols} vs ${b.rows}x${b.cols}")
    val scale = math.max(1.0, norm(b))
    for i <- 0 until a.rows; j <- 0 until a.cols do
      assert(math.abs(a(i, j) - b(i, j)) <= tol * scale, s"($i,$j): expected ${b(i, j)} but got ${a(i, j)}")

  private def assertOrthonormalColumns(q: _MatrixValue): Unit =
    assertClose(q.transpose.multiply(q), _MatrixValue.identity(q.cols))

  private def diag(sigma: Vector[Double]): _MatrixValue =
    val k = sigma.size
    _MatrixValue(k, k, Array.tabulate(k * k)(idx => if idx / k == idx % k then sigma(idx / k) else 0.0))

  /** Everything an economy SVD must satisfy, for any input. */
  private def assertValidSvd(a: _MatrixValue): Unit =
    val (u, sigma, v) = decompose(a)
    val k = math.min(a.rows, a.cols)
    assert(u.rows == a.rows && u.cols == k && v.rows == a.cols && v.cols == k && sigma.size == k)
    assert(sigma.forall(_ >= 0), s"negative singular value in $sigma")
    assert(sigma == sigma.sortBy(-_), s"not descending: $sigma")
    assertOrthonormalColumns(u)
    assertOrthonormalColumns(v)
    assertClose(u.multiply(diag(sigma)).multiply(v.transpose), a)

  // --- the dense kernel ---

  "svd" should "give the acceptance singular values √45 and √5 for [[3, 0], [4, 5]]" in
  {
    val (_, sigma, _) = decompose(dense(2, 2, 3, 0, 4, 5))
    assert(math.abs(sigma(0) - math.sqrt(45)) < 1e-12)
    assert(math.abs(sigma(1) - math.sqrt(5)) < 1e-12)
  }

  it should "decompose tall, wide, square and rank-deficient matrices" in
  {
    assertValidSvd(random(8, 3, 1))
    assertValidSvd(random(3, 8, 2))
    assertValidSvd(random(5, 5, 3))
    assertValidSvd(random(6, 2, 4).multiply(random(2, 4, 5)))   // rank 2 in a 6x4
    assertValidSvd(random(1, 6, 6))
    assertValidSvd(random(6, 1, 7))
  }

  it should "decompose every rank-deficient matrix, not just the lucky ones (F_0099)" in
  {
    // A zero singular value's column is pure rounding noise, whose angle to the live columns
    // never settles; the sweep used to keep rotating it until SvdMaxSweeps and then decline.
    // [[1, 2, 3], [2, 4, 6], [1, 0, 1]] is rank 2 and was declined outright.
    assertValidSvd(dense(3, 3, 1, 2, 3, 2, 4, 6, 1, 0, 1))
    assert(number("rank([[1, 2, 3], [2, 4, 6], [1, 0, 1]])") == 2.0)
    for n <- 3 to 8; k <- 1 until n; seed <- 1 to 5 do
      val a = random(n, k, 1000L * n + 10 * k + seed).multiply(random(k, n, 7L * n + k + 31 * seed))
      assert(a.svd.isDefined, s"rank-$k ${n}x$n, seed $seed: declined")
      assertValidSvd(a)
      assert(number(s"rank(${literalOf(a)})") == k.toDouble, s"rank-$k ${n}x$n, seed $seed")
  }

  it should "handle repeated singular values, the zero matrix and a single entry" in
  {
    val c = math.cos(0.7)
    val s = math.sin(0.7)
    assertValidSvd(dense(2, 2, c, -s, s, c))          // a rotation: σ = 1, 1
    assertValidSvd(_MatrixValue.identity(4))
    assertValidSvd(dense(2, 3, 0, 0, 0, 0, 0, 0))
    assert(decompose(dense(2, 3, 0, 0, 0, 0, 0, 0))._2 == Vector(0.0, 0.0))
    assert(decompose(dense(1, 1, -3))._2 == Vector(3.0))
  }

  it should "give a transpose the same singular values" in
  {
    val a = random(5, 3, 8)
    val (s1, s2) = (decompose(a)._2, decompose(a.transpose)._2)
    s1.zip(s2).foreach((x, y) => assert(math.abs(x - y) < 1e-12))
  }

  it should "resolve a graded matrix's small singular values to high relative accuracy" in
  {
    // One-sided Jacobi works on the columns themselves, never on AᵀA, so a tiny singular
    // value is not swamped by the large one's rounding (the conditioning rule).
    val (_, sigma, _) = decompose(dense(3, 3, 1e10, 0, 0, 0, 1, 0, 0, 0, 1e-10))
    assert(math.abs(sigma(2) - 1e-10) <= 1e-6 * 1e-10, s"σ_min = ${sigma(2)}")
  }

  it should "decline a matrix with a non-finite entry" in
  {
    assert(dense(2, 2, 1, Double.NaN, 0, 1).svd.isEmpty)
    assert(dense(2, 2, 1, Double.PositiveInfinity, 0, 1).svd.isEmpty)
  }

  it should "refuse past MaxSvdWork before doing any work, and say so" in
  {
    assert(_MatrixValue.svdWork(200, 200) <= _MatrixValue.MaxSvdWork)    // the demo's size
    assert(_MatrixValue.svdWork(1000, 100) <= _MatrixValue.MaxSvdWork)
    assert(_MatrixValue.svdWork(1001, 100) > _MatrixValue.MaxSvdWork)
    assert(_MatrixValue.svdWork(100, 1001) == _MatrixValue.svdWork(1001, 100))
    val big = _MatrixValue(1001, 100, Array.fill(1001 * 100)(1.0))
    assert(big.svd.isEmpty && big.transpose.svd.isEmpty)
    val reason = svdDecline(Right(big), Limits.Default).getOrElse(fail("expected a reason"))
    assert(reason.contains("1001x100") && reason.contains("work cap"), reason)
    assert(svdDecline(Right(dense(2, 2, 1, Double.NaN, 0, 1)), Limits.Default).exists(_.contains("non-finite")))
    assert(svdDecline(Right(dense(2, 2, 1, 0, 0, 1)), Limits.Default).isEmpty)
  }

  // --- the node ---

  it should "evaluate to the economy row [[U, S, V]] by default" in
  {
    val (u, s, v) = factors("svd([[1, 2], [3, 4], [5, 6], [7, 8]])")
    assert((u.rows, u.cols, s.rows, s.cols, v.rows, v.cols) == (4, 2, 2, 2, 2, 2))
    assertClose(u.multiply(s).multiply(v.transpose), dense(4, 2, 1, 2, 3, 4, 5, 6, 7, 8))
  }

  it should "evaluate svd(A, full) to square orthogonal U and V and an m×n S" in
  {
    val a = dense(4, 2, 1, 2, 3, 4, 5, 6, 7, 8)
    val (u, s, v) = factors("svd([[1, 2], [3, 4], [5, 6], [7, 8]], full)")
    assert((u.rows, u.cols, s.rows, s.cols, v.rows, v.cols) == (4, 4, 4, 2, 2, 2))
    assertOrthonormalColumns(u)
    assertOrthonormalColumns(v)
    assertClose(u.multiply(s).multiply(v.transpose), a)
    // The completing columns span the left null space: U₃, U₄ are orthogonal to A's range.
    val (wu, ws, wv) = factors("svd([[1, 2, 3], [4, 5, 6]], full)")
    assert((wu.rows, wu.cols, ws.rows, ws.cols, wv.rows, wv.cols) == (2, 2, 2, 3, 3, 3))
    assertOrthonormalColumns(wv)
    assertClose(dense(2, 3, 1, 2, 3, 4, 5, 6).multiply(dense(3, 1, wv(0, 2), wv(1, 2), wv(2, 2))),
      dense(2, 1, 0, 0))
  }

  it should "demote an exact matrix and leave a symbolic one unevaluated" in
  {
    evaluated("svd([[3, 0], [4, 5]])", exact = Some(30)) match
      case Left(_Matrix(1, 3, Vector(_, sm: _MatrixValue, _))) => assert(math.abs(sm(0, 0) - math.sqrt(45)) < 1e-12)
      case other => fail(s"expected the factors, got $other")
    assert(evaluated("svd([[x, 1], [0, 1]])") match { case Left(_: _Svd) => true; case _ => false })
    assert(evaluated("pinv([[x, 1], [0, 1]])") match { case Left(_: _PInv) => true; case _ => false })
    assert(evaluated("rank([[x, 1], [0, 1]])") match { case Left(_: _Rank) => true; case _ => false })
    assert(evaluated("cond([[x, 1], [0, 1]])") match { case Left(_: _Cond) => true; case _ => false })
  }

  it should "echo a refused call as written rather than spell out its operand" in
  {
    assert(parse("ones(1001, 100)").eval(env).isRight, "the operand must really be a matrix")
    val call = parse("svd(ones(1001, 100))")
    assert(call.eval(env) == Left(call))
    assert(parse("rank([[1, 0], [0, 1]], 2)").eval(env) == Left(parse("rank([[1, 0], [0, 1]], 2)")))
  }

  it should "round-trip all six spellings through toString" in
  {
    for s <- List("svd(A)", "svd(A, full)", "pinv(A)", "rank(A)", "rank(A, 0.01)", "cond(A)") do
      val e = parse(s)
      assert(parse(e.toString) == e, s"$s -> $e")
    assert(parse("svd(A, full)") == _Svd(_Variable("A"), full = true))
  }

  it should "reserve its four names but not the slot word full" in
  {
    for w <- List("svd", "pinv", "rank", "cond") do assert(Parser.ReservedWords.contains(w), w)
    assert(!Parser.ReservedWords.contains("full"))
    assert(parse("full + 1").freeVars.contains("full"))
  }

  // --- rank ---

  "rank" should "give the acceptance rank 1 for [[1, 2], [2, 4]] and the obvious ranks elsewhere" in
  {
    assert(number("rank([[1, 2], [2, 4]])") == 1.0)
    assert(number("rank(eye(3))") == 3.0)
    assert(number("rank(zeros(2, 3))") == 0.0)
    assert(number("rank([[1, 2, 3], [4, 5, 6], [7, 8, 9]])") == 2.0)
  }

  it should "not depend on the matrix's units (the tolerance is relative to σ_max)" in
  {
    val a = random(5, 2, 9).multiply(random(2, 4, 10))
    for k <- List(1e-9, 1.0, 1e9) do
      val scaled = a.scale(k)
      assert(numericalRank(decompose(scaled)._2, rankTolerance(env)) == 2, s"k = $k")
      assert(conditionNumber(decompose(scaled)._2, 5, 4).isPosInfinity, s"k = $k")
  }

  it should "count a singular value as zero at or below 0.5·10^-precision·σ_max (Decision B)" in
  {
    assert(number("rank([[1, 0], [0, 0.0000001]])") == 1.0)                       // 1e-7 < 5e-6
    assert(number("rank([[1, 0], [0, 0.0000001]])", new Environment(8)) == 2.0)  // 1e-7 > 5e-9
    assert(number("rank([[1, 0], [0, 0.0001]])") == 2.0)
  }

  it should "take an explicit tolerance relative to σ_max, and refuse one outside [0, 1)" in
  {
    assert(number("rank([[1, 0], [0, 0.001]], 0.01)") == 1.0)
    assert(number("rank([[1, 0], [0, 0.001]], 0.0001)") == 2.0)
    assert(number("rank([[1000, 0], [0, 1]], 0.01)") == 1.0)
    assert(evaluated("rank([[1, 0], [0, 1]], -1)") match { case Left(_: _Rank) => true; case _ => false })
    assert(evaluated("rank([[1, 0], [0, 1]], 2)") match { case Left(_: _Rank) => true; case _ => false })
    assert(evaluated("rank([[1, 0], [0, 1]], t)") match { case Left(_: _Rank) => true; case _ => false })
  }

  it should "answer an exact integer for an exact matrix (the numeric tier rule)" in
  {
    assert(evaluated("rank([[1, 2], [2, 4]])", exact = Some(30)) == Right(_Rational(1)))
  }

  // --- cond ---

  "cond" should "be σ_max/σ_min, 1 for an orthogonal matrix, and inf for a singular one" in
  {
    assert(math.abs(number("cond([[1, 0], [0, 0.001]])") - 1000) < 1e-9)
    assert(math.abs(number("cond(eye(3))") - 1) < 1e-12)
    assert(math.abs(number("cond([[3, 0], [4, 5]])") - 3) < 1e-12)       // √45/√5
    assert(number("cond([[1, 2], [2, 4]])").isPosInfinity)
    assert(number("cond(zeros(2, 2))").isPosInfinity)
  }

  it should "stay finite past Decision B's cut-off, which is rank's question and not cond's" in
  {
    // rank already counts 1e-8 as zero at precision 5; cond reports how close to singular.
    assert(number("rank([[1, 0], [0, 0.00000001]])") == 1.0)
    assert(math.abs(number("cond([[1, 0], [0, 0.00000001]])") / 1e8 - 1) < 1e-9)
  }

  it should "use the min(m, n) singular values of a non-square matrix" in
  {
    val a = random(6, 3, 11)
    val sigma = decompose(a)._2
    assert(math.abs(conditionNumber(sigma, 6, 3) - sigma(0) / sigma(2)) < 1e-12)
  }

  // --- pinv ---

  "pinv" should "be the inverse of a square non-singular matrix" in
  {
    assertClose(matrix("pinv([[4, 7], [2, 6]])"), matrix("inv([[4, 7], [2, 6]])"))
  }

  it should "satisfy the four Moore–Penrose identities on a rank-deficient matrix" in
  {
    val a = random(6, 2, 12).multiply(random(2, 4, 13))
    val p = pseudoInverse(a, rankTolerance(env), Limits.Default).getOrElse(fail("expected a pseudo-inverse"))
    assert(p.rows == 4 && p.cols == 6)
    assertClose(a.multiply(p).multiply(a), a, 1e-9)
    assertClose(p.multiply(a).multiply(p), p, 1e-9)
    assertClose(a.multiply(p).transpose, a.multiply(p), 1e-9)
    assertClose(p.multiply(a).transpose, p.multiply(a), 1e-9)
  }

  it should "be the least-squares solver (AᵀA)⁻¹Aᵀ for a tall full-rank matrix" in
  {
    val a = random(7, 3, 14)
    val p = pseudoInverse(a, rankTolerance(env), Limits.Default).getOrElse(fail("expected a pseudo-inverse"))
    val normal = a.transpose.multiply(a).inverse.getOrElse(fail("AᵀA singular")).multiply(a.transpose)
    assertClose(p, normal, 1e-9)
  }

  it should "discard the directions rank discards, at the same tolerance" in
  {
    val p = matrix("pinv([[1, 0], [0, 0.0000001]])")
    assertClose(p, dense(2, 2, 1, 0, 0, 0))
    val p8 = matrix("pinv([[1, 0], [0, 0.0000001]])", new Environment(8))
    assert(math.abs(p8(1, 1) - 1e7) < 1e-3)
  }

  it should "transpose the shape of a wide matrix and dispatch as a matrix in a product" in
  {
    val p = matrix("pinv([[1, 2, 3], [4, 5, 6]])")
    assert(p.rows == 3 && p.cols == 2)
    assertClose(matrix("[[1, 2, 3], [4, 5, 6]] * pinv([[1, 2, 3], [4, 5, 6]])"), _MatrixValue.identity(2))
  }

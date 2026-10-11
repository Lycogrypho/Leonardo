package it.grypho.scala.leonardo
package probability

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** F_0067 (MPC T3-03): seeded draws by inversion and the maximal-length PRBS.
 *
 *  Runs on both platforms, which is the point of the golden values: they were captured on the JVM
 *  and the Scala.js run asserts the same numbers — bit for bit for the uniform stream and the PRBS,
 *  to rounding for a draw that goes through `exp` (Decision C).  Statistical checks use tolerances
 *  derived from the standard errors (±5σ), never guessed ones.
 */
class SamplingTest extends AnyFlatSpec:

  private val env = new Environment()

  private def parse(src: String): _Expression =
    val r = Parser.parse(src)
    assert(r.successful, s"parse failed for \"$src\": $r")
    r.get

  private def dist(src: String): _Distribution = parse(src).eval(env) match
    case Right(d: _Distribution) => d
    case other                   => fail(s"$src is not a distribution: $other")

  private def draws(src: String, n: Int, seed: Long, offset: Long = 0L): Vector[Double] =
    drawSamples(dist(src), n, seed, offset).fold(f => fail(s"declined: $f"), identity)

  private def mean(xs: Seq[Double]): Double = xs.sum / xs.size
  private def variance(xs: Seq[Double]): Double =
    val m = mean(xs)
    xs.map(x => (x - m) * (x - m)).sum / (xs.size - 1)

  // ── the generator ──

  "the uniform stream" should "lie strictly inside (0, 1)" in
  {
    val us = (0L until 20000L).map(SplitMix64.uniform(7L, _))
    assert(us.forall(u => u > 0.0 && u < 1.0))
  }

  it should "keep neighbouring seeds' streams apart, never one shifted by one step" in
  {
    // The defect a seed used as a POSITION on the counter would have: the stream for s + 1 is the
    // stream for s moved along by one, so two "independent" runs share all but one value.
    for s <- List(0L, 1L, 41L, -3L) do
      val a = (0L until 200L).map(SplitMix64.uniform(s, _))
      val b = (0L until 200L).map(SplitMix64.uniform(s + 1, _))
      assert(a.tail.zip(b).count((x, y) => x == y) == 0, s"seed $s + 1 repeats seed $s shifted")
      assert(a.zip(b.tail).count((x, y) => x == y) == 0, s"seed $s repeats seed $s + 1 shifted")
  }

  it should "give uncorrelated streams for neighbouring seeds" in
  {
    val n  = 10000
    val a  = (0L until n.toLong).map(SplitMix64.uniform(1L, _))
    val b  = (0L until n.toLong).map(SplitMix64.uniform(2L, _))
    val (ma, mb) = (mean(a), mean(b))
    val cov = a.zip(b).map((x, y) => (x - ma) * (y - mb)).sum / (n - 1)
    val r   = cov / math.sqrt(variance(a) * variance(b))
    assert(math.abs(r) <= 5.0 / math.sqrt(n), s"correlation $r")
  }

  // ── the cross-platform pin (Decision C) ──
  // Captured on the JVM 2026-10-10; this suite runs on Node too, and must find the same numbers.

  "seed 42" should "give the same integer stream, uniforms and PRBS on every platform, bit for bit" in
  {
    assert((0L until 4L).map(SplitMix64.at(42L, _)) ==
      List(-7450291807549245335L, 2958219263312191191L, 3069497704473277141L, 885919558081284366L))
    assert((0L until 4L).map(SplitMix64.uniform(42L, _)) ==
      List(0.5961188718302077, 0.16036538759857727, 0.1663978039814598, 0.04802579547595637))
    assert(prbsSequence(6, 16, 42L) ==
      Right(Vector(1, -1, -1, 1, -1, 1, 1, -1, 1, 1, 1, -1, 1, 1, -1, -1).map(_.toDouble)))
    assert(draws("poisson(4)", 6, 42L) == Vector(4.0, 2.0, 2.0, 1.0, 9.0, 2.0))
    assert(draws("binomial(10, 0.3)", 6, 42L) == Vector(3.0, 2.0, 2.0, 1.0, 6.0, 2.0))
  }

  it should "give the same continuous draws on every platform, to rounding" in
  {
    // Through the cdf's exp/log, which each platform computes itself: equal to 1e-12, not to the bit.
    def near(xs: Vector[Double], ys: List[Double]) =
      xs.size == ys.size && xs.zip(ys).forall((x, y) => math.abs(x - y) <= 1e-12 * math.max(1.0, math.abs(y)))
    assert(near(draws("normal(0, 1)", 4, 42L),
      List(0.24331387536681248, -0.9929572767442814, -0.9684982191711562, -1.6643045132309826)))
    assert(near(draws("gammadist(2, 3)", 3, 42L),
      List(0.6692884363396687, 0.23769444399352066, 0.2434284189712877)))
  }

  // ── draw ──

  "drawSamples" should "meet T3-03's acceptance: mean and variance of 10^5 standard normals" in
  {
    val n  = 100000
    val xs = draws("normal(0, 1)", n, 2026L)
    assert(math.abs(mean(xs)) <= 5.0 / math.sqrt(n), s"mean ${mean(xs)}")
    assert(math.abs(variance(xs) - 1.0) <= 5.0 * math.sqrt(2.0 / n), s"variance ${variance(xs)}")
  }

  it should "continue a stream exactly from an offset (Decision B)" in
  {
    for fam <- List("normal(2, 0.5)", "poisson(4)") do
      val whole = draws(fam, 8, 99L)
      assert(draws(fam, 5, 99L) ++ draws(fam, 3, 99L, 5L) == whole, fam)
      assert(draws(fam, 3, 99L, 5L) == whole.drop(5), fam)
  }

  it should "give integers in the support for a discrete family, with the right mean" in
  {
    val n  = 10000
    val bs = draws("binomial(10, 0.3)", 2000, 5L)
    assert(bs.forall(x => x == math.floor(x) && x >= 0 && x <= 10))
    val ps = draws("poisson(4)", n, 6L)
    assert(ps.forall(x => x == math.floor(x) && x >= 0))
    assert(math.abs(mean(ps) - 4.0) <= 5.0 * math.sqrt(4.0 / n), s"mean ${mean(ps)}")
  }

  it should "refuse a count outside 1 … MaxDrawCount and a negative offset" in
  {
    val d = dist("normal(0, 1)")
    assert(drawSamples(d, 0, 1L) == Left(SamplingFailure.InvalidCount(0, MaxDrawCount)))
    assert(drawSamples(d, MaxDrawCount + 1, 1L).isLeft)
    assert(drawSamples(d, 3, 1L, -1L) == Left(SamplingFailure.InvalidInteger("the offset")))
  }

  // ── prbs ──

  "the PRBS table" should "give every order 2 … 20 a maximal period with one more 1 than 0" in
  {
    // The IntegralTableTest rule: a table is checked entry by entry against what it claims, not
    // proofread.  Maximal means the register visits all 2^n - 1 non-zero states in one period;
    // the n-bit windows of the output ARE those states, so every one of them must appear among
    // the p windows of one period -- a wrong tap repeats a window early and leaves one out.
    for order <- MinPrbsOrder to MaxPrbsOrder do
      val p    = (1 << order) - 1
      val bits = lfsrBits(order, 1, p + order - 1)
      val seen = new java.util.BitSet(p + 1)
      var w = 0
      var i = 0
      while i < order - 1 do { w = (w << 1) | bits(i); i += 1 }
      while i < p + order - 1 do { w = ((w << 1) | bits(i)) & p; seen.set(w); i += 1 }
      assert(seen.cardinality == p && !seen.get(0), s"order $order: ${seen.cardinality} distinct states of $p")
      val ones = (0 until p).count(bits(_) == 1)
      assert(ones == (p + 1) / 2, s"order $order: $ones ones in $p")
  }

  "prbsSequence" should "be ±1 with period 2^n - 1 and one more +1 than -1 per period" in
  {
    val order = 7
    val p     = (1 << order) - 1
    val xs    = prbsSequence(order, 2 * p, 3L).fold(f => fail(s"$f"), identity)
    assert(xs.forall(x => x == 1.0 || x == -1.0))
    assert(xs.take(p) == xs.drop(p))
    assert(xs.take(p).count(_ == 1.0) - xs.take(p).count(_ == -1.0) == 1)
  }

  it should "give two seeds rotations of one sequence: the seed picks the phase" in
  {
    val p = (1 << 9) - 1
    val a = prbsSequence(9, p, 1L).fold(f => fail(s"$f"), identity)
    val b = prbsSequence(9, p, 2L).fold(f => fail(s"$f"), identity)
    assert((0 until p).exists(k => a.drop(k) ++ a.take(k) == b))
  }

  it should "refuse an order outside the verified table and a count outside 1 … MaxPrbsLength" in
  {
    assert(prbsSequence(1, 10, 1L) == Left(SamplingFailure.InvalidOrder(1)))
    assert(prbsSequence(21, 10, 1L) == Left(SamplingFailure.InvalidOrder(21)))
    assert(prbsSequence(5, 0, 1L).isLeft)
    assert(prbsSequence(5, MaxPrbsLength + 1, 1L).isLeft)
  }

  // ── the nodes ──

  "draw at the prompt" should "answer an n x 1 column (Decision A), reading the offset when given" in
  {
    parse("draw(normal(0, 1), 4, 11)").eval(env) match
      case Right(m: _MatrixValue) =>
        assert(m.rows == 4 && m.cols == 1)
        assert(m.toVector == draws("normal(0, 1)", 4, 11L))
      case other => fail(s"$other")
    parse("draw(normal(0, 1), 2, 11, 2)").eval(env) match
      case Right(m: _MatrixValue) => assert(m.toVector == draws("normal(0, 1)", 4, 11L).drop(2))
      case other                  => fail(s"$other")
  }

  it should "take a distribution bound to a name" in
  {
    val bound = env.withBinding("X", dist("exponential(2)"))
    assert(parse("draw(X, 3, 1)").eval(bound).isRight)
  }

  it should "stay as written, with its reason, when it cannot answer" in
  {
    for (src, why) <- List(
          "draw(normal(m, 1), 3, 1)"   -> SamplingFailure.NotADistribution,
          "draw(normal(0, 1), 3, 1.5)" -> SamplingFailure.InvalidInteger("the seed"),
          "draw(normal(0, 1), 0, 1)"   -> SamplingFailure.InvalidCount(0, MaxDrawCount),
          "draw(normal(0, 1), 3, 1, -1)" -> SamplingFailure.InvalidInteger("the offset")) do
      val e = parse(src)
      assert(e.eval(env) == Left(e), src)
      e match
        case d: _Draw => assert(d.detailed(env) == Left(why), src)
        case other    => fail(s"$src parsed as $other")
  }

  "prbs at the prompt" should "answer an n x 1 column of ±1" in
  {
    parse("prbs(5, 31, 4)").eval(env) match
      case Right(m: _MatrixValue) =>
        assert(m.rows == 31 && m.cols == 1)
        assert(m.toVector == prbsSequence(5, 31, 4L).fold(f => fail(s"$f"), identity))
      case other => fail(s"$other")
    val bad = parse("prbs(25, 10, 1)")
    assert(bad.eval(env) == Left(bad))
  }

  "draw and prbs" should "round-trip through toString and be reserved" in
  {
    for src <- List("draw(normal(0, 1), 3, 7)", "draw(X, n, s, 4)", "prbs(7, 127, 1)") do
      val e = parse(src)
      assert(parse(e.toString) == e, src)
    assert(Parser.ReservedWords.contains("draw") && Parser.ReservedWords.contains("prbs"))
  }

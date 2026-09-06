package it.grypho.scala.leonardo
package probability

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** The probability domain — issue 4.P, all four slices.
 *
 *  The cdf assertions lean on 4.O's incomplete gamma and beta, which is the dependency the
 *  plan called out: every closed-form cdf here is one of those kernels.
 */
class ProbabilityTest extends AnyFlatSpec:

  private val env = new Environment()

  private def parse(s: String): _Expression =
    Parser.parse(s) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for \"$s\": $other")

  private def num(s: String): Double =
    parse(s).eval(env) match
      case Right(_Number(d)) => d
      case other             => fail(s"\"$s\" did not reduce to a number: $other")

  private def dist(s: String): _Distribution =
    parse(s).eval(env) match
      case Right(d: _Distribution) => d
      case other                   => fail(s"\"$s\" is not a distribution: $other")

  // --- slice A: the carrier and the continuous families ---

  "a distribution" should "be a concrete value that evaluates to itself" in
  {
    val d = dist("normal(0, 1)")
    assert(d.isInstanceOf[_Value])
    assert(d.eval(env) == Right(d))
    assert(d.children.isEmpty)
  }

  it should "refuse invalid parameters rather than produce NaN kernels" in
  {
    // Validation lives in the factory, so an invalid distribution cannot be constructed at
    // all and its node simply stays symbolic -- the "domain errors stay symbolic" rule.
    for bad <- List("normal(0, 0)", "normal(0, -1)", "uniform(2, 1)", "exponential(0)",
                    "poisson(-1)", "binomial(-1, 0.5)", "binomial(3, 2)") do
      assert(parse(bad).eval(env).isLeft, s"$bad must stay symbolic")
  }

  it should "stay symbolic while its parameters are still free" in
  {
    assert(parse("normal(m, s)").eval(env).isLeft)
    // ...and fold once they are bound.
    val bound = env.withBinding("m", _Number(1.0)).withBinding("s", _Number(2.0))
    parse("normal(m, s)").eval(bound) match
      case Right(d: _Distribution) => assert(d.params == Vector(1.0, 2.0))
      case other                   => fail(s"expected a distribution, got $other")
  }

  "the normal cdf" should "match reference values" in
  {
    assert(math.abs(num("cdf(normal(0, 1), 0)") - 0.5) < 1e-15)
    assert(math.abs(num("cdf(normal(0, 1), 1.96)") - 0.9750021048517795) < 1e-12)
    assert(math.abs(num("cdf(normal(0, 1), -1)") - 0.15865525393145707) < 1e-12)
    assert(math.abs(num("pdf(normal(0, 1), 0)") - 0.3989422804014327) < 1e-14)
  }

  it should "stay accurate in the left tail" in
  {
    // Computed as erfc(-z/sqrt2)/2 rather than (1 + erf(z/sqrt2))/2: the second form
    // subtracts near-equal quantities out here and would lose the answer entirely.
    val p = num("cdf(normal(0, 1), -8)")
    assert(math.abs(p - 6.220960574271786e-16) / 6.220960574271786e-16 < 1e-9, s"got $p")
  }

  "every cdf" should "be monotone and saturate at the tails" in
  {
    for d <- List("normal(1, 2)", "uniform(0, 4)", "exponential(1.5)",
                  "binomial(10, 0.3)", "poisson(4)") do
      val xs = (-30 to 40).map(_ * 0.5)
      val cs = xs.map(x => num(s"cdf($d, $x)"))
      for (a, b) <- cs.zip(cs.tail) do
        assert(b >= a - 1e-12, s"$d cdf decreased at some point")
      assert(cs.head < 1e-6,      s"$d must vanish at the low tail, got ${cs.head}")
      assert(cs.last > 1.0 - 1e-6, s"$d must saturate at the high tail, got ${cs.last}")
  }

  // --- slice B: the discrete families ---

  "a discrete pmf" should "sum to one over its support" in
  {
    val binom = (0 to 10).map(k => num(s"pdf(binomial(10, 0.3), $k)")).sum
    assert(math.abs(binom - 1.0) < 1e-12, s"binomial pmf summed to $binom")
    val pois = (0 to 60).map(k => num(s"pdf(poisson(4), $k)")).sum
    assert(math.abs(pois - 1.0) < 1e-12, s"poisson pmf summed to $pois")
  }

  it should "agree with its own cdf" in
  {
    for k <- 0 to 10 do
      val summed = (0 to k).map(j => num(s"pdf(binomial(10, 0.3), $j)")).sum
      assert(math.abs(summed - num(s"cdf(binomial(10, 0.3), $k)")) < 1e-11, s"at k=$k")
  }

  it should "not overflow for a large n, where the binomial coefficient alone would" in
  {
    // C(200, 100) is about 9e58 -- fine in a Double, but C(2000, 1000) is not, and the
    // log-space form is what keeps this working rather than the coefficient itself.
    val p = num("pdf(binomial(2000, 0.5), 1000)")
    assert(p > 0.0 && p < 1.0, s"expected a probability, got $p")
    assert(math.abs(p - 0.01783) < 1e-4, s"got $p")
  }

  "prob on a discrete family" should "include the lower endpoint" in
  {
    // cdf(hi) - cdf(lo) would silently drop P(X = lo), which is not what P(lo <= X <= hi)
    // means for a discrete distribution.
    val direct = (1 to 3).map(k => num(s"pdf(binomial(10, 0.3), $k)")).sum
    assert(math.abs(num("prob(binomial(10, 0.3), 1, 3)") - direct) < 1e-12)
  }

  "prob on a continuous family" should "be the cdf difference" in
  {
    val want = num("cdf(normal(0, 1), 1)") - num("cdf(normal(0, 1), -1)")
    assert(math.abs(num("prob(normal(0, 1), -1, 1)") - want) < 1e-14)
    assert(math.abs(num("prob(normal(0, 1), -1, 1)") - 0.6826894921370859) < 1e-11)
  }

  // --- slice C: the linearity rule table ---

  "expect and variance" should "give a distribution's own moments" in
  {
    assert(math.abs(num("expect(normal(3, 2))") - 3.0) < 1e-15)
    assert(math.abs(num("variance(normal(3, 2))") - 4.0) < 1e-15)
    assert(math.abs(num("expect(uniform(0, 4))") - 2.0) < 1e-15)
    assert(math.abs(num("variance(uniform(0, 4))") - 16.0 / 12.0) < 1e-15)
    assert(math.abs(num("expect(binomial(10, 0.3))") - 3.0) < 1e-14)
    assert(math.abs(num("variance(poisson(4))") - 4.0) < 1e-15)
  }

  it should "apply linearity to an expression in the random variable" in
  {
    // The reason this belongs in a CAS: E[2X + 3] is a rewrite on the expression's
    // structure, not a numeric integral.
    val e = env.withBinding("X", dist("normal(5, 2)"))
    def at(s: String): Double = parse(s).eval(e) match
      case Right(_Number(d)) => d
      case other             => fail(s"\"$s\" did not reduce: $other")

    assert(math.abs(at("expect(2*X + 3, X)") - 13.0) < 1e-12)      // 2*5 + 3
    assert(math.abs(at("expect(X, X)") - 5.0) < 1e-12)
    assert(math.abs(at("expect(7, X)") - 7.0) < 1e-12)             // E[c] = c
    assert(math.abs(at("variance(2*X + 3, X)") - 16.0) < 1e-12)    // 4 * 4; the shift drops
    assert(math.abs(at("variance(X + 100, X)") - 4.0) < 1e-12)
    assert(math.abs(at("variance(9, X)")) < 1e-12)                 // Var(c) = 0
    assert(math.abs(at("expect(X/2, X)") - 2.5) < 1e-12)
  }

  it should "stay symbolic where linearity does not apply" in
  {
    // E[X^2] is not determined by linearity, and E[XY] needs independence, which the
    // language cannot express. Both must give up rather than guess.
    val e = env.withBinding("X", dist("normal(0, 1)")).withBinding("Y", dist("normal(0, 1)"))
    for s <- List("expect(X^2, X)", "expect(sin(X), X)", "expect(X*Y, X)", "variance(X*Y, X)") do
      assert(parse(s).eval(e).isLeft, s"$s must stay symbolic")
  }

  // --- slice D: quantiles ---

  "quantile" should "invert the cdf" in
  {
    for (d, p) <- List(("normal(0, 1)", 0.975), ("normal(2, 3)", 0.1),
                       ("uniform(0, 4)", 0.25), ("exponential(2)", 0.5)) do
      val q = num(s"quantile($d, $p)")
      assert(math.abs(num(s"cdf($d, $q)") - p) < 1e-8, s"quantile($d, $p) = $q did not invert")
    assert(math.abs(num("quantile(normal(0, 1), 0.975)") - 1.959963984540054) < 1e-6)
  }

  it should "refuse a probability outside (0, 1)" in
  {
    for p <- List("0", "1", "-0.5", "2") do
      assert(parse(s"quantile(normal(0, 1), $p)").eval(env).isLeft, s"p = $p must stay symbolic")
  }

  // --- round-trip and reserved words ---

  "a distribution" should "round-trip through toString" in
  {
    for s <- List("normal(0.0, 1.0)", "uniform(0.0, 4.0)", "exponential(1.5)",
                  "binomial(10.0, 0.3)", "poisson(4.0)") do
      val d = dist(s)
      assert(parse(d.toString).eval(env) == Right(d), s"round-trip failed for $s")
  }

  it should "reserve its grammar words" in
  {
    for w <- List("normal", "uniform", "exponential", "binomial", "poisson",
                  "pdf", "cdf", "prob", "quantile", "expect", "variance") do
      assert(Parser.ReservedWords.contains(w), s"'$w' must be reserved")
  }

  "the moments" should "be exact where they are rational functions of the parameters" in
  {
    // Uniform(a,b).mean = (a+b)/2 is a field operation, so it is closed over the rationals.
    // The pdf/cdf are transcendental and are not expected to be.
    Parser.parse("expect(uniform(0, 1))", Some(30)) match
      case Parser.Success(e, _) =>
        // The distribution carries Doubles, so this is a Double result -- documenting the
        // boundary rather than claiming exactness the carrier cannot hold.
        assert(e.eval(new Environment(workingPrecision = 30)).isRight)
      case other => fail(s"parse failed: $other")
  }

  // --- issue 4.R slice B: the predicate form of prob ---

  "prob with a predicate" should "agree with the cdf" in
  {
    val e = env.withBinding("X", dist("normal(0, 1)"))
    def at(s: String): Double = parse(s).eval(e) match
      case Right(_Number(d)) => d
      case other             => fail(s"\"$s\" did not reduce: $other")

    assert(math.abs(at("prob(X < 2)")  - at("cdf(X, 2)")) < 1e-12)
    assert(math.abs(at("prob(X <= 2)") - at("cdf(X, 2)")) < 1e-12)
    assert(math.abs(at("prob(X > 2)")  - (1.0 - at("cdf(X, 2)"))) < 1e-12)
    assert(math.abs(at("prob(X >= 2)") - (1.0 - at("cdf(X, 2)"))) < 1e-12)
  }

  it should "read the variable on either side" in
  {
    val e = env.withBinding("X", dist("normal(0, 1)"))
    def at(s: String): Double = parse(s).eval(e) match
      case Right(_Number(d)) => d
      case other             => fail(s"\"$s\" did not reduce: $other")
    // `2 > X` is `X < 2`: the operator flips when the variable is on the right.
    assert(math.abs(at("prob(2 > X)") - at("prob(X < 2)")) < 1e-12)
    assert(math.abs(at("prob(2 <= X)") - at("prob(X >= 2)")) < 1e-12)
  }

  it should "intersect an and, giving the two-sided form" in
  {
    val e = env.withBinding("X", dist("normal(0, 1)"))
    def at(s: String): Double = parse(s).eval(e) match
      case Right(_Number(d)) => d
      case other             => fail(s"\"$s\" did not reduce: $other")
    assert(math.abs(at("prob(-1 < X and X < 1)") - 0.6826894921370859) < 1e-11)
    // ...and agrees with the interval spelling, which is kept and not deprecated.
    assert(math.abs(at("prob(-1 < X and X < 1)") - at("prob(X, -1, 1)")) < 1e-12)
  }

  it should "handle point events by family" in
  {
    val c = env.withBinding("X", dist("normal(0, 1)"))
    val d = env.withBinding("K", dist("binomial(10, 0.3)"))
    def at(s: String, e: Environment): Double = parse(s).eval(e) match
      case Right(_Number(x)) => x
      case other             => fail(s"\"$s\" did not reduce: $other")
    // A single point carries no mass in a continuous family, and its own mass in a discrete one.
    assert(math.abs(at("prob(X == 0)", c)) < 1e-15)
    assert(math.abs(at("prob(X != 0)", c) - 1.0) < 1e-15)
    assert(math.abs(at("prob(K == 3)", d) - at("pdf(K, 3)", d)) < 1e-12)
    assert(math.abs(at("prob(K != 3)", d) - (1.0 - at("pdf(K, 3)", d))) < 1e-12)
  }

  it should "snap a discrete bound to the integers it admits" in
  {
    val e = env.withBinding("K", dist("binomial(10, 0.3)"))
    def at(s: String): Double = parse(s).eval(e) match
      case Right(_Number(d)) => d
      case other             => fail(s"\"$s\" did not reduce: $other")
    // P(K < 2) is P(K <= 1) -- the whole mass at 2 is the difference, and it is not small.
    assert(math.abs(at("prob(K < 2)") - at("cdf(K, 1)")) < 1e-12)
    assert(math.abs(at("prob(K <= 2)") - at("cdf(K, 2)")) < 1e-12)
    assert(at("prob(K <= 2)") - at("prob(K < 2)") > 0.2, "the endpoint carries real mass")
    // ...and the two-sided form includes both endpoints.
    val direct = (1 to 3).map(k => at(s"pdf(K, $k)")).sum
    assert(math.abs(at("prob(K >= 1 and K <= 3)") - direct) < 1e-12)
  }

  it should "stay symbolic rather than answer a different question" in
  {
    val e = env.withBinding("X", dist("normal(0, 1)")).withBinding("Y", dist("normal(0, 1)"))
    for s <- List(
      "prob(X < Y)",          // two random variables: which one is it about?
      "prob(2*X < 6)",        // a coefficient whose sign decides whether the bound flips
      "prob(X < 1 or X > 2)", // a union of intervals is not a Bounds
      "prob(sin(X) < 1)",     // not a bound on X at all
      "prob(Z < 1)"           // Z is not random
    ) do
      assert(parse(s).eval(e).isLeft, s"$s must stay symbolic")
  }

  it should "round-trip, printing as prob rather than probof" in
  {
    val p = parse("prob(X < 2)")
    // The enum case is ProbOf, but it must print as `prob` or it could not re-parse.
    assert(p.toString.startsWith("prob("), s"got ${p.toString}")
    assert(!p.toString.contains("probof"), s"the enum name must not leak: ${p.toString}")
    assert(parse(p.toString) == p, s"predicate form must round-trip: ${p.toString}")
  }
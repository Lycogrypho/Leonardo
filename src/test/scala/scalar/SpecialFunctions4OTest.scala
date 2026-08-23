package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** The incomplete gamma / beta family and the functions built on it — issue 4.O.
 *
 *  These are the analytic kernels only; the AST nodes that expose them are covered by the
 *  parser and evaluation suites.  Reference values are from standard tables, and the
 *  tolerances are chosen against what the algorithm can deliver rather than picked to pass.
 */
class SpecialFunctions4OTest extends AnyFlatSpec:

  private def value(o: Option[Double], what: String): Double =
    o.getOrElse(fail(s"$what was undefined"))

  // --- the engine ---

  "the incomplete gamma" should "satisfy P + Q = 1 across the domain" in
  {
    // The two use different algorithms either side of x = a + 1, so this also checks that
    // the series and the continued fraction agree where they meet.
    for a <- List(0.5, 1.0, 2.5, 10.0, 50.0); x <- List(0.1, 0.9, 1.5, 3.0, 12.0, 60.0) do
      val p = value(lowerGammaP(a, x), s"P($a, $x)")
      val q = value(upperGammaQ(a, x), s"Q($a, $x)")
      assert(math.abs(p + q - 1.0) < 1e-13, s"P+Q != 1 at a=$a x=$x: $p + $q")
      assert(p >= 0.0 && p <= 1.0, s"P out of range at a=$a x=$x: $p")
  }

  it should "match known closed forms" in
  {
    // P(1, x) = 1 - e^-x exactly.
    for x <- List(0.5, 1.0, 4.0) do
      assert(math.abs(value(lowerGammaP(1.0, x), "P") - (1.0 - math.exp(-x))) < 1e-14, s"at $x")
    assert(math.abs(value(lowerGammaP(0.5, 1.0), "P(1/2,1)") - 0.8427007929497149) < 1e-13)
    assert(lowerGammaP(1.0, 0.0).contains(0.0))
    assert(upperGammaQ(1.0, 0.0).contains(1.0))
  }

  it should "stay undefined outside its domain" in
  {
    assert(lowerGammaP(0.0, 1.0).isEmpty,  "a must be positive")
    assert(lowerGammaP(-1.0, 1.0).isEmpty, "a must be positive")
    assert(lowerGammaP(1.0, -1.0).isEmpty, "x must be non-negative")
    assert(lowerGammaP(Double.NaN, 1.0).isEmpty)
  }

  // --- erf / erfc ---

  "erf" should "match reference values" in
  {
    assert(math.abs(value(erfOf(1.0), "erf(1)")   - 0.8427007929497149) < 1e-14)
    assert(math.abs(value(erfOf(0.5), "erf(0.5)") - 0.5204998778130465) < 1e-14)
    assert(math.abs(value(erfOf(2.0), "erf(2)")   - 0.9953222650189527) < 1e-14)
    assert(erfOf(0.0).contains(0.0))
  }

  it should "be odd" in
  {
    for x <- List(0.25, 1.0, 3.0) do
      assert(math.abs(value(erfOf(-x), "erf(-x)") + value(erfOf(x), "erf(x)")) < 1e-15, s"at $x")
  }

  it should "saturate at infinity rather than fail" in
  {
    assert(erfOf(Double.PositiveInfinity).contains(1.0))
    assert(erfOf(Double.NegativeInfinity).contains(-1.0))
    assert(erfOf(Double.NaN).isEmpty)
  }

  "erfc" should "stay accurate in the tail, where 1 - erf(x) cannot" in
  {
    // erf(5) is within 2e-12 of 1, so `1 - erf(5)` keeps only ~4 correct digits of a value
    // near 1.5e-12.  Computing Q directly keeps all of them -- this is the whole reason
    // erfc exists as its own kernel rather than as sugar.
    val direct   = value(erfcOf(5.0), "erfc(5)")
    val expected = 1.5374597944280351e-12
    assert(math.abs(direct - expected) / expected < 1e-10,
           s"erfc(5) = $direct, expected about $expected")
    val naive = 1.0 - value(erfOf(5.0), "erf(5)")
    assert(math.abs(naive - expected) / expected > math.abs(direct - expected) / expected,
           "the subtraction really is the worse route")
  }

  it should "agree with 1 - erf where that is well conditioned" in
  {
    for x <- List(-2.0, -0.5, 0.0, 0.5, 1.0) do
      assert(math.abs(value(erfcOf(x), "erfc") - (1.0 - value(erfOf(x), "erf"))) < 1e-14, s"at $x")
  }

  // --- digamma ---

  "digamma" should "match reference values" in
  {
    val euler = 0.5772156649015329
    assert(math.abs(value(digammaOf(1.0), "psi(1)") + euler) < 1e-13)
    // psi(1/2) = -gamma - 2 ln 2
    assert(math.abs(value(digammaOf(0.5), "psi(1/2)") - (-euler - 2.0 * math.log(2.0))) < 1e-13)
    // psi(n) = -gamma + sum_{k=1}^{n-1} 1/k
    val psi5 = -euler + (1 to 4).map(k => 1.0 / k).sum
    assert(math.abs(value(digammaOf(5.0), "psi(5)") - psi5) < 1e-13)
  }

  it should "satisfy its recurrence" in
  {
    for z <- List(0.3, 1.0, 2.7, 9.5, 40.0) do
      val lhs = value(digammaOf(z + 1.0), "psi(z+1)")
      val rhs = value(digammaOf(z), "psi(z)") + 1.0 / z
      assert(math.abs(lhs - rhs) < 1e-12, s"recurrence failed at $z: $lhs vs $rhs")
  }

  it should "handle negative arguments by reflection, and refuse the poles" in
  {
    // psi(-0.5) = 2 - gamma - 2 ln 2
    assert(math.abs(value(digammaOf(-0.5), "psi(-1/2)") -
                    (2.0 - 0.5772156649015329 - 2.0 * math.log(2.0))) < 1e-12)
    for pole <- List(0.0, -1.0, -2.0, -10.0) do
      assert(digammaOf(pole).isEmpty, s"psi has a pole at $pole")
  }

  // --- incomplete beta ---

  "the incomplete beta" should "satisfy its reflection symmetry" in
  {
    for x <- List(0.1, 0.35, 0.5, 0.8); a <- List(0.5, 1.0, 3.0, 12.0); b <- List(0.5, 2.0, 7.0) do
      val lhs = value(incompleteBetaOf(x, a, b), s"I_$x($a,$b)")
      val rhs = 1.0 - value(incompleteBetaOf(1.0 - x, b, a), "reflected")
      assert(math.abs(lhs - rhs) < 1e-12, s"asymmetry at x=$x a=$a b=$b: $lhs vs $rhs")
  }

  it should "match known closed forms" in
  {
    // I_x(1,1) = x, since Beta(1,1) is the uniform distribution.
    for x <- List(0.0, 0.25, 0.5, 1.0) do
      assert(math.abs(value(incompleteBetaOf(x, 1.0, 1.0), "I(1,1)") - x) < 1e-14, s"at $x")
    // I_x(a,1) = x^a.
    assert(math.abs(value(incompleteBetaOf(0.5, 3.0, 1.0), "I(3,1)") - 0.125) < 1e-13)
    assert(incompleteBetaOf(0.5, 2.0, 2.0).exists(v => math.abs(v - 0.5) < 1e-14),
           "symmetric parameters put the median at 1/2")
  }

  it should "stay undefined outside its domain" in
  {
    assert(incompleteBetaOf(-0.1, 1.0, 1.0).isEmpty)
    assert(incompleteBetaOf(1.1, 1.0, 1.0).isEmpty)
    assert(incompleteBetaOf(0.5, 0.0, 1.0).isEmpty)
    assert(incompleteBetaOf(0.5, 1.0, -1.0).isEmpty)
  }

  // --- complex gamma (slice 4) ---

  "the complex gamma" should "agree with the real kernel on the real axis" in
  {
    for x <- List(0.25, 1.0, 2.5, 5.0, -1.5) do
      val (re, im) = gammaComplex(x, 0.0).getOrElse(fail(s"Gamma($x) undefined"))
      assert(math.abs(re - value(gammaOf(x), "real")) < 1e-9, s"real part differs at $x")
      assert(im == 0.0, s"a real argument must stay real at $x")
  }

  it should "satisfy the recurrence in the complex plane" in
  {
    // Gamma(z+1) = z * Gamma(z) holds everywhere off the poles, so it tests the Lanczos
    // branch, the reflection branch and the complex arithmetic together.
    for (r, i) <- List((1.5, 2.0), (0.3, 1.0), (-2.4, 0.7), (4.0, -3.0)) do
      val (gr, gi)   = gammaComplex(r, i).getOrElse(fail(s"Gamma($r + ${i}i) undefined"))
      val (g1r, g1i) = gammaComplex(r + 1.0, i).getOrElse(fail("Gamma(z+1) undefined"))
      // z * Gamma(z)
      val pr = r * gr - i * gi
      val pi = r * gi + i * gr
      val scale = math.max(1.0, math.hypot(g1r, g1i))
      assert(math.hypot(g1r - pr, g1i - pi) / scale < 1e-9,
             s"recurrence failed at $r + ${i}i: ($g1r, $g1i) vs ($pr, $pi)")
  }

  it should "reproduce the known value of Gamma(i)" in
  {
    // Gamma(i) = -0.15494982830181068 - 0.49801566811835604i
    val (re, im) = gammaComplex(0.0, 1.0).getOrElse(fail("Gamma(i) undefined"))
    assert(math.abs(re - -0.15494982830181068) < 1e-10, s"re = $re")
    assert(math.abs(im - -0.49801566811835604) < 1e-10, s"im = $im")
  }

  it should "refuse the real poles and non-finite input" in
  {
    for pole <- List(0.0, -1.0, -5.0) do
      assert(gammaComplex(pole, 0.0).isEmpty, s"pole at $pole")
    assert(gammaComplex(Double.NaN, 1.0).isEmpty)
    assert(gammaComplex(1.0, Double.PositiveInfinity).isEmpty)
  }

  // --- the AST nodes and their grammar ---

  private def parse(s: String): _Expression =
    Parser.parse(s) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for \"$s\": $other")

  private def evalNum(s: String): Double =
    parse(s).eval(new Environment()) match
      case Right(_Number(d)) => d
      case other             => fail(s"\"$s\" did not reduce to a number: $other")

  "the 4.O grammar" should "parse and evaluate every new function" in
  {
    assert(math.abs(evalNum("erf(1)")            - 0.8427007929497149) < 1e-12)
    assert(math.abs(evalNum("erfc(1)")           - 0.15729920705028513) < 1e-12)
    assert(math.abs(evalNum("digamma(1)")        + 0.5772156649015329) < 1e-12)
    assert(math.abs(evalNum("gammaP(1, 1)")      - (1.0 - math.exp(-1.0))) < 1e-12)
    assert(math.abs(evalNum("gammaQ(1, 1)")      - math.exp(-1.0)) < 1e-12)
    assert(math.abs(evalNum("betaI(0.5, 1, 1)")  - 0.5) < 1e-12)
  }

  it should "round-trip through toString" in
  {
    for s <- List("erf(x)", "erfc(x)", "digamma(x)", "gammaP(a, x)", "gammaQ(a, x)",
                  "betaI(x, a, b)") do
      assert(parse(parse(s).toString) == parse(s), s"round-trip failed for $s")
  }

  it should "reserve the new names, so they cannot become variables" in
  {
    for w <- List("erf", "erfc", "digamma", "gammaP", "gammaQ", "betaI") do
      assert(Parser.ReservedWords.contains(w), s"'$w' must be reserved")
    // ...while names merely starting with one stay legal, per the word-boundary rule.
    assert(Parser.parse("erfx").successful, "erfx must stay an ordinary variable")
  }

  it should "stay symbolic outside the domain, never returning NaN" in
  {
    for s <- List("digamma(0)", "gammaP(0, 1)", "betaI(2, 1, 1)") do
      assert(parse(s).eval(new Environment()).isLeft, s"$s must stay symbolic")
  }

  "Gamma of a complex argument" should "now evaluate through the node" in
  {
    // Before slice 4 this fell through to the generic fallback and stayed symbolic.
    parse("Gamma(i)").eval(new Environment()) match
      case Right(c: _Complex) =>
        assert(math.abs(c.re - -0.15494982830181068) < 1e-9, s"re = ${c.re}")
        assert(math.abs(c.im - -0.49801566811835604) < 1e-9, s"im = ${c.im}")
      case other => fail(s"expected a complex value, got $other")
  }

  "the new functions" should "be on the compile fast path" in
  {
    // Which is what puts them inside `sample` and Simpson's rule; a `None` here would mean
    // a definite integral of erf silently fell back to tree-eval.
    val v = _Variable("x")
    for s <- List("erf(x)", "erfc(x)", "digamma(x)", "gammaP(2, x)", "betaI(x, 2, 3)") do
      assert(compile(parse(s), v, new Environment()).isDefined, s"$s must compile")
  }

  "the derivative of erf" should "be 2/sqrt(pi) * exp(-x^2)" in
  {
    val env = new Environment().withBinding("x", _Number(0.7))
    derive(parse("erf(x)"), _Variable("x")).eval(env) match
      case Right(_Number(got)) =>
        val want = 2.0 / math.sqrt(math.Pi) * math.exp(-0.49)
        assert(math.abs(got - want) < 1e-12, s"got $got, want $want")
      case other => fail(s"expected a number, got $other")
  }
package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


class SeriesTest extends AnyFlatSpec:

  val x = _Variable("x")
  val env = new Environment()

  def parse(input: String): _Expression =
    val result = Parser.parse(input)
    assert(result.successful, s"parse failed for \"$input\": $result")
    result.get

  /** Evaluates `e` with `x` bound to `at`, failing when it does not reduce to a number. */
  def valueAt(e: _Expression, at: Double): Double =
    e.eval(env.withBinding("x", _Number(at))) match
      case Right(_Number(d)) => d
      case other             => fail(s"did not reduce at x = $at: $other")

  def assertClose(actual: Double, expected: Double, tol: Double = 1e-9, clue: String = ""): Unit =
    assert(math.abs(actual - expected) <= tol * math.max(1.0, math.abs(expected)),
      s"$clue expected $expected but got $actual")

  // --- the standard expansions, checked by their coefficients ---

  "the Maclaurin series of exp(x)" should "have coefficients 1/k!" in
  {
    val s = parse("maclaurin(exp(x), x, 5)")
    // exp(x) = 1 + x + x^2/2 + x^3/6 + x^4/24 + x^5/120; check by evaluating at points
    for at <- List(0.0, 0.25, 0.5, 1.0) do
      assertClose(valueAt(s, at), math.exp(at), 1e-3, s"exp at $at:")
    // the constant term is exactly 1
    assertClose(valueAt(s, 0.0), 1.0, 1e-12, "constant term:")
  }

  "the Maclaurin series of sin(x)" should "match sin to within its truncation error" in
  {
    val s = parse("maclaurin(sin(x), x, 7)")
    // sin is alternating with decreasing terms, so the error is bounded by the first
    // omitted term, x^9/9!. Asserting that rather than a magic tolerance is both the
    // sharper test and the one that says WHY the residual is the size it is.
    for at <- List(0.0, 0.3, 0.7, 1.0) do
      val bound = math.pow(at, 9) / (1 to 9).product
      val err   = math.abs(valueAt(s, at) - math.sin(at))
      assert(err <= bound + 1e-12, s"sin at $at: error $err exceeds the x^9/9! bound $bound")
    // odd function: every even coefficient vanishes, so s(-a) = -s(a)
    assertClose(valueAt(s, -0.6), -valueAt(s, 0.6), 1e-12, "oddness:")
  }

  "the Maclaurin series of cos(x)" should "match cos to within its truncation error" in
  {
    val s = parse("maclaurin(cos(x), x, 6)")
    for at <- List(0.0, 0.4, 0.9) do
      val bound = math.pow(at, 8) / (1 to 8).product     // first omitted term
      val err   = math.abs(valueAt(s, at) - math.cos(at))
      assert(err <= bound + 1e-12, s"cos at $at: error $err exceeds the x^8/8! bound $bound")
    assertClose(valueAt(s, -0.5), valueAt(s, 0.5), 1e-12, "evenness:")
  }

  "the Maclaurin series of 1/(1-x)" should "be the geometric series" in
  {
    val s = parse("maclaurin(1 / (1 - x), x, 6)")
    // 1 + x + x^2 + ... : at x = 1/2 the truncation is 2 - (1/2)^6 * 2
    for at <- List(0.0, 0.2, 0.5) do
      assertClose(valueAt(s, at), (0 to 6).map(k => math.pow(at, k)).sum, 1e-12, s"at $at:")
  }

  "the Maclaurin series of ln(1+x)" should "match near zero" in
  {
    val s = parse("maclaurin(ln(1 + x), x, 5)")
    for at <- List(0.0, 0.1, 0.4) do
      assertClose(valueAt(s, at), math.log(1 + at), 1e-3, s"ln(1+x) at $at:")
  }

  // --- truncation order is respected ---

  "the order" should "control how many terms are kept" in
  {
    // exp truncated at order 1 is exactly 1 + x
    val linear = parse("maclaurin(exp(x), x, 1)")
    assertClose(valueAt(linear, 2.0), 3.0, 1e-12, "1 + x at 2:")
    // order 0 keeps the constant term only
    val constant = parse("maclaurin(exp(x), x, 0)")
    assertClose(valueAt(constant, 5.0), 1.0, 1e-12, "constant only:")
  }

  it should "improve the approximation monotonically, within the Lagrange bound" in
  {
    val at = 0.9
    val errors = (1 to 12).map { k =>
      math.abs(valueAt(parse(s"maclaurin(exp(x), x, $k)"), at) - math.exp(at))
    }
    assert(errors.sliding(2).forall(p => p(1) <= p(0)),
      s"error should not grow with order: $errors")
    // Lagrange remainder for exp on [0, a]:  |R_n| <= e^a * a^(n+1)/(n+1)!
    for (err, i) <- errors.zipWithIndex do
      val n     = i + 1
      val bound = math.exp(at) * math.pow(at, n + 1) / (1 to (n + 1)).product
      assert(err <= bound + 1e-15, s"order $n: error $err exceeds the Lagrange bound $bound")
    // No magic absolute constant here: the Lagrange bound above is the meaningful
    // claim. What is worth asserting in addition is that raising the order actually buys
    // orders of magnitude -- order 12 is ~9 decades better than order 1.
    assert(errors.last < errors.head / 1e6,
      s"raising the order should buy orders of magnitude: ${errors.head} -> ${errors.last}")
  }

  // --- expansion about a non-zero and a symbolic point ---

  "a Taylor expansion about a non-zero point" should "match the function there" in
  {
    val s = parse("taylor(exp(x), x, 1, 4)")
    for at <- List(1.0, 1.2, 0.8) do
      assertClose(valueAt(s, at), math.exp(at), 1e-3, s"exp about 1, at $at:")
    // at the centre it is exactly e
    assertClose(valueAt(s, 1.0), math.E, 1e-12, "at the centre:")
  }

  it should "expand a polynomial exactly (the series terminates)" in
  {
    // x^3 - 2x about 2, order 3, is exact everywhere
    val s = parse("taylor(x^3 - 2 * x, x, 2, 3)")
    for at <- List(-3.0, 0.0, 2.0, 5.0) do
      assertClose(valueAt(s, at), at * at * at - 2 * at, 1e-9, s"at $at:")
  }

  "a Taylor expansion about a symbolic point" should "stay symbolic in that point" in
  {
    val s = parse("taylor(x^2, x, a, 2)").eval(env).toExpression
    // free in both x and a
    assert(s.freeVars.contains("a"), s"expected 'a' free in: $s")
    assert(s.freeVars.contains("x"), s"expected 'x' free in: $s")
    // and it is still x^2: check by binding both
    val bound = s.eval(env.withBinding("a", _Number(3.0)).withBinding("x", _Number(5.0)))
    bound match
      case Right(_Number(d)) => assertClose(d, 25.0, 1e-9, "x^2 at 5 about 3:")
      case other             => fail(s"did not reduce: $other")
  }

  // --- staying symbolic rather than answering wrongly ---

  "an order beyond the cap" should "stay symbolic" in
  {
    assert(parse(s"maclaurin(exp(x), x, ${MaxTaylorOrder + 1})").eval(env).isLeft)
    assert(parse(s"maclaurin(exp(x), x, $MaxTaylorOrder)").eval(env).isRight ||
           parse(s"maclaurin(exp(x), x, $MaxTaylorOrder)").eval(env).isLeft,
      "at the cap it must not throw")
  }

  "a negative or non-integer order" should "stay symbolic" in
  {
    assert(parse("maclaurin(exp(x), x, -1)").eval(env).isLeft)
    assert(parse("maclaurin(exp(x), x, 2.5)").eval(env).isLeft)
    assert(parse("maclaurin(exp(x), x, n)").eval(env).isLeft, "unbound order")
  }

  "an expression whose derivative stays symbolic" should "stay symbolic, not emit a bogus series" in
  {
    // derive(Gamma(x), x) needs digamma, which is not implemented
    val g = parse("maclaurin(Gamma(x), x, 3)")
    assert(g.eval(env).isLeft, s"expected symbolic, got: ${g.eval(env)}")
    val f = parse("maclaurin(fact(x), x, 3)")
    assert(f.eval(env).isLeft, s"expected symbolic, got: ${f.eval(env)}")
  }

  it should "not leave a _Derivative inside a returned series" in
  {
    def hasDeriv(e: _Expression): Boolean = e match
      case _: _Derivative => true
      case o              => o.children.exists(hasDeriv)
    for src <- List("maclaurin(exp(x), x, 4)", "taylor(sin(x), x, 1, 3)", "maclaurin(Gamma(x), x, 2)") do
      val r = parse(src).eval(env).toExpression
      assert(!hasDeriv(r) || r.isInstanceOf[_Taylor],
        s"$src produced a series containing a derivative: $r")
  }

  // --- the algorithm functions directly ---

  "taylorSeries" should "reject an out-of-range order" in
  {
    assert(taylorSeries(parse("exp(x)"), x, _Number(0), -1).isEmpty)
    assert(taylorSeries(parse("exp(x)"), x, _Number(0), MaxTaylorOrder + 1).isEmpty)
    assert(taylorSeries(parse("exp(x)"), x, _Number(0), 3).isDefined)
  }

  "maclaurinSeries" should "equal taylorSeries about zero" in
  {
    assert(maclaurinSeries(parse("sin(x)"), x, 5) == taylorSeries(parse("sin(x)"), x, _Number(0), 5))
  }

  // --- parsing and round-trip ---

  "the grammar" should "parse both spellings, with maclaurin as sugar" in
  {
    assert(parse("taylor(exp(x), x, 1, 4)") == _Taylor(Exp(x), x, _Number(1), _Number(4)))
    // maclaurin desugars to a Taylor about 0 and prints in that form
    assert(parse("maclaurin(exp(x), x, 4)") == _Taylor(Exp(x), x, _Number(0), _Number(4)))
  }

  it should "reserve the keywords" in
  {
    for w <- List("taylor", "maclaurin") do
      assert(Parser.ReservedWords.contains(w), s"'$w' must be reserved")
    assert(!Parser.parse("taylor").successful)
  }

  it should "round-trip through toString" in
  {
    val cases = List[_Expression](
      _Taylor(Exp(x), x, _Number(0), _Number(4)),
      _Taylor(Sin(x), x, _Number(1), _Number(3)),
      _Taylor(Sum(x, _Number(1)), x, _Variable("a"), _Number(2)))
    for e <- cases do
      val printed  = e.toString
      val reparsed = Parser.parse(printed)
      assert(reparsed.successful, s"toString did not re-parse: \"$printed\" ($reparsed)")
      assert(reparsed.get == e, s"round-trip changed \"$printed\": ${reparsed.get}")
  }

  // --- composition with the rest of the library ---

  "a series" should "be differentiable term by term" in
  {
    // d/dx of the exp series at order 4 is the exp series at order 3
    val s  = parse("maclaurin(exp(x), x, 4)").eval(env).toExpression
    val ds = simplifyFully(derive(s, x))
    for at <- List(0.0, 0.3, 0.7) do
      val expected = (0 to 3).map(k => math.pow(at, k) / (1 to k).product).sum
      assertClose(valueAt(ds, at), expected, 1e-9, s"derivative at $at:")
  }

  it should "agree with the function it expands, sampled near the centre" in
  {
    val s = parse("maclaurin(cos(x), x, 8)").eval(env).toExpression
    val pts = sample(s, x, -0.5, 0.5, 11, env)
    assert(pts.size == 11)
    for (px, py) <- pts do assertClose(py, math.cos(px), 1e-6, s"at $px:")
  }

  // --- 4.J tier 2: Fourier series ---

  /** The Fourier series of `src` sampled at `at`. */
  def fourierAt(src: String, at: Double): Double = valueAt(parse(src), at)

  "the Fourier series of a constant" should "be that constant" in
  {
    // a0/2 = 1, every harmonic vanishes
    val s = parse("fourierSeries(1, x, 2*pi, 3)")
    for at <- List(-2.0, 0.0, 1.5, 3.0) do
      assertClose(valueAt(s, at), 1.0, 1e-9, s"constant at $at:")
  }

  "the Fourier series of the sawtooth x on [-pi, pi]" should "have b_k = 2(-1)^(k+1)/k" in
  {
    // f(x) = x has the classic expansion 2*sin(x) - sin(2x) + (2/3)sin(3x) - ...
    // Compare against the analytic partial sum rather than against x itself: the series
    // converges to x only in the mean, and Gibbs oscillation near +-pi is real, not error.
    val s = parse("fourierSeries(x, x, 2*pi, 4)")
    def analytic(t: Double): Double =
      (1 to 4).map(k => 2.0 * math.pow(-1, k + 1) / k * math.sin(k * t)).sum
    for at <- List(-2.0, -0.5, 0.0, 0.5, 2.0) do
      assertClose(valueAt(s, at), analytic(at), 1e-6, s"sawtooth partial sum at $at:")
  }

  it should "be odd, so every cosine coefficient vanishes" in
  {
    // f(x) = x is odd => a_k = 0 for all k, so the partial sum is an odd function
    val s = parse("fourierSeries(x, x, 2*pi, 4)")
    for at <- List(0.3, 1.0, 2.5) do
      assertClose(valueAt(s, at), -valueAt(s, -at), 1e-6, s"oddness at $at:")
    assertClose(valueAt(s, 0.0), 0.0, 1e-6, "an odd series vanishes at 0:")
  }

  "the Fourier series of x^2 on [-pi, pi]" should "have a_0/2 = pi^2/3 and a_k = 4(-1)^k/k^2" in
  {
    val s = parse("fourierSeries(x^2, x, 2*pi, 4)")
    def analytic(t: Double): Double =
      math.Pi * math.Pi / 3.0 +
      (1 to 4).map(k => 4.0 * math.pow(-1, k) / (k * k) * math.cos(k * t)).sum
    for at <- List(-2.5, -1.0, 0.0, 1.0, 2.5) do
      assertClose(valueAt(s, at), analytic(at), 1e-6, s"x^2 partial sum at $at:")
  }

  it should "be even, so every sine coefficient vanishes" in
  {
    val s = parse("fourierSeries(x^2, x, 2*pi, 3)")
    for at <- List(0.4, 1.2, 2.8) do
      assertClose(valueAt(s, at), valueAt(s, -at), 1e-6, s"evenness at $at:")
  }

  "a pure harmonic" should "be reproduced exactly by its own coefficient" in
  {
    // sin(x) over period 2pi is already a Fourier series: b_1 = 1, everything else 0
    val s = parse("fourierSeries(sin(x), x, 2*pi, 3)")
    for at <- List(-2.0, -0.7, 0.0, 0.7, 2.0) do
      assertClose(valueAt(s, at), math.sin(at), 1e-6, s"sin reproduced at $at:")
    val c = parse("fourierSeries(cos(2*x), x, 2*pi, 3)")
    for at <- List(-1.5, 0.0, 1.5) do
      assertClose(valueAt(c, at), math.cos(2 * at), 1e-6, s"cos(2x) reproduced at $at:")
  }

  "a non-2pi period" should "scale the harmonics correctly" in
  {
    // period 2 => omega = pi; sin(pi*x) is the first harmonic and must come back as itself
    val s = parse("fourierSeries(sin(pi*x), x, 2, 3)")
    for at <- List(-0.8, -0.25, 0.0, 0.25, 0.8) do
      assertClose(valueAt(s, at), math.sin(math.Pi * at), 1e-6, s"period-2 harmonic at $at:")
  }

  "raising the Fourier order" should "not worsen the approximation of a smooth periodic function" in
  {
    // cos(x) + 0.5*sin(2x) is exactly representable from order 2 onwards
    def err(n: Int): Double =
      val s = parse(s"fourierSeries(cos(x) + 0.5 * sin(2*x), x, 2*pi, $n)")
      List(-2.0, -0.6, 0.4, 1.7).map(at =>
        math.abs(valueAt(s, at) - (math.cos(at) + 0.5 * math.sin(2 * at)))).max
    assert(err(2) < 1e-6, s"order 2 should already be exact: ${err(2)}")
    assert(err(4) <= err(2) + 1e-9, s"order 4 should not be worse: ${err(2)} -> ${err(4)}")
  }

  // --- guards ---

  "a non-positive or non-numeric period" should "stay symbolic" in
  {
    assert(parse("fourierSeries(x, x, 0, 3)").eval(env).isLeft, "zero period")
    assert(parse("fourierSeries(x, x, -1, 3)").eval(env).isLeft, "negative period")
    assert(parse("fourierSeries(x, x, T, 3)").eval(env).isLeft, "symbolic period")
  }

  "a bad Fourier order" should "stay symbolic" in
  {
    assert(parse("fourierSeries(x, x, 2*pi, -1)").eval(env).isLeft)
    assert(parse("fourierSeries(x, x, 2*pi, 1.5)").eval(env).isLeft)
    assert(parse(s"fourierSeries(x, x, 2*pi, ${MaxFourierOrder + 1})").eval(env).isLeft)
  }

  "an integrand with another free variable" should "stay symbolic" in
  {
    // the coefficient integrals cannot reduce to numbers
    assert(parse("fourierSeries(a * x, x, 2*pi, 2)").eval(env).isLeft)
  }

  "fourierSeries" should "not be shadowed by the fourier transform keyword" in
  {
    // both must parse, to their own node types
    assert(parse("fourierSeries(x, x, 2*pi, 2)").isInstanceOf[_FourierSeries])
    assert(parse("fourier(exp(-2*t), t, w)").isInstanceOf[transform._Fourier])
    assert(Parser.ReservedWords.contains("fourierSeries"))
  }

  it should "round-trip through toString" in
  {
    // An exactly-representable period: _Number.toString rounds at DefaultPrecision, so
    // an irrational literal such as pi cannot survive an exact AST round-trip -- that is a
    // pre-existing property of _Number, not of this node.
    for e <- List[_Expression](
          _FourierSeries(Sin(x), x, _Number(2), _Number(3)),
          _FourierSeries(Sum(x, _Number(1)), x, _Number(4), _Number(0))) do
      val reparsed = Parser.parse(e.toString)
      assert(reparsed.successful, s"did not re-parse: ${e.toString} ($reparsed)")
      assert(reparsed.get == e, s"round-trip changed it: ${reparsed.get}")
  }
  // --- 4.J tier 3: Pade approximants ---

  "the Pade [1/1] of exp(x)" should "be the known (2 + x)/(2 - x)" in
  {
    val s = parse("pade(exp(x), x, 1, 1)")
    for at <- List(-1.0, -0.3, 0.0, 0.3, 1.0) do
      assertClose(valueAt(s, at), (2.0 + at) / (2.0 - at), 1e-9, s"[1/1] of exp at $at:")
  }

  "the Pade [2/2] of exp(x)" should "be the known (1 + x/2 + x^2/12)/(1 - x/2 + x^2/12)" in
  {
    val s = parse("pade(exp(x), x, 2, 2)")
    def known(t: Double): Double =
      (1 + t / 2 + t * t / 12) / (1 - t / 2 + t * t / 12)
    for at <- List(-1.5, -0.5, 0.0, 0.5, 1.5) do
      assertClose(valueAt(s, at), known(at), 1e-9, s"[2/2] of exp at $at:")
  }

  "a Pade approximant" should "match the function's series to order m + n" in
  {
    // the defining property: R(x) - f(x) = O(x^(m+n+1)), so the error must fall off at
    // that rate. Halving x should cut the error by about 2^(m+n+1).
    val s = parse("pade(exp(x), x, 2, 2)")           // m + n = 4, so error ~ x^5
    def err(at: Double): Double = math.abs(valueAt(s, at) - math.exp(at))
    val (e1, e2) = (err(0.2), err(0.1))
    assert(e2 > 0.0, "the approximation should not be exact, or the test proves nothing")
    val ratio = e1 / e2
    assert(ratio > 16.0 && ratio < 64.0,
      s"error should fall like x^5 (ratio about 32 when x halves), got $ratio")
  }

  it should "reproduce a rational function exactly" in
  {
    // 1/(1+x) IS a [0/1] rational function, so its own Pade approximant is itself
    val s = parse("pade(1 / (1 + x), x, 0, 1)")
    for at <- List(-0.5, 0.0, 0.5, 2.0, 10.0) do
      assertClose(valueAt(s, at), 1.0 / (1.0 + at), 1e-9, s"1/(1+x) at $at:")
  }

  it should "reduce to the Taylor polynomial when the denominator degree is 0" in
  {
    val pade   = parse("pade(exp(x), x, 4, 0)")
    val taylor = parse("maclaurin(exp(x), x, 4)")
    for at <- List(-1.0, -0.2, 0.0, 0.7, 1.3) do
      assertClose(valueAt(pade, at), valueAt(taylor, at), 1e-9, s"[4/0] vs Taylor at $at:")
  }

  "Pade" should "beat the Taylor polynomial of the same total degree away from zero" in
  {
    // the whole point of the method: a rational form can model behaviour a polynomial
    // of equal cost cannot. Compare [2/2] against the order-4 Taylor at x = 1.
    val padeErr   = math.abs(valueAt(parse("pade(exp(x), x, 2, 2)"), 1.0) - math.E)
    val taylorErr = math.abs(valueAt(parse("maclaurin(exp(x), x, 4)"), 1.0) - math.E)
    assert(padeErr < taylorErr,
      s"Pade should be more accurate here: pade $padeErr vs taylor $taylorErr")
  }

  // --- guards ---

  "a bad Pade degree" should "stay symbolic" in
  {
    assert(parse("pade(exp(x), x, -1, 2)").eval(env).isLeft, "negative m")
    assert(parse("pade(exp(x), x, 2, -1)").eval(env).isLeft, "negative n")
    assert(parse("pade(exp(x), x, 1.5, 2)").eval(env).isLeft, "non-integer m")
    assert(parse(s"pade(exp(x), x, $MaxTaylorOrder, 2)").eval(env).isLeft, "m + n beyond the cap")
  }

  "a Pade of an underivable expression" should "stay symbolic" in
  {
    assert(parse("pade(Gamma(x), x, 1, 1)").eval(env).isLeft)
  }

  "a Pade whose linear system is singular" should "stay symbolic, not divide by zero" in
  {
    // f = x has c0 = 0, c1 = 1, c2 = 0; the [1/1] system is 1*q1*c1 = -c2 -> q1 = 0,
    // which is fine, but f = x^2 with [1/1] gives a singular system (c1 = 0).
    val r = parse("pade(x^2, x, 1, 1)").eval(env)
    assert(r.isLeft || r.isRight, "must not throw")
    // whatever it decides, it must not produce a non-finite value
    r match
      case Right(_Number(d)) => assert(!d.isNaN && !d.isInfinite, s"non-finite result: $d")
      case _                 => succeed
  }

  "the Pade grammar" should "parse and round-trip" in
  {
    assert(parse("pade(exp(x), x, 2, 3)") == _Pade(Exp(x), x, _Number(2), _Number(3)))
    assert(Parser.ReservedWords.contains("pade"))
    val e: _Expression = _Pade(Sin(x), x, _Number(1), _Number(2))
    val reparsed = Parser.parse(e.toString)
    assert(reparsed.successful, s"did not re-parse: ${e.toString}")
    assert(reparsed.get == e, s"round-trip changed it: ${reparsed.get}")
  }
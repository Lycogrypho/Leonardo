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

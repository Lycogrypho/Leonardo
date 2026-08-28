package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** Issue 3.7 — the table of integrals.
 *
 *  **Every rule is verified by differentiating the antiderivative back** and comparing it
 *  against the integrand at several points.  A transcription slip and a correct rule look
 *  identical in the source; only `d/dx` tells them apart.  The comparison is against the
 *  *parsed integrand itself*, not against a hand-written Scala lambda, so the test cannot
 *  repeat the same mistake the rule would have made.
 */
class IntegralTableTest extends AnyFlatSpec:

  private val env = new Environment()
  private val x   = _Variable("x")

  private def run(src: String): String =
    Parser.parse(src) match
      case Parser.Success(e, _) => e.eval(env).fold(_.toString, _.toString)
      case other                => fail(s"parse failed for '$src': $other")

  private def exprOf(src: String): _Expression =
    Parser.parse(src) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for '$src': $other")

  private def hasIntegral(e: _Expression): Boolean =
    e.isInstanceOf[_Integral] || e.children.exists(hasIntegral)

  /** Numeric value of an expression at `x = p`. */
  private def valueAt(e: _Expression, p: Double): Option[Double] =
    e.eval(new Environment(variables = Map("x" -> _Number(p)))) match
      case Right(_Number(d)) if !d.isNaN && !d.isInfinite => Some(d)
      case _                                              => None

  /** Integrates, asserts it closed, then differentiates back and compares to the integrand.
   *
   *  **Everything stays an `_Expression`.**  Round-tripping the antiderivative through
   *  `toString` and re-parsing would quantise every irrational constant it contains to the
   *  display precision — `ln(2)` becomes `0.69315`, a 4e-6 relative error that shows up as a
   *  failing rule rather than as the display artefact it is.  `derive` is applied to the
   *  expression the table actually produced.
   */
  private def verify(integrand: String, points: Double*): Unit =
    val f    = exprOf(integrand)
    val anti = integrate(f, x)
    assert(!hasIntegral(anti), s"∫ $integrand dx should close, got $anti")
    val back = derive(anti, x)
    for p <- points do
      (valueAt(back, p), valueAt(f, p)) match
        case (Some(got), Some(want)) =>
          assert(math.abs(got - want) < 1e-9 * math.max(1.0, math.abs(want)),
                 s"d/dx of ∫ $integrand dx at x=$p: got $got, integrand is $want" +
                 s"\n  antiderivative = $anti")
        case (d, g) =>
          fail(s"could not evaluate at x=$p (derivative=$d, integrand=$g)\n  anti = $anti")

  // ── trigonometric entries ──────────────────────────────────────────────────

  "the table" should "integrate tan(v)^2" in    { verify("tan(x)^2", 0.3, 0.7, 1.0) }
  it should "integrate cot(v)" in               { verify("cos(x) / sin(x)", 0.5, 1.0, 2.0) }
  it should "integrate sec(v)^2" in             { verify("1 / cos(x)^2", 0.3, 0.7, 1.0) }
  it should "integrate csc(v)^2" in             { verify("1 / sin(x)^2", 0.5, 1.0, 2.0) }
  it should "integrate sec(v)*tan(v)" in        { verify("sin(x) / cos(x)^2", 0.3, 0.7, 1.0) }
  it should "integrate sec(v)" in               { verify("1 / cos(x)", 0.3, 0.7, 1.0) }
  it should "integrate csc(v)" in               { verify("1 / sin(x)", 0.5, 1.0, 2.0) }
  it should "integrate sin(v)*cos(v)" in        { verify("sin(x) * cos(x)", 0.3, 0.7, 1.0) }
  it should "integrate the commuted cos(v)*sin(v)" in { verify("cos(x) * sin(x)", 0.3, 0.7, 1.0) }

  // ── exponential and logarithmic entries ────────────────────────────────────

  it should "integrate ln(v)^2" in              { verify("ln(x)^2", 1.5, 2.0, 3.0) }
  it should "integrate a constant-base power" in { verify("2^x", 0.5, 1.0, 2.0) }
  it should "integrate exp(v)/(1+exp(v))" in    { verify("exp(x) / (1 + exp(x))", 0.3, 1.0, 2.0) }
  it should "integrate the commuted exp(v)/(exp(v)+1)" in
                                                { verify("exp(x) / (exp(x) + 1)", 0.3, 1.0, 2.0) }

  // ── the cyclic pair, which integration by parts deliberately refuses ───────

  it should "integrate exp(v)*sin(v)" in        { verify("exp(x) * sin(x)", 0.3, 1.0, 2.0) }
  it should "integrate the commuted sin(v)*exp(v)" in { verify("sin(x) * exp(x)", 0.3, 1.0) }
  it should "integrate exp(v)*cos(v)" in        { verify("exp(x) * cos(x)", 0.3, 1.0, 2.0) }
  it should "integrate the commuted cos(v)*exp(v)" in { verify("cos(x) * exp(x)", 0.3, 1.0) }

  // ── the linear-argument generalisation (slice A) ───────────────────────────

  "a linear inner argument" should "be handled by dividing through by the slope" in
  {
    verify("tan(2 * x)", 0.3, 0.5)
    verify("1 / cos(3 * x)", 0.2, 0.4)
    verify("sin(2 * x) * cos(2 * x)", 0.3, 0.7)
    verify("2^(3 * x)", 0.2, 0.5)
  }

  it should "handle an offset as well as a slope" in
  {
    // Points chosen so the argument stays below pi/2: past it cos is negative and the
    // antiderivative -ln(cos(2x+1))/2 is legitimately complex, which would fail the test
    // for a reason unrelated to the rule.
    verify("tan(2 * x + 1)", 0.05, 0.15)
    verify("cos(2 * x + 1) / sin(2 * x + 1)", 0.3, 0.6)
  }

  it should "handle a negative slope" in
  {
    verify("tan(-x)", 0.3, 0.7)
  }

  // ── what the table must NOT claim ──────────────────────────────────────────

  "a non-linear inner argument" should "stay symbolic rather than be answered wrongly" in
  {
    // tan(x^2) has no elementary antiderivative. The slope test is what refuses it: a rule
    // shaped Tg(?v) would otherwise fire and produce -ln(cos(x^2)), which is simply wrong.
    assert(run("integral(tan(x^2), x)").startsWith("integral("))
  }

  "a rule in the wrong variable" should "not fire" in
  {
    // ?v binds `y`, whose derivative in x is zero -- so no slope, so no rule.
    val r = run("integral(cos(y) / sin(y), x)")
    assert(!r.contains("ln"), s"a rule about x must not fire on an integrand in y, got $r")
  }

  "a variable exponential base" should "be refused conservatively" in
  {
    // x^x is not a^v: the base is not constant. The condition must reject it rather than
    // return x^x/ln(x).
    assert(run("integral(x^x, x)").startsWith("integral("))
  }

  "a base of one" should "be refused" in
  {
    // 1^x = 1 integrates to x, but a^v/ln(a) would divide by ln(1) = 0. The compiled
    // constant rule should answer this, and in any case the table must not.
    val r = run("integral(1^x, x)")
    assert(!r.contains("ln("), s"must not divide by ln(1), got $r")
  }

  // ── the safety property: nothing that already worked may change ────────────

  "the compiled arms" should "still take precedence over the table" in
  {
    assert(run("integral(1 / x, x)") == "ln(x)")
    assert(run("integral(x^2, x)") == "((x ^ 3.0) / 3.0)")
    assert(run("integral(sin(x)^2, x)").contains("0.5"))
    // The atan/asin ratio rules and the rational tier close before the table is consulted.
    assert(run("integral(1 / (1 + x^2), x)").contains("atan"))
    assert(run("integral(x / (1 + x^2), x)").contains("ln"))
  }

  "an integrand no rule covers" should "still stay symbolic" in
  {
    assert(run("integral(exp(x^2), x)").startsWith("integral("))
  }

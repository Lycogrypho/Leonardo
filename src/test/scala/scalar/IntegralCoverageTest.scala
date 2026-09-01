package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** Issue 3.18 — the three coverage gaps the 3.16 census probe left open.
 *
 *  1. half-integer radical powers, which the trig-substitution tier matched only at `^0.5`;
 *  2. the Weierstrass tier, which required the *bare* variable inside every trig node;
 *  3. commuted inner products in the parameterised table rules, resolved once by teaching
 *     the unifier that `Product` is commutative rather than by adding spelling twins.
 *
 *  Every antiderivative is verified by differentiating it back.
 */
class IntegralCoverageTest extends AnyFlatSpec:

  private val x = _Variable("x")

  private def parse(src: String): _Expression =
    Parser.parse(src) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for '$src': $other")

  private def valueAt(e: _Expression, p: Double): Option[Double] =
    e.eval(new Environment(variables = Map("x" -> _Number(p)))) match
      case Right(_Number(d)) if !d.isNaN && !d.isInfinite => Some(d)
      case _                                              => None

  private def hasIntegral(e: _Expression): Boolean =
    e.isInstanceOf[_Integral] || e.children.exists(hasIntegral)

  private def verify(integrand: String, points: Double*): Unit =
    val f    = parse(integrand)
    val anti = integrate(f, x)
    assert(!hasIntegral(anti), s"∫ $integrand should close, got $anti")
    val back = derive(anti, x)
    for p <- points do
      (valueAt(back, p), valueAt(f, p)) match
        case (Some(got), Some(want)) =>
          assert(math.abs(got - want) < 1e-6 * math.max(1.0, math.abs(want)),
                 s"d/dx ∫ $integrand at x=$p: got $got, want $want  (anti=$anti)")
        case (g, w) => fail(s"could not evaluate at x=$p ($g, $w); anti=$anti")

  // ── 1. half-integer radical powers ──────────────────────────────────────────

  "a radical raised to 3/2" should "close by trig substitution" in
  {
    verify("(4 - x^2)^1.5", -1.5, 0.5, 1.5)
    verify("(x^2 + 1)^1.5", -1.0, 0.5, 1.5)
  }

  "a negative half-integer radical power" should "close" in
  {
    verify("(4 - x^2)^-1.5", -1.2, 0.5, 1.2)
  }

  // ── 2. Weierstrass with a linear argument ───────────────────────────────────

  "a trig rational in a linear argument" should "close by the half-angle substitution" in
  {
    verify("1 / (1 + sin(2 * x))", 0.2, 0.6)
    verify("1 / (2 + cos(3 * x))", 0.2, 0.5)
  }

  it should "still refuse a genuinely non-rational trig integrand" in
  {
    // two different frequencies are not a rational function of one half-angle t
    val f = parse("1 / (sin(x) + cos(2 * x))")
    assert(hasIntegral(integrate(f, x)), "mixed frequencies must stay symbolic")
  }

  // ── 3. commuted inner products in the parameterised rules ───────────────────

  "a commuted inner product" should "match the same table rule" in
  {
    // sin(x*a) is the same integrand as sin(a*x); the rule is written only one way
    verify("sinh(x * 3)", 0.2, 0.5)
    verify("cosh(x * 2)", 0.2, 0.5)
  }

  it should "match commuted outer products too" in
  {
    verify("cos(5 * x) * sin(2 * x)", 0.3, 0.7)
    verify("sin(3 * x) * exp(2 * x)", 0.3, 0.7)
  }

  // ── regression: the compiled tiers still win ────────────────────────────────

  "the compiled arms" should "still take precedence" in
  {
    assert(integrate(parse("x^2"), x) == Ratio(Power(x, _Number(3.0)), _Number(3.0)))
    assert(integrate(parse("1 / x"), x) == Ln(x))
  }

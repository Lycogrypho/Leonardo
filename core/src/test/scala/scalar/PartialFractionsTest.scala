package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** Issue 3.12 — full partial-fraction integration: repeated and complex roots at degree ≥ 3.
 *
 *  The old residue path closed only DISTINCT REAL roots at degree ≥ 3; this lifts that to the
 *  general decomposition (repeated real roots, irreducible quadratics from complex conjugate
 *  pairs, and repeated quadratics), each verified by differentiating the antiderivative back.
 */
class PartialFractionsTest extends AnyFlatSpec:

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

  // ── F_0119: a tiny leading coefficient is kept, a residue one is not ──

  /** The antiderivative differentiates back to the integrand **relatively**, at every point. */
  private def verifyRelative(integrand: String, points: Double*): Unit =
    val f    = parse(integrand)
    val anti = integrate(f, x)
    assert(!hasIntegral(anti), s"∫ $integrand should close, got $anti")
    val back = derive(anti, x)
    for p <- points do
      (valueAt(back, p), valueAt(f, p)) match
        case (Some(got), Some(want)) =>
          assert(math.abs(got - want) <= 1e-6 * math.abs(want), s"d/dx ∫ $integrand at x=$p: got $got, want $want  (anti=$anti)")
        case (g, w) => fail(s"could not evaluate at x=$p ($g, $w); anti=$anti")

  "1/(1e-12x² + x + 1)" should "integrate as the quadratic it is, not as 1/(x + 1) (F_0119)" in
  {
    verifyRelative("1/(1e-12*x^2 + x + 1)", 0.0, 3.0, 1e11, -2e12)
  }

  "an improper fraction with a tiny leading coefficient" should "keep it in the polynomial part (F_0121)" in
  {
    // The quotient's 1e-12·x² term was dropped as below 1e-9: 60 % wrong by x = 1e12.
    verifyRelative("(1e-12*x^3 + x^2 + 1)/(x + 1)", 0.0, 3.0, 1e6, 1e12)
  }

  "a rational integral at a small scale" should "keep its numerators, its tiny coefficients, its remainder and its degree (F_0125)" in
  {
    // Every partial-fraction numerator of 1e-12/((x-1)(x-2)(x-3)) is below 1e-9: the antiderivative was 0.
    verifyRelative("1e-12/((x - 1)*(x - 2)*(x - 3))", 4.0, 5.0, -1.0)
    // A coefficient below 1e-15 folded to 0: ∫ 1e-16/(x + 1) was 0.
    verifyRelative("1e-16/(x + 1)", 0.0, 3.0)
    // The remainder 1e-10 of (x + 1e-10)/x was dropped, and with it 1e-10·ln x; near x = 1e-12
    // that term is most of the integrand.
    verifyRelative("(x + 1e-10)/x", 1e-12, 1.0)
    // factorDenominator read 1e-10·x³ + x + 1 as linear through polyDegree, and declined.
    verifyRelative("1/(1e-10*x^3 + x + 1)", 0.0, 3.0, 1e4)
  }

  "a denominator whose x² coefficient is rounding residue" should "integrate as the linear denominator it is" in
  {
    verifyRelative("1/((0.1*x + 0.2)*(0.3*x) - 0.03*x^2)", 1.0, 1e3, -5.0)
  }

  // ── F_0118: distinct roots near the origin stay distinct ──

  "a cubic denominator with roots near 1e-6" should "decompose into its three simple poles, as it does at unit scale" in
  {
    verify("1/((x - 1e-6)*(x - 2e-6)*(x - 3e-6))", 5e-6, 1.5e-6, -2e-6)
    verify("1/((x - 1)*(x - 2)*(x - 3))", 5.0, 1.5, -2.0)
  }

  // ── repeated real roots at degree ≥ 3 ────────────────────────────────────────

  "a repeated real root (deg 3)" should "decompose and close" in
  {
    verify("1 / ((x - 1)^2 * (x - 2))", 2.5, 3.0, -1.0)
    verify("x / ((x - 1)^2 * (x + 2))", 2.5, 3.0)
  }

  "a triple real root (deg 3)" should "close" in
  {
    verify("(x + 1) / (x - 3)^3", 4.0, 5.0, -1.0)
  }

  // ── complex conjugate pairs at degree ≥ 3 ────────────────────────────────────

  "a complex pair with a real root (deg 3)" should "close in real log+arctan form" in
  {
    verify("1 / ((x - 1) * (x^2 + 1))", 2.0, 3.0, -1.0)
    verify("x / ((x + 2) * (x^2 + 4))", 1.0, 2.0, -1.0)
  }

  // ── repeated complex pair ────────────────────────────────────────────────────

  "a repeated irreducible quadratic (deg 4)" should "close" in
  {
    verify("1 / (x^2 + 1)^2", 0.5, 1.0, 2.0)
    verify("(x + 1) / (x^2 + 1)^2", 0.5, 1.0, 2.0)
  }

  // ── mixed ────────────────────────────────────────────────────────────────────

  "a mixed repeated-real and complex denominator (deg 4)" should "close" in
  {
    verify("x / ((x - 1)^2 * (x^2 + 4))", 2.0, 3.0)
  }

  // ── regression: distinct real roots (the pre-existing residue path) ───────────

  "distinct real roots (deg 3)" should "still close" in
  {
    verify("(x^2 + 1) / ((x - 1) * (x - 2) * (x - 3))", 4.0, 5.0, -1.0)
  }

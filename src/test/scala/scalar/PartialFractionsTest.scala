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

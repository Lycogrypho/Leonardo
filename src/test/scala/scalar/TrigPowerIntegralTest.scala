package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** Issue 3.11 — reduction formulas for `tan`/`sec`/`csc`/`cot` powers.
 *
 *  The mirror of `reduceSinCosPower`: `∫ tanⁿ = tanⁿ⁻¹/((n−1)a) − ∫ tanⁿ⁻²`,
 *  `∫ secⁿ = secⁿ⁻²·tan/((n−1)a) + (n−2)/(n−1)·∫ secⁿ⁻²`, cot/csc the sign mirrors.
 *  Each antiderivative is checked by differentiating it back at sample points.
 */
class TrigPowerIntegralTest extends AnyFlatSpec:

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

  /** Integrate, assert it closed, differentiate back and compare to the integrand. */
  private def verify(integrand: String, points: Double*): Unit =
    val f    = parse(integrand)
    val anti = integrate(f, x)
    assert(!hasIntegral(anti), s"∫ $integrand should close, got $anti")
    val back = derive(anti, x)
    for p <- points do
      (valueAt(back, p), valueAt(f, p)) match
        case (Some(got), Some(want)) =>
          assert(math.abs(got - want) < 1e-7 * math.max(1.0, math.abs(want)),
                 s"d/dx ∫ $integrand at x=$p: got $got, want $want  (anti=$anti)")
        case (g, w) => fail(s"could not evaluate at x=$p ($g, $w); anti=$anti")

  // Points are kept well inside a single branch so the antiderivative stays real.

  "tangent powers" should "reduce and close" in
  {
    verify("tan(x)^2", 0.3, 0.7, 1.0)
    verify("tan(x)^3", 0.3, 0.7, 1.0)
    verify("tan(x)^4", 0.3, 0.7)
    verify("tan(x)^5", 0.3, 0.6)
  }

  "cotangent powers" should "reduce and close" in
  {
    verify("cot(x)^2", 0.5, 1.0, 1.3)
    verify("cot(x)^3", 0.5, 1.0)
    verify("cot(x)^4", 0.6, 1.0)
  }

  "secant powers" should "reduce and close" in
  {
    verify("sec(x)^2", 0.3, 0.7, 1.0)
    verify("sec(x)^3", 0.3, 0.7)
    verify("sec(x)^4", 0.3, 0.7)
  }

  "cosecant powers" should "reduce and close" in
  {
    verify("csc(x)^2", 0.5, 1.0, 1.3)
    verify("csc(x)^4", 0.6, 1.0)
  }

  "the linear-argument chain rule" should "carry through the reduction" in
  {
    verify("tan(2 * x)^2", 0.2, 0.5)
    verify("sec(2 * x)^3", 0.1, 0.3)
  }

  "a non-linear inner argument" should "stay symbolic" in
  {
    val f = parse("tan(x^2)^2")
    assert(hasIntegral(integrate(f, x)), s"∫ tan(x²)² should stay symbolic, got ${integrate(f, x)}")
  }

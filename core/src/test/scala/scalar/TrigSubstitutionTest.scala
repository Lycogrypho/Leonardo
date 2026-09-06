package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** Issue 3.13 — trigonometric (and hyperbolic) substitution for radical integrands.
 *
 *  `√(a²−v²)` → `v = a·sinθ`; `√(a²+v²)` → `v = a·sinh t`; `√(v²−a²)` → `v = a·cosh t`
 *  (the last two use the hyperbolic substitution so the result is written with the 3.9
 *  inverse-hyperbolic functions).  Each antiderivative is differentiated back and compared to
 *  the integrand, so its algebraic form is irrelevant — only that it is a correct primitive.
 */
class TrigSubstitutionTest extends AnyFlatSpec:

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

  // ── √(a² − v²): sine substitution ────────────────────────────────────────────

  "√(a² − v²) forms" should "close by sine substitution" in
  {
    verify("1 / (4 - x^2)^0.5", -1.5, 0.5, 1.5)     // asin(x/2)
    verify("(4 - x^2)^0.5", -1.5, 0.5, 1.5)         // area form
    verify("x^2 / (4 - x^2)^0.5", -1.5, 0.5, 1.5)
  }

  // ── √(a² + v²): hyperbolic-sine substitution ─────────────────────────────────

  "√(a² + v²) forms" should "close by hyperbolic substitution" in
  {
    verify("1 / (x^2 + 1)^0.5", -1.0, 0.5, 2.0)     // asinh(x)
    verify("(x^2 + 9)^0.5", -2.0, 0.5, 3.0)
  }

  // ── √(v² − a²): hyperbolic-cosine substitution ───────────────────────────────

  "√(v² − a²) forms" should "close by hyperbolic substitution" in
  {
    verify("1 / (x^2 - 4)^0.5", 2.5, 3.0, 4.0)      // acosh(x/2)
    verify("(x^2 - 1)^0.5", 1.5, 2.0, 3.0)
  }

  // ── a non-radical integrand is untouched ─────────────────────────────────────

  "an integrand with no radical" should "not be affected by the trig-sub tier" in
  {
    assert(integrate(parse("x^2"), x) == Ratio(Power(x, _Number(3.0)), _Number(3.0)))
  }

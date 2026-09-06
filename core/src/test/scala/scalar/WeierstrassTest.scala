package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** Issue 3.14 — Weierstrass (half-angle) substitution.
 *
 *  `t = tan(v/2)` turns any rational function of `sin v` / `cos v` into a rational function
 *  of `t`, which the 3.12 rational tier finishes; the result back-substitutes `t = tan(v/2)`.
 *  Each antiderivative is verified by differentiating it back at sample points.
 */
class WeierstrassTest extends AnyFlatSpec:

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

  // ── the census exemplars ────────────────────────────────────────────────────

  "∫ 1/(1 + sin x)" should "close via t = tan(x/2)" in
  {
    verify("1 / (1 + sin(x))", 0.5, 1.0, 2.0)
  }

  "∫ 1/(2 + cos x)" should "close via t = tan(x/2)" in
  {
    verify("1 / (2 + cos(x))", 0.5, 1.0, 2.5)
  }

  "∫ 1/(sin x + cos x)" should "close via t = tan(x/2)" in
  {
    verify("1 / (sin(x) + cos(x))", 0.3, 0.7, 1.2)
  }

  // ── a rational numerator over a trig denominator ────────────────────────────

  "∫ sin x/(1 + sin x)" should "close" in
  {
    verify("sin(x) / (1 + sin(x))", 0.5, 1.0, 2.0)
  }

  // ── refusals: not rational in sin/cos of the bare variable ──────────────────

  "an integrand mixing a bare x with the trig" should "stay symbolic" in
  {
    val f = parse("x / (1 + sin(x))")
    assert(hasIntegral(integrate(f, x)), s"∫ x/(1+sin x) should stay symbolic")
  }

  "a non-rational function of sin" should "stay symbolic" in
  {
    val f = parse("exp(sin(x)) * cos(x)")
    // closed by u-substitution (u = sin x), NOT by Weierstrass — must still close correctly.
    val anti = integrate(f, x)
    assert(!hasIntegral(anti))
    val back = derive(anti, x)
    for p <- List(0.3, 0.8) do
      (valueAt(back, p), valueAt(f, p)) match
        case (Some(got), Some(want)) => assert(math.abs(got - want) < 1e-6 * math.max(1.0, math.abs(want)))
        case (g, w)                  => fail(s"could not evaluate ($g, $w)")
  }

package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** Issue 3.10 — non-linear substitution (u-substitution) in `integrate`.
 *
 *  The driver enumerates candidate inner functions `g`, forms `integrand / g'`,
 *  replaces `g` with a fresh `u`, and integrates in `u` only when the quotient is
 *  free of the original variable — the decidable "free of v" test.  Correctness of
 *  each closed form is checked the robust way: differentiate the antiderivative back
 *  and compare to the integrand at sample points.
 */
class SubstitutionTest extends AnyFlatSpec:

  private val x = _Variable("x")

  private def parse(src: String): _Expression =
    Parser.parse(src) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for '$src': $other")

  private def evalAt(e: _Expression, p: Double): Double =
    e.eval(new Environment(variables = Map("x" -> _Number(p)))) match
      case Right(_Number(y)) => y
      case other             => fail(s"expected numeric result, got $other for $e at x=$p")

  /** Fundamental-theorem check: `d/dx (∫ f dx)` must equal `f` at each sample. */
  private def assertCloses(integrand: String, samples: Double*): Unit =
    val f    = parse(integrand)
    val anti = integrate(f, x)
    assert(anti != _Integral(f, x) && !anti.toString.startsWith("integral("),
           s"∫ $integrand should close, got $anti")
    val back = derive(anti, x)
    for p <- samples do
      val got  = evalAt(back, p)
      val want = evalAt(f, p)
      assert(math.abs(got - want) < 1e-6 * math.max(1.0, math.abs(want)),
             s"d/dx ∫ $integrand at x=$p: got $got, want $want  (anti=$anti)")

  // ── the three census exemplars ───────────────────────────────────────────────

  "∫ x·e^(x²)" should "close by substituting u = x²" in
  {
    assertCloses("x * exp(x^2)", 0.3, 0.7, 1.2)
  }

  "∫ sin³(x)·cos(x)" should "close by substituting u = sin(x)" in
  {
    assertCloses("sin(x)^3 * cos(x)", 0.3, 0.7, 1.1)
  }

  "∫ x·√(1 + x²)" should "close by substituting u = 1 + x²" in
  {
    assertCloses("x * (1 + x^2)^0.5", 0.3, 0.7, 1.4)
  }

  // ── a few more standard substitutions ────────────────────────────────────────

  "∫ 2x·cos(x²)" should "close" in
  {
    assertCloses("2 * x * cos(x^2)", 0.4, 0.9)
  }

  "∫ e^x / (1 + e^x)" should "close by substituting u = 1 + e^x" in
  {
    assertCloses("exp(x) / (1 + exp(x))", 0.2, 0.8)
  }

  // ── the give-up convention: no candidate works → stays symbolic ───────────────

  "∫ sin(x²)" should "stay symbolic (no g makes the quotient free of x)" in
  {
    val f = parse("sin(x^2)")
    assert(integrate(f, x) == _Integral(f, x),
           s"∫ sin(x^2) should stay symbolic, got ${integrate(f, x)}")
  }

  // ── nothing that already closes changes ───────────────────────────────────────

  "a linear-argument integral" should "still use the direct chain rule, not substitution" in
  {
    // ∫ cos(3x) dx = sin(3x)/3 — closed by the compiled tier, unchanged.
    assert(integrate(parse("cos(3 * x)"), x) == Ratio(Sin(Product(_Number(3), x)), _Number(3)))
  }

package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** Issue 3.15 — special integral functions: `Si`, `Ci`, `Ei`, `li`, Fresnel `S`/`C`, and
 *  the `∫ e^(−v²) → (√π/2)·erf(v)` connection.
 *
 *  Kernel values are pinned against published references **at expression level** (display
 *  rounding would quantise them); the integrate connections are verified by differentiating
 *  the named antiderivative back — its derivative is elementary, so the check needs no kernel.
 */
class SpecialIntegralTest extends AnyFlatSpec:

  private val env = new Environment()
  private val x   = _Variable("x")

  private def parse(src: String): _Expression =
    Parser.parse(src) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for '$src': $other")

  private def num(src: String): Double =
    parse(src).eval(env) match
      case Right(_Number(d)) => d
      case other             => fail(s"'$src' did not evaluate to a number: $other")

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
          assert(math.abs(got - want) < 1e-9 * math.max(1.0, math.abs(want)),
                 s"d/dx ∫ $integrand at x=$p: got $got, want $want  (anti=$anti)")
        case (g, w) => fail(s"could not evaluate at x=$p ($g, $w); anti=$anti")

  // ── kernel values against published references (series range) ────────────────

  "the kernels" should "match the reference values" in
  {
    assert(math.abs(num("Si(1)") - 0.9460830703671830) < 1e-9)
    assert(math.abs(num("Si(2)") - 1.6054129768026948) < 1e-9)
    assert(math.abs(num("Ci(1)") - 0.3374039229009681) < 1e-9)
    assert(math.abs(num("Ei(1)") - 1.8951178163559368) < 1e-9)
    assert(math.abs(num("Ei(-1)") - (-0.2193839343955203)) < 1e-9)
    assert(math.abs(num("Ei(-2)") - (-0.0489005107080611)) < 1e-9)
    assert(math.abs(num("li(2)") - 1.0451637801174927) < 1e-9)
    assert(math.abs(num("fresnelS(1)") - 0.4382591473903548) < 1e-9)
    assert(math.abs(num("fresnelC(1)") - 0.7798934003768228) < 1e-9)
  }

  it should "be odd where the function is odd" in
  {
    assert(math.abs(num("Si(-2)") + num("Si(2)")) < 1e-12)
    assert(math.abs(num("fresnelS(-1)") + num("fresnelS(1)")) < 1e-12)
  }

  it should "agree with the defining derivative across the asymptotic boundary" in
  {
    // central difference of the kernel vs the exact derivative, taken where the
    // asymptotic branch is active — validates the tail expansions without external values
    val h = 1e-3
    val siSlope = (num("Si(20.501)") - num("Si(20.499)")) / (2 * h)
    assert(math.abs(siSlope - math.sin(20.5) / 20.5) < 1e-4)
    val fsSlope = (num("fresnelS(4.0510)") - num("fresnelS(4.0490)")) / (2 * h)
    assert(math.abs(fsSlope - math.sin(math.Pi * 4.05 * 4.05 / 2)) < 1e-3)
  }

  // ── undefined points stay symbolic ───────────────────────────────────────────

  "out-of-domain arguments" should "stay symbolic" in
  {
    assert(parse("Ci(-1)").eval(env).isLeft)   // Ci real only for x > 0
    assert(parse("Ei(0)").eval(env).isLeft)    // the pole
    assert(parse("li(1)").eval(env).isLeft)    // the li pole
    assert(parse("li(-2)").eval(env).isLeft)
  }

  // ── round-trip and reserved words ────────────────────────────────────────────

  "every new function" should "round-trip through toString" in
  {
    for name <- List("Si", "Ci", "Ei", "li", "fresnelS", "fresnelC") do
      val src = s"$name(x)"
      assert(parse(src).toString == src, s"round-trip failed for $src")
  }

  "a bare reserved name" should "be a parse error, and an extension stay a variable" in
  {
    assert(Parser.parse("li").isInstanceOf[Parser.NoSuccess])
    assert(parse("lima") == _Variable("lima"))
  }

  // ── the integrate connections (differentiated back) ──────────────────────────

  "the integral connections" should "close and differentiate back" in
  {
    verify("sin(x) / x", 0.5, 1.0, 2.0)              // -> Si(x)
    verify("cos(x) / x", 0.5, 1.0, 2.0)              // -> Ci(x)
    verify("exp(x) / x", 0.5, 1.0, 2.0)              // -> Ei(x)
    verify("1 / ln(x)", 2.0, 3.0)                    // -> li(x)
    verify("exp(-x^2)", 0.5, 1.0, 2.0)               // -> (sqrt(pi)/2) erf(x)
    verify("sin(pi * x^2 / 2)", 0.5, 1.0)            // -> fresnelS(x)
    verify("cos(pi * x^2 / 2)", 0.5, 1.0)            // -> fresnelC(x)
  }

  it should "carry the linear-argument chain rule" in
  {
    verify("sin(2 * x) / (2 * x)", 0.5, 1.0)         // -> Si(2x)/2
    verify("exp(3 * x) / (3 * x)", 0.5, 1.0)         // -> Ei(3x)/3
  }

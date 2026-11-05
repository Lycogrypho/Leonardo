package it.grypho.scala.leonardo
package scalar

import org.scalatest.flatspec.AnyFlatSpec
import core.*
import parser.Parser


/** F_0281 (LT L_0064): the definite integral answered across a pole and, over a long range, a
 *  number Simpson's rule at a fixed step count could not resolve.  Each case answers its true value
 *  or stays unevaluated; a wrong number is the defect. */
class DefIntegralTest extends AnyFlatSpec:

  private val env = new Environment()

  private def value(src: String, exact: Boolean = false): Option[_Value] =
    val p = Parser.parse(src, if exact then Some(30) else None)
    assert(p.successful, s"parse failed: $src")
    p.get.eval(env).toOption

  private def number(src: String): Option[Double] = value(src).collect { case _Number(d) => d }

  private def close(src: String, truth: Double, tol: Double = 1e-9): Unit =
    number(src) match
      case Some(d) => assert(math.abs(d - truth) <= tol * math.max(1.0, math.abs(truth)), s"$src: $d, true $truth")
      case None    => fail(s"$src declined")

  "an integral across a pole" should "decline, not answer (F_0281)" in {
    for src <- Seq("integral(sec(x)^2, x, -pi, pi)", "integral(1/(x - 2), x, 0, 3)",
                   "integral(1/x^2, x, -1, 2)", "integral(1/x^2, x, -1, 1)", "integral(tan(x), x, 1, 2)") do
      assert(value(src).isEmpty, s"$src answered ${value(src)}")
  }

  "an integral over a long range" should "answer its true value (F_0281)" in {
    close("integral(1/x^2, x, 1, 10000)", 0.9999)
    close("integral(cos(x), x, 0, 1000)", math.sin(1000))
    close("integral(cos(x), x, 1000, 0)", -math.sin(1000))                  // reversed bounds
    // Numeric, certified to the precision asked (5 digits): it answered 1.57078 before, 2.3e-5 off.
    close("integral((1 - x^2)^(1/2), x, -1, 1)", math.Pi / 2, 1e-6)
    close("integral(1/(1 + x^2), x, -1000, 1000)", 2 * math.atan(1000))     // FTC: atan
  }

  "an integrand with no antiderivative" should "be integrated to the precision asked, certified (F_0281)" in {
    // ∫₀¹ sin(x²) dx (Fresnel), and a narrow peak a fixed grid of 1000 steps resolves badly.
    close("integral(sin(x^2), x, 0, 1)", 0.3102683017233811, 1e-6)
    close("integral(exp(-((x - 3)/0.01)^2) * erf(x), x, 0, 10)", 0.01 * math.sqrt(math.Pi) * 0.9999779095030014, 1e-5)
    // The iterated integral with a variable inner limit still answers.
    close("integral(integral(x*y, y, 0, x), x, 0, 1)", 0.125, 1e-9)
  }

  "an exact integral" should "be exact through the antiderivative" in {
    value("integral(x^2, x, 0, 1)", exact = true) match
      case Some(r: _Rational) => assert(r.multiply(_Rational(3)) == _Rational.One, s"$r")
      case other              => fail(s"not exact: $other")
  }

  "a decline that spends the whole evaluation budget" should "come back promptly (F_0281, measured)" in {
    // A pole stops at the rounding of its panels long before the budget; what spends the whole
    // budget is an integrand finite everywhere that never settles: sin(1/x) near 0, compiled and
    // tree-evaluated (Gamma does not compile).
    for src <- Seq("integral(sec(x)^2, x, -pi, pi)", "integral(Gamma(x) * tan(x), x, 1, 2)",
                   "integral(abs(sin(1000*x)), x, 0, 100)", "integral(Gamma(2 + abs(sin(1000*x))), x, 0, 100)",
                   // nested: each outer sample sets up an inner integral, its setup charged to one budget
                   "integral(integral(abs(sin(1000*x*y)), y, 0, 1), x, 0, 100)",
                   "integral(integral(Gamma(2 + sin(x*y)), y, 0, 1), x, 0, 100000)",
                   // an expensive tree-evaluated sample: a sum per sample
                   "integral(sum(abs(sin(1000*k*x)), k, 1, 50), x, 0, 100)") do
      val t0 = System.nanoTime
      assert(value(src).isEmpty, src)
      val s = (System.nanoTime - t0) / 1e9
      info(f"$src declined in $s%.3f s")
      assert(s < 8, s"$src took $s s")
    // With more budget the oscillating integral must still never answer wrong.  A local error
    // estimate aliased it to 59.46 (true 200/π = 63.66) with every panel certified; two
    // incommensurate grids disagree there, so it declines.
    val big = env.withLimit(Limit.MaxQuadratureEvaluations, 2000000L)
    Parser.parse("integral(abs(sin(1000*x)), x, 0, 100)").get.eval(big) match
      case Right(_Number(d)) => assert(math.abs(d - 200 / math.Pi) <= 1e-3, s"answered $d")
      case Right(other)      => fail(s"not a number: $other")
      case Left(_)           => succeed
  }

  // F_0288 (LT L_0054): an improper integral is lim F(b) - lim F(a), when the antiderivative closes and is
  // differentiable across the open interval.
  "an improper integral" should "answer by the limits of its antiderivative (F_0288)" in {
    close("integral(exp(-x), x, 0, inf)", 1.0)
    close("integral(1/x^2, x, 1, inf)", 1.0)
    close("integral(exp(-x^2), x, -inf, inf)", math.sqrt(math.Pi))
    close("integral(x^(-1/2), x, 0, 1)", 2.0)                 // an integrable singular endpoint
    close("integral(ln(x), x, 0, 1)", -1.0)                   // F(0) only as a limit: x ln x − x → 0
    close("integral(1/(1 + x^2), x, -inf, inf)", math.Pi)
    assert(number("integral(1/x, x, 1, inf)").contains(Double.PositiveInfinity), "diverges to inf")
    assert(value("integral(sin(x), x, 0, inf)").isEmpty, "no limit at infinity")
    assert(value("integral(1/x^2, x, -1, inf)").isEmpty, "a pole inside")
  }

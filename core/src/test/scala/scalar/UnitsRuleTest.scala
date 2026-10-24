package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** F_0211: the thresholds left after F_0125 that were absolute below unit scale, and the naive
 *  complex division.  Each case failed before its fix. */
class UnitsRuleTest extends AnyFlatSpec:

  private val env = new Environment()
  private def eval(s: String): Either[_Expression, _Value] = Parser.parse(s).get.eval(env)

  "a numeric limit" should "not answer a small oscillation as a limit" in {
    // Both answered 0.0: the `1 + |y|` floor made the convergence test absolute below unit scale.
    assert(eval("limit(sin(x)/1e7, x, inf)").left.exists(_.isInstanceOf[_Limit]))
    assert(eval("limit(sin(1/x)*1e-7, x, 0, +)").left.exists(_.isInstanceOf[_Limit]))
  }

  it should "still answer a genuine limit of small size" in {
    // sin(1/x)/(1e7/x) → 1e-7 as x → ∞, through the numeric fallback
    eval("limit(sin(1/x)/(1e7/x), x, inf)") match
      case Right(_Number(d)) => assert(math.abs(d / 1e-7 - 1) < 1e-6, d)
      case other             => fail(s"$other")
  }

  "expand" should "not drop an operand whose complex product overflows" in {
    // It answered 1e200i·x²: `times` kept its left operand when the fold failed.
    val (r, why) = expandReporting(Parser.parse("(1e200*i*x)*(1e200*i*x)").get)
    assert(!r.toString.contains("(x ^ 2"), r)
    assert(why.exists(_.contains("coefficient")), why)
  }

  "complex division" should "not underflow the divisor's squared modulus (Smith)" in {
    eval("1/(1e-170*i)") match
      case Right(c: _Complex) => assert(c.re == 0.0 && math.abs(c.im / -1e170 - 1) < 1e-12, c)
      case other              => fail(s"$other")
    eval("(3 + 4*i)/(1 + 2*i)") match
      case Right(c: _Complex) => assert(math.abs(c.re - 2.2) < 1e-12 && math.abs(c.im + 0.4) < 1e-12, c)
      case other              => fail(s"$other")
  }

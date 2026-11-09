package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0320 (LT L_0074) — `derive` and `integral` on a series node compute the series first.  Before the
 *  fix each echoed itself (`derive(taylor(…), x)`), while `simplify derive(P, x)` answered. */
class SeriesCalculusTest extends AnyFlatSpec:

  private val env = new Environment()

  private def parse(s: String, exact: Option[Int] = None): _Expression = Parser.parse(s, exact) match
    case Parser.Success(e, _) => e
    case other                => fail(s"parse failed for \"$s\": $other")

  private def valueAt(e: _Expression, x: Double): Double =
    e.eval(env.withBinding("x", _Number(x))) match
      case Right(_Number(d)) => d
      case other             => fail(s"did not reduce at x = $x: $other")

  private def evaluated(s: String, exact: Option[Int] = None): _Expression = parse(s, exact).eval(env).toExpression

  "derive of a maclaurin series" should "be the derivative of the polynomial" in {
    val d = evaluated("derive(maclaurin(1/(1 - x), x, 6), x)")
    assert(!d.isInstanceOf[_Derivative], d.toString)
    // 1 + x + ... + x^6 differentiated: 1 + 2x + 3x^2 + 4x^3 + 5x^4 + 6x^5
    val expected = (1 to 6).map(k => k * math.pow(0.5, k - 1)).sum
    assert(math.abs(valueAt(d, 0.5) - expected) < 1e-12)
  }

  "integral of a maclaurin series" should "be the integral of the polynomial" in {
    val i = evaluated("integral(maclaurin(1/(1 + x), x, 5), x)")
    assert(!i.isInstanceOf[_Integral], i.toString)
    // x - x^2/2 + x^3/3 - x^4/4 + x^5/5 - x^6/6, up to a constant: compare F(0.5) - F(0)
    val expected = (1 to 6).map(k => math.pow(-1, k - 1) * math.pow(0.5, k) / k).sum
    assert(math.abs((valueAt(i, 0.5) - valueAt(i, 0.0)) - expected) < 1e-12)
  }

  "derive of a pade approximant" should "differentiate the rational function" in {
    val d = evaluated("derive(pade(exp(x), x, 2, 2), x)")
    assert(!d.isInstanceOf[_Derivative], d.toString)
    // the [2/2] approximant of exp agrees with exp to O(x^5), so its derivative is close to exp near 0
    assert(math.abs(valueAt(d, 0.3) - math.exp(0.3)) < 1e-3)
  }

  "a laurent series" should "differentiate and integrate too" in {
    val d = evaluated("derive(laurent(1/(x*(1 - x)), x, 0, 1, 2), x)")
    assert(!d.isInstanceOf[_Derivative], d.toString)
    // 1/x + 1 + x + x^2 differentiated: -1/x^2 + 1 + 2x
    assert(math.abs(valueAt(d, 0.5) - (-4.0 + 1.0 + 1.0)) < 1e-12)
  }

  "in exact mode" should "keep the coefficients exact" in {
    val d = evaluated("derive(maclaurin(sin(x), x, 5), x)", Some(30))
    assert(_Rational.containsExact(d), d.toString)
    // 1 - x^2/2 + x^4/24 at x = 1/2
    assert(math.abs(valueAt(d, 0.5) - (1 - 0.125 + 0.0625 / 24)) < 1e-12)
  }

  "a series that declines" should "leave the derivative as written" in {
    // Gamma has no series: the maclaurin declines, and the derivative keeps the node
    val d = evaluated("derive(maclaurin(Gamma(x), x, 3), x)")
    assert(d.isInstanceOf[_Derivative], d.toString)
  }

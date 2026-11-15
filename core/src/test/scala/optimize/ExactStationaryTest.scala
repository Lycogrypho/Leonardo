package it.grypho.scala.leonardo
package optimize

import core.*
import equation._Equation
import matrix._Matrix
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** F_0228: elimination's univariate step takes a rational root exactly on exact input, so one
 *  answer no longer mixes `x = 1.0` with `y = 1`. */
class ExactStationaryTest extends AnyFlatSpec:

  private val env = Environment()

  private def values(s: String, exact: Option[Int]): Vector[_Expression] =
    Parser.parse(s, exact).get.eval(env) match
      case Left(m: _Matrix) => m.elems.map { case _Equation(_, rhs) => rhs; case o => fail(s"not an equation: $o") }
      case other            => fail(s"no points: $other")

  "stationary on exact input with rational points" should "answer every coordinate exactly" in {
    val vs = values("stationary(2*x^3 - 6*x*y + 3*y^2, x, y)", Some(30))
    assert(vs.size == 4)
    assert(vs.forall(_.isInstanceOf[_Rational]), vs)
    assert(vs.toSet == Set[_Expression](_Rational(0), _Rational(1)))
  }

  it should "take a rational root exactly beside an irrational one" in {
    // x^3 - 2x = x (x^2 - 2): the root 0 is exact, ±√2 stay decimals.
    val vs = values("stationary(x^4/4 - x^2, x)", Some(30))
    assert(vs.size == 3)
    assert(vs.count(_.isInstanceOf[_Rational]) == 1)
  }

  "stationary on inexact input" should "keep the Double route" in {
    val vs = values("stationary(2*x^3 - 6*x*y + 3*y^2, x, y)", None)
    assert(vs.forall(v => !v.isInstanceOf[_Rational]))
  }

  "a letters-only problem under exact on" should "be answered and stated in the exact tier (F_0345)" in {
    val exactEnv = Environment(workingPrecision = 30, exactMode = true)
    def under(s: String): _Expression = Parser.parse(s, Some(30)).get.eval(exactEnv).toExpression
    // `x = 0.0` beside `y = 1`, and `2.0*x` for the derivative of `x^2`
    under("stationary(x*y - x, x, y)") match
      case m: _Matrix => assert(m.elems.forall { case _Equation(_, r) => r.isInstanceOf[_Rational]; case _ => false }, m)
      case other      => fail(other.toString)
    under("stationary(x^2 + y^2 - 2*x, x, y)") match
      case m: _Matrix => assert(m.elems == Vector(_Equation(equation_x, _Rational(1)), _Equation(equation_y, _Rational(0))), m)
      case other      => fail(other.toString)
    under("lagrange(x*y, x + y - 1, x, y)") match
      case m: _Matrix => assert(m.elems.forall { case _Equation(_, r) => _Surd.isExact(r); case _ => false }, m)
      case other      => fail(other.toString)
    // the conditions are stated with exact zeros: no `0.0` anywhere
    val k = under("kkt(x*y, x + y - 1, 0, x, y)").toString
    assert(!k.contains("0.0") && !k.contains("1.0"), k)
  }

  private val equation_x = _Variable("x")
  private val equation_y = _Variable("y")

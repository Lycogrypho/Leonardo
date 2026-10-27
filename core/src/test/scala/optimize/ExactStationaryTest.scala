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

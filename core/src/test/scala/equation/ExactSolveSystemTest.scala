package it.grypho.scala.leonardo
package equation

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0166 — `solveSystem` solves an exactly stated system exactly.
 *
 *  It read every coefficient through the widening `_Number` extractor and eliminated in `Double`,
 *  and its normalisation invented an inexact `-1` and bound the unknowns to an inexact `0`, so an
 *  exact system came back as `c = -3.57143` where the answer is `-25/7`.  An all-exact system now
 *  goes through Gauss-Jordan over the rationals; anything else is unchanged.
 */
class ExactSolveSystemTest extends AnyFlatSpec:

  private val env = new Environment()

  private def eval(s: String, exact: Boolean = true): Either[_Expression, _Value] =
    Parser.parse(s, Option.when(exact)(30)) match
      case Parser.Success(x, _) => x.eval(env)
      case other                => fail(s"parse failed for \"$s\": $other")

  private def rat(n: Int, d: Int): _Rational = _Rational.of(BigInt(n), BigInt(d)).getOrElse(fail(s"$n/$d"))
  private val (c, d) = (_Variable("c"), _Variable("d"))

  "solveSystem" should "answer an exact system exactly" in {
    assert(eval("solveSystem([[3*c - d = -31], [c + 2*d = 37]], c, d)") ==
      Left(matrix._Matrix(1, 2, Vector(_Equation(c, rat(-25, 7)), _Equation(d, rat(142, 7))))))
    assert(eval("solveSystem([[c/2 + d/3 = 1], [c - d = 1/6]], c, d)") ==
      Left(matrix._Matrix(1, 2, Vector(_Equation(c, rat(19, 15)), _Equation(d, rat(11, 10))))))
  }

  it should "still decline a singular exact system" in {
    assert(eval("solveSystem([[-c + 2*d = 3], [c - 2*d = 0]], c, d)").fold(_.isInstanceOf[_SolveSystem], _ => false))
  }

  it should "answer an inexact system as before" in {
    eval("solveSystem([[3*c - d = -31], [c + 2*d = 37]], c, d)", exact = false) match
      case Left(matrix._Matrix(1, 2, Vector(_Equation(`c`, x: _Number), _Equation(`d`, y: _Number)))) =>
        assert(math.abs(x.d + 25.0 / 7) < 1e-12 && math.abs(y.d - 142.0 / 7) < 1e-12, (x, y))
      case other => fail(s"$other")
  }

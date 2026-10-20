package it.grypho.scala.leonardo
package matrix

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0163 — `eye`, `zeros` and `ones` build their cells in the tier of their dimensions.
 *
 *  The cells are constants the constructor invents, so the numeric tier rule applies: with exact
 *  dimensions (exact mode) they are exact, and `A - 2*eye(3)` keeps an exact `A` exact where it
 *  used to be demoted.  Inexact dimensions keep the dense carrier, byte-identical to before, and an
 *  exact constructor past `Limit.MaxMatrixCells` falls back to it rather than allocating millions
 *  of rationals.
 */
class ExactConstructorsTest extends AnyFlatSpec:

  private val env = new Environment()

  private def eval(s: String, exact: Boolean = true, e: Environment = env): Either[_Expression, _Value] =
    Parser.parse(s, Option.when(exact)(30)) match
      case Parser.Success(x, _) => x.eval(e)
      case other                => fail(s"parse failed for \"$s\": $other")

  private def exactCells(s: String, e: Environment = env): Vector[_Expression] = eval(s, e = e) match
    case Left(m: _Matrix) if m.elems.forall(_.isInstanceOf[_Rational]) => m.elems
    case other => fail(s"\"$s\" gave no exact matrix: $other")

  "eye, zeros and ones" should "build exact cells from exact dimensions" in {
    assert(exactCells("eye(2)") == Vector(1, 0, 0, 1).map(_Rational(_)))
    assert(exactCells("zeros(1, 2)") == Vector(_Rational(0), _Rational(0)))
    assert(exactCells("ones(2)") == Vector.fill(4)(_Rational(1)))
  }

  it should "keep an exact matrix exact when combined with one" in {
    val a = "[[1/2, 2], [2, -3]]"
    assert(exactCells(s"$a - 2*eye(2)") ==
      Vector(_Rational.of(BigInt(-3), BigInt(2)).getOrElse(fail("-3/2")), _Rational(2), _Rational(2), _Rational(-5)))
    assert(exactCells(s"$a * eye(2)") == exactCells(a))
  }

  it should "stay dense from inexact dimensions, as before" in {
    assert(eval("eye(2)", exact = false) == Right(_MatrixValue(2, 2, Array(1.0, 0.0, 0.0, 1.0))))
    assert(eval("zeros(1, 2)", exact = false) == Right(_MatrixValue(1, 2, Array(0.0, 0.0))))
  }

  it should "fall back to the dense carrier past the cell budget" in {
    val tight = env.withLimit(Limit.MaxMatrixCells, 3)
    assert(eval("eye(2)", e = tight) == Right(_MatrixValue(2, 2, Array(1.0, 0.0, 0.0, 1.0))))
  }

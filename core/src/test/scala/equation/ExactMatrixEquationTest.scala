package it.grypho.scala.leonardo
package equation

import core.*
import matrix.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec
import spire.math.Rational as Q


/** F_0178 — Sylvester and Lyapunov equations over exact matrices.
 *
 *  The vectorised tier read only dense coefficients, so in exact mode `solve(A*X + X*B = C, X)`
 *  stayed unevaluated.  It now solves the Kronecker system over the rationals and answers an exact
 *  matrix; numeric input keeps the dense path.
 */
class ExactMatrixEquationTest extends AnyFlatSpec:

  private val env = new Environment()

  private def solved(s: String, exact: Boolean = true): _Expression =
    Parser.parse(s, Option.when(exact)(30)) match
      case Parser.Success(e, _) => e.eval(env) match
        case Left(_Equation(_, x)) => x
        case other                 => fail(s"'$s' did not solve: $other")
      case other => fail(s"parse failed for '$s': $other")

  private def exactCellsOf(e: _Expression): Vector[Q] = e match
    case m: _Matrix => m.elems.map {
      case r: _Rational => Q(r.num, r.den)
      case other        => fail(s"cell $other is not exact")
    }
    case other => fail(s"not an exact matrix: $other")

  "a Sylvester equation over exact matrices" should "solve exactly" in {
    assert(exactCellsOf(solved("solve([[1, 0], [0, 2]] * X + X * [[3, 0], [0, 4]] = [[4, 5], [6, 6]], X)")) ==
      Vector(Q(1), Q(1), Q(6, 5), Q(1)))
    assert(exactCellsOf(solved("solve([[2, 0], [0, 3]] * X + X * [[1, 1], [0, 1]] = [[3, 5], [4, 9]], X)")) ==
      Vector(Q(1), Q(4, 3), Q(1), Q(2)))
  }

  "a Lyapunov equation over exact matrices" should "solve exactly" in {
    assert(exactCellsOf(solved("solve([[-1, 1], [0, -2]] * X + X * transpose([[-1, 1], [0, -2]]) = -eye(2), X)")) ==
      Vector(Q(7, 12), Q(1, 12), Q(1, 12), Q(1, 4)))
    assert(exactCellsOf(solved("solve([[-2, 0], [0, -3]] * X + X * [[-2, 0], [0, -3]] = -eye(2), X)")) ==
      Vector(Q(1, 4), Q(0), Q(0), Q(1, 6)))
  }

  it should "accept a scaled unknown and a constant term" in {
    assert(exactCellsOf(solved("solve(2 * X + [[1, 0], [0, 1]] = [[3, 2], [4, 5]], X)")) ==
      Vector(Q(1), Q(1), Q(2), Q(2)))
  }

  it should "stay unevaluated past MaxExactVectorizedSize" in {
    // a 7×7 unknown is 49 unknowns, above the measured cap of 36
    val src = "solve(eye(7) * X + X * eye(7) = ones(7), X)"
    Parser.parse(src, Some(30)) match
      case Parser.Success(e, _) => assert(e.eval(env).fold(_.toString, _.toString).startsWith("solve("))
      case other                => fail(s"$other")
  }

  "the dense path" should "be unchanged outside exact mode" in {
    solved("solve([[1, 0], [0, 2]] * X + X * [[3, 0], [0, 4]] = [[4, 5], [6, 6]], X)", exact = false) match
      case m: _MatrixValue => assert(m.toVector.zip(Vector(1.0, 1.0, 1.2, 1.0)).forall((a, b) => math.abs(a - b) < 1e-12))
      case other           => fail(s"$other")
  }

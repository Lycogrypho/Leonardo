package it.grypho.scala.leonardo
package vector

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0285 (LT L_0067): an operator's result is evaluated in the environment it is evaluated in, so a
 *  bound coordinate folds and `hessian(f, x, y)`, `det` and `eigen` of it agree; a nested operator is
 *  still differentiated before anything folds. */
class VectorBindingTest extends AnyFlatSpec:

  private val at = new Environment().withBinding("x", _Number(2)).withBinding("y", _Number(3))
  private def ev(s: String, env: Environment = at): Either[_Expression, _Value] = Parser.parse(s).get.eval(env)

  private def dense(s: String): Vector[Double] = ev(s) match
    case Right(m: _MatrixValue) => m.toVector
    case other                  => fail(s"$s: $other")

  private def number(s: String): Double = ev(s) match
    case Right(_Number(d)) => d
    case other             => fail(s"$s: $other")

  "an operator with bound coordinates" should "answer its value there (F_0285)" in {
    assert(dense("hessian(x^2*y + y^3, x, y)") == Vector(6.0, 4.0, 4.0, 18.0))
    assert(dense("grad(x^2*y, x, y)") == Vector(12.0, 4.0))
    assert(dense("jacobian([[x*y], [x]], x, y)") == Vector(3.0, 2.0, 1.0, 0.0))
    assert(dense("curl([[0], [x*y], [0]], x, y, z)") == Vector(0.0, 0.0, 3.0))
  }

  it should "agree with det of it" in {
    assert(number("det(hessian(x^2*y + y^3, x, y))") == 6.0 * 18.0 - 16.0)
  }

  "a nested operator" should "be differentiated before the coordinates fold" in {
    assert(number("div(grad(x^2*y, x, y), x, y)") == 6.0)
    assert(number("laplacian(x^2*y, x, y)") == 6.0)
    assert(dense("curl(grad(x^2*y, x, y, z), x, y, z)") == Vector(0.0, 0.0, 0.0))
    assert(dense("jacobian(grad(x^3*y, x, y), x, y)") == Vector(36.0, 12.0, 12.0, 0.0))
  }

  "an operator with free coordinates" should "stay symbolic in them" in {
    ev("grad(x^2*y, x, y)", new Environment()) match
      case Left(m: matrix._Matrix) => assert(m.elems.forall(_.freeVars.nonEmpty))
      case other                   => fail(s"$other")
  }

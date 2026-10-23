package it.grypho.scala.leonardo
package vector

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** Regressions from the audit of 2026-10-23 in vector calculus: each case failed before its fix. */
class VectorEdgeCaseTest extends AnyFlatSpec:

  private val env = new Environment()
  private def ev(s: String): Either[_Expression, _Value] = Parser.parse(s).get.eval(env)

  "hessian and jacobian" should "stay symbolic in curvilinear coordinates (F_0191)" in {
    ev("hessian(r, r, t, p, spherical)") match
      case Left(_: _Hessian) => succeed
      case other             => fail(s"got $other")
    ev("jacobian([[r], [0], [0]], r, t, z, cylindrical)") match
      case Left(_: _Jacobian) => succeed
      case other              => fail(s"got $other")
  }

  "grad and hessian" should "refuse a vector field, as laplacian does (F_0191)" in {
    ev("grad([[x], [y]], x, y)") match
      case Left(_: _Grad) => succeed
      case other          => fail(s"got $other")
    ev("hessian([[x], [y]], x, y)") match
      case Left(_: _Hessian) => succeed
      case other             => fail(s"got $other")
  }

  "the Cartesian operators" should "still answer (F_0191)" in {
    assert(!ev("hessian(x^2*y, x, y)").fold(_.isInstanceOf[_Hessian], _ => false))
    assert(!ev("jacobian([[x*y], [x]], x, y)").fold(_.isInstanceOf[_Jacobian], _ => false))
  }

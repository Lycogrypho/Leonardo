package it.grypho.scala.leonardo
package ode

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** Regressions from the audit of 2026-10-23 in the ODE tiers: each case failed before its fix. */
class ODEEdgeCaseTest extends AnyFlatSpec:

  private val env = new Environment()
  private def ev(s: String): Either[_Expression, _Value] = Parser.parse(s).get.eval(env)
  private val x = _Variable("x")
  private val t = _Variable("t")

  "the separable tier" should "decline past a blow-up rather than converge onto G's pole (F_0190)" in {
    ev("ode(y^2, y, t, 0, 1, 2)") match
      case Right(v) => fail(s"answered $v past the blow-up at t = 1")
      case Left(_)  => succeed
  }

  it should "still answer before the blow-up, and at its own scale (F_0190)" in {
    ev("ode(y^2, y, t, 0, 1, 0.5)") match
      case Right(_Number(d)) => assert(math.abs(d - 2.0) < 1e-9, d)
      case other             => fail(s"got $other")
    ev("ode(y^2, y, t, 0, 1e-14, 1)") match
      case Right(_Number(d)) => assert(math.abs(d - 1e-14 / (1 - 1e-14)) <= 1e-6 * 1e-14, d)
      case other             => fail(s"got $other")
  }

  "odeSolve" should "refuse a row count past the cap even when it overflows a Long (F_0184)" in {
    val rhs = Parser.parse("[[-x]]").get
    odeSolve(rhs, OdeState.Components(Vector(x)), t, 0.0, Vector(1.0), 1.0, 1e-300, OdeMethod.RK4, env) match
      case Left(OdeFailure.TooManyRows(_)) => succeed
      case other                           => fail(s"got $other")
  }

  it should "terminate when the spacing is below half an ulp of t0 (F_0184)" in {
    val rhs = Parser.parse("[[-x]]").get
    odeSolve(rhs, OdeState.Components(Vector(x)), t, 1e9, Vector(1.0), 1e9 + 1e-3, 5e-8, OdeMethod.Euler, env) match
      case Right(rows) => assert(rows.last._1 == 1e9 + 1e-3)
      case Left(why)   => fail(s"$why")
  }

  "odeCertified" should "validate its input like the step and the trajectory (F_0184)" in {
    val rhs = Parser.parse("[[-x]]").get
    odeCertified(rhs, OdeState.Components(Vector(x)), t, 0.0, Vector.empty, 1.0, env) match
      case Left(OdeFailure.InvalidInput(_)) => succeed
      case other                            => fail(s"got $other")
    odeCertified(Parser.parse("[[-x], [x]]").get, OdeState.Components(Vector(x, x)), t, 0.0,
                 Vector(1.0, 1.0), 1.0, env) match
      case Left(OdeFailure.InvalidInput(_)) => succeed
      case other                            => fail(s"repeated names answered $other")
  }

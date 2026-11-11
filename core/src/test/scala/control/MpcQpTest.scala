package it.grypho.scala.leonardo
package control

import core.*
import org.scalatest.flatspec.AnyFlatSpec


/** F_0299 (AIACE L_0002): the condensed MPC quadratic programme. */
class MpcQpTest extends AnyFlatSpec:

  private def mat(rows: Int, cols: Int, xs: Double*): _MatrixValue = _MatrixValue(rows, cols, xs.toArray)
  private val env = new Environment()

  // One state, one input, one output: x(k+1) = 0.9x + 0.5u, y = x.
  private val (a, b, c) = (mat(1, 1, 0.9), mat(1, 1, 0.5), mat(1, 1, 1.0))
  private val (q, r) = (mat(1, 1, 1.0), mat(1, 1, 0.1))
  private val (np, nc) = (8, 3)

  private def incremental = mpcIncrementalPrediction(a, b, c, np, nc, Limits.Default).fold(f => fail(s"$f"), identity)
  private def absolute    = mpcPrediction(a, b, c, np, nc, Limits.Default).fold(f => fail(s"$f"), identity)

  private def solved(p: MpcPredictionMatrices, ref: Vector[Double], x: Vector[Double], uPrev: Vector[Double],
                     bounds: MpcBounds = MpcBounds()): Either[MpcFailure, MpcSolution] =
    mpcQp(p, q, r, ref, x, uPrev, bounds, env.limits).flatMap(_.solve(Set.empty, env))

  "an unconstrained problem" should "have the analytic optimum (F_0299)" in {
    val p = incremental
    val (x, uPrev, ref) = (Vector(0.5), Vector(0.2), Vector(1.0))
    // ΔU = (ΘᵀQ̄Θ + R̄)⁻¹ΘᵀQ̄(r − Φx − Ψu₋₁), with Q̄ = I and R̄ = 0.1·I here
    val e = Vector.tabulate(np)(i => 1.0 - p.phi(i, 0) * x(0) - p.psi(i, 0) * uPrev(0))
    val th = p.theta
    val h  = th.transpose.multiply(th).add(_MatrixValue.identity(nc).scale(0.1))
    val g  = th.transpose.multiply(_MatrixValue(np, 1, e.toArray))
    val want = h.inverse.map(_.multiply(g).toVector).getOrElse(fail("singular"))
    solved(p, ref, x, uPrev) match
      case Right(s) =>
        s.moves.flatten.zip(want).foreach((got, w) => assert(math.abs(got - w) < 1e-6, s"${s.moves} vs $want"))
        assert(math.abs(s.input(0) - (uPrev(0) + want(0))) < 1e-6)
        assert(s.slacks.isEmpty)
      case Left(why) => fail(s"$why")
  }

  "an input bound" should "hold, and be reported active" in {
    solved(absolute, Vector(5.0), Vector(0.0), Vector(0.0), MpcBounds(u = Some((Vector(-1.0), Vector(1.0))))) match
      case Right(s) =>
        assert(s.moves.flatten.forall(u => u <= 1.0 + 1e-9 && u >= -1.0 - 1e-9), s"${s.moves}")
        assert(math.abs(s.input(0) - 1.0) < 1e-6, s"${s.input}")
        assert(s.qp.active.nonEmpty)
      case Left(why) => fail(s"$why")
  }

  "a rate bound" should "hold in either formulation" in {
    val bounds = MpcBounds(du = Some((Vector(-0.2), Vector(0.2))))
    for p <- Seq(absolute, incremental) do
      solved(p, Vector(5.0), Vector(0.0), Vector(0.1), bounds) match
        case Right(s) =>
          val us = if p.isInstanceOf[MpcIncrementalPrediction] then s.moves.flatten.scanLeft(0.1)(_ + _).tail
                   else s.moves.flatten
          (0.1 +: us).zip(us).foreach((u0, u1) => assert(math.abs(u1 - u0) <= 0.2 + 1e-9, s"$us"))
        case Left(why) => fail(s"$why")
  }

  "an input bound in the incremental formulation" should "bound the accumulated input" in {
    solved(incremental, Vector(5.0), Vector(0.0), Vector(0.5), MpcBounds(u = Some((Vector(-1.0), Vector(1.0))))) match
      case Right(s) =>
        s.moves.flatten.scanLeft(0.5)(_ + _).tail.foreach(u => assert(u <= 1.0 + 1e-9, s"${s.moves}"))
      case Left(why) => fail(s"$why")
  }

  "a hard output bound the inputs cannot meet" should "be infeasible, and feasible once softened" in {
    // u in [-0.1, 0.1] cannot keep y <= 0.2 from x = 3
    val hard = MpcBounds(u = Some((Vector(-0.1), Vector(0.1))), y = Some((Vector(Double.NegativeInfinity), Vector(0.2))))
    assert(solved(absolute, Vector(0.0), Vector(3.0), Vector(0.0), hard) == Left(MpcFailure.Qp(optimize.QpFailure.Infeasible)))
    val soft = hard.copy(soft = Some(MpcSoftOutputs(linear = Vector(100.0), quadratic = Vector(10.0))))
    solved(absolute, Vector(0.0), Vector(3.0), Vector(0.0), soft) match
      case Right(s) => assert(s.slacks.size == 1 && s.slacks(0) > 1.0, s"${s.slacks}")
      case Left(why) => fail(s"$why")
  }

  "a softened bound the inputs can meet" should "carry no slack (an exact penalty)" in {
    val soft = MpcBounds(y = Some((Vector(Double.NegativeInfinity), Vector(2.0))),
                         soft = Some(MpcSoftOutputs(Vector(100.0), Vector(10.0))))
    solved(absolute, Vector(5.0), Vector(0.0), Vector(0.0), soft) match
      case Right(s) => assert(s.slacks(0) < 1e-6, s"${s.slacks}")
      case Left(why) => fail(s"$why")
  }

  "the receding-horizon loop" should "track a setpoint on the exact model" in {
    val p = incremental
    val (_, xEnd, _) = (1 to 60).foldLeft((Vector(0.0), Vector(0.0), Vector(0.0))) { case ((_, x, uPrev), _) =>
      val s  = solved(p, Vector(1.0), x, uPrev).fold(f => fail(s"$f"), identity)
      val u  = s.input
      (u, Vector(0.9 * x(0) + 0.5 * u(0)), u)
    }
    assert(math.abs(xEnd(0) - 1.0) < 1e-6, s"$xEnd")
  }

  "a malformed problem" should "be declined" in {
    assert(mpcQp(absolute, mat(2, 2, 1, 0, 0, 1), r, Vector(1.0), Vector(0.0), Vector(0.0), MpcBounds(), env.limits).isLeft)
    assert(mpcQp(absolute, q, r, Vector(1.0, 2.0), Vector(0.0), Vector(0.0), MpcBounds(), env.limits).isLeft)
    assert(mpcQp(absolute, q, r, Vector(1.0), Vector(0.0), Vector(0.0),
                 MpcBounds(soft = Some(MpcSoftOutputs(Vector(1.0), Vector(0.0)))), env.limits).isLeft)   // rho2 = 0
  }

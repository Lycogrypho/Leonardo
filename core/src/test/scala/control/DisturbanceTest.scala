package it.grypho.scala.leonardo
package control

import core.*
import org.scalatest.flatspec.AnyFlatSpec

class DisturbanceTest extends AnyFlatSpec:

  private val env = new Environment(Environment.DefaultPrecision, Map.empty)
  private def m(rows: Int, cols: Int, xs: Double*) = _MatrixValue(rows, cols, xs.toArray)
  private def mul(a: _MatrixValue, v: Vector[Double]) =
    Vector.tabulate(a.rows)(i => (0 until a.cols).map(j => a(i, j) * v(j)).sum)

  "augmentDisturbance" should "build [[A, Bd], [0, I]], [[B], [0]] and [C, Cd]" in {
    val (a, b, c) = (m(1, 1, 0.8), m(1, 1, 0.5), m(1, 1, 2.0))
    outputDisturbanceModel(a, b, c, env) match
      case Right(dm) =>
        assert(dm.a.toVector == Vector(0.8, 0.0, 0.0, 1.0))
        assert(dm.b.toVector == Vector(0.5, 0.0))
        assert(dm.c.toVector == Vector(2.0, 1.0))
        assert(dm.n == 1 && dm.nd == 1)
      case Left(e) => fail(e.toString)
  }

  it should "decline an output disturbance on a plant with an integrator" in {
    // [[A - I, Bd], [C, Cd]] = [[0, 0], [1, 1]] has rank 1: the disturbance and the integrator's
    // state cannot be told apart
    val r = outputDisturbanceModel(m(1, 1, 1.0), m(1, 1, 1.0), m(1, 1, 1.0), env)
    assert(r.isLeft && r.left.exists(_.isInstanceOf[MpcFailure.NotDetectable]))
  }

  it should "decline more disturbances than outputs" in {
    val a = m(2, 2, 0.5, 0.0, 0.0, 0.6)
    val r = augmentDisturbance(a, m(2, 1, 1.0, 1.0), m(1, 2, 1.0, 1.0), m(2, 2, 1.0, 0.0, 0.0, 1.0), m(1, 2, 0.0, 0.0), env)
    assert(r.left.exists(_.isInstanceOf[MpcFailure.NotDetectable]))
  }

  it should "decline a plant with an unstable mode the outputs do not see" in {
    val a = m(2, 2, 0.5, 0.0, 0.0, 1.2)
    val r = outputDisturbanceModel(a, m(2, 1, 1.0, 1.0), m(1, 2, 1.0, 0.0), env)
    assert(r.left.exists(_.isInstanceOf[MpcFailure.NotDetectable]))
  }

  it should "accept a stable unobservable mode" in {
    val a = m(2, 2, 0.5, 0.0, 0.0, 0.7)
    assert(outputDisturbanceModel(a, m(2, 1, 1.0, 1.0), m(1, 2, 1.0, 0.0), env).isRight)
  }

  "steadyStateTarget" should "solve the steady-state equations exactly when unconstrained" in {
    val a  = m(2, 2, 0.9, 0.1, 0.0, 0.8)
    val b  = m(2, 1, 0.0, 0.5)
    val c  = m(1, 2, 1.0, 0.0)
    val bd = m(2, 1, 0.0, 0.5)
    val cd = m(1, 1, 0.3)
    steadyStateTarget(a, b, c, bd, cd, Vector(2.0), Vector(0.4), None, env) match
      case Right(t) =>
        assert(t.reached)
        val next = mul(a, t.x).zip(mul(b, t.u)).zip(mul(bd, Vector(0.4))).map { case ((p, q), s) => p + q + s }
        next.zip(t.x).foreach((p, q) => assert(math.abs(p - q) < 1e-12))
        assert(math.abs(mul(c, t.x).head + 0.3 * 0.4 - 2.0) < 1e-12)
      case Left(e) => fail(e.toString)
  }

  it should "return the closest reachable target when the input bound binds" in {
    val (a, b, c) = (m(1, 1, 0.5), m(1, 1, 1.0), m(1, 1, 1.0))
    // y_s = 2·u_s, so r = 4 needs u_s = 2, outside [-1, 1]
    steadyStateTarget(a, b, c, m(1, 1, 0.0), m(1, 1, 1.0), Vector(4.0), Vector(0.0),
                      Some((Vector(-1.0), Vector(1.0))), env) match
      case Right(t) =>
        assert(!t.reached)
        assert(math.abs(t.u.head - 1.0) < 1e-6)
        assert(math.abs(t.x.head - 2.0) < 1e-6)
      case Left(e) => fail(e.toString)
  }

  it should "use the least-squares path for a non-square system and reach the setpoint when it can" in {
    // two inputs, one output: infinitely many targets, any one that meets r
    val (a, b, c) = (m(1, 1, 0.5), m(1, 2, 1.0, 1.0), m(1, 1, 1.0))
    steadyStateTarget(a, b, c, m(1, 1, 0.0), m(1, 1, 1.0), Vector(1.0), Vector(0.2),
                      Some((Vector(-5.0, -5.0), Vector(5.0, 5.0))), env) match
      case Right(t) => assert(t.reached && math.abs(t.x.head + 0.2 - 1.0) < 1e-6)
      case Left(e)  => fail(e.toString)
  }

  it should "decline malformed input" in {
    val o = m(1, 1, 1.0)
    assert(steadyStateTarget(o, o, o, o, o, Vector(1.0, 2.0), Vector(0.0), None, env).isLeft)
    assert(steadyStateTarget(o, o, o, o, o, Vector(1.0), Vector(0.0), Some((Vector(1.0), Vector(0.0))), env).isLeft)
  }

  "offset-free MPC" should "track a setpoint without offset on a mismatched plant" in {
    // plant: gain 5, pole 0.9; model: gain 2.67, pole 0.85
    val (ap, bp) = (0.9, 0.5)
    val (a, b, c) = (m(1, 1, 0.85), m(1, 1, 0.4), m(1, 1, 1.0))
    val result = for
      dm  <- outputDisturbanceModel(a, b, c, env)
      obs <- disturbanceObserver(dm, m(2, 2, 0.01, 0.0, 0.0, 0.1), m(1, 1, 0.01), env)
      pr  <- mpcIncrementalPrediction(a, b, c, 10, 3, env.limits)
    yield (dm, obs._1, pr)
    result match
      case Left(e) => fail(e.toString)
      case Right((dm, gain, pr)) =>
        val r = 1.0
        var xPlant = 0.0
        var xhat   = Vector(0.0, 0.0)   // a priori estimate of [x; d]
        var u      = 0.0
        var y      = 0.0
        for _ <- 0 until 150 do
          y = xPlant
          val innov = y - mul(dm.c, xhat).head
          val post  = xhat.zip(mul(gain, Vector(innov))).map(_ + _)
          val ref   = Vector(r - post(1))
          val step = for
            qp <- mpcQp(pr, m(1, 1, 1.0), m(1, 1, 0.1), ref, Vector(post(0)), Vector(u),
                        MpcBounds(u = Some((Vector(-10.0), Vector(10.0)))), env.limits)
            s  <- qp.solve(Set.empty, env)
          yield s.input.head
          u = step.fold(e => fail(e.toString), identity)
          xPlant = ap * xPlant + bp * u
          xhat = mul(dm.a, post).zip(mul(dm.b, Vector(u))).map(_ + _)
        assert(math.abs(y - r) < 1e-6, s"y = $y")
  }

  it should "leave an offset without the disturbance estimate" in {
    // the same loop with d̂ held at 0 ends away from the setpoint: the test above is decisive
    val (ap, bp) = (0.9, 0.5)
    val (a, b, c) = (m(1, 1, 0.85), m(1, 1, 0.4), m(1, 1, 1.0))
    mpcPrediction(a, b, c, 10, 3, env.limits) match
      case Left(e) => fail(e.toString)
      case Right(pr) =>
        var (xPlant, xhat, u, y) = (0.0, 0.0, 0.0, 0.0)
        for _ <- 0 until 150 do
          y = xPlant
          val s = mpcQp(pr, m(1, 1, 1.0), m(1, 1, 0.1), Vector(1.0), Vector(xhat), Vector(u), MpcBounds(), env.limits)
            .flatMap(_.solve(Set.empty, env))
          u = s.fold(e => fail(e.toString), _.input.head)
          xPlant = ap * xPlant + bp * u
          xhat = 0.85 * xhat + 0.4 * u
        assert(math.abs(y - 1.0) > 0.01, s"y = $y")
  }

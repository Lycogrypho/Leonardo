package it.grypho.scala.leonardo
package control

import core.*
import TargetGoal.*
import org.scalatest.flatspec.AnyFlatSpec


/** F_0375 (AIACE L_0087): the target layer prepared once per model and specification, solved per cycle by
 *  `at`, from the exact LP's start by default or from the previous cycle's target on request. */
class TargetLayerPreparedTest extends AnyFlatSpec:

  private val env = new Environment(Environment.DefaultPrecision, Map.empty)
  private def m(rows: Int, cols: Int, xs: Double*) = _MatrixValue(rows, cols, xs.toArray)
  private def eye(k: Int) = _MatrixValue(k, k, Array.tabulate(k * k)(i => if i / k == i % k then 1.0 else 0.0))
  private val inf = Double.PositiveInfinity
  private def ok[A](r: Either[MpcFailure, A]): A = r.fold(f => fail(s"$f"), identity)
  private def near(a: Vector[Double], b: Vector[Double], tol: Double) =
    assert(a.size == b.size && a.zip(b).forall((x, y) => math.abs(x - y) <= tol * math.max(1.0, math.abs(y))), s"$a vs $b")

  // AIACE's shape: a 3x3 gain, three setpoints in priority order, resting values, bounded inputs
  private val rnd0 = new java.util.Random(87)
  private val g    = m(3, 3, Seq.fill(9)(0.2 + rnd0.nextDouble())*)
  private val rest = Some(RestingValues(Vector(0.2, 0.5, 0.5), eye(3)))
  private def spec(r: Vector[Double]) = TargetSpec(r.indices.toVector.map(i => Vector(Setpoint(i, r(i)))), rest)
  private val box  = Some((Vector(0.0, 0.0, 0.0), Vector(1.0, 1.0, 1.0)))

  /** A cycle's data: measured outputs and inputs drifting, setpoints changing now and then. */
  private def cycles(n: Int): Vector[(GainSteadyState, TargetSpec, Vector[Double])] =
    val rnd = new java.util.Random(375)
    Vector.tabulate(n) { k =>
      val u0 = Vector.fill(3)(0.3 + 0.1 * rnd.nextDouble())
      val y0 = Vector.fill(3)(1.0 + 0.1 * rnd.nextGaussian())
      val r  = if k % 10 < 5 then Vector(1.2, 1.0, 0.8) else Vector(1.1, 0.9, 0.9)
      (GainSteadyState(g, y0, u0), spec(r), u0)
    }

  "a prepared target layer" should "answer every cycle as steadyStateTargets does, to the bit (F_0375)" in {
    val first    = cycles(1).head
    val prepared = ok(steadyStateTargetsPrepared(first._1, first._2, box, env))
    for (model, sp, u) <- cycles(40) do
      assert(prepared.at(model, sp, box, u, env) == steadyStateTargets(model, sp, box, u, env))
  }

  it should "agree on state-space models with a moving disturbance estimate, bounds and output bounds" in {
    val rnd = new java.util.Random(359)
    for i <- 0 until 20 do
      val (mm, p, n) = (1 + rnd.nextInt(4), 1 + rnd.nextInt(3), 1 + rnd.nextInt(5))
      def model(d: Double) = StateSpaceSteadyState(
        m(n, n, Seq.tabulate(n * n)(k => if k / n == k % n then 0.5 else 0.0)*),
        m(n, mm, Seq.tabulate(n * mm)(k => 0.3 + 0.1 * k)*), m(p, n, Seq.tabulate(p * n)(k => 1.0 - 0.2 * k)*),
        m(n, 1, Seq.fill(n)(1.0)*), m(p, 1, Seq.fill(p)(0.5)*), Vector(d))
      val goals  = (0 until p).toVector.map(o => if o == 0 && p > 1 then OutputBound(o, -inf, 1.5) else Setpoint(o, 0.5 * o))
      val sp     = TargetSpec(goals.map(Vector(_)), Option.when(i % 2 == 0)(RestingValues(Vector.fill(mm)(0.1), eye(mm))))
      val bounds = Option.when(i % 3 != 0)((Vector.fill(mm)(-2.0), Vector.fill(mm)(2.0)))
      val prepared = ok(steadyStateTargetsPrepared(model(0.0), sp, bounds, env))
      for c <- 0 until 5 do
        val d = 0.1 * rnd.nextGaussian()
        val u = Vector.fill(mm)(0.2 * rnd.nextGaussian())
        assert(prepared.at(model(d), sp, bounds, u, env) == steadyStateTargets(model(d), sp, bounds, u, env), s"case $i, cycle $c")
  }

  it should "start from the previous target on request, reaching the same targets within the held band" in {
    val first    = cycles(1).head
    val prepared = ok(steadyStateTargetsPrepared(first._1, first._2, box, env))
    cycles(40).foldLeft(Option.empty[TargetLayer]) { case (previous, (model, sp, u)) =>
      val warm = ok(prepared.at(model, sp, box, u, env, from = previous))
      val cold = ok(steadyStateTargets(model, sp, box, u, env))
      near(warm.u, cold.u, 1e-6)
      near(warm.y, cold.y, 1e-6)
      assert(warm.unmet == cold.unmet)
      Some(warm)
    }
  }

  it should "not start from a previous target far beyond the data (F_0380)" in {
    // feasible, but 1e12 away under bounds of 1e12: as a start it was taken as it was, and a reachable
    // setpoint was reported unmet
    val g2   = m(2, 3, 1.0, 0.5, 0.2, 0.3, 1.0, 0.4)
    val mdl  = GainSteadyState(g2, Vector(1.0, 0.5), Vector(0.3, 0.3, 0.3))
    val sp   = TargetSpec(Vector(Vector(Setpoint(0, 1.0)), Vector(Setpoint(1, 1.0))))
    val wide = Some((Vector.fill(3)(-1e12), Vector.fill(3)(1e12)))
    val u    = Vector(0.3, 0.3, 0.3)
    val prepared = ok(steadyStateTargetsPrepared(mdl, sp, wide, env))
    val cold = ok(prepared.at(mdl, sp, wide, u, env))
    assert(cold.reached, cold.toString)
    for far <- Seq(Vector(1e12, -1e12, 1e12), Vector(1e12, 1e12, 1e12), Vector(-3e11, 7e11, 1e10)) do
      val warm = ok(prepared.at(mdl, sp, wide, u, env, from = Some(TargetLayer(None, far, Vector(0.0, 0.0), Vector.empty))))
      assert(warm.unmet == cold.unmet, s"from $far: $warm")
      near(warm.u, cold.u, 1e-6)
  }

  it should "refuse a cycle whose fixed parts differ from the prepared ones" in {
    val (model, sp, u) = cycles(1).head
    val prepared = ok(steadyStateTargetsPrepared(model, sp, box, env))
    def refused(r: Either[MpcFailure, TargetLayer]) = r match
      case Left(MpcFailure.InvalidInput(_)) => succeed
      case other                           => fail(s"not refused: $other")
    refused(prepared.at(GainSteadyState(eye(3), model.y0, model.u0), sp, box, u, env))           // another gain
    refused(prepared.at(model, sp.copy(levels = Vector(sp.levels.flatten)), box, u, env))         // another shape
    refused(prepared.at(model, TargetSpec(sp.levels.map(_.map {
      case Setpoint(i, v, _) => Setpoint(i, v, 2.0); case o => o }), rest), box, u, env))            // another weight
    refused(prepared.at(model, sp.copy(rest = None), box, u, env))                                 // no resting values
    refused(prepared.at(model, sp, Some((Vector(0.0, 0.0, -inf), Vector(1.0, 1.0, 1.0))), u, env)) // another finite set
    refused(prepared.at(model, sp, box, u, env, from = Some(TargetLayer(None, Vector(0.5), Vector(1.0), Vector.empty))))
  }

  it should "refuse to prepare what steadyStateTargets would refuse" in {
    val (model, _, _) = cycles(1).head
    assert(steadyStateTargetsPrepared(model, TargetSpec(Vector(Vector(Setpoint(7, 1.0)))), box, env).isLeft)
  }

package it.grypho.scala.leonardo
package control

import core.*
import TargetGoal.*
import org.scalatest.flatspec.AnyFlatSpec


/** F_0358 (AIACE L_0079): the steady-state target layer. */
class TargetLayerTest extends AnyFlatSpec:

  private val env = new Environment(Environment.DefaultPrecision, Map.empty)
  private def m(rows: Int, cols: Int, xs: Double*) = _MatrixValue(rows, cols, xs.toArray)
  private def eye(k: Int, d: Double = 1.0) = _MatrixValue(k, k, Array.tabulate(k * k)(i => if i / k == i % k then d else 0.0))
  private val inf = Double.PositiveInfinity
  private def ok[A](r: Either[MpcFailure, A]): A = r.fold(f => fail(s"$f"), identity)
  private def near(a: Vector[Double], b: Vector[Double], tol: Double = 1e-6) =
    assert(a.size == b.size && a.zip(b).forall((x, y) => math.abs(x - y) <= tol * math.max(1.0, math.abs(y))), s"$a vs $b")

  // three inputs, two outputs: y1 = u1 + u2, y2 = u2 + u3; every u = (2 - t, t, 2 - t) meets r = (2, 2)
  private val g3 = m(2, 3, 1, 1, 0, 0, 1, 1)
  private val gain3 = GainSteadyState(g3, Vector(0.0, 0.0), Vector(0.0, 0.0, 0.0))
  private val zero3 = Vector(0.0, 0.0, 0.0)

  "the target layer" should "meet the setpoints and spend the free direction on the resting values (F_0358)" in {
    // min (2 - t)^2 + t^2 + (2 - t)^2: t = 4/3
    val all = ok(steadyStateTargets(gain3, TargetSpec.of(Vector(2.0, 2.0), None, Some(RestingValues(zero3, eye(3)))), None, zero3, env))
    assert(all.reached && all.x.isEmpty)
    near(all.u, Vector(2.0 / 3, 4.0 / 3, 2.0 / 3))
    near(all.y, Vector(2.0, 2.0))
    // a weight on the first input only: t = 2, u = (0, 2, 0)
    val one = ok(steadyStateTargets(gain3, TargetSpec.of(Vector(2.0, 2.0), None, Some(RestingValues(zero3, m(3, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0)))), None,
                                    zero3, env))
    near(one.u, Vector(0.0, 2.0, 0.0))
  }

  it should "give the same target through a state-space model as through its gain" in {
    // (I - A)^-1 B = g3, with A = 0.5 I and B = 0.5 g3
    val ss = StateSpaceSteadyState(eye(2, 0.5), m(2, 3, 0.5, 0.5, 0, 0, 0.5, 0.5), eye(2), m(2, 1, 0, 0), m(2, 1, 0, 0), Vector(0.0))
    val spec = TargetSpec.of(Vector(2.0, 2.0), None, Some(RestingValues(zero3, eye(3))))
    val t = ok(steadyStateTargets(ss, spec, None, zero3, env))
    near(t.u, ok(steadyStateTargets(gain3, spec, None, zero3, env)).u)
    t.x match
      case Some(x) => near(x, Vector(2.0, 2.0))
      case None    => fail("a state-space target has a state")
  }

  it should "follow the priority order when the setpoints conflict" in {
    // one input, y1 = u, y2 = 2u: r = (1, 4) cannot both hold
    val g = GainSteadyState(m(2, 1, 1, 2), Vector(0.0, 0.0), Vector(0.0))
    val first = ok(steadyStateTargets(g, TargetSpec(Vector(Vector(Setpoint(0, 1.0)), Vector(Setpoint(1, 4.0)))), None, Vector(0.0), env))
    near(first.u, Vector(1.0))
    assert(first.unmet == Vector(Setpoint(1, 4.0)))
    val second = ok(steadyStateTargets(g, TargetSpec(Vector(Vector(Setpoint(1, 4.0)), Vector(Setpoint(0, 1.0)))), None, Vector(0.0), env))
    near(second.u, Vector(2.0))
    assert(second.unmet == Vector(Setpoint(0, 1.0)))
    // one level weighs both: min (u - 1)^2 + (2u - 4)^2, u = 9/5
    val both = ok(steadyStateTargets(g, TargetSpec(Vector(Vector(Setpoint(0, 1.0), Setpoint(1, 4.0)))), None, Vector(0.0), env))
    near(both.u, Vector(1.8))
    assert(both.unmet.size == 2 && !both.reached)
  }

  it should "hold an output bound before a setpoint, and report the bound it cannot keep" in {
    // y1 = 2u (a pressure drop, at most 1.8), y2 = u (a flow, setpoint 1)
    val g = GainSteadyState(m(2, 1, 2, 1), Vector(0.0, 0.0), Vector(0.0))
    val bounded = TargetSpec(Vector(Vector(OutputBound(0, -inf, 1.8)), Vector(Setpoint(1, 1.0))))
    // TargetSpec.of puts every bound first, as one level, an output with no finite bound left out
    assert(TargetSpec.of(Vector(0.0, 1.0), Some((Vector(-inf, -inf), Vector(1.8, inf)))).levels ==
             Vector(Vector(OutputBound(0, -inf, 1.8)), Vector(Setpoint(0, 0.0), Setpoint(1, 1.0))))
    val t = ok(steadyStateTargets(g, bounded, None, Vector(0.0), env))
    near(t.u, Vector(0.9))
    assert(t.unmet == Vector(Setpoint(1, 1.0)))
    // the input cannot go below 1: the bound is exceeded as little as possible, and said so
    val forced = ok(steadyStateTargets(g, bounded, Some((Vector(1.0), Vector(5.0))), Vector(1.0), env))
    near(forced.u, Vector(1.0))
    assert(forced.unmet == Vector(OutputBound(0, -inf, 1.8)))
  }

  it should "handle a plant with an integrator, which no gain can state" in {
    // x = x + u: the steady state needs u = 0, and y = x is free to meet r
    val ss = StateSpaceSteadyState(m(1, 1, 1.0), m(1, 1, 1.0), m(1, 1, 1.0), m(1, 1, 0.0), m(1, 1, 0.0), Vector(0.0))
    val t = ok(steadyStateTargets(ss, TargetSpec.of(Vector(3.0)), None, Vector(0.5), env))
    assert(t.reached)
    near(t.u, Vector(0.0))
    t.x.foreach(x => near(x, Vector(3.0)))
  }

  it should "give the same target whatever the units of an output or an input" in {
    val spec = TargetSpec.of(Vector(2.0, 2.0), None, Some(RestingValues(zero3, eye(3))))
    val base = ok(steadyStateTargets(gain3, spec, Some((Vector.fill(3)(-5.0), Vector.fill(3)(5.0))), zero3, env)).u
    // the second output in units 1000 times smaller, its weight scaled to match
    val g = m(2, 3, 1, 1, 0, 0, 1000, 1000)
    val scaled = TargetSpec(Vector(Vector(Setpoint(0, 2.0), Setpoint(1, 2000.0, 1e-6))), Some(RestingValues(zero3, eye(3))))
    near(ok(steadyStateTargets(GainSteadyState(g, Vector(0.0, 0.0), zero3), scaled, Some((Vector.fill(3)(-5.0), Vector.fill(3)(5.0))),
                               zero3, env)).u, base)
    // the third input in units 100 times larger: its value, bounds and resting weight follow
    val gi = m(2, 3, 1, 1, 0, 0, 1, 0.01)
    val si = m(3, 3, 1, 0, 0, 0, 1, 0, 0, 0, 1e-4)
    val ui = ok(steadyStateTargets(GainSteadyState(gi, Vector(0.0, 0.0), zero3), TargetSpec.of(Vector(2.0, 2.0), None, Some(RestingValues(zero3, si))),
                                   Some((Vector(-5.0, -5.0, -500.0), Vector(5.0, 5.0, 500.0))), zero3, env)).u
    near(ui, Vector(base(0), base(1), 100 * base(2)))
  }

  it should "move least from the current input among equally good targets" in {
    // y = u1 + u2 = 2 with no resting values: the target nearest the current input
    val g = GainSteadyState(m(1, 2, 1, 1), Vector(0.0), Vector(0.0, 0.0))
    val bounds = Some((Vector(-10.0, -10.0), Vector(10.0, 10.0)))
    near(ok(steadyStateTargets(g, TargetSpec.of(Vector(2.0)), bounds, Vector(2.0, 0.0), env)).u, Vector(2.0, 0.0))
    near(ok(steadyStateTargets(g, TargetSpec.of(Vector(2.0)), bounds, Vector(0.0, 0.0), env)).u, Vector(1.0, 1.0))
  }

  it should "decline what it cannot read" in {
    def bad(spec: TargetSpec, u: Vector[Double] = zero3) =
      steadyStateTargets(gain3, spec, None, u, env).left.exists(_.isInstanceOf[MpcFailure.InvalidInput])
    assert(bad(TargetSpec(Vector(Vector(Setpoint(2, 1.0))))))
    assert(bad(TargetSpec(Vector(Vector(Setpoint(0, 1.0, 0.0))))))
    assert(bad(TargetSpec(Vector(Vector(OutputBound(0, 2.0, 1.0))))))
    assert(bad(TargetSpec.of(Vector(1.0, 1.0), None, Some(RestingValues(zero3, m(3, 3, 1, 1, 0, 0, 1, 0, 0, 0, 1))))))
    assert(bad(TargetSpec.of(Vector(1.0, 1.0)), Vector(0.0)))
    assert(steadyStateTargets(GainSteadyState(g3, Vector(0.0), zero3), TargetSpec.of(Vector(1.0)), None, zero3, env).isLeft)
  }

  it should "not give up a met setpoint to a later level" in {
    // the tie-break pulls towards u = 0, which would lower y; the setpoint level holds it to the
    // rounding level of its terms (1e-10 of |u1| + |u2| = 2)
    val g = GainSteadyState(m(1, 2, 1, 1), Vector(0.0), Vector(0.0, 0.0))
    val t = ok(steadyStateTargets(g, TargetSpec.of(Vector(2.0)), None, Vector(0.0, 0.0), env))
    assert(t.reached)
    assert(math.abs(t.y(0) - 2.0) <= 3e-10, s"${t.y}")
  }

  "steadyStateTarget" should "choose the smallest input among several that meet the setpoint (F_0358)" in {
    // two inputs, one output: x = 0.5x + u1 + u2 and x + 0.2 = 1, so u1 + u2 = 0.4, split evenly
    val t = ok(steadyStateTarget(m(1, 1, 0.5), m(1, 2, 1, 1), m(1, 1, 1), m(1, 1, 0), m(1, 1, 1), Vector(1.0), Vector(0.2),
                                 Some((Vector(-5.0, -5.0), Vector(5.0, 5.0))), env))
    assert(t.reached)
    near(t.u, Vector(0.2, 0.2))
  }

package it.grypho.scala.leonardo
package ode

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.BeforeAndAfter
import core.*
import scalar.*
import ode.*


class ODETest extends AnyFlatSpec with BeforeAndAfter:

  private val emptyEnv = new Environment()

  // y' = y, y(0) = 1, evaluated at t = 1  (solution y(1) = e).
  private def sample: _ODE =
    _ODE(_Variable("y"), _Variable("y"), _Variable("t"),
         _Number(0), _Number(1), _Number(1))

  // ───────────────────────────── construction ─────────────────────────────

  "_ODE" should "expose its right-hand side, binders, and initial/target positions" in:
    val node = sample
    assert(node.rhs == _Variable("y"))
    assert(node.depVar.variable == "y")
    assert(node.indepVar.variable == "t")
    assert(node.t0 == _Number(0))
    assert(node.y0 == _Number(1))
    assert(node.target == _Number(1))

  // ───────────────────────────── toString ─────────────────────────────

  it should "render as ode(rhs, depVar, indepVar, t0, y0, target)" in:
    assert(sample.toString == "ode(y, y, t, 0.0, 1.0, 1.0)")

  // ───────────────────────────── children / rebuild ─────────────────────────────

  it should "exclude the depVar and indepVar binders from children" in:
    val kids = sample.children
    assert(kids == List(_Variable("y"), _Number(0), _Number(1), _Number(1)))
    // both binder names appear only as the (excluded) binders here — the sole free
    // child that could name them is rhs = y, so children carries exactly one "y".
    assert(kids.count(_ == _Variable("y")) == 1)

  it should "rebuild from a new child list while preserving the binders" in:
    val rebuilt = sample.rebuild(List(_Variable("t"), _Number(2), _Number(3), _Number(4)))
    rebuilt match
      case o: _ODE =>
        assert(o.rhs == _Variable("t"))
        assert(o.depVar.variable == "y")
        assert(o.indepVar.variable == "t")
        assert(o.t0 == _Number(2))
        assert(o.y0 == _Number(3))
        assert(o.target == _Number(4))
      case other => fail(s"expected _ODE, got $other")

  // ───────────────────────────── eval (scaffolding: stays symbolic) ─────────────────────────────

  it should "stay symbolic on eval for now (Left(this))" in:
    assert(sample.eval(emptyEnv) == Left(sample))

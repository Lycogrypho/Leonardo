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

  // ───────────────────────────── RK4 numeric eval ─────────────────────────────

  // Small helper: build and eval an _ODE, expecting a finite _Number close to `expected`.
  private def approxSolve(node: _ODE, expected: Double, tol: Double): Unit =
    node.eval(emptyEnv) match
      case Right(_Number(d)) => assert(math.abs(d - expected) <= tol, s"$node: expected ≈ $expected but got $d")
      case other             => fail(s"$node: expected _Number ≈ $expected, got $other")

  it should "solve y' = y, y(0) = 1 at t = 1 (→ e)" in:
    approxSolve(sample, math.E, 1e-6)

  it should "solve y' = -2y, y(0) = 1 at t = 1 (→ e^-2)" in:
    val node = _ODE(Product(_Number(-2), _Variable("y")), _Variable("y"), _Variable("t"),
                    _Number(0), _Number(1), _Number(1))
    approxSolve(node, math.exp(-2), 1e-6)

  it should "solve y' = t, y(0) = 0 at t = 2 (→ 2)" in:
    val node = _ODE(_Variable("t"), _Variable("y"), _Variable("t"),
                    _Number(0), _Number(0), _Number(2))
    approxSolve(node, 2.0, 1e-6)   // y = t^2/2 → 2

  it should "solve y' = t*y, y(0) = 1 at t = 1 (→ e^{1/2})" in:
    val node = _ODE(Product(_Variable("t"), _Variable("y")), _Variable("y"), _Variable("t"),
                    _Number(0), _Number(1), _Number(1))
    approxSolve(node, math.exp(0.5), 1e-6)   // y = e^{t^2/2}

  it should "integrate backwards when target < t0 (y' = y, y(0)=1 at t=-1 → e^-1)" in:
    val node = _ODE(_Variable("y"), _Variable("y"), _Variable("t"),
                    _Number(0), _Number(1), _Number(-1))
    approxSolve(node, math.exp(-1), 1e-6)

  it should "return y0 unchanged when target == t0" in:
    val node = _ODE(_Variable("y"), _Variable("y"), _Variable("t"),
                    _Number(3), _Number(7), _Number(3))
    approxSolve(node, 7.0, 1e-12)

  it should "stay symbolic when rhs is not numerically evaluable" in:
    // rhs references an unbound variable k (neither t nor y), so f(t, y) can't reduce.
    val node = _ODE(Product(_Variable("k"), _Variable("y")), _Variable("y"), _Variable("t"),
                    _Number(0), _Number(1), _Number(1))
    assert(node.eval(emptyEnv) == Left(node))

  it should "stay symbolic when the initial condition is symbolic" in:
    val node = _ODE(_Variable("y"), _Variable("y"), _Variable("t"),
                    _Number(0), _Variable("c"), _Number(1))
    assert(node.eval(emptyEnv) == Left(node))

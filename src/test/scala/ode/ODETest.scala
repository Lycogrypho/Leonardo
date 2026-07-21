package it.grypho.scala.leonardo
package ode

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.BeforeAndAfter
import core.*
import scalar.*
import ode.*
import parser.Parser


class ODETest extends AnyFlatSpec with BeforeAndAfter:

  private val emptyEnv = new Environment()

  private def parse(s: String): _Expression = Parser.parse(s).get

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

  it should "stay symbolic when the shape is unrecognised and the target is symbolic" in:
    // sin(y) is nonlinear (collect → None), so the symbolic tier declines; a free target
    // means RK4 cannot run either → the node stays fully symbolic.
    val node = _ODE(Sin(_Variable("y")), _Variable("y"), _Variable("t"),
                    _Number(0), _Number(1), _Variable("T"))
    assert(node.eval(emptyEnv) == Left(node))

  it should "stay symbolic for a t-dependent coefficient with a symbolic target" in:
    // y' = t*y is linear in y but the coefficient depends on t → not constant-coefficient,
    // so the symbolic tier declines; a free target blocks RK4 → fully symbolic.
    val node = _ODE(Product(_Variable("t"), _Variable("y")), _Variable("y"), _Variable("t"),
                    _Number(0), _Number(1), _Variable("T"))
    assert(node.eval(emptyEnv) == Left(node))

  // ───────────────────────────── symbolic (closed-form) tier ─────────────────────────────

  it should "return a symbolic closed form when the target is free (y' = y → e^{tau})" in:
    val node = _ODE(_Variable("y"), _Variable("y"), _Variable("t"),
                    _Number(0), _Number(1), _Variable("tau"))
    // free target → symbolic result
    assert(node.eval(emptyEnv).isLeft, s"expected symbolic closed form, got ${node.eval(emptyEnv)}")
    // …that folds to e once tau is bound to 1
    node.eval(new Environment(5, Map("tau" -> _Number(1)))).toExpression match
      case _Number(d) => assert(math.abs(d - math.E) <= 1e-9)
      case other      => fail(s"expected _Number ≈ e, got $other")

  it should "solve the affine ODE y' = 2y + 3, y(0)=1 at t=1 in closed form" in:
    // analytic: (1 + 3/2)e^2 − 3/2 = 2.5·e^2 − 1.5
    val expected = 2.5 * math.exp(2) - 1.5
    val node = _ODE(Sum(Product(_Number(2), _Variable("y")), _Number(3)),
                    _Variable("y"), _Variable("t"), _Number(0), _Number(1), _Number(1))
    approxSolve(node, expected, 1e-9)

  it should "carry a free parameter through the closed form (y' = k*y → e^k)" in:
    val node = _ODE(Product(_Variable("k"), _Variable("y")), _Variable("y"), _Variable("t"),
                    _Number(0), _Number(1), _Number(1))
    assert(node.eval(emptyEnv).isLeft)   // k free → symbolic
    node.eval(new Environment(5, Map("k" -> _Number(-2)))).toExpression match
      case _Number(d) => assert(math.abs(d - math.exp(-2)) <= 1e-9)
      case other      => fail(s"expected _Number ≈ e^-2, got $other")

  it should "carry a symbolic initial condition through the closed form (y' = y, y(0)=c → c*e)" in:
    val node = _ODE(_Variable("y"), _Variable("y"), _Variable("t"),
                    _Number(0), _Variable("c"), _Number(1))
    assert(node.eval(emptyEnv).isLeft)   // c free → symbolic
    node.eval(new Environment(5, Map("c" -> _Number(4)))).toExpression match
      case _Number(d) => assert(math.abs(d - 4 * math.E) <= 1e-9)
      case other      => fail(s"expected _Number ≈ 4e, got $other")

  it should "produce a closed form that matches RK4 numerically (y' = 2y + 3)" in:
    val rhs  = Sum(Product(_Number(2), _Variable("y")), _Number(3))
    val node = _ODE(rhs, _Variable("y"), _Variable("t"), _Number(0), _Number(1), _Number(1))
    val symbolic = node.eval(emptyEnv).toExpression match
      case _Number(d) => d
      case other      => fail(s"expected numeric closed form, got $other")
    val rk4 = solveODE(rhs, _Variable("y"), _Variable("t"), 0.0, 1.0, 1.0, emptyEnv) match
      case Some(d) => d
      case None    => fail("RK4 unexpectedly failed")
    assert(math.abs(symbolic - rk4) <= 1e-4, s"closed form $symbolic vs RK4 $rk4")

  // ───────────────────────────── parser ─────────────────────────────

  "the ode parser" should "build an _ODE node with the fields in order" in:
    parse("ode(t*y, y, t, 0, 1, 2)") match
      case o: _ODE =>
        assert(o.rhs == Product(_Variable("t"), _Variable("y")))
        assert(o.depVar.variable == "y")
        assert(o.indepVar.variable == "t")
        assert(o.t0 == _Number(0))
        assert(o.y0 == _Number(1))
        assert(o.target == _Number(2))
      case other => fail(s"expected _ODE, got $other")

  it should "round-trip through toString and re-parse" in:
    val e  = parse("ode(2*y + 3, y, t, 0, 1, 1)")
    val e2 = parse(e.toString)
    assert(e == e2)

  it should "reject a bare 'ode' as a variable (reserved word)" in:
    assert(Parser.parse("ode + 1").successful == false)

  it should "evaluate ode(y, y, t, 0, 1, 1) end-to-end to ≈ e" in:
    parse("ode(y, y, t, 0, 1, 1)").eval(emptyEnv).toExpression match
      case _Number(d) => assert(math.abs(d - math.E) <= 1e-6)
      case other      => fail(s"expected _Number ≈ e, got $other")

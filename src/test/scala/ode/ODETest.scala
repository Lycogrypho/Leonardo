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

  // ───────────────────────────── step-count policy (issue 2.3) ─────────────────────────────

  it should "floor the step count at BaseSteps for a short interval" in:
    // span = 1 at default precision → the BaseSteps floor (1000).
    assert(stepCount(1.0, 5) == 1000)

  it should "scale the step count with the interval length" in:
    // span = 100 → ~100× the floor, so h stays ≈ 1e-3 instead of ballooning to 0.1.
    assert(stepCount(100.0, 5) == 100000)

  it should "keep the floor for a tiny interval" in:
    assert(stepCount(0.001, 5) == 1000)

  it should "not collapse to a single step at precision 0" in:
    // The old formula (BaseSteps * precision / DefaultPrecision) gave 0 → 1 step here.
    assert(stepCount(1.0, 0) == 1000)

  it should "refine further at higher precision" in:
    assert(stepCount(1.0, 10) == 2000)

  it should "cap the step count for a very long interval" in:
    assert(stepCount(1e9, 5) == 1000000)

  it should "stay accurate over a long interval (y' = -y^2, y(0)=1 at t=1000 → 1/1001)" in:
    // Nonlinear rhs (collect → degree 2 → None) forces RK4, not the closed-form tier;
    // exercises the interval-scaled step count over a long span (h stays ≈ 1e-3). With the
    // old span-independent count h would be ~1 here.
    val rhs  = Product(_Number(-1), Power(_Variable("y"), _Number(2)))
    val node = _ODE(rhs, _Variable("y"), _Variable("t"), _Number(0), _Number(1), _Number(1000))
    approxSolve(node, 1.0 / 1001.0, 1e-6)   // exact solution y = 1/(1+t)

  it should "not degrade at precision 0 (interval step count is precision-independent floor)" in:
    // y' = -y^2 is nonlinear (collect → None) so it forces RK4; at precision 0 the old code
    // used 1 step (large error). Exact solution y = 1/(1+t) → 0.5 at t = 1.
    val rhs  = Product(_Number(-1), Power(_Variable("y"), _Number(2)))
    val node = _ODE(rhs, _Variable("y"), _Variable("t"), _Number(0), _Number(1), _Number(1))
    node.eval(new Environment(0)).toExpression match
      case _Number(d) => assert(math.abs(d - 0.5) <= 1e-6, s"got $d")
      case other      => fail(s"expected _Number ≈ 0.5, got $other")

  it should "stay symbolic when the shape is unrecognised and the target is symbolic" in:
    // sin(y) is nonlinear (collect → None), so the symbolic tier declines; a free target
    // means RK4 cannot run either → the node stays fully symbolic.
    val node = _ODE(Sin(_Variable("y")), _Variable("y"), _Variable("t"),
                    _Number(0), _Number(1), _Variable("T"))
    assert(node.eval(emptyEnv) == Left(node))

  it should "solve a t-dependent-coefficient ODE in closed form (y' = t*y → e^{T^2/2})" in:
    // Integrating-factor tier (issue 4.D): mu = exp(-t^2/2), b = 0 → y = y0*exp((T^2 - t0^2)/2).
    // A free target keeps the result symbolic; it folds once T is bound. Exact y = e^{t^2/2}.
    val node = _ODE(Product(_Variable("t"), _Variable("y")), _Variable("y"), _Variable("t"),
                    _Number(0), _Number(1), _Variable("T"))
    assert(node.eval(emptyEnv).isLeft, s"expected symbolic closed form, got ${node.eval(emptyEnv)}")
    node.eval(new Environment(5, Map("T" -> _Number(1)))).toExpression match
      case _Number(d) => assert(math.abs(d - math.exp(0.5)) <= 1e-9, s"got $d")
      case other      => fail(s"expected _Number ≈ e^{1/2}, got $other")

  it should "now close through the tan rule the 6.21 table supplies" in:
    // This previously asserted "fully symbolic, because integral(tan(t)) is not in the
    // table".  6.21's data-driven table supplies that integral, so the integrating-factor
    // tier closes: y' = tan(t)*y with y(0) = 1 integrates to y = 1/cos(t).
    val node = _ODE(Product(Tg(_Variable("t")), _Variable("y")), _Variable("y"), _Variable("t"),
                    _Number(0), _Number(1), _Variable("T"))
    assert(node.eval(emptyEnv) != Left(node), "should no longer be the bare node")
    node.eval(new Environment(5, Map("T" -> _Number(0.5)))).toExpression match
      case _Number(d) => assert(math.abs(d - 1.0 / math.cos(0.5)) <= 1e-9, s"got $d")
      case other      => fail(s"expected 1/cos(0.5), got $other")

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

  // ─────────────────────── variable-coefficient tier (issue 4.D) ───────────────────────

  it should "solve the linear ODE y' = -y + t, y(0)=1 at t=1 in closed form (→ 2/e)" in:
    // integrating factor mu = e^t; mu*q = t*e^t is integrated by parts. Exact y = t - 1 + 2e^{-t}.
    val rhs  = Sum(Product(_Number(-1), _Variable("y")), _Variable("t"))
    val node = _ODE(rhs, _Variable("y"), _Variable("t"), _Number(0), _Number(1), _Number(1))
    approxSolve(node, 2.0 / math.E, 1e-9)

  it should "solve a variable-coefficient ODE y' = -y/(1+t), y(0)=1 at t=3 in closed form (→ 1/4)" in:
    // coefficient a = -1/(1+t): integral(a) = -ln(1+t), mu = 1+t. Exact y = 1/(1+t).
    val rhs  = Ratio(Product(_Number(-1), _Variable("y")), Sum(_Number(1), _Variable("t")))
    val node = _ODE(rhs, _Variable("y"), _Variable("t"), _Number(0), _Number(1), _Number(3))
    approxSolve(node, 0.25, 1e-9)

  it should "match RK4 numerically for the variable-coefficient ODE y' = -y + t" in:
    val rhs  = Sum(Product(_Number(-1), _Variable("y")), _Variable("t"))
    val node = _ODE(rhs, _Variable("y"), _Variable("t"), _Number(0), _Number(1), _Number(1))
    val symbolic = node.eval(emptyEnv).toExpression match
      case _Number(d) => d
      case other      => fail(s"expected numeric closed form, got $other")
    val rk4 = solveODE(rhs, _Variable("y"), _Variable("t"), 0.0, 1.0, 1.0, emptyEnv) match
      case Some(d) => d
      case None    => fail("RK4 unexpectedly failed")
    assert(math.abs(symbolic - rk4) <= 1e-4, s"closed form $symbolic vs RK4 $rk4")

  it should "fall back to RK4 when the coefficient integral has no closed form (y' = tan(t)*y)" in:
    // integral(tan(t)) is not in the table → integrating-factor tier declines; with a numeric
    // target RK4 takes over. Exact solution y = sec(t) → 1/cos(0.5).
    val rhs  = Product(Tg(_Variable("t")), _Variable("y"))
    val node = _ODE(rhs, _Variable("y"), _Variable("t"), _Number(0), _Number(1), _Number(0.5))
    approxSolve(node, 1.0 / math.cos(0.5), 1e-6)

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

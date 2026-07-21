package it.grypho.scala.leonardo
package ode

import core.*
import scalar.*


// First-order initial-value problem  y' = rhs(t, y),  y(t₀) = y₀.
// The node denotes the solution value y(target) of that IVP.
//
//   depVar   = y  — the dependent variable (the unknown function's name)
//   indepVar = t  — the independent variable (what y is differentiated against)
//   rhs           — the right-hand side f(t, y)
//   t0, y0        — the initial condition
//   target        — the point at which the solution is evaluated
//
// Both depVar and indepVar are binders and are excluded from children (same
// convention as _Derivative's v and _Limit's binder), so Substitute/Analysis never
// recurse into them. rhs/t0/y0/target are ordinary expression positions (they may
// hold free variables) and so are the children, in that fixed order.
//
// eval reduces to Right(_Number(y(target))) when the initial condition and target fold
// to numbers and the RK4 solver (SolveODE.scala) succeeds; otherwise it stays symbolic
// (Left(this)) — the fixpoint convention shared with Integrate/Derive/the transforms.
// A symbolic tier for closed-form cases (substep 3) will run ahead of the numeric path.
case class _ODE(rhs: _Expression, depVar: _Variable, indepVar: _Variable,
                t0: _Expression, y0: _Expression, target: _Expression) extends _Functional:
  override def toString: String = s"ode($rhs, $depVar, $indepVar, $t0, $y0, $target)"
  override def children: List[_Expression] = List(rhs, t0, y0, target)
  override def rebuild(c: List[_Expression]): _Expression =
    _ODE(c.head, depVar, indepVar, c(1), c(2), c(3))

  override def eval(env: Environment): Either[_Expression, _Value] =
    (t0.eval(env), y0.eval(env), target.eval(env)) match
      case (Right(_Number(a)), Right(_Number(y)), Right(_Number(b))) =>
        solveODE(rhs, depVar, indepVar, a, y, b, env) match
          case Some(result) => Right(_Number(result))
          case None         => Left(this)
      case _ => Left(this)

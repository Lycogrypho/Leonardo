package it.grypho.scala.leonardo
package ode

import core.*
import scalar.*


/** Symbolic node for the solution value of a first-order initial-value problem.
 *
 *  Represents `y(target)` where `y` satisfies `y' = rhs(t, y)`, `y(t0) = y0`.
 *
 *  Both `depVar` (the dependent variable `y`) and `indepVar` (the independent
 *  variable `t`) are **binders**: they are excluded from `children` so that
 *  `substitute` and `dependsOn` never recurse into them, matching the convention
 *  of `scalar._Derivative` and `scalar._Limit`.  The expression positions
 *  `rhs`, `t0`, `y0`, and `target` are ordinary children and may hold free
 *  variables (e.g. a symbolic `target` keeps the result closed-form).
 *
 *  Evaluation strategy (two tiers, in order):
 *   1. **Closed-form** via `solveODESymbolic`: handles `y' = a*y + b` for
 *      constant (t-free) `a`, `b`; returns a symbolic expression that stays
 *      closed-form when `t0`/`y0`/`target` are free.
 *   2. **Numeric RK4** via `solveODE`: folds `t0`/`y0`/`target` to concrete
 *      `_Number`s and runs the integrator; returns `Right(_Number(result))`.
 *  When neither applies, `eval` returns `Left(this)` — the fixpoint convention
 *  shared with the transform nodes and the `scalar._Functional` hierarchy.
 *
 *  Round-trip: `toString` emits `ode(rhs, depVar, indepVar, t0, y0, target)`,
 *  which re-parses to an equivalent node.
 *
 *  @param rhs      the right-hand side `f(t, y)` of the ODE
 *  @param depVar   the dependent variable (the unknown function, e.g. `y`)
 *  @param indepVar the independent variable (what `y` is differentiated against, e.g. `t`)
 *  @param t0       the initial time point
 *  @param y0       the initial value `y(t0)`
 *  @param target   the point at which the solution is evaluated
 */
case class _ODE(rhs: _Expression, depVar: _Variable, indepVar: _Variable,
                t0: _Expression, y0: _Expression, target: _Expression) extends _Functional:
  override def toString: String = s"ode($rhs, $depVar, $indepVar, $t0, $y0, $target)"
  override def children: List[_Expression] = List(rhs, t0, y0, target)
  override def rebuild(c: List[_Expression]): _Expression =
    _ODE(c.head, depVar, indepVar, c(1), c(2), c(3))

  override def eval(env: Environment): Either[_Expression, _Value] =
    solveODESymbolic(rhs, depVar, indepVar, t0, y0, target, env) match
      case Some(sol) => sol.eval(env)     // numeric when it folds, symbolic closed form otherwise
      case None =>
        (t0.eval(env), y0.eval(env), target.eval(env)) match
          case (Right(_Number(a)), Right(_Number(y)), Right(_Number(b))) =>
            solveODE(rhs, depVar, indepVar, a, y, b, env) match
              case Some(result) => Right(_Number(result))
              case None         => Left(this)
          case _ => Left(this)

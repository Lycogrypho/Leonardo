package it.grypho.scala.leonardo

/** Ordinary differential equations: first-order IVP solver with closed-form and numeric tiers.
 *
 *  The single public node is `_ODE(rhs, depVar, indepVar, t0, y0, target)`, which
 *  represents the solution value `y(target)` of the initial-value problem
 *  `y' = rhs(t, y)`, `y(t0) = y0`.  Evaluation proceeds in two tiers:
 *
 *  1. **Closed-form** (`SolveODESymbolic`): recognises the constant-coefficient
 *     linear shape `y' = a*y + b` and emits an exact symbolic result.  Works even
 *     when `target`, `y0`, or `t0` are free variables.
 *  2. **Numeric RK4** (`SolveODE`): fourth-order Runge-Kutta over a step count
 *     scaled by both the interval length and the session precision.  Returns
 *     `Some(Double)` when every stage is finite, `None` otherwise.
 *
 *  When neither tier applies, the node stays symbolic (`Left(this)`) — the
 *  fixpoint convention shared with `scalar._Functional` and the transform nodes.
 *
 *  Package layering: `ode` imports `core` and `scalar`; nothing imports `ode`
 *  except `parser` and `cli` (the leaf packages).
 */
package ode

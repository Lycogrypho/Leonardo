package it.grypho.scala.leonardo

/** Ordinary differential equations: first-order IVP solver with closed-form and numeric tiers.
 *
 *  The single public node is `_ODE(rhs, depVar, indepVar, t0, y0, target)`, which
 *  represents the solution value `y(target)` of the initial-value problem
 *  `y' = rhs(t, y)`, `y(t0) = y0`.  Evaluation proceeds in three tiers:
 *
 *  1. **Linear system** (`SolveODESystem`, F_0006): when `y0` is an `n×1` column and
 *     `rhs` is `A*y` or `A*y + b` with constant matrices, the state `y(target)` by the
 *     augmented matrix exponential — the kernel `control.c2dExact` shares.  Numeric only.
 *  2. **Closed-form** (`SolveODESymbolic`): recognises the scalar linear shape
 *     `y' = a(t)*y + b(t)` and the separable `y' = f(t)*g(y)`, and emits an exact symbolic
 *     result.  Works even when `target`, `y0`, or `t0` are free variables.  Declines a
 *     matrix coefficient, which would otherwise build an element-wise `exp` in the wrong
 *     order.
 *  3. **Numeric RK4** (`SolveODE`): fourth-order Runge-Kutta over a step count
 *     scaled by both the interval length and the session precision.  Returns
 *     `Some(Double)` when every stage is finite, `None` otherwise.  Scalar state only.
 *
 *  When no tier applies, the node stays symbolic (`Left(this)`) — the
 *  fixpoint convention shared with `scalar._Functional` and the transform nodes.
 *
 *  Package layering: `ode` imports `core` and `scalar` — a literal matrix's `MatProduct` /
 *  `MatSum` are read through the `core` shape traits, never by importing `matrix`; nothing imports `ode`
 *  except `parser` and `cli` (the leaf packages).
 */
package ode

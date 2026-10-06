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
 *  Evaluation strategy (four tiers, in order):
 *   1. **Linear system** via `solveODESystem` (F_0006): when `y0` evaluates to an `n×1`
 *      column and `rhs` is `A*y` or `A*y + b` with a constant square matrix `A`, the
 *      answer is `e^(A*tau)*y0` (plus the input term) by the augmented matrix exponential.
 *      Tried first so a vector problem never reaches the scalar `collect`.  Numeric only.
 *   2. **Closed-form** via `solveODESymbolic`: handles `y' = a*y + b` for a scalar
 *      coefficient (constant or, via the integrating factor, time-varying) and the
 *      separable shapes; returns a symbolic expression that stays closed-form when
 *      `t0`/`y0`/`target` are free.  It declines a matrix coefficient, which is tier 1's.
 *   3. **Numeric RK4** via `solveODE`: folds `t0`/`y0`/`target` to concrete
 *      `_Number`s and runs the integrator; returns `Right(_Number(result))`.
 *   4. **Vector RK4** (F_0051) via `odeCertified`, when `y0` is a column and tiers 1–2 declined
 *      — a time-varying `A(t)*y`, or a nonlinear system written through `at(y, i, 1)`: `depVar`
 *      is bound to the whole column, and the answer is certified by step doubling.  The
 *      column-of-names spelling builds [[_ODESystem]] instead.
 *  When none applies, `eval` returns `Left(this)` — the fixpoint convention
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
    solveODESystem(rhs, depVar, indepVar, t0, y0, target, env) match
      case Some(state) => Right(state)
      case None        =>
        evalScalar(env) match
          case l @ Left(_) =>
            vectorDetail(env).fold(l)(_.fold(_ => l, s => Right(_MatrixValue(s.size, 1, s.toArray))))
          case r => r

  /** The vector tier (F_0051): when `y0` is a column and the closed forms declined, RK4 with
   *  `depVar` bound to the whole column, certified by step doubling.  `None` when `y0` is not a
   *  column (the scalar tiers own that case); otherwise the state or why there is none — what
   *  the REPL appends as a note. */
  def vectorDetail(env: Environment): Option[Either[OdeFailure, Vector[Double]]] =
    columnState(y0, env).map { y =>
      (t0.eval(env), target.eval(env)) match
        case (Right(_Number(a)), Right(_Number(b))) =>
          odeCertified(rhs, OdeState.Whole(depVar), indepVar, a, y, b, env)
        case _ => Left(OdeFailure.InvalidInput("t0 and the target must be numbers"))
    }

  /** The scalar tiers: the closed forms, then RK4. */
  private def evalScalar(env: Environment): Either[_Expression, _Value] =
    solveODESymbolic(rhs, depVar, indepVar, t0, y0, target, env) match
      case Some(sol) => sol.eval(env)     // numeric when it folds, symbolic closed form otherwise
      case None =>
        (t0.eval(env), y0.eval(env), target.eval(env)) match
          case (Right(_Number(a)), Right(_Number(y)), Right(_Number(b))) =>
            solveODE(rhs, depVar, indepVar, a, y, b, env) match
              case Some(result) => Right(_Number(result))
              case None         => Left(this)
          case _ => Left(this)

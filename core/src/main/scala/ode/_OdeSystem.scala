package it.grypho.scala.leonardo
package ode

import core.*


/** `ode(rhs, [[x], [v]], t, t0, y0, target)` — a system whose state is written as a column of
 *  names (F_0051 Decision A), each bound to its component, so `[[v], [-x]]` reads as written.
 *
 *  A separate node rather than a widened [[_ODE]]: `_ODE` is a published case class whose
 *  `depVar` is one `_Variable`, and changing that field's type would be a binary break for a
 *  spelling.  The single-name form still builds `_ODE`, unchanged.
 *
 *  Integrated by [[odeCertified]]: RK4 at the scalar tier's step count, answered only when `n`
 *  and `2n` steps agree within the tolerance `=` uses (Decision B).  The names are binders —
 *  excluded from `children`, carried through `rebuild`.
 *
 *  @param rhs      the right-hand side, an `n×1` column in the state names and `indepVar`
 *  @param states   the state names, in the order of `y0`
 *  @param indepVar the independent variable
 *  @param t0       the initial time
 *  @param y0       the initial state, an `n×1` column
 *  @param target   the time the state is wanted at
 */
case class _ODESystem(rhs: _Expression, states: Vector[_Variable], indepVar: _Variable,
                      t0: _Expression, y0: _Expression, target: _Expression) extends _Expression:
  override def toString: String =
    s"ode($rhs, ${OdeState.render(OdeState.Components(states))}, $indepVar, $t0, $y0, $target)"
  override def children: List[_Expression] = List(rhs, t0, y0, target)
  override def rebuild(c: List[_Expression]): _Expression = _ODESystem(c.head, states, indepVar, c(1), c(2), c(3))

  override def eval(env: Environment): Either[_Expression, _Value] =
    detailed(env).fold(_ => Left(this), s => Right(_MatrixValue(s.size, 1, s.toArray)))

  /** The integrator's full answer: the state, or why there is none (what the REPL explains). */
  def detailed(env: Environment): Either[OdeFailure, Vector[Double]] =
    (number(t0, env), columnState(y0, env), number(target, env)) match
      case (Some(a), Some(y), Some(b)) =>
        if y.size != states.size then
          Left(OdeFailure.InvalidInput(s"${states.size} state names for an initial state of ${y.size} entries"))
        else odeCertified(rhs, OdeState.Components(states), indepVar, a, y, b, env)
      case _ => Left(OdeFailure.InvalidInput("t0 and the target must be numbers, and y0 a numeric column"))


/** `odeStep(F, y, t, t0, y0, h, method)` — the state one step of `method` after `t0`
 *  (F_0051, MPC requirement T1-04).  `y` is a name (bound to the whole column) or a column of
 *  names.  For `euler`/`rk4` exactly one step of size `h`, uncertified, since the step is the
 *  caller's; for `rk45`, the state at `t0 + h` by adaptive steps.  Names bound in the environment
 *  — a controller's inputs — are constants during the step.
 *
 *  @param rhs      the right-hand side
 *  @param state    how the state is named
 *  @param indepVar the independent variable
 *  @param t0       the time of the current state
 *  @param y0       the current state, an `n×1` column
 *  @param h        the step (or the interval, for `rk45`)
 *  @param method   the method, always named
 */
case class _OdeStep(rhs: _Expression, state: OdeState, indepVar: _Variable, t0: _Expression,
                    y0: _Expression, h: _Expression, method: OdeMethod) extends _Expression:
  override def toString: String =
    s"odeStep($rhs, ${OdeState.render(state)}, $indepVar, $t0, $y0, $h, ${OdeMethod.keyword(method)})"
  override def children: List[_Expression] = List(rhs, t0, y0, h)
  override def rebuild(c: List[_Expression]): _Expression = _OdeStep(c.head, state, indepVar, c(1), c(2), c(3), method)

  override def eval(env: Environment): Either[_Expression, _Value] =
    detailed(env).fold(_ => Left(this), s => Right(_MatrixValue(s.size, 1, s.toArray)))

  /** The integrator's full answer: the state, or why there is none. */
  def detailed(env: Environment): Either[OdeFailure, Vector[Double]] =
    (number(t0, env), columnState(y0, env), number(h, env)) match
      case (Some(a), Some(y), Some(step)) => odeStep(rhs, state, indepVar, a, y, step, method, env)
      case _ => Left(OdeFailure.InvalidInput("t0 and h must be numbers, and y0 a numeric column"))


/** `odeSolve(F, y, t, t0, y0, t1, h, method)` — the trajectory from `t0` to `t1` as a matrix,
 *  one row per output time with `t` in the first column then the state (F_0051 Decision D).
 *
 *  @param rhs      the right-hand side
 *  @param state    how the state is named
 *  @param indepVar the independent variable
 *  @param t0       the initial time
 *  @param y0       the initial state, an `n×1` column
 *  @param t1       the final time, which is always a row
 *  @param h        the output spacing (and the step, for `euler`/`rk4`)
 *  @param method   the method, always named
 */
case class _OdeSolve(rhs: _Expression, state: OdeState, indepVar: _Variable, t0: _Expression,
                     y0: _Expression, t1: _Expression, h: _Expression, method: OdeMethod) extends _Expression:
  override def toString: String =
    s"odeSolve($rhs, ${OdeState.render(state)}, $indepVar, $t0, $y0, $t1, $h, ${OdeMethod.keyword(method)})"
  override def children: List[_Expression] = List(rhs, t0, y0, t1, h)
  override def rebuild(c: List[_Expression]): _Expression =
    _OdeSolve(c.head, state, indepVar, c(1), c(2), c(3), c(4), method)

  override def eval(env: Environment): Either[_Expression, _Value] =
    detailed(env).fold(_ => Left(this), rows => {
      val n = rows.head._2.size
      Right(_MatrixValue(rows.size, n + 1, rows.flatMap((t, s) => t +: s).toArray))
    })

  /** The integrator's full answer: the rows, or why there are none. */
  def detailed(env: Environment): Either[OdeFailure, Vector[(Double, Vector[Double])]] =
    (number(t0, env), columnState(y0, env), number(t1, env), number(h, env)) match
      case (Some(a), Some(y), Some(b), Some(step)) => odeSolve(rhs, state, indepVar, a, y, b, step, method, env)
      case _ => Left(OdeFailure.InvalidInput("t0, t1 and h must be numbers, and y0 a numeric column"))


/** A finite number, or `None`. */
private def number(e: _Expression, env: Environment): Option[Double] = e.eval(env) match
  case Right(_Number(d)) if !d.isNaN && !d.isInfinite => Some(d)
  case _                                              => None

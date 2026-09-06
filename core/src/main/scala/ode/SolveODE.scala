package it.grypho.scala.leonardo
package ode

import core.*


/** Fourth-order Runge-Kutta integrator for first-order IVPs.
 *
 *  Computes `y(target)` for `y' = rhs(t, y)`, `y(t0) = y0` using the classic
 *  RK4 scheme.  Returns `Some(result)` when every stage evaluates to a finite
 *  `Double`; `None` when any stage is non-numeric or non-finite (the caller
 *  keeps the `_ODE` node symbolic).
 *
 *  Step-count policy: the step count is driven by both `|target - t0|` and
 *  `env.precision`, so the step size `h` stays bounded regardless of interval
 *  length — integrating over 100 units does not reuse the same step count as
 *  integrating over 1 unit.  The signed step `h = (target - t0) / n` makes the
 *  scheme integrate backwards when `target < t0` without any special-casing.
 *
 *  Evaluation: each RK4 stage calls `rhs.eval` with `indepVar` and `depVar`
 *  bound in a scoped `Environment` at full `Double` precision — no rounding is
 *  applied during accumulation (matching `core._Number`'s eval-vs-display split).
 */

/** Minimum number of steps regardless of interval length or precision. */
private val BaseSteps    = 1000

/** Target steps per unit of `|target - t0|`; keeps `h` near `1 / StepsPerUnit`. */
private val StepsPerUnit = 1000

/** Hard ceiling on the step count so a very long interval cannot run away. */
private val MaxSteps     = 1_000_000

/** Computes the RK4 step count for an interval of length `span` at the given `precision`.
 *
 *  The interval factor `ceil(span * StepsPerUnit)` keeps `h` near `1 / StepsPerUnit`
 *  for any span; `BaseSteps` floors the count so short intervals and `precision 0`
 *  remain well-resolved; the precision factor (>= 1) refines at higher precision;
 *  `MaxSteps` caps the work for very long intervals.
 *
 *  @param span      absolute length of the integration interval `|target - t0|`
 *  @param precision the session display precision (from `env.precision`)
 *  @return number of RK4 steps to use
 */
private[ode] def stepCount(span: Double, precision: Int): Int =
  val precFactor = math.max(1.0, precision.toDouble / Environment.DefaultPrecision)
  (math.max(BaseSteps.toDouble, math.ceil(span * StepsPerUnit)) * precFactor)
    .min(MaxSteps.toDouble)
    .max(1.0)
    .toInt

/** Integrates `y' = rhs(t, y)`, `y(t0) = y0` to `y(target)` via RK4.
 *
 *  @param rhs      the right-hand side `f(t, y)` as a symbolic expression
 *  @param depVar   the dependent variable name (bound to the current `y` at each stage)
 *  @param indepVar the independent variable name (bound to the current `t` at each stage)
 *  @param t0       the initial time
 *  @param y0       the initial value `y(t0)`
 *  @param target   the time at which the solution is requested
 *  @param env      the evaluation environment (provides precision and other bindings)
 *  @return `Some(y(target))` on success; `None` if any stage is non-numeric or non-finite
 */
def solveODE(rhs: _Expression, depVar: _Variable, indepVar: _Variable,
             t0: Double, y0: Double, target: Double, env: Environment): Option[Double] =
  val tv = indepVar.variable
  val yv = depVar.variable

  // f(t, y) = rhs with t and y bound; None on any non-numeric or non-finite result.
  def f(t: Double, y: Double): Option[Double] =
    rhs.eval(env.withBinding(tv, _Number(t)).withBinding(yv, _Number(y))) match
      case Right(_Number(d)) if d.isFinite => Some(d)
      case _                               => None

  if target == t0 then Some(y0)
  else
    val n = stepCount(math.abs(target - t0), env.precision)
    val h = (target - t0) / n

    @annotation.tailrec
    def loop(i: Int, t: Double, y: Double): Option[Double] =
      if i >= n then Some(y)
      else
        val next =
          for
            k1 <- f(t, y)
            k2 <- f(t + h / 2, y + h / 2 * k1)
            k3 <- f(t + h / 2, y + h / 2 * k2)
            k4 <- f(t + h, y + h * k3)
          yield y + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
        next match
          case Some(yNext) if yNext.isFinite => loop(i + 1, t + h, yNext)
          case _                             => None

    loop(0, t0, y0)

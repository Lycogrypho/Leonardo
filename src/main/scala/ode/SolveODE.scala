package it.grypho.scala.leonardo
package ode

import core.*


// Classic fourth-order Runge–Kutta integrator for the first-order initial-value
// problem  y' = rhs(t, y),  y(t₀) = y₀.  Returns Some(y(target)) when every stage
// evaluates to a finite Double, else None (unresolvable rhs, or a non-finite
// intermediate → the caller keeps the _ODE node symbolic).
//
// The step count (see stepCount) is driven by the INTERVAL length |target − t₀|, not by
// env.precision alone, so the step size h stays bounded no matter how long the interval is
// — integrating over 100 units no longer uses the same 1000 steps as over 1 unit. The
// signed step h = (target − t₀)/n makes the scheme integrate backwards when target < t₀
// without any special-casing.
//
// rhs is evaluated per stage by tree-eval with the independent variable (t) and the
// dependent variable (y) bound in a scoped Environment. eval performs no rounding
// (values propagate at full Double precision — see _Number), so the RK4 accumulation
// is not degraded by the display precision. A compiled two-variable fast path (à la
// scalar.compile) is a possible future optimization; tree-eval keeps this correct and
// uniform over every expression shape (functions, ratios, nested _ODE, …).

private val BaseSteps    = 1000        // step-count floor: every interval gets at least this many
private val StepsPerUnit = 1000        // steps per unit of |target − t₀|  ⇒  h ≈ 1/StepsPerUnit
private val MaxSteps     = 1_000_000   // work ceiling so a very long interval cannot run away

// RK4 step count for integrating over an interval of length `span` = |target − t₀| at the
// given display precision. The interval term keeps h ≈ 1/StepsPerUnit for any span; the
// BaseSteps floor keeps a short interval well-resolved (and stops `precision 0` collapsing
// to a single step — the previous formula multiplied by precision/DefaultPrecision, which is
// 0 at precision 0); the precision factor (≥ 1) refines further at higher precision; MaxSteps
// bounds the work for a very long interval (beyond it, h grows rather than the step count).
private[ode] def stepCount(span: Double, precision: Int): Int =
  val precFactor = math.max(1.0, precision.toDouble / Environment.DefaultPrecision)
  (math.max(BaseSteps.toDouble, math.ceil(span * StepsPerUnit)) * precFactor)
    .min(MaxSteps.toDouble)
    .max(1.0)
    .toInt

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

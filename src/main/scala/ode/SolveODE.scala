package it.grypho.scala.leonardo
package ode

import core.*


// Classic fourth-order Runge–Kutta integrator for the first-order initial-value
// problem  y' = rhs(t, y),  y(t₀) = y₀.  Returns Some(y(target)) when every stage
// evaluates to a finite Double, else None (unresolvable rhs, or a non-finite
// intermediate → the caller keeps the _ODE node symbolic).
//
// The step count scales with env.precision relative to the default (same idea as
// _DefIntegral's Simpson grid), and the signed step h = (target − t₀)/n makes the
// scheme integrate backwards when target < t₀ without any special-casing.
//
// rhs is evaluated per stage by tree-eval with the independent variable (t) and the
// dependent variable (y) bound in a scoped Environment. eval performs no rounding
// (values propagate at full Double precision — see _Number), so the RK4 accumulation
// is not degraded by the display precision. A compiled two-variable fast path (à la
// scalar.compile) is a possible future optimization; tree-eval keeps this correct and
// uniform over every expression shape (functions, ratios, nested _ODE, …).

private val BaseSteps = 1000

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
    val n = (BaseSteps.toLong * env.precision / Environment.DefaultPrecision)
              .max(1L).min(Int.MaxValue.toLong).toInt
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

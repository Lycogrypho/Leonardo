package it.grypho.scala.leonardo
package ode

import core.*
import scalar.*


/** Closed-form tier for constant-coefficient linear first-order IVPs.
 *
 *  Recognises `y' = a*y + b` where `a` and `b` are free of the independent
 *  variable `t` (they may hold other free parameters).  The three sub-cases are:
 *
 *  - `y' = a*y`      (b = 0): `y(t) = y0 * exp(a * (t - t0))`
 *  - `y' = b`        (a = 0): `y(t) = y0 + b * (t - t0)`
 *  - `y' = a*y + b`  (general): `y(t) = (y0 + b/a) * exp(a * (t - t0)) - b/a`
 *
 *  Returns `Some(expr)` where `expr` is the closed-form solution evaluated at
 *  `target`.  When `target`, `y0`, and `t0` are concrete numbers the result folds
 *  to a `_Number`; when they contain free variables the result stays symbolic.
 *  Returns `None` when the shape is not recognised (caller falls back to RK4).
 *
 *  Linearity in `depVar` is checked via `scalar.collect`: a degree-1 result gives
 *  the pair `(b, a)` directly; degree >= 2 or unrecognised forms (e.g. `sin(y)`)
 *  return `None`.  The constant-coefficient requirement rejects any `a` or `b` that
 *  `scalar.dependsOn` finds to depend on `indepVar`.
 *
 *  @param rhs      the right-hand side of the ODE `y' = rhs`
 *  @param depVar   the dependent variable (`y`)
 *  @param indepVar the independent variable (`t`)
 *  @param t0       the initial time (may be symbolic)
 *  @param y0       the initial value (may be symbolic)
 *  @param target   the evaluation point (may be symbolic)
 *  @param env      the evaluation environment
 *  @return `Some(closed-form expression for y(target))`, or `None` if not recognised
 */
def solveODESymbolic(rhs: _Expression, depVar: _Variable, indepVar: _Variable,
                     t0: _Expression, y0: _Expression, target: _Expression,
                     env: Environment): Option[_Expression] =
  collect(rhs, depVar) match
    case Some(coeffs) if coeffs.length <= 2 =>
      val b = coeffs.headOption.getOrElse(_Number(0))          // constant term
      val a = if coeffs.length == 2 then coeffs(1) else _Number(0)  // coefficient of y
      // Constant-coefficient guard: a, b must not depend on t (dependence on y is
      // already excluded — collect gives a linear-in-y form).
      if dependsOn(a, indepVar) || dependsOn(b, indepVar) then None
      else
        val aZero = simplifyFully(a) == _Number(0.0)
        val bZero = simplifyFully(b) == _Number(0.0)
        val tau   = Sum(target, Product(_Number(-1), t0))       // target - t0
        val sol =
          if bZero then Product(y0, Exp(Product(a, tau)))        // y0 * exp(a * tau)
          else if aZero then Sum(y0, Product(b, tau))            // y0 + b * tau
          else                                                   // (y0 + b/a) * exp(a * tau) - b/a
            val boa = Ratio(b, a)
            Sum(Product(Sum(y0, boa), Exp(Product(a, tau))), Product(_Number(-1), boa))
        Some(simplifyFully(sol))
    case _ => None

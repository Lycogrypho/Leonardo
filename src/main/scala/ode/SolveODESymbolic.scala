package it.grypho.scala.leonardo
package ode

import core.*
import scalar.*


/** Closed-form tier for linear first-order IVPs `y' = a(t)*y + b(t)`.
 *
 *  The right-hand side must be linear in the dependent variable (`collect(rhs, depVar)`
 *  yields at most a degree-1 coefficient vector `[b, a]`); non-linear shapes such as
 *  `sin(y)` or `y^2` return `None` (the caller falls back to RK4).
 *
 *  Two sub-tiers:
 *
 *  1. **Constant coefficients** (`a`, `b` free of the independent variable `t`) — the
 *     exact `tau = target - t0` forms, avoiding any substitution:
 *     - `y' = a*y`      (b = 0): `y = y0 * exp(a*tau)`
 *     - `y' = b`        (a = 0): `y = y0 + b*tau`
 *     - `y' = a*y + b`  (general): `y = (y0 + b/a) * exp(a*tau) - b/a`
 *
 *  2. **Variable coefficients** (`a` or `b` depends on `t`) — the integrating-factor
 *     method.  With `A = integral(a dt)` and `mu = exp(-A)` the equation `(mu*y)' = mu*b`
 *     integrates to `mu*y = Q + C`, `Q = integral(mu*b dt)`; the initial condition fixes
 *     `C = mu(t0)*y0 - Q(t0)`, giving
 *     {{{
 *     y(target) = (Q(target) - Q(t0) + y0*mu(t0)) / mu(target)
 *     }}}
 *     This closes whenever both `integral(a dt)` and `integral(mu*b dt)` reduce to a
 *     closed form (it reuses the full indefinite-integration engine, so e.g. `mu*b = t*e^t`
 *     is handled by integration by parts); if either stays symbolic, this tier returns
 *     `None` and the caller falls back to RK4.
 *
 *  In both tiers the result stays symbolic when `target`, `y0`, `t0`, or a coefficient is
 *  free, and folds to a `_Number` once everything is concrete.
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
      val b = coeffs.headOption.getOrElse(_Number(0))          // constant term  (free of y)
      val a = if coeffs.length == 2 then coeffs(1) else _Number(0)  // coefficient of y
      if dependsOn(a, indepVar) || dependsOn(b, indepVar) then
        // Variable coefficients: integrating-factor method (falls through to None -> RK4
        // when either required integral stays symbolic).
        integratingFactorSolution(a, b, indepVar, t0, y0, target)
      else
        // Constant coefficients: the exact tau-form closed solutions.
        constantCoefficientSolution(a, b, t0, y0, target)
    case _ => None

/** Closed form for the constant-coefficient case `y' = a*y + b` (`a`, `b` free of `t`). */
private def constantCoefficientSolution(a: _Expression, b: _Expression,
                                        t0: _Expression, y0: _Expression,
                                        target: _Expression): Option[_Expression] =
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

/** Closed form for the variable-coefficient case `y' = a(t)*y + b(t)` via the integrating
 *  factor `mu = exp(-integral(a dt))`; `None` when either integral does not close.
 *
 *  @param a        the coefficient of `y` (may depend on `indepVar`, free of `depVar`)
 *  @param b        the forcing term (may depend on `indepVar`, free of `depVar`)
 *  @param indepVar the independent variable `t`
 *  @param t0       the initial time
 *  @param y0       the initial value
 *  @param target   the evaluation point
 *  @return `Some(y(target))` when both integrals close, `None` otherwise
 */
private def integratingFactorSolution(a: _Expression, b: _Expression, indepVar: _Variable,
                                      t0: _Expression, y0: _Expression,
                                      target: _Expression): Option[_Expression] =
  val bigA = integrate(a, indepVar)                        // A(t) = integral(a dt)
  if hasIntegral(bigA) then None
  else
    val mu   = simplifyFully(Exp(Product(_Number(-1), bigA)))   // mu(t) = exp(-A(t))
    val bigQ = integrate(simplifyFully(Product(mu, b)), indepVar)   // Q(t) = integral(mu*b dt)
    if hasIntegral(bigQ) then None
    else
      val iv = indepVar.variable
      def at(e: _Expression, x: _Expression): _Expression = substitute(e, Map(iv -> x))
      // y(target) = (Q(target) - Q(t0) + y0*mu(t0)) / mu(target)
      val numer = Sum(Sum(at(bigQ, target), Product(_Number(-1), at(bigQ, t0))),
                      Product(y0, at(mu, t0)))
      Some(simplifyFully(Ratio(numer, at(mu, target))))

/** True when `e` still contains an unresolved `scalar._Integral` node (an integral that
 *  the integration engine could not reduce to a closed form). */
private def hasIntegral(e: _Expression): Boolean = e match
  case _: _Integral => true
  case _            => e.children.exists(hasIntegral)

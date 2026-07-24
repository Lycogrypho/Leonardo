package it.grypho.scala.leonardo
package scalar

import core.*


/** Symbolic indefinite integration.
 *
 *  [[integrate]] returns an antiderivative of `e` with respect to `v` (the `+ C`
 *  constant is dropped), implemented as a rule table over expression shapes -- the dual
 *  of [[derive]] in `Derive.scala`.  Powers the [[_Integral]] node's `eval`.
 *
 *  When no rule applies, [[integrate]] returns the original `_Integral(e, v)` node
 *  unchanged; `_Integral.eval` treats that fixpoint as "stays symbolic", exactly as
 *  `_Derivative` does when [[derive]] cannot reduce.
 *
 *  Chain-rule coverage is limited to a linear inner argument `u = a*v + b`:
 *  there the substitution `t = u` has constant `dt/dv = a`, so the integral of
 *  `f(u)` w.r.t. `v` equals `F(u)/a`.  Non-linear inner arguments (which would need a
 *  general substitution or parts) are left symbolic.  [[linearSlope]] returns `Some(a)`
 *  exactly when `u` is linear in `v`.
 */

/** Returns the slope `a` when `u` is linear in `v` (i.e. `d(u)/dv` folds to a nonzero
 *  constant); `None` for non-linear or constant `u`.
 *
 *  @param u the expression to test for linearity
 *  @param v the variable
 *  @return `Some(a)` when `u = a*v + b` with `a != 0`, `None` otherwise
 */
private def linearSlope(u: _Expression, v: _Variable): Option[Double] =
  derive(u, v).eval(new Environment()) match
    case Right(_Number(a)) if a != 0.0 => Some(a)
    case _                             => None


/** Returns an antiderivative of `e` with respect to `v`, or `_Integral(e, v)` when
 *  no rule applies.
 *
 *  `_ElementWise` containers (matrices, equations) are integrated element-wise.
 *  Chain-rule support is limited to linear inner arguments; non-linear forms stay
 *  symbolic.
 *
 *  @param e the integrand
 *  @param v the integration variable
 *  @return an antiderivative of `e` (constant of integration omitted),
 *          or `_Integral(e, v)` when no rule fires
 */
def integrate(e: _Expression, v: _Variable): _Expression = e match
  // Element-wise containers (see core._ElementWise): integrate each child. Must
  // precede the constant rule below, which would otherwise wrap a v-independent
  // matrix in the scalar node Product(matrix, v) instead of integrating per element.
  case ew: _ElementWise => ew.rebuild(ew.children.map(integrate(_, v)))

  // integral(c, v) = c*v          (c independent of v)
  case _ if !dependsOn(e, v) => Product(e, v)

  // linearity: integral(a + b, v) = integral(a, v) + integral(b, v)
  case Sum(a, b) => Sum(integrate(a, v), integrate(b, v))

  // constant multiple: integral(c*f, v) = c * integral(f, v)
  case Product(a, b) if !dependsOn(a, v) => Product(a, integrate(b, v))
  case Product(a, b) if !dependsOn(b, v) => Product(b, integrate(a, v))

  // constant denominator: integral(f/c, v) = (integral(f, v))/c
  case Ratio(a, b) if !dependsOn(b, v) => Ratio(integrate(a, v), b)

  // the bare variable: integral(v, v) = v^2/2   (Power rule below only sees v wrapped in Power)
  case x: _Variable if x.variable == v.variable => Ratio(Power(v, _Number(2)), _Number(2))

  // power rule over a linear argument u (slope a):
  //   integral(u^n, v) = u^(n+1) / (a*(n+1))   for n != -1
  //   integral(u^-1, v) = log(u) / a            for n = -1
  case Power(u, _Number(n)) => linearSlope(u, v) match
    case Some(a) if n == -1.0 => Ratio(Ln(u), _Number(a))
    case Some(a)              => Ratio(Power(u, _Number(n + 1)), _Number(a * (n + 1)))
    case None                 => _Integral(e, v)

  // integral(a/(1 + u^2), v) = a*atan(u)/slope   (standard atan primitive; linear-arg chain rule)
  // Both orderings of the denominator sum are matched (1+u^2 and u^2+1).
  case Ratio(a, Sum(_Number(1.0), Power(u, _Number(2.0)))) if !dependsOn(a, v) =>
    linearSlope(u, v) match
      case Some(s) => Product(a, Ratio(Atan(u), _Number(s)))
      case None    => _Integral(e, v)

  case Ratio(a, Sum(Power(u, _Number(2.0)), _Number(1.0))) if !dependsOn(a, v) =>
    linearSlope(u, v) match
      case Some(s) => Product(a, Ratio(Atan(u), _Number(s)))
      case None    => _Integral(e, v)

  // integral(a/sqrt(1 - u^2), v) = a*asin(u)/slope  (standard asin primitive; linear-arg chain rule)
  case Ratio(a, Power(Sum(_Number(1.0), Product(_Number(-1.0), Power(u, _Number(2.0)))), _Number(0.5)))
      if !dependsOn(a, v) =>
    linearSlope(u, v) match
      case Some(s) => Product(a, Ratio(Asin(u), _Number(s)))
      case None    => _Integral(e, v)

  // integral(c/u, v) = c*log(u)/a   (reciprocal written as a Ratio rather than Power(u, -1))
  case Ratio(a, u) if !dependsOn(a, v) => linearSlope(u, v) match
    case Some(s) => Product(a, Ratio(Ln(u), _Number(s)))
    case None    => _Integral(e, v)

  // exponential / trigonometric primitives over a linear argument u (slope a):
  case Exp(u) => linearSlope(u, v) match
    case Some(a) => Ratio(Exp(u), _Number(a))
    case None    => _Integral(e, v)

  case Sin(u) => linearSlope(u, v) match
    case Some(a) => Ratio(Product(_Number(-1), Cos(u)), _Number(a))
    case None    => _Integral(e, v)

  case Cos(u) => linearSlope(u, v) match
    case Some(a) => Ratio(Sin(u), _Number(a))
    case None    => _Integral(e, v)

  // No rule (e.g. products of two v-dependent factors needing parts, tan, inverse
  // trig, non-linear compositions): stay symbolic.
  case _ => _Integral(e, v)

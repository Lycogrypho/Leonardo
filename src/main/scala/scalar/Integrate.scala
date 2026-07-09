package it.grypho.scala.leonardo
package scalar

import core.*


// Symbolic indefinite integration. integrate(e, v) returns an antiderivative of e
// with respect to v (the "+ C" is dropped), implemented as a rule table over
// expression shapes — the dual of derive(e, v) in Derive.scala. Powers the
// _Integral node's eval.
//
// When no rule applies, integrate returns the original _Integral(e, v) node
// unchanged; _Integral.eval treats that fixpoint as "stays symbolic", exactly as
// _Derivative does when derive cannot reduce.
//
// Chain-rule coverage is limited to a LINEAR inner argument u = a*v + b: there the
// substitution t = u has constant dt/dv = a, so ∫ f(u) dv = F(u)/a. Non-linear
// inner arguments (which would need a general substitution or parts) are left
// symbolic. linearSlope(u, v) returns Some(a) exactly when u is linear in v.

// u is linear in v iff d(u)/dv folds to a nonzero constant; that constant is the slope.
private def linearSlope(u: _Expression, v: _Variable): Option[Double] =
  derive(u, v).eval(new Environment()) match
    case Right(_Number(a)) if a != 0.0 => Some(a)
    case _                             => None


def integrate(e: _Expression, v: _Variable): _Expression = e match
  // ∫ c dv = c*v          (c independent of v)
  case _ if !dependsOn(e, v) => Product(e, v)

  // linearity: ∫ (a + b) dv = ∫a dv + ∫b dv
  case Sum(a, b) => Sum(integrate(a, v), integrate(b, v))

  // constant multiple: ∫ c*f dv = c * ∫f dv
  case Product(a, b) if !dependsOn(a, v) => Product(a, integrate(b, v))
  case Product(a, b) if !dependsOn(b, v) => Product(b, integrate(a, v))

  // constant denominator: ∫ f/c dv = (∫f dv)/c
  case Ratio(a, b) if !dependsOn(b, v) => Ratio(integrate(a, v), b)

  // the bare variable: ∫ v dv = v²/2   (Power rule below only sees v wrapped in Power)
  case x: _Variable if x.variable == v.variable => Ratio(Power(v, _Number(2)), _Number(2))

  // power rule over a linear argument u (slope a):
  //   ∫ uⁿ dv = u^(n+1) / (a·(n+1))   for n ≠ -1
  //   ∫ u⁻¹ dv = log(u) / a           for n = -1
  case Power(u, _Number(n)) => linearSlope(u, v) match
    case Some(a) if n == -1.0 => Ratio(Log(u), _Number(a))
    case Some(a)              => Ratio(Power(u, _Number(n + 1)), _Number(a * (n + 1)))
    case None                 => _Integral(e, v)

  // ∫ a/(1 + u²) dv = a·atan(u)/slope   (standard atan primitive; linear-arg chain rule)
  // Both orderings of the denominator sum are matched (1+u² and u²+1).
  case Ratio(a, Sum(_Number(1.0), Power(u, _Number(2.0)))) if !dependsOn(a, v) =>
    linearSlope(u, v) match
      case Some(s) => Product(a, Ratio(Atan(u), _Number(s)))
      case None    => _Integral(e, v)

  case Ratio(a, Sum(Power(u, _Number(2.0)), _Number(1.0))) if !dependsOn(a, v) =>
    linearSlope(u, v) match
      case Some(s) => Product(a, Ratio(Atan(u), _Number(s)))
      case None    => _Integral(e, v)

  // ∫ a/√(1 − u²) dv = a·asin(u)/slope  (standard asin primitive; linear-arg chain rule)
  case Ratio(a, Power(Sum(_Number(1.0), Product(_Number(-1.0), Power(u, _Number(2.0)))), _Number(0.5)))
      if !dependsOn(a, v) =>
    linearSlope(u, v) match
      case Some(s) => Product(a, Ratio(Asin(u), _Number(s)))
      case None    => _Integral(e, v)

  // ∫ c/u dv = c·log(u)/a   (reciprocal written as a Ratio rather than Power(u, -1))
  case Ratio(a, u) if !dependsOn(a, v) => linearSlope(u, v) match
    case Some(s) => Product(a, Ratio(Log(u), _Number(s)))
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

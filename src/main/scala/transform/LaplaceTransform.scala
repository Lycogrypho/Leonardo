package it.grypho.scala.leonardo
package transform

import core.*
import scalar.*


// Laplace transform rule table.
// laplaceOf(e, t, s) computes L{e} treating t as the time variable and s as the
// complex frequency variable. Returns _Laplace(e, t, s) unchanged when no rule applies
// (fixpoint / stay-symbolic convention, matching Integrate and Derive).
//
// Rules implemented:
//   Constant:    L{c}              = c/s                    (any expr free of t)
//   Linearity:   L{a + b}          = L{a} + L{b}
//                L{c * f}          = c * L{f}               (c free of t)
//   Power:       L{t^n}            = n! / s^{n+1}           (n a non-negative integer ≤ 20)
//   Exponential: L{e^{ct}}         = 1 / (s - c)
//   Sine:        L{sin(wt)}        = w / (s^2 + w^2)
//   Cosine:      L{cos(wt)}        = s / (s^2 + w^2)
//   First shift: L{e^{at}g(t)}     = G(s-a)                (G = L{g}; recursive)
//   Step:        L{u(t)}           = 1/s
//   2nd shift:   L{u(t-a)}         = e^{-as}/s             (a > 0)
//                L{u(t-a)·g(t-a)}  = e^{-as}·G(s)          (a > 0; full second-shift)
//
// The power rule is capped at n ≤ 20 (matching Expand.scala and Normalize.scala).
// Larger exponents stay symbolic: n! overflows Double above n=170 and the recursive
// implementation overflows the stack for large n.

private val MaxLaplacePowerN = 20

// Iterative factorial — avoids stack overflow and is safe for n up to MaxLaplacePowerN.
private def factorial(n: Int): Double =
  (2 to n).foldLeft(1.0)(_ * _)

// Returns Some(c) when ex = c * t (or t alone → c = 1). c may be a symbolic expression.
private def coeffOfT(ex: _Expression, tv: String): Option[_Expression] = ex match
  case vv: _Variable if vv.variable == tv                 => Some(_Number(1))
  case Product(c, vv: _Variable) if vv.variable == tv     => Some(c)
  case Product(vv: _Variable, c) if vv.variable == tv     => Some(c)
  case _                                                   => None

// Returns Some(a) for a positive numeric a when inner matches tv - a.
// Handles both the parser-native form Sum(tv, Product(-1, a)) and the folded Sum(tv, _Number(-a)).
private def shiftOf(inner: _Expression, tv: String): Option[Double] = inner match
  case Sum(vv: _Variable, _Number(a)) if vv.variable == tv && a < 0                          => Some(-a)
  case Sum(vv: _Variable, Product(_Number(c), _Number(a))) if vv.variable == tv && c * a < 0 => Some(-(c * a))
  case _                                                                                       => None

def laplaceOf(e: _Expression, t: _Variable, s: _Variable): _Expression =
  laplaceImpl(e, t, s, t.variable)

private def laplaceImpl(e: _Expression, t: _Variable, s: _Variable, tv: String): _Expression =

  // First-shift theorem: L{e^{at}·g(t)} = G(s−a) where G(s) = L{g(t)}.
  // Nested here to capture t, s, tv, e from the enclosing scope; shared by both
  // Product orderings (Exp·g and g·Exp) to eliminate the duplicated body.
  def firstShift(inner: _Expression, g: _Expression): _Expression =
    coeffOfT(inner, tv) match
      case Some(a) =>
        val G = laplaceImpl(g, t, s, tv)
        if G.isInstanceOf[_Laplace] then _Laplace(e, t, s)
        else substitute(G, Map(s.variable -> Sum(s, Product(_Number(-1), a))))
      case None => _Laplace(e, t, s)

  // Full second-shift: L{u(t-a)·g(t-a)} = e^{-as}·G(s). Substitutes t→t+a in g,
  // simplifies the result (so that e.g. sin(t+a-a) reduces to sin(t) before matching),
  // computes G = L{g(t+a)}, and multiplies by e^{-as}. Stays symbolic if G stays symbolic.
  def secondShift(inner: _Expression, g: _Expression): _Expression =
    shiftOf(inner, tv) match
      case Some(a) =>
        val gShifted = simplifyFully(substitute(g, Map(tv -> Sum(t, _Number(a)))))
        val G        = laplaceImpl(gShifted, t, s, tv)
        if G.isInstanceOf[_Laplace] then _Laplace(e, t, s)
        else Product(Exp(Product(_Number(-a), s)), G)
      case None => _Laplace(e, t, s)

  e match

    // Constant (free of t): L{c} = c/s
    case _ if !dependsOn(e, t) => Ratio(e, s)

    // Linearity: L{a + b} = L{a} + L{b}
    case Sum(a, b) =>
      Sum(laplaceImpl(a, t, s, tv), laplaceImpl(b, t, s, tv))

    // Linearity: L{c * f} = c * L{f} when c is free of t (both orderings)
    case Product(c, f) if !dependsOn(c, t) =>
      Product(c, laplaceImpl(f, t, s, tv))
    case Product(f, c) if !dependsOn(c, t) =>
      Product(c, laplaceImpl(f, t, s, tv))

    // L{t} = 1/s^2
    case vv: _Variable if vv.variable == tv =>
      Ratio(_Number(1), Power(s, _Number(2)))

    // L{t^n} = n! / s^{n+1} for non-negative integer n ≤ MaxLaplacePowerN
    case Power(vv: _Variable, _Number(n))
        if vv.variable == tv && n.toInt.toDouble == n && n >= 0 && n <= MaxLaplacePowerN =>
      Ratio(_Number(factorial(n.toInt)), Power(s, _Number(n + 1)))

    // L{e^{ct}} = 1 / (s - c)
    case Exp(inner) => coeffOfT(inner, tv) match
      case Some(c) => Ratio(_Number(1), Sum(s, Product(_Number(-1), c)))
      case None    => _Laplace(e, t, s)

    // L{sin(wt)} = w / (s^2 + w^2)
    case Sin(inner) => coeffOfT(inner, tv) match
      case Some(c) => Ratio(c, Sum(Power(s, _Number(2)), Power(c, _Number(2))))
      case None    => _Laplace(e, t, s)

    // L{cos(wt)} = s / (s^2 + w^2)
    case Cos(inner) => coeffOfT(inner, tv) match
      case Some(c) => Ratio(s, Sum(Power(s, _Number(2)), Power(c, _Number(2))))
      case None    => _Laplace(e, t, s)

    // First-shift theorem — both Product orderings delegate to the helper above.
    case Product(Exp(inner), g) => firstShift(inner, g)
    case Product(g, Exp(inner)) => firstShift(inner, g)

    // L{u(t)} = 1/s  (unit step at origin)
    case _Heaviside(vv: _Variable) if vv.variable == tv => Ratio(_Number(1), s)

    // L{u(t-a)} = e^{-as}/s  (second-shift: shifted unit step, a > 0)
    case _Heaviside(inner) => shiftOf(inner, tv) match
      case Some(a) => Ratio(Exp(Product(_Number(-a), s)), s)
      case None    => _Laplace(e, t, s)

    // L{u(t-a)·g(t-a)} = e^{-as}·G(s)  (full second-shift theorem, both Product orderings)
    case Product(_Heaviside(inner), g) => secondShift(inner, g)
    case Product(g, _Heaviside(inner)) => secondShift(inner, g)

    case _ => _Laplace(e, t, s)

package it.grypho.scala.leonardo
package transform

import core.*
import scalar.*


/** [[https://en.wikipedia.org/wiki/Laplace_transform Laplace transform]] rule table.
 *
 *  [[laplaceOf]] computes `L{e}` treating `t` as the time variable and `s` as the
 *  complex frequency variable.  Returns [[_Laplace]]`(e, t, s)` unchanged when no
 *  rule applies (fixpoint / stay-symbolic convention, matching `integrate` and `derive`).
 *
 *  Rules implemented:
 *  - **Constant**           `L{c}          = c/s`              (any expr free of `t`)
 *  - **Linearity**          `L{a + b}      = L{a} + L{b}`
 *                           `L{c * f}      = c * L{f}`         (`c` free of `t`)
 *  - **Power**              `L{t^n}        = n! / s^(n+1)`     (integer `n` in `[0, MaxLaplacePowerN]`)
 *  - **Exponential**        `L{exp(c*t)}   = 1 / (s - c)`
 *  - **Sine**               `L{sin(w*t)}   = w / (s^2 + w^2)`
 *  - **Cosine**             `L{cos(w*t)}   = s / (s^2 + w^2)`
 *  - **First-shift**        `L{exp(a*t)*g} = G(s-a)`           (`G = L{g}`; recursive)
 *  - **Unit step**          `L{u(t)}       = 1/s`
 *  - **Second-shift**       `L{u(t-a)}     = exp(-a*s)/s`      (`a > 0`)
 *                           `L{u(t-a)*g(t-a)} = exp(-a*s)*G(s)`
 *  - **Deriv-of-transform** `L{t^n * g(t)} = (-1)^n * d^n G/ds^n`  (`G = L{g}`; `n <= MaxLaplacePowerN`)
 */

/** Maximum power `n` accepted by the power rule and derivative-of-transform rule.  Fixed, not a
 *  `core.Limit` (F_0124 phase C): `laplaceOf` is a pure rule table with no environment, and the
 *  cap matches `expand`'s and the series' default orders. */
private val MaxLaplacePowerN = 20

/** `n!` in the input's tier: an exact `BigInt` in exact mode, the `Double` it always was otherwise
 *  (F_0210).  Safe for `n` up to `MaxLaplacePowerN`. */
private def factorial(n: Int)(using lit: Int => _Value): _Value =
  lit(1) match
    case _: _Rational => _Rational.of((2 to n).foldLeft(BigInt(1))(_ * _), BigInt(1)).getOrElse(_Number((2 to n).foldLeft(1.0)(_ * _)))
    case _            => _Number((2 to n).foldLeft(1.0)(_ * _))

/** An integer constant in the tier of the whole transformed expression (F_0210): the tables have
 *  no environment, so the tier is read from content, the `literalLike` rule. */
private[transform] def tierOf(whole: _Expression): Int => _Value = n => _Rational.literalLike(n, whole)

/** Returns `Some(c)` when `ex = c * t`, `t/d` or `c*t/d` (or `t` alone -> `c = 1`); `None` otherwise.
 *  The divisor forms are F_0210's: `exp(t/3)` and `sin(t/2)` stayed unevaluated. */
private def coeffOfT(ex: _Expression, tv: String)(using lit: Int => _Value): Option[_Expression] = ex match
  case vv: _Variable if vv.variable == tv                 => Some(lit(1))
  case Product(c, vv: _Variable) if vv.variable == tv     => Some(c)
  case Product(vv: _Variable, c) if vv.variable == tv     => Some(c)
  case Ratio(inner, d) if !d.freeVars.contains(tv)        => coeffOfT(inner, tv).map(c => Ratio(c, d))
  case _                                                   => None

/** Returns `Some(a)` when `inner` matches `tv - a` for a positive numeric `a`.
 *  Handles both the parser-native form `Sum(tv, Product(-1, a))` and the folded
 *  `Sum(tv, _Number(-a))`.
 */
private def shiftOf(inner: _Expression, tv: String): Option[Double] = inner match
  case Sum(vv: _Variable, _Number(a)) if vv.variable == tv && a < 0                          => Some(-a)
  case Sum(vv: _Variable, Product(_Number(c), _Number(a))) if vv.variable == tv && c * a < 0 => Some(-(c * a))
  case _                                                                                       => None

/** Returns `Some((n, g))` when `e = t^n * g` or `g * t^n` for positive integer `n <= MaxLaplacePowerN`.
 *  Bare `t^n` (no other factor) is excluded -- it is covered by the dedicated power rule.
 */
private def tPowerFactor(e: _Expression, tv: String): Option[(Int, _Expression)] = e match
  case Product(vv: _Variable, g) if vv.variable == tv                                         => Some((1, g))
  case Product(g, vv: _Variable) if vv.variable == tv                                         => Some((1, g))
  case Product(Power(vv: _Variable, _Number(n)), g)
      if vv.variable == tv && n.toInt.toDouble == n && n >= 1 && n <= MaxLaplacePowerN        => Some((n.toInt, g))
  case Product(g, Power(vv: _Variable, _Number(n)))
      if vv.variable == tv && n.toInt.toDouble == n && n >= 1 && n <= MaxLaplacePowerN        => Some((n.toInt, g))
  case _                                                                                       => None

/** Computes `L{e}` with time variable `t` and Laplace variable `s`.
 *
 *  Delegates to `laplaceImpl` which carries the string name of `t` for fast pattern
 *  matching without re-boxing.  Returns [[_Laplace]]`(e, t, s)` when no rule fires.
 *
 *  @param e the time-domain expression
 *  @param t the time variable
 *  @param s the Laplace frequency variable
 *  @return the Laplace transform of `e`, or [[_Laplace]]`(e, t, s)` if no rule applies
 */
def laplaceOf(e: _Expression, t: _Variable, s: _Variable): _Expression =
  laplaceImpl(e, t, s, t.variable)(using tierOf(e))

/** Recursive implementation of the rule table; carries `tv = t.variable` to avoid re-boxing, and
 *  the whole input's tier for every constant a rule invents (F_0210). */
private def laplaceImpl(e: _Expression, t: _Variable, s: _Variable, tv: String)(using lit: Int => _Value): _Expression =

  // First-shift theorem: L{exp(a*t)*g(t)} = G(s-a) where G(s) = L{g(t)}.
  // Nested here to capture t, s, tv, e from the enclosing scope; shared by both
  // Product orderings (Exp*g and g*Exp) to eliminate the duplicated body.
  def firstShift(inner: _Expression, g: _Expression): _Expression =
    coeffOfT(inner, tv) match
      case Some(a) =>
        val G = laplaceImpl(g, t, s, tv)
        if G.isInstanceOf[_Laplace] then _Laplace(e, t, s)
        else substitute(G, Map(s.variable -> Sum(s, Product(lit(-1), a))))
      case None => _Laplace(e, t, s)

  // Full second-shift: L{u(t-a)*g(t-a)} = exp(-a*s)*G(s). Substitutes t->t+a in g,
  // simplifies the result, computes G = L{g(t+a)}, and multiplies by exp(-a*s).
  // Stays symbolic if G stays symbolic.
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
    // and over a constant divisor (F_0210): `laplace(t/3, t, s)` stayed unevaluated
    case Ratio(f, c) if !dependsOn(c, t) =>
      laplaceImpl(f, t, s, tv) match
        case _: _Laplace => _Laplace(e, t, s)
        case transformed => Ratio(transformed, c)

    // L{t} = 1/s^2
    case vv: _Variable if vv.variable == tv =>
      Ratio(lit(1), Power(s, lit(2)))

    // L{t^n} = n! / s^(n+1) for non-negative integer n <= MaxLaplacePowerN
    case Power(vv: _Variable, _Number(n))
        if vv.variable == tv && n.toInt.toDouble == n && n >= 0 && n <= MaxLaplacePowerN =>
      Ratio(factorial(n.toInt), Power(s, lit(n.toInt + 1)))

    // L{exp(c*t)} = 1 / (s - c)
    case Exp(inner) => coeffOfT(inner, tv) match
      case Some(c) => Ratio(lit(1), Sum(s, Product(lit(-1), c)))
      case None    => _Laplace(e, t, s)

    // L{sin(w*t)} = w / (s^2 + w^2)
    case Sin(inner) => coeffOfT(inner, tv) match
      case Some(c) => Ratio(c, Sum(Power(s, lit(2)), Power(c, lit(2))))
      case None    => _Laplace(e, t, s)

    // L{cos(w*t)} = s / (s^2 + w^2)
    case Cos(inner) => coeffOfT(inner, tv) match
      case Some(c) => Ratio(s, Sum(Power(s, lit(2)), Power(c, lit(2))))
      case None    => _Laplace(e, t, s)

    // First-shift theorem -- both Product orderings delegate to the helper above.
    case Product(Exp(inner), g) => firstShift(inner, g)
    case Product(g, Exp(inner)) => firstShift(inner, g)

    // L{u(t)} = 1/s  (unit step at origin)
    case _Heaviside(vv: _Variable) if vv.variable == tv => Ratio(lit(1), s)

    // L{u(t-a)} = exp(-a*s)/s  (second-shift: shifted unit step, a > 0)
    case _Heaviside(inner) => shiftOf(inner, tv) match
      case Some(a) => Ratio(Exp(Product(_Number(-a), s)), s)
      case None    => _Laplace(e, t, s)

    // L{u(t-a)*g(t-a)} = exp(-a*s)*G(s)  (full second-shift theorem, both Product orderings)
    case Product(_Heaviside(inner), g) => secondShift(inner, g)
    case Product(g, _Heaviside(inner)) => secondShift(inner, g)

    // Derivative-of-transform: L{t^n * g(t)} = (-1)^n * d^n G(s)/ds^n  where G = L{g(t)}.
    // Applies to Product nodes not already dispatched (constant-multiple / first-shift / step).
    // Stays symbolic when G cannot be resolved.
    case other =>
      tPowerFactor(other, tv) match
        case Some((n, g)) =>
          val G = laplaceImpl(g, t, s, tv)
          if G.isInstanceOf[_Laplace] then _Laplace(e, t, s)
          else
            val dG = simplifyFully(deriveN(G, s, n))
            if n % 2 == 0 then dG else Product(lit(-1), dG)
        case None => _Laplace(e, t, s)

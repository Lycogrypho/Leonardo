package it.grypho.scala.leonardo
package scalar

import core.*


/** Symbolic differentiation.
 *
 *  [[derive]] returns d(`e`)/d(`v`) as a new expression, implemented as a rule table
 *  over expression shapes (the dual of the integration table in `Integrate.scala`).
 *  Powers the [[_Derivative]] node's `eval`.
 *
 *  Results are memoised (pure in `(e, v)`): the definite-integral tree-eval fallback
 *  re-derives its integrand at every Simpson sample point, and shared sub-trees across
 *  all callers hit the same cache instead of re-walking the rule table.
 */

/** The tier every constant `derive` and `simplify` invent is built in (F_0071, F_0073).
 *
 *  Decided ONCE, from the whole expression being differentiated, and threaded through the
 *  recursion: a subterm such as `x` carries no tier of its own, so a per-node test could not
 *  tell that `x` in `x^2 + 1/3` belongs to an exact computation.  An expression with no exact
 *  value gets the `Double` constants it always had, so that path is byte-identical; an exact
 *  one gets rationals, and float contagion still demotes them wherever they meet a `Double`.
 *
 *  An IRRATIONAL constant (`2/√π` in `erf`, `π` in the Fresnel rules) stays a `Double`: the
 *  exact tier approximates irrationals at the working precision, which lives in an
 *  `Environment` that `derive` does not have.
 */
private[scalar] final class Tier(val exact: Boolean):
  /** The integer `k` in this tier. */
  def n(k: Int): _Value = if exact then _Rational(k) else _Number(k)
  /** One half, the exponent of a square root. */
  def half: _Value =
    if exact then _Rational(1).divide(_Rational(2)).getOrElse(_Number(0.5)) else _Number(0.5)

/** Compact helpers for constructing derivative results without noise terms.
 *  Without these, terms like `0*x` or `1*f` would remain symbolic and block numeric eval.
 *  The zero they return is invented, so it is built in the tier: `_Number(0.0)` is a
 *  WIDENING pattern and matches an exact zero too, which a hard-coded `_Number(0)` would demote.
 */
private def dmul(a: _Expression, b: _Expression)(using t: Tier): _Expression = (a, b) match
  case (_Number(0.0), _) | (_, _Number(0.0)) => t.n(0)
  case (_Number(1.0), x)                      => x
  case (x,            _Number(1.0))           => x
  case _                                      => Product(a, b)

private def dadd(a: _Expression, b: _Expression): _Expression = (a, b) match
  case (_Number(0.0), x) => x
  case (x, _Number(0.0)) => x
  case _                 => Sum(a, b)

/** Avoids `Power(a, 0) = 1` and `Power(a, 1) = a` blowing up when `a` is `0`. */
private def dpow(a: _Expression, b: _Expression)(using t: Tier): _Expression = b match
  case _Number(0.0) => t.n(1)
  case _Number(1.0) => a
  case _            => Power(a, b)


/** Keyed by the tier too: the same subterm differentiates to different constants inside an
 *  exact and an inexact expression, and a cache keyed on `(e, v)` alone would hand one the
 *  other's answer. */
private val deriveMemo = new Memo[(_Expression, String, Boolean), _Expression](10000)

/** Returns the symbolic derivative of `e` with respect to `v`.
 *
 *  The result is memoised: calling `derive(e, v)` multiple times with the same
 *  arguments returns the cached result without re-walking the rule table.
 *
 *  @param e the expression to differentiate
 *  @param v the differentiation variable
 *  @return d(`e`)/d(`v`) as a new expression (never [[_Derivative]] for rules that fire)
 */
def derive(e: _Expression, v: _Variable): _Expression =
  deriveIn(e, v)(using Tier(_Rational.containsExact(e)))

/** [[derive]] within a tier already decided by the outermost call. */
private def deriveIn(e: _Expression, v: _Variable)(using t: Tier): _Expression =
  deriveMemo.getOrElseUpdate((e, v.variable, t.exact))(deriveImpl(e, v))

/** Returns the n-th derivative of `e` with respect to `v`.
 *
 *  `n = 0` returns `e` unchanged; `n < 0` is rejected.
 *
 *  @param e the expression to differentiate
 *  @param v the differentiation variable
 *  @param n the derivative order (non-negative)
 *  @return d^n(`e`)/d(`v`)^n
 */
def deriveN(e: _Expression, v: _Variable, n: Int): _Expression =
  require(n >= 0, s"derivative order must be non-negative, got $n")
  (1 to n).foldLeft(e)((acc, _) => derive(acc, v))

/** Returns the mixed or higher-order derivative of `e` obtained by differentiating
 *  left-to-right across `v1`, `v2`, and `rest`.
 *
 *  Requires at least two variables so the signature is unambiguous with the
 *  single-variable overload.  Examples: `derive(f, x, x)` = d²f/dx²;
 *  `derive(f, x, y)` = ∂/∂y(∂f/∂x).
 *
 *  @param e    the expression to differentiate
 *  @param v1   first variable in the differentiation sequence
 *  @param v2   second variable in the differentiation sequence
 *  @param rest additional variables, applied left-to-right
 *  @return the result of differentiating with respect to `v1`, then `v2`, then `rest`
 */
def derive(e: _Expression, v1: _Variable, v2: _Variable, rest: _Variable*): _Expression =
  (v1 +: v2 +: rest).foldLeft(e)((acc, v) => derive(acc, v))

private def deriveImpl(e: _Expression, v: _Variable)(using t: Tier): _Expression = e match
  case _Number(_)           => t.n(0)
  case _: _Complex          => t.n(0)    // a complex constant, like any number
  case x: _Variable         => if x.variable == v.variable then t.n(1) else t.n(0)
  case Sum(a, b)            => dadd(deriveIn(a, v), deriveIn(b, v))
  case Product(a, b)        => dadd(dmul(deriveIn(a, v), b), dmul(a, deriveIn(b, v)))
  case Ratio(a, b)          => Ratio(
                                 dadd(dmul(deriveIn(a, v), b), dmul(t.n(-1), dmul(a, deriveIn(b, v)))),
                                 Product(b, b)
                               )
  // Literal-exponent power rule: b * a^(b-1) * a'  — avoids b/a singularity at a=0.
  // The EXACT arm comes first: `_Number(n)` is a widening extractor, so on its own it would
  // read an exact exponent as a Double and rebuild `n` and `n - 1` inexact (F_0071).
  case Power(a, r: _Rational) => dmul(dmul(r, dpow(a, r.add(_Rational(-1)))), deriveIn(a, v))
  case Power(a, _Number(n)) => dmul(dmul(_Number(n), dpow(a, _Number(n - 1))), deriveIn(a, v))
  // Guard: base 0 with a symbolic exponent. The general rule below would produce
  // log(0) in the derivative tree. When the exponent is v-independent, 0^b is a
  // constant → derivative is 0. When the exponent depends on v the derivative is
  // mathematically undefined (sign of b unknown); stay symbolic rather than emit log(0).
  case Power(_Number(0.0), b) =>
    if !dependsOn(b, v) then t.n(0) else _Derivative(e, v)
  // General power rule: a^b * (b' * ln(a) + b * a' / a)
  case Power(a, b)          => dmul(
                                 Power(a, b),
                                 dadd(dmul(deriveIn(b, v), Ln(a)), Ratio(dmul(b, deriveIn(a, v)), a))
                               )
  case Exp(a)               => dmul(Exp(a), deriveIn(a, v))
  case Ln(a)                => Ratio(deriveIn(a, v), a)
  // d/dx log_b(f(x)) = f'(x) / (f(x) * ln(b))   when b is constant w.r.t. x
  // general case: reduce to the ratio rule on ln(f)/ln(b)
  case LogBase(a, b) if !dependsOn(b, v) =>
                               Ratio(deriveIn(a, v), Product(a, Ln(b)))
  case LogBase(a, b)        => deriveIn(Ratio(Ln(a), Ln(b)), v)
  case Sin(a)               => dmul(Cos(a), deriveIn(a, v))
  case Cos(a)               => dmul(t.n(-1), dmul(Sin(a), deriveIn(a, v)))
  case Tg(a)                => Ratio(deriveIn(a, v), Product(Cos(a), Cos(a)))
  // asin'(u) =  u' / sqrt(1 - u²)
  case Asin(a)              => dmul(Ratio(t.n(1), Power(Sum(t.n(1), dmul(t.n(-1), Power(a, t.n(2)))), t.half)), deriveIn(a, v))
  // acos'(u) = -u' / sqrt(1 - u²)
  case Acos(a)              => dmul(dmul(t.n(-1), Ratio(t.n(1), Power(Sum(t.n(1), dmul(t.n(-1), Power(a, t.n(2)))), t.half))), deriveIn(a, v))
  // atan'(u) =  u' / (1 + u²)
  case Atan(a)              => dmul(Ratio(t.n(1), Sum(t.n(1), Power(a, t.n(2)))), deriveIn(a, v))
  // hyperbolic and reciprocal-trigonometric derivatives
  case Sinh(a)              => dmul(Cosh(a), deriveIn(a, v))
  case Cosh(a)              => dmul(Sinh(a), deriveIn(a, v))
  case Tanh(a)              => dmul(Power(Sech(a), t.n(2)), deriveIn(a, v))
  // asinh'(u) = u' / sqrt(u² + 1)
  case Asinh(a)             => dmul(Ratio(t.n(1), Power(Sum(Power(a, t.n(2)), t.n(1)), t.half)), deriveIn(a, v))
  // acosh'(u) = u' / sqrt(u² − 1)
  case Acosh(a)             => dmul(Ratio(t.n(1), Power(Sum(Power(a, t.n(2)), t.n(-1)), t.half)), deriveIn(a, v))
  // atanh'(u) = u' / (1 − u²)
  case Atanh(a)             => dmul(Ratio(t.n(1), Sum(t.n(1), dmul(t.n(-1), Power(a, t.n(2))))), deriveIn(a, v))
  // sec'(u) = sec(u)·tan(u)·u' ;  csc'(u) = −csc(u)·cot(u)·u' ;  cot'(u) = −csc²(u)·u'
  case Sec(a)               => dmul(Product(Sec(a), Tg(a)), deriveIn(a, v))
  case Csc(a)               => dmul(dmul(t.n(-1), Product(Csc(a), Cot(a))), deriveIn(a, v))
  case Cot(a)               => dmul(dmul(t.n(-1), Power(Csc(a), t.n(2))), deriveIn(a, v))
  // sech'(u) = −sech(u)·tanh(u)·u' ;  csch'(u) = −csch(u)·coth(u)·u' ;  coth'(u) = −csch²(u)·u'
  // abs'(u) = u·u'/abs(u) — the sign of u times u', wherever abs is differentiable.  At
  // u = 0 the form evaluates to 0/0, which stays symbolic under Ratio's own rule: the one
  // undifferentiable point refuses itself, so no gate is needed — and the abs(u) DENOMINATOR
  // is what hands `differentiableDomainOf` its NonZero constraint through the existing
  // Ratio arm, with no abs-specific rule in Domain.scala (issue F_0036).
  case Abs(a)               => Ratio(dmul(a, deriveIn(a, v)), Abs(a))
  // F_0056: the join through the step, the FIRST argument favoured at a tie — a stated
  // subgradient choice.  For maximum the step is on a − b, for minimum on b − a; either way it
  // is 1 exactly when `a` is chosen, ties included, so the slope there is a' rather than the
  // a' + b' that `step(a−b)·a' + step(b−a)·b'` would give (step(0) = 1 on both sides).  An
  // n-ary join is the left fold of the binary one, so the leftmost argument wins a tie.
  case _Join(k, List(a, b))  =>
    val s = _Heaviside(k match
      case Extreme.Max => Sum(a, dmul(t.n(-1), b))
      case Extreme.Min => Sum(b, dmul(t.n(-1), a)))
    dadd(dmul(s, deriveIn(a, v)), dmul(Sum(t.n(1), dmul(t.n(-1), s)), deriveIn(b, v)))
  case _Join(k, args) if args.size > 2 => deriveIn(_Join(k, List(_Join(k, args.init), args.last)), v)
  case _Clamp(x, lo, hi)    => deriveIn(_Join(Extreme.Min, List(_Join(Extreme.Max, List(x, lo)), hi)), v)
  // softplus'(u) = u' / (1 + e^(−k·u)) for a constant k — the logistic form, which cannot
  // overflow to ∞/∞ the way the quotient of the definition's derivative does.
  case _Softplus(u, k) if !dependsOn(k, v) =>
    Ratio(deriveIn(u, v), Sum(t.n(1), Exp(dmul(t.n(-1), Product(k, u)))))
  case _Softplus(u, k)      => deriveIn(Ratio(Ln(Sum(t.n(1), Exp(Product(k, u)))), k), v)
  case Sech(a)              => dmul(dmul(t.n(-1), Product(Sech(a), Tanh(a))), deriveIn(a, v))
  case Csch(a)              => dmul(dmul(t.n(-1), Product(Csch(a), Coth(a))), deriveIn(a, v))
  case Coth(a)              => dmul(dmul(t.n(-1), Power(Csch(a), t.n(2))), deriveIn(a, v))
  // With digamma available, the gamma family is differentiable.  Before it,
  // Gamma and fact had no rule at all and fell through to a bare _Derivative wrapper.
  //   d/dx Gamma(u) = Gamma(u)*psi(u)*u'   and   d/dx u! = Gamma(u+1)*psi(u+1)*u'
  //   d/dx lgamma(u) = psi(u)*u'           -- the reason lgamma is the tidier one to use
  case Gamma(a)             => dmul(Product(Gamma(a), Digamma(a)), deriveIn(a, v))
  case LogGamma(a)          => dmul(Digamma(a), deriveIn(a, v))
  case Factorial(a)         =>
    val ap = Sum(a, t.n(1))
    dmul(Product(Gamma(ap), Digamma(ap)), deriveIn(a, v))
  // NOTE there is deliberately no rule for `Digamma` itself: its derivative is the
  // trigamma function, which the library does not implement.  It falls through to the generic
  // `_Derivative` wrapper, which is the same "no rule" behaviour Gamma had until now.
  // erf'(u) = 2/sqrt(pi) * e^(-u^2) * u'
  case Erf(a)               =>
    dmul(Product(_Number(2.0 / math.sqrt(math.Pi)),
                 Exp(dmul(t.n(-1), Power(a, t.n(2))))), deriveIn(a, v))
  case Erfc(a)              => dmul(t.n(-1), deriveIn(Erf(a), v))
  // The special integral functions ARE derivatives by definition:
  //   Si'(u) = sin(u)/u·u',  Ci'(u) = cos(u)/u·u',  Ei'(u) = e^u/u·u',  li'(u) = u'/ln(u),
  //   fresnelS'(u) = sin(π u²/2)·u',  fresnelC'(u) = cos(π u²/2)·u'
  case Si(a)                => dmul(Ratio(Sin(a), a), deriveIn(a, v))
  case Ci(a)                => dmul(Ratio(Cos(a), a), deriveIn(a, v))
  case Ei(a)                => dmul(Ratio(Exp(a), a), deriveIn(a, v))
  case Li(a)                => dmul(Ratio(t.n(1), Ln(a)), deriveIn(a, v))
  case FresnelS(a)          =>
    dmul(Sin(Ratio(Product(_Number(math.Pi), Power(a, t.n(2))), t.n(2))), deriveIn(a, v))
  case FresnelC(a)          =>
    dmul(Cos(Ratio(Product(_Number(math.Pi), Power(a, t.n(2))), t.n(2))), deriveIn(a, v))
  // Functional nodes must be reduced here, not left to fall through to a bare
  // _Derivative wrapper: that wrapper's eval calls derive again on the same node,
  // looping forever (StackOverflow).
  //   - _Derivative: differentiate again → higher-order derivative.
  //   - _Integral:   fundamental theorem, d/dx ∫f dx = f, when the variables match.
  //   - _DefIntegral: a definite integral is a constant except through its limits,
  //     which the engine does not track symbolically, so leave it symbolic.
  // Element-wise containers (matrix literals, matrix sums, transpose — see
  // core._ElementWise): differentiation distributes over the children.
  case ew: _ElementWise         => ew.rebuild(ew.children.map(deriveIn(_, v)))
  case _Derivative(inner, iv)   => deriveIn(deriveIn(inner, iv), v)
  case _Integral(inner, iv)     => if iv.variable == v.variable then inner
                                   else _Derivative(e, v)
  case _DefIntegral(_, _, _, _) => _Derivative(e, v)
  // A limit is treated as opaque under differentiation (swapping d/dx and lim is
  // valid under continuity, but the engine does not verify that condition, so we
  // stay symbolic to be conservative).
  case _: _Limit                => _Derivative(e, v)
  case other                    => _Derivative(other, v)

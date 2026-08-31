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

/** Compact helpers for constructing derivative results without noise terms.
 *  Without these, terms like `0*x` or `1*f` would remain symbolic and block numeric eval.
 */
private def dmul(a: _Expression, b: _Expression): _Expression = (a, b) match
  case (_Number(0.0), _) | (_, _Number(0.0)) => _Number(0)
  case (_Number(1.0), x)                      => x
  case (x,            _Number(1.0))           => x
  case _                                      => Product(a, b)

private def dadd(a: _Expression, b: _Expression): _Expression = (a, b) match
  case (_Number(0.0), x) => x
  case (x, _Number(0.0)) => x
  case _                 => Sum(a, b)

/** Avoids `Power(a, 0) = 1` and `Power(a, 1) = a` blowing up when `a` is `0`. */
private def dpow(a: _Expression, b: _Expression): _Expression = b match
  case _Number(0.0) => _Number(1)
  case _Number(1.0) => a
  case _            => Power(a, b)


private val deriveMemo = new Memo[(_Expression, String), _Expression](10000)

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
  deriveMemo.getOrElseUpdate((e, v.variable))(deriveImpl(e, v))

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

private def deriveImpl(e: _Expression, v: _Variable): _Expression = e match
  case _Number(_)           => _Number(0)
  case _: _Complex          => _Number(0)    // a complex constant, like any number
  case x: _Variable         => if x.variable == v.variable then _Number(1) else _Number(0)
  case Sum(a, b)            => dadd(derive(a, v), derive(b, v))
  case Product(a, b)        => dadd(dmul(derive(a, v), b), dmul(a, derive(b, v)))
  case Ratio(a, b)          => Ratio(
                                 dadd(dmul(derive(a, v), b), dmul(_Number(-1), dmul(a, derive(b, v)))),
                                 Product(b, b)
                               )
  // Literal-exponent power rule: b * a^(b-1) * a'  — avoids b/a singularity at a=0
  case Power(a, _Number(n)) => dmul(dmul(_Number(n), dpow(a, _Number(n - 1))), derive(a, v))
  // Guard: base 0 with a symbolic exponent. The general rule below would produce
  // log(0) in the derivative tree. When the exponent is v-independent, 0^b is a
  // constant → derivative is 0. When the exponent depends on v the derivative is
  // mathematically undefined (sign of b unknown); stay symbolic rather than emit log(0).
  case Power(_Number(0.0), b) =>
    if !dependsOn(b, v) then _Number(0) else _Derivative(e, v)
  // General power rule: a^b * (b' * ln(a) + b * a' / a)
  case Power(a, b)          => dmul(
                                 Power(a, b),
                                 dadd(dmul(derive(b, v), Ln(a)), Ratio(dmul(b, derive(a, v)), a))
                               )
  case Exp(a)               => dmul(Exp(a), derive(a, v))
  case Ln(a)                => Ratio(derive(a, v), a)
  // d/dx log_b(f(x)) = f'(x) / (f(x) * ln(b))   when b is constant w.r.t. x
  // general case: reduce to the ratio rule on ln(f)/ln(b)
  case LogBase(a, b) if !dependsOn(b, v) =>
                               Ratio(derive(a, v), Product(a, Ln(b)))
  case LogBase(a, b)        => derive(Ratio(Ln(a), Ln(b)), v)
  case Sin(a)               => dmul(Cos(a), derive(a, v))
  case Cos(a)               => dmul(_Number(-1), dmul(Sin(a), derive(a, v)))
  case Tg(a)                => Ratio(derive(a, v), Product(Cos(a), Cos(a)))
  // asin'(u) =  u' / sqrt(1 - u²)
  case Asin(a)              => dmul(Ratio(_Number(1), Power(Sum(_Number(1), dmul(_Number(-1), Power(a, _Number(2)))), _Number(0.5))), derive(a, v))
  // acos'(u) = -u' / sqrt(1 - u²)
  case Acos(a)              => dmul(dmul(_Number(-1), Ratio(_Number(1), Power(Sum(_Number(1), dmul(_Number(-1), Power(a, _Number(2)))), _Number(0.5)))), derive(a, v))
  // atan'(u) =  u' / (1 + u²)
  case Atan(a)              => dmul(Ratio(_Number(1), Sum(_Number(1), Power(a, _Number(2)))), derive(a, v))
  // hyperbolic and reciprocal-trigonometric derivatives (issue 3.9)
  case Sinh(a)              => dmul(Cosh(a), derive(a, v))
  case Cosh(a)              => dmul(Sinh(a), derive(a, v))
  case Tanh(a)              => dmul(Power(Sech(a), _Number(2)), derive(a, v))
  // asinh'(u) = u' / sqrt(u² + 1)
  case Asinh(a)             => dmul(Ratio(_Number(1), Power(Sum(Power(a, _Number(2)), _Number(1)), _Number(0.5))), derive(a, v))
  // acosh'(u) = u' / sqrt(u² − 1)
  case Acosh(a)             => dmul(Ratio(_Number(1), Power(Sum(Power(a, _Number(2)), _Number(-1)), _Number(0.5))), derive(a, v))
  // atanh'(u) = u' / (1 − u²)
  case Atanh(a)             => dmul(Ratio(_Number(1), Sum(_Number(1), dmul(_Number(-1), Power(a, _Number(2))))), derive(a, v))
  // sec'(u) = sec(u)·tan(u)·u' ;  csc'(u) = −csc(u)·cot(u)·u' ;  cot'(u) = −csc²(u)·u'
  case Sec(a)               => dmul(Product(Sec(a), Tg(a)), derive(a, v))
  case Csc(a)               => dmul(dmul(_Number(-1), Product(Csc(a), Cot(a))), derive(a, v))
  case Cot(a)               => dmul(dmul(_Number(-1), Power(Csc(a), _Number(2))), derive(a, v))
  // sech'(u) = −sech(u)·tanh(u)·u' ;  csch'(u) = −csch(u)·coth(u)·u' ;  coth'(u) = −csch²(u)·u'
  case Sech(a)              => dmul(dmul(_Number(-1), Product(Sech(a), Tanh(a))), derive(a, v))
  case Csch(a)              => dmul(dmul(_Number(-1), Product(Csch(a), Coth(a))), derive(a, v))
  case Coth(a)              => dmul(dmul(_Number(-1), Power(Csch(a), _Number(2))), derive(a, v))
  // 4.O: with digamma available, the gamma family is finally differentiable.  Before it,
  // Gamma and fact had no rule at all and fell through to a bare _Derivative wrapper.
  //   d/dx Gamma(u) = Gamma(u)*psi(u)*u'   and   d/dx u! = Gamma(u+1)*psi(u+1)*u'
  //   d/dx lgamma(u) = psi(u)*u'           -- the reason lgamma is the tidier one to use
  case Gamma(a)             => dmul(Product(Gamma(a), Digamma(a)), derive(a, v))
  case LogGamma(a)          => dmul(Digamma(a), derive(a, v))
  case Factorial(a)         =>
    val ap = Sum(a, _Number(1))
    dmul(Product(Gamma(ap), Digamma(ap)), derive(a, v))
  // NOTE there is deliberately no rule for `Digamma` itself: its derivative is the
  // trigamma function, which 4.O does not introduce.  It falls through to the generic
  // `_Derivative` wrapper, which is the same "no rule" behaviour Gamma had until now.
  // erf'(u) = 2/sqrt(pi) * e^(-u^2) * u'
  case Erf(a)               =>
    dmul(Product(_Number(2.0 / math.sqrt(math.Pi)),
                 Exp(dmul(_Number(-1), Power(a, _Number(2))))), derive(a, v))
  case Erfc(a)              => dmul(_Number(-1), derive(Erf(a), v))
  // Functional nodes must be reduced here, not left to fall through to a bare
  // _Derivative wrapper: that wrapper's eval calls derive again on the same node,
  // looping forever (StackOverflow).
  //   - _Derivative: differentiate again → higher-order derivative.
  //   - _Integral:   fundamental theorem, d/dx ∫f dx = f, when the variables match.
  //   - _DefIntegral: a definite integral is a constant except through its limits,
  //     which the engine does not track symbolically, so leave it symbolic.
  // Element-wise containers (matrix literals, matrix sums, transpose — see
  // core._ElementWise): differentiation distributes over the children.
  case ew: _ElementWise         => ew.rebuild(ew.children.map(derive(_, v)))
  case _Derivative(inner, iv)   => derive(derive(inner, iv), v)
  case _Integral(inner, iv)     => if iv.variable == v.variable then inner
                                   else _Derivative(e, v)
  case _DefIntegral(_, _, _, _) => _Derivative(e, v)
  // A limit is treated as opaque under differentiation (swapping d/dx and lim is
  // valid under continuity, but the engine does not verify that condition, so we
  // stay symbolic to be conservative).
  case _: _Limit                => _Derivative(e, v)
  case other                    => _Derivative(other, v)

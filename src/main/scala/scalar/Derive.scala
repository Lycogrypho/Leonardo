package it.grypho.scala.leonardo
package scalar

import core.*


// Symbolic differentiation. derive(e, v) returns d(e)/d(v) as a new expression,
// implemented as a rule table over expression shapes (the dual of the planned
// integration table). Powers the _Derivative node's eval.

// Algebraic helpers used only inside derive to keep the output compact.
// Without these, terms like 0*x or 1*f remain symbolic and block numeric eval.

private def dmul(a: _Expression, b: _Expression): _Expression = (a, b) match
  case (_Number(0.0), _) | (_, _Number(0.0)) => _Number(0)
  case (_Number(1.0), x)                      => x
  case (x,            _Number(1.0))           => x
  case _                                      => Product(a, b)

private def dadd(a: _Expression, b: _Expression): _Expression = (a, b) match
  case (_Number(0.0), x) => x
  case (x, _Number(0.0)) => x
  case _                 => Sum(a, b)

// Avoids Power(a, 0) = 1 and Power(a, 1) = a blowing up when a is 0.
private def dpow(a: _Expression, b: _Expression): _Expression = b match
  case _Number(0.0) => _Number(1)
  case _Number(1.0) => a
  case _            => Power(a, b)


// Memoized entry point: derive is pure in (e, v), so results are cached across
// calls. This pays off heavily where the same derivative is requested repeatedly —
// e.g. _DefIntegral's tree-eval fallback re-derives its integrand at every Simpson
// sample point — and across shared subtrees, since every recursive call lands here.
private val deriveMemo = new Memo[(_Expression, String), _Expression](10000)

def derive(e: _Expression, v: _Variable): _Expression =
  deriveMemo.getOrElseUpdate((e, v.variable))(deriveImpl(e, v))

private def deriveImpl(e: _Expression, v: _Variable): _Expression = e match
  case _Number(_)           => _Number(0)
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
  // General power rule: a^b * (b' * log(a) + b * a' / a)
  case Power(a, b)          => dmul(
                                 Power(a, b),
                                 dadd(dmul(derive(b, v), Log(a)), Ratio(dmul(b, derive(a, v)), a))
                               )
  case Exp(a)               => dmul(Exp(a), derive(a, v))
  case Log(a)               => Ratio(derive(a, v), a)
  case Sin(a)               => dmul(Cos(a), derive(a, v))
  case Cos(a)               => dmul(_Number(-1), dmul(Sin(a), derive(a, v)))
  case Tg(a)                => Ratio(derive(a, v), Product(Cos(a), Cos(a)))
  // asin'(u) =  u' / sqrt(1 - u²)
  case Asin(a)              => dmul(Ratio(_Number(1), Power(Sum(_Number(1), dmul(_Number(-1), Power(a, _Number(2)))), _Number(0.5))), derive(a, v))
  // acos'(u) = -u' / sqrt(1 - u²)
  case Acos(a)              => dmul(dmul(_Number(-1), Ratio(_Number(1), Power(Sum(_Number(1), dmul(_Number(-1), Power(a, _Number(2)))), _Number(0.5)))), derive(a, v))
  // atan'(u) =  u' / (1 + u²)
  case Atan(a)              => dmul(Ratio(_Number(1), Sum(_Number(1), Power(a, _Number(2)))), derive(a, v))
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
  case other                    => _Derivative(other, v)

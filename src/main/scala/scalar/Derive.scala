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


def derive(e: _Expression, v: _Variable): _Expression = e match
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
  case other                => _Derivative(other, v)

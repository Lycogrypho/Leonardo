package it.grypho.scala.leonardo
package expr

import parser.Environment


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


private def derive(e: _Expression, v: _Variable): _Expression = e match
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


abstract class _Functional extends _Expression


case class _Derivative(e: _Expression, v: _Variable) extends _Functional:
  override def toString: String = s"derive($e, $v)"

  override def eval(env: Environment): Either[_Expression, Double] =
    derive(e, v).eval(env)


case class _Integral(e: _Expression, v: _Variable) extends _Functional:
  override def toString: String = s"integral($e, $v)"

  // Indefinite integration is not implemented; result stays symbolic.
  override def eval(env: Environment): Either[_Expression, Double] = Left(this)


case class _DefIntegral(e: _Expression, v: _Variable, low_limit: _Expression, up_limit: _Expression) extends _Functional:
  override def toString: String = s"integral($e, $v, $low_limit, $up_limit)"

  override def eval(env: Environment): Either[_Expression, Double] =
    (low_limit.eval(env), up_limit.eval(env)) match
      case (Right(a), Right(b)) =>
        val n    = 1000        // must be even; O(h^4) error with composite Simpson
        val h    = (b - a) / n
        var sum  = 0.0
        var i    = 0
        var done = true
        while i <= n && done do
          val xi = a + i * h
          e.eval(env.withBinding(v.variable, _Number(xi))) match
            case Right(y) =>
              val coeff = if i == 0 || i == n then 1.0
                          else if i % 2 == 1   then 4.0
                          else                       2.0
              sum += coeff * y
            case Left(_) => done = false
          i += 1
        if done then _Number(h / 3.0 * sum).eval(env)
        else Left(this)
      case _ => Left(this)

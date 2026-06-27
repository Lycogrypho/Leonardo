package it.grypho.scala.leonardo
package scalar

import core.*


// Higher-order operators that take an expression (and a variable) and produce a new
// one: differentiation and integration. The algorithms live in their own files
// (Derive.scala; integration to come); these classes are just the AST nodes.
abstract class _Functional extends _Expression


case class _Derivative(e: _Expression, v: _Variable) extends _Functional:
  override def toString: String = s"derive($e, $v)"

  override def eval(env: Environment): Either[_Expression, _Value] =
    it.grypho.scala.leonardo.scalar.derive(e, v).eval(env)


case class _Integral(e: _Expression, v: _Variable) extends _Functional:
  override def toString: String = s"integral($e, $v)"

  // Indefinite integration is not implemented; result stays symbolic.
  override def eval(env: Environment): Either[_Expression, _Value] = Left(this)


case class _DefIntegral(e: _Expression, v: _Variable, low_limit: _Expression, up_limit: _Expression) extends _Functional:
  override def toString: String = s"integral($e, $v, $low_limit, $up_limit)"

  override def eval(env: Environment): Either[_Expression, _Value] =
    (low_limit.eval(env), up_limit.eval(env)) match
      case (Right(_Number(a)), Right(_Number(b))) =>
        val n    = 1000        // must be even; O(h^4) error with composite Simpson
        val h    = (b - a) / n
        var sum  = 0.0
        var i    = 0
        var done = true
        while i <= n && done do
          val xi = a + i * h
          e.eval(env.withBinding(v.variable, _Number(xi))) match
            case Right(_Number(y)) =>
              val coeff = if i == 0 || i == n then 1.0
                          else if i % 2 == 1   then 4.0
                          else                       2.0
              sum += coeff * y
            case _ => done = false
          i += 1
        if done then _Number(h / 3.0 * sum).eval(env)
        else Left(this)
      case _ => Left(this)

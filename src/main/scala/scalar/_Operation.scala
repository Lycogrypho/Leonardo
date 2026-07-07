package it.grypho.scala.leonardo
package scalar

import core.*
import scala.math.pow


abstract class _Operation(a: _Expression, b: _Expression) extends _Expression


case class Sum(a: _Expression, b: _Expression) extends _Operation(a, b):
  override def toString: String = s"($a + $b)"

  override def eval(env: Environment): Either[_Expression, _Value] =
    (a.eval(env), b.eval(env)) match
      case (Right(_Number(x)), Right(_Number(y))) => _Number(x + y).eval(env)
      case (ra, rb)                               => Left(Sum(ra.toExpression, rb.toExpression))


case class Product(a: _Expression, b: _Expression) extends _Operation(a, b):
  override def toString: String = s"($a * $b)"

  override def eval(env: Environment): Either[_Expression, _Value] =
    val ea = a.eval(env)
    ea match
      case Right(_Number(0.0)) => _Number(0.0).eval(env)
      case _ =>
        val eb = b.eval(env)
        eb match
          case Right(_Number(0.0)) => _Number(0.0).eval(env)
          case _ =>
            (ea, eb) match
              case (Right(_Number(x)), Right(_Number(y))) => _Number(x * y).eval(env)
              case (ra, rb)                               => Left(Product(ra.toExpression, rb.toExpression))


case class Ratio(a: _Expression, b: _Expression) extends _Operation(a, b):
  override def toString: String = s"($a / $b)"

  override def eval(env: Environment): Either[_Expression, _Value] =
    (a.eval(env), b.eval(env)) match
      case (Right(_Number(x)), Right(_Number(y))) =>
        val r = x / y
        // x/0 and 0/0 are domain errors: stay symbolic instead of propagating ±Infinity/NaN
        if r.isNaN || r.isInfinite then Left(this) else _Number(r).eval(env)
      case (ra, rb)                               => Left(Ratio(ra.toExpression, rb.toExpression))


// Exponentiation is a binary operation like the others; the parser keeps it
// right-associative (2 ^ 3 ^ 2 = 2 ^ (3 ^ 2)).
case class Power(base: _Expression, exp: _Expression) extends _Operation(base, exp):
  override def toString: String = s"($base ^ $exp)"

  override def eval(env: Environment): Either[_Expression, _Value] =
    (base.eval(env), exp.eval(env)) match
      case (Right(_Number(b)), Right(_Number(e))) =>
        val r = pow(b, e)
        // 0^negative and (negative)^fractional are domain errors: stay symbolic
        // instead of propagating ±Infinity/NaN
        if r.isNaN || r.isInfinite then Left(this) else _Number(r).eval(env)
      case (rb, re)                               => Left(Power(rb.toExpression, re.toExpression))

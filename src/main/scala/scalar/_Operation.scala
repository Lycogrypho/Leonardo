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
    (a.eval(env), b.eval(env)) match
      case (Right(_Number(x)), Right(_Number(y))) => _Number(x * y).eval(env)
      case (ra, rb)                               => Left(Product(ra.toExpression, rb.toExpression))


case class Ratio(a: _Expression, b: _Expression) extends _Operation(a, b):
  override def toString: String = s"($a / $b)"

  override def eval(env: Environment): Either[_Expression, _Value] =
    (a.eval(env), b.eval(env)) match
      case (Right(_Number(x)), Right(_Number(y))) => _Number(x / y).eval(env)
      case (ra, rb)                               => Left(Ratio(ra.toExpression, rb.toExpression))


// Exponentiation is a binary operation like the others; the parser keeps it
// right-associative (2 ^ 3 ^ 2 = 2 ^ (3 ^ 2)).
case class Power(base: _Expression, exp: _Expression) extends _Operation(base, exp):
  override def toString: String = s"($base ^ $exp)"

  override def eval(env: Environment): Either[_Expression, _Value] =
    (base.eval(env), exp.eval(env)) match
      case (Right(_Number(b)), Right(_Number(e))) => _Number(pow(b, e)).eval(env)
      case (rb, re)                               => Left(Power(rb.toExpression, re.toExpression))

package it.grypho.scala.leonardo
package expr

import parser.Environment


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

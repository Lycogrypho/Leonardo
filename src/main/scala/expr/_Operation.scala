package it.grypho.scala.leonardo
package expr

import parser.Environment


// Base class for binary operations. Intended to be extended with a derive() method
// when symbolic differentiation is implemented.
abstract class _Operation(a: _Expression, b: _Expression) extends _Expression


case class Sum(a: _Expression, b: _Expression) extends _Operation(a, b):
  override def toString: String = s"($a + $b)"

  override def eval(env: Environment): Either[_Expression, Double] =
    (a.eval(env), b.eval(env)) match
      case (Right(x), Right(y)) => _Number(x + y).eval(env)
      case (Right(x), Left(y))  => Left(Sum(_Number(x), y))
      case (Left(x),  Right(y)) => Left(Sum(x, _Number(y)))
      case (Left(x),  Left(y))  => Left(Sum(x, y))


case class Product(a: _Expression, b: _Expression) extends _Operation(a, b):
  override def toString: String = s"($a * $b)"

  override def eval(env: Environment): Either[_Expression, Double] =
    (a.eval(env), b.eval(env)) match
      case (Right(x), Right(y)) => _Number(x * y).eval(env)
      case (Right(x), Left(y))  => Left(Product(_Number(x), y))
      case (Left(x),  Right(y)) => Left(Product(x, _Number(y)))
      case (Left(x),  Left(y))  => Left(Product(x, y))


case class Ratio(a: _Expression, b: _Expression) extends _Operation(a, b):
  override def toString: String = s"($a / $b)"

  override def eval(env: Environment): Either[_Expression, Double] =
    (a.eval(env), b.eval(env)) match
      case (Right(x), Right(y)) => _Number(x / y).eval(env)
      case (Right(x), Left(y))  => Left(Ratio(_Number(x), y))
      case (Left(x),  Right(y)) => Left(Ratio(x, _Number(y)))
      case (Left(x),  Left(y))  => Left(Ratio(x, y))

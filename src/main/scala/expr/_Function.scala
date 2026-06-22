package it.grypho.scala.leonardo
package expr

import parser.Environment
import scala.math.{exp, log, sin, cos, tan}


// Base class for unary mathematical functions. Intended to be extended with a derive()
// method when symbolic differentiation is implemented.
abstract class _Function extends _Expression


case class Exp(e: _Expression) extends _Function:
  override def toString: String = s"exp($e)"

  override def eval(env: Environment): Either[_Expression, Double] =
    e.eval(env) match
      case Left(x)  => Left(Exp(x))
      case Right(x) => _Number(exp(x)).eval(env)


case class Log(e: _Expression) extends _Function:
  override def toString: String = s"log($e)"

  override def eval(env: Environment): Either[_Expression, Double] =
    e.eval(env) match
      case Left(x)  => Left(Log(x))
      case Right(x) => _Number(log(x)).eval(env)


case class Sin(e: _Expression) extends _Function:
  override def toString: String = s"sin($e)"

  override def eval(env: Environment): Either[_Expression, Double] =
    e.eval(env) match
      case Left(x)  => Left(Sin(x))
      case Right(x) => _Number(sin(x)).eval(env)


case class Cos(e: _Expression) extends _Function:
  override def toString: String = s"cos($e)"

  override def eval(env: Environment): Either[_Expression, Double] =
    e.eval(env) match
      case Left(x)  => Left(Cos(x))
      case Right(x) => _Number(cos(x)).eval(env)


case class Tg(e: _Expression) extends _Function:
  override def toString: String = s"tg($e)"

  override def eval(env: Environment): Either[_Expression, Double] =
    e.eval(env) match
      case Left(x)  => Left(Tg(x))
      case Right(x) => _Number(tan(x)).eval(env)

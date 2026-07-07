package it.grypho.scala.leonardo
package scalar

import core.*
import scala.math.{exp, log, sin, cos, tan}


abstract class _Function extends _Expression


case class Exp(e: _Expression) extends _Function:
  override def toString: String = s"exp($e)"

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(_Number(x)) => _Number(exp(x)).eval(env)
      case other             => Left(Exp(other.toExpression))


case class Log(e: _Expression) extends _Function:
  override def toString: String = s"log($e)"

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(_Number(x)) =>
        val r = log(x)
        // log of a non-positive number is a domain error: stay symbolic
        // instead of propagating -Infinity/NaN
        if r.isNaN || r.isInfinite then Left(this) else _Number(r).eval(env)
      case other             => Left(Log(other.toExpression))


case class Sin(e: _Expression) extends _Function:
  override def toString: String = s"sin($e)"

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(_Number(x)) => _Number(sin(x)).eval(env)
      case other             => Left(Sin(other.toExpression))


case class Cos(e: _Expression) extends _Function:
  override def toString: String = s"cos($e)"

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(_Number(x)) => _Number(cos(x)).eval(env)
      case other             => Left(Cos(other.toExpression))


case class Tg(e: _Expression) extends _Function:
  override def toString: String = s"tan($e)"

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(_Number(x)) => _Number(tan(x)).eval(env)
      case other             => Left(Tg(other.toExpression))

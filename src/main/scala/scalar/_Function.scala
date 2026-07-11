package it.grypho.scala.leonardo
package scalar

import core.*
import scala.math.{exp, log, sin, cos, tan, asin, acos, atan}


abstract class _Function extends _Expression


case class Exp(e: _Expression) extends _Function:
  override def toString: String = s"exp($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Exp(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(_Number(x)) => Right(_Number(exp(x)))
      case Right(c: _Complex) => _Complex.expc(c).map(Right(_)).getOrElse(Left(Exp(c)))
      case other             => Left(Exp(other.toExpression))


case class Log(e: _Expression) extends _Function:
  override def toString: String = s"log($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Log(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(_Number(x)) =>
        val r = log(x)
        // log of a negative number is now the principal complex value ln|x| + iπ;
        // log(0) is still undefined (_Complex.logc returns None) → stays symbolic.
        if r.isNaN || r.isInfinite then
          _Complex.logc(_Number(x)).map(Right(_)).getOrElse(Left(this))
        else Right(_Number(r))
      case Right(c: _Complex) => _Complex.logc(c).map(Right(_)).getOrElse(Left(Log(c)))
      case other             => Left(Log(other.toExpression))


case class Sin(e: _Expression) extends _Function:
  override def toString: String = s"sin($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Sin(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(_Number(x)) => Right(_Number(sin(x)))
      case Right(c: _Complex) => _Complex.sinc(c).map(Right(_)).getOrElse(Left(Sin(c)))
      case other             => Left(Sin(other.toExpression))


case class Cos(e: _Expression) extends _Function:
  override def toString: String = s"cos($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Cos(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(_Number(x)) => Right(_Number(cos(x)))
      case Right(c: _Complex) => _Complex.cosc(c).map(Right(_)).getOrElse(Left(Cos(c)))
      case other             => Left(Cos(other.toExpression))


case class Tg(e: _Expression) extends _Function:
  override def toString: String = s"tan($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Tg(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(_Number(x)) =>
        val r = tan(x)
        if r.isNaN || r.isInfinite then Left(this) else Right(_Number(r))
      case Right(c: _Complex) => _Complex.tanc(c).map(Right(_)).getOrElse(Left(Tg(c)))
      case other             => Left(Tg(other.toExpression))


case class Asin(e: _Expression) extends _Function:
  override def toString: String = s"asin($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Asin(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(_Number(x)) =>
        val r = asin(x)
        if r.isNaN then Left(this) else Right(_Number(r))
      case other             => Left(Asin(other.toExpression))


case class Acos(e: _Expression) extends _Function:
  override def toString: String = s"acos($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Acos(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(_Number(x)) =>
        val r = acos(x)
        if r.isNaN then Left(this) else Right(_Number(r))
      case other             => Left(Acos(other.toExpression))


case class Atan(e: _Expression) extends _Function:
  override def toString: String = s"atan($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Atan(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(_Number(x)) => Right(_Number(atan(x)))
      case other             => Left(Atan(other.toExpression))

package it.grypho.scala.leonardo
package expr

import parser.Environment
import scala.math.pow


case class Power(base: _Expression, exp: _Expression) extends _Expression:
  override def toString: String = f"($base ^ $exp)"

  override def eval(env: Environment): Either[_Expression, Double] =
    (base.eval(env), exp.eval(env)) match
      case (Right(b), Right(e)) => _Number(pow(b, e)).eval(env)
      case (Right(b), Left(e))  => Left(Power(_Number(b), e))
      case (Left(b),  Right(e)) => Left(Power(b, _Number(e)))
      case (Left(b),  Left(e))  => Left(Power(b, e))

package it.grypho.scala.leonardo
package expr

import parser.Environment
import scala.math.pow


case class Power(base: _Expression, exp: _Expression) extends _Expression:
  override def toString: String = s"($base ^ $exp)"

  override def eval(env: Environment): Either[_Expression, _Value] =
    (base.eval(env), exp.eval(env)) match
      case (Right(_Number(b)), Right(_Number(e))) => _Number(pow(b, e)).eval(env)
      case (rb, re)                               => Left(Power(rb.toExpression, re.toExpression))

package it.grypho.scala.leonardo
package expr

import parser.Environment
import it.grypho.scala.leonardo.expr._Number



case class _Variable(variable: String)(implicit env: Environment) extends _Value
{
  def value: Either[_Number, _Variable] = env.get(variable) match
  {
    case Some(n) => Left(n)
    case None => Right(this)
  }

  override def toString(): String = variable
}


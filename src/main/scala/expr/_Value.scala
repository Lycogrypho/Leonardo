package it.grypho.scala.leonardo
package expr

import it.grypho.scala.leonardo.parser.Environment

trait _Value(implicit env: Environment) extends _Expression
{
}

case class _Number(d: Double)(implicit env: Environment) extends _Value
{
  /* TODO: Store numbers as rationals. The precision fixed by the environment */
  override def toString(): String = d.toString

  val num: Long = (d * scala.math.pow(10,(env.precision))).round
  val den: Long =  10^(env.precision)

  val value = num / den
}


case class _Variable(variable: String)(implicit env: Environment) extends _Value
{
  def value: Either[_Number, _Variable] = env.get(variable) match
  {
    case Some(n) => Left(n)
    case None => Right(this)
  }

  override def toString(): String = variable
}


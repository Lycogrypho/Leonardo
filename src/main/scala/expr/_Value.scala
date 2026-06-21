package it.grypho.scala.leonardo
package expr

import parser.Environment


trait _Value extends _Expression


case class _Number(d: Double) extends _Value:
  override def toString(): String = d.toString

  override def eval(env: Environment): Either[_Expression, Double] =
    val factor = scala.math.pow(10, env.precision)
    Right((d * factor).round.toDouble / factor)


case class _Variable(variable: String) extends _Value:
  override def toString(): String = variable

  override def eval(env: Environment): Either[_Expression, Double] =
    env.get(variable) match
      case Some(n) => n.eval(env)
      case None    => Left(this)

  def set(value: _Number)(implicit env: Environment): Unit =
    env.set(variable, Some(value))
package it.grypho.scala.leonardo
package expr

import parser.Environment


trait _Value extends _Expression


object _Number:
  val DefaultPrecision: Int = 5
  private def round(d: Double, precision: Int): Double =
    if d.isNaN || d.isInfinite then d
    else
      val factor = scala.math.pow(10, precision)
      // guard: d * factor must fit in Long, otherwise rounding is meaningless
      if scala.math.abs(d) * factor > Long.MaxValue.toDouble then d
      else (d * factor).round.toDouble / factor

case class _Number(d: Double) extends _Value:
  val value: Double = _Number.round(d, _Number.DefaultPrecision)

  override def toString(): String = value.toString

  override def eval(env: Environment): Either[_Expression, Double] =
    Right(_Number.round(d, env.precision))


case class _Variable(variable: String) extends _Value:
  override def toString(): String = variable

  override def eval(env: Environment): Either[_Expression, Double] =
    env.get(variable) match
      case Some(n) => n.eval(env)
      case None    => Left(this)

  def set(value: _Number)(implicit env: Environment): Unit =
    env.assign(variable, value)
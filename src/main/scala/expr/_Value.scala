package it.grypho.scala.leonardo
package expr

import parser.Environment


trait _Value extends _Expression


object _Number:
  val DefaultPrecision: Int = 5
  private val defaultFactor: Double = scala.math.pow(10, DefaultPrecision)

  private def round(d: Double, precision: Int): Double =
    if d.isNaN || d.isInfinite then d
    else
      val factor = if precision == DefaultPrecision then defaultFactor
                   else scala.math.pow(10, precision)
      // guard: d * factor must fit in Long, otherwise rounding is meaningless
      if scala.math.abs(d) * factor > Long.MaxValue.toDouble then d
      else (d * factor).round.toDouble / factor

case class _Number(d: Double) extends _Value:
  override def toString: String = _Number.round(d, _Number.DefaultPrecision).toString

  override def eval(env: Environment): Either[_Expression, _Value] =
    Right(_Number(_Number.round(d, env.precision)))


// A free variable is a symbolic atom, not a concrete value, so it is not a _Value:
// eval yields a value (Right) only once the variable is bound to one.
case class _Variable(variable: String) extends _Expression:
  override def toString: String = variable

  override def eval(env: Environment): Either[_Expression, _Value] =
    env.get(variable) match
      case Some(n) => n.eval(env)
      case None    => Left(this)

  def set(value: _Number)(implicit env: Environment): Unit =
    env.assign(variable, value)
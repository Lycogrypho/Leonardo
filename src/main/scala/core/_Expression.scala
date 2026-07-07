package it.grypho.scala.leonardo
package core


// Foundation of every expression tree, shared across all domains.
// eval reduces an expression: Right holds a fully-reduced _Value; Left holds a
// symbolic expression that could not be fully reduced.
trait _Expression:
  def eval(env: Environment): Either[_Expression, _Value]


// Marker for a fully-reduced, concrete result — a number now, a matrix/boolean/… later.
// Distinct from a symbolic atom such as a free variable.
trait _Value extends _Expression


object _Number:
  private val factorTable: Array[Double] = Array.tabulate(16)(i => scala.math.pow(10.0, i))

  private[core] def round(d: Double, precision: Int): Double =
    if d.isNaN || d.isInfinite then d
    else
      val factor = if precision >= 0 && precision < factorTable.length then factorTable(precision)
                   else scala.math.pow(10, precision)
      // guard: d * factor must fit in Long, otherwise rounding is meaningless
      if scala.math.abs(d) * factor > Long.MaxValue.toDouble then d
      else (d * factor).round.toDouble / factor

case class _Number(d: Double) extends _Value:
  override def toString: String = _Number.round(d, Environment.DefaultPrecision).toString

  override def eval(env: Environment): Either[_Expression, _Value] =
    Right(_Number(_Number.round(d, env.precision)))


// A free variable is a symbolic atom, not a concrete value, so it is not a _Value:
// eval yields Right only once the variable is bound to one in the environment.
case class _Variable(variable: String) extends _Expression:
  override def toString: String = variable

  override def eval(env: Environment): Either[_Expression, _Value] =
    env.get(variable) match
      case Some(n) => n.eval(env)
      case None    => Left(this)


// Collapse an eval result back to a plain expression — used when rebuilding a
// symbolic node from partially-reduced operands.
extension (result: Either[_Expression, _Value])
  def toExpression: _Expression = result match
    case Left(e)  => e
    case Right(v) => v

package it.grypho.scala.leonardo
package core


/** The small numeric readers every package needs, defined once (F_0147).  Each had grown a
 *  private copy in several packages, and the copies differed in their guards — the whole-number
 *  tests in particular, whose different range checks are exactly what F_0142 fell through.
 */

/** An evaluated operand as a single real number, or `None`.
 *  @param r the evaluated operand
 *  @return the number */
private[leonardo] def numberOf(r: Either[_Expression, _Value]): Option[Double] = r match
  case Right(_Number(d)) => Some(d)
  case _                 => None

/** `d` when it is finite, else `None`.
 *  @param d the value
 *  @return the value, or `None` for a NaN or an infinity */
private[leonardo] def finiteOption(d: Double): Option[Double] = Option.when(d.isFinite)(d)

/** The order a list of roots or eigenvalues is answered in: real part ascending, then imaginary
 *  part ascending, so a conjugate pair comes `−im` first (F_0230).  One key for `tfpoles`,
 *  `eigen`, `eig` and `jordan`, so the four cannot list the same values differently; a value
 *  with no real reading sorts last.
 *  @param v the value
 *  @return the sort key */
private[leonardo] def rootOrder(v: _Value): (Double, Double) = v match
  case c: _Complex => _Complex.parts(c).getOrElse((Double.MaxValue, Double.MaxValue))
  case _Number(d)  => (d, 0.0)
  case _           => (Double.MaxValue, Double.MaxValue)

/** Whole-number readers.  `isWhole` is the plain test; `asInt` is the one to use before an
 *  index or a count, since `Double.toInt` saturates at `Int.MaxValue` rather than failing. */
private[leonardo] object Whole:

  /** Whether `d` is a finite whole number.
   *  @param d the value
   *  @return whether it is whole */
  def isWhole(d: Double): Boolean = d.isFinite && d == math.floor(d)

  /** `d` as an `Int` when it is whole and within `Int`'s range.
   *  @param d the value
   *  @return the integer, or `None` */
  def asInt(d: Double): Option[Int] =
    Option.when(isWhole(d) && d >= Int.MinValue && d <= Int.MaxValue)(d.toInt)

  /** `d` as a non-negative `Int`, the shape of a count or an order.
   *  @param d the value
   *  @return the integer, or `None` */
  def asCount(d: Double): Option[Int] = asInt(d).filter(_ >= 0)

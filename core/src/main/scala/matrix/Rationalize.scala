package it.grypho.scala.leonardo
package matrix

import core.*


/** `rationalize(x)` / `rationalize(x, digits)` — every decimal of `x` replaced by the rational
 *  it is closest to, with a denominator at most `10^digits` (F_0170).
 *
 *  **The way back into the exact tier, and only when asked.**  A `Double` stays visibly inexact
 *  by design (the one-way float contagion of 4.L), so a value known to be exact that arrived as
 *  a decimal — a `lu` factor in exact mode, a frozen `consolidate` result, a number read from
 *  data — had no way back.  Applied silently, the conversion would relabel a computed
 *  `0.3333333333333333` as an exact `1/3`; asked for, it is the reader's statement that the
 *  value is rational.
 *
 *  - A number becomes `_Rational.fromApproximation(d, digits)`: the best fraction by
 *    continued-fraction truncation, so `0.3333333333333333` is `1/3`.
 *  - A dense matrix becomes the exact `_Matrix` carrier — the reason this node lives in
 *    `matrix`: `scalar` cannot build that carrier.
 *  - A symbolic expression keeps its shape, its decimal leaves made exact; an exact leaf is
 *    left as it is.
 *  - **`digits` defaults to the working precision, capped at 15**, the digits a `Double`
 *    carries (`_Rational.DoubleReliableDigits`); an explicit one must be a whole number in
 *    `1 .. 15`.
 *  - **A value it cannot make exact declines the whole call** (the owner's decision): a complex
 *    number (no exact complex carrier exists), an interval, a non-finite value.  Converting the
 *    rest would leave a result in two tiers that demotes again on evaluation.
 *
 *  It works outside exact mode too, since it is asked for; `:save` groups the exact value it
 *  leaves under `exact on` (F_0090).
 *
 *  @param x      the expression whose decimals to convert
 *  @param digits the decimal digits the approximation is good for, optional
 */
case class _Rationalize(x: _Expression, digits: Option[_Expression] = None) extends _Expression:
  override def toString: String = digits.fold(s"rationalize($x)")(d => s"rationalize($x, $d)")
  override def children: List[_Expression] = x :: digits.toList
  override def rebuild(c: List[_Expression]): _Expression = _Rationalize(c.head, c.tail.headOption)

  override def eval(env: Environment): Either[_Expression, _Value] =
    val r = x.eval(env).toExpression
    detailed(r, env) match
      case Right(exact) => exact.eval(env)
      case Left(_)      => Left(_Rationalize(r, digits.map(_.eval(env).toExpression)))

  /** Why this call stays unevaluated: the digits, or the leaf that has no exact value.
   *  @param env the evaluation environment
   *  @return the reason, or `None` when it answers */
  def declineReason(env: Environment): Option[String] = detailed(x.eval(env).toExpression, env).left.toOption

  /** The converted expression, or why there is none. */
  private def detailed(r: _Expression, env: Environment): Either[String, _Expression] =
    digitsOf(env).flatMap(d => rationalizeExpr(r, d))

  /** The digits asked for, or the default: the working precision, capped at 15. */
  private def digitsOf(env: Environment): Either[String, Int] =
    digits.map(_.eval(env)) match
      case None => Right(math.min(env.workingPrecision, _Rational.DoubleReliableDigits))
      case Some(Right(_Number(d))) if Whole.isWhole(d) && d >= 1 && d <= _Rational.DoubleReliableDigits => Right(d.toInt)
      case Some(_) =>
        Left(s"the digits must be a whole number from 1 to ${_Rational.DoubleReliableDigits}: a Double carries no more")


/** `e` with every decimal leaf made exact at `digits` (F_0170), or why it cannot be.
 *
 *  @param e      an evaluated expression
 *  @param digits the decimal digits, `1 .. 15`
 *  @return the converted expression, or the reason naming the leaf that has no exact value
 */
private[leonardo] def rationalizeExpr(e: _Expression, digits: Int): Either[String, _Expression] =
  def number(d: Double): Either[String, _Expression] =
    _Rational.fromApproximation(d, digits).toRight(s"${_Number.render(d)} has no exact value")
  e match
    // The exact arm first: the widening `_Number` would read a rational as a Double.
    case r: _Rational  => Right(r)
    case c: _Complex   => Left(s"$c is complex, and no exact complex value exists")
    case i: _Interval  => Left(s"$i is an interval, which has no single exact value")
    case _Number(d)    => number(d)
    case m: _MatrixValue =>
      sequence(m.toVector.map(number).map(_.toOption)) match
        case Some(cells) => Right(_Matrix(m.rows, m.cols, cells))
        case None        => Left("a matrix cell has no exact value")
    case v: _Value     => Right(v)   // a truth value, a distribution: no decimal of its own to convert
    case other =>
      other.children.foldRight[Either[String, List[_Expression]]](Right(Nil)) { (c, acc) =>
        for tail <- acc; head <- rationalizeExpr(c, digits) yield head :: tail
      }.map(cs => if cs.isEmpty then other else other.rebuild(cs))


/** Whether a decimal looks like a small fraction (F_0170's note): a rational within **4 ulps** of
 *  it whose denominator is at most `Q(d) = √(10⁻³ / (4·ulp(d)))`.  `0.6666666666666666` passes;
 *  `√13`, `π` and `1000·π` do not, so the note never offers to make an irrational exact.
 *
 *  **The denominator bound is what keeps the verdict free of units.**  The fractions with
 *  denominator at most `Q` lie about `1/(q·Q)` apart, so an irrational falls within `tol` of one
 *  of them with probability about `tol·Q²`; `Q(d)` holds that chance at `10⁻³` for the
 *  tolerance `tol = 4·ulp(d)`.  At unit scale `Q` is about `10⁶`; it shrinks as `d` grows.  Two
 *  figures were wrong first, both caught by a test: a relative `10⁻¹²` with a fixed `10⁶`
 *  bound accepted `π`, and 4 ulps with the fixed bound accepted nearly every decimal above
 *  `1`, an LA lesson's `cond(D) = 2451.94087…` included, since 4 ulps grow with the value and
 *  the fixed set of fractions does not.  Refusing a borderline case only withholds a note, the
 *  safe direction.
 *
 *  @param d the decimal
 *  @return whether it looks rational
 */
private[leonardo] def looksRational(d: Double): Boolean = d == 0.0 || smallFraction(d).isDefined

/** The small fraction a decimal lies within 4 ulps of, under [[looksRational]]'s denominator bound. */
private def smallFraction(d: Double): Option[_Rational] =
  if !d.isFinite || d == 0.0 then None
  else _Rational.fromDouble(d).map { r =>
    val tolerance = 4 * Math.ulp(d)
    val maxDen    = BigDecimal(math.max(1.0, math.floor(math.sqrt(1e-3 / tolerance)))).toBigInt
    (r.approximate(maxDen), tolerance)
  }.collect { case (f, tolerance) if math.abs(f.toDouble - d) <= tolerance => f }

/** The fewest digits at which `rationalize` turns every decimal into its small fraction (F_0170's
 *  note), or `None` when one of them does not look rational.
 *
 *  The note must offer what the call gives.  At the default fifteen digits a computed
 *  `1.9999999999999991` stays a fifteen-digit fraction that still prints `2.0`, since the
 *  approximation is faithful to every digit a `Double` carries; the digits that give `2` are
 *  fewer, and the note names them.  The best approximation with a denominator up to `10ᵏ` is the
 *  small fraction once `10ᵏ` passes its denominator, because the next convergent's denominator is
 *  orders of magnitude past [[looksRational]]'s bound; each candidate is checked regardless.
 *
 *  @param ds the decimals of an answer
 *  @return the digits, `1 .. 15`, or `None`
 */
private[leonardo] def rationalizeDigits(ds: List[Double]): Option[Int] =
  sequence(ds.filter(_ != 0.0).map(d => smallFraction(d).map(d -> _))).flatMap { pairs =>
    (1 to _Rational.DoubleReliableDigits).find(k =>
      pairs.forall((d, f) => _Rational.fromApproximation(d, k).contains(f)))
  }

/** The decimal leaves of an evaluated expression, a dense matrix's cells included, or `None` when
 *  it holds a value `rationalize` declines (a complex number, an interval). */
private[leonardo] def decimalLeaves(e: _Expression): Option[List[Double]] = e match
  case _: _Rational              => Some(Nil)
  case _: _Complex | _: _Interval => None
  case _Number(d)                => Some(List(d))
  case m: _MatrixValue           => Some(m.toVector.toList)
  case _: _Value                 => Some(Nil)
  case other => sequence(other.children.map(decimalLeaves)).map(_.flatten)

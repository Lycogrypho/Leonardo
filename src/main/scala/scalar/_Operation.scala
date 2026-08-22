package it.grypho.scala.leonardo
package scalar

import core.*
import scala.math.pow


/** Marker trait for the four scalar binary operations: [[Sum]], [[Product]], [[Ratio]], and [[Power]]. */
trait _Operation extends _Expression


/** Addition of two expressions: `a + b`.
 *
 *  Also evaluates concrete matrix operands via `_MatrixValue.add` (conforming dimensions)
 *  and concrete complex-valued operands via `_Complex.add`.  Dimension mismatches stay
 *  symbolic.
 *
 *  @param a left operand
 *  @param b right operand
 */
case class Sum(a: _Expression, b: _Expression) extends _Operation:
  override def toString: String = s"($a + $b)"
  override def children: List[_Expression] = List(a, b)
  override def rebuild(c: List[_Expression]): _Expression = Sum(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    (a.eval(env), b.eval(env)) match
      // ORDER IS LOAD-BEARING: `_Number` is a widening extractor that also matches a
      // `_Rational` (see core._Number.unapply), so the exact case has to come first or two
      // rationals would be read as Doubles and silently lose their exactness.  A MIXED pair
      // has no case of its own on purpose — it falls through to `_Number` and comes back a
      // `_Number`, which is exactly the promotion lattice's float contagion.
      case (Right(x: _Rational), Right(y: _Rational)) => Right(x.add(y, env.rationalPolicy))
      case (Right(_Number(x)), Right(_Number(y))) => Right(_Number(x + y))
      case (Right(x: _MatrixValue), Right(y: _MatrixValue)) =>
        if x.rows == y.rows && x.cols == y.cols then x.add(y).guarded(this) else Left(this)
      // Both operands are concrete: try complex field addition; None → stay symbolic.
      case (Right(av: _Value), Right(bv: _Value)) =>
        _Complex.add(av, bv).map(Right(_)).getOrElse(Left(Sum(av, bv)))
      case (ra, rb)                               => Left(Sum(ra.toExpression, rb.toExpression))


/** Multiplication of two expressions: `a * b`.
 *
 *  Matrix cases are evaluated before the scalar zero short-circuit (`0 * M` yields the
 *  zero matrix, not `_Number(0)`).  Matrix–matrix uses `_MatrixValue.multiply`;
 *  scalar–matrix uses `_MatrixValue.scale`.  Complex closure is applied when both
 *  operands are concrete values.
 *
 *  @param a left operand
 *  @param b right operand
 */
case class Product(a: _Expression, b: _Expression) extends _Operation:
  override def toString: String = s"($a * $b)"
  override def children: List[_Expression] = List(a, b)
  override def rebuild(c: List[_Expression]): _Expression = Product(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    (a.eval(env), b.eval(env)) match
      // matrix cases precede the zero short-circuit: 0 * M is the zero MATRIX.
      // `_Number(k)` here also matches an exact scalar, so `2 * A` keeps working in exact
      // mode; exact matrix ENTRIES are slice B.
      case (Right(_Number(k)), Right(m: _MatrixValue))      => m.scale(k).guarded(this)
      case (Right(m: _MatrixValue), Right(_Number(k)))      => m.scale(k).guarded(this)
      case (Right(x: _MatrixValue), Right(y: _MatrixValue)) =>
        if x.cols == y.rows then x.multiply(y).guarded(this) else Left(this)
      // Exact tier first — see the ORDER note in Sum.  The exact zero gets its own
      // short-circuit because `_Number(0.0)` matches on the *value*, and returning
      // `_Number(0.0)` for it would demote an exact operand for no reason: nothing about
      // multiplying by zero makes an exact zero less exact.
      case (Right(z: _Rational), _) if z.isZero => Right(z)
      case (_, Right(z: _Rational)) if z.isZero => Right(z)
      case (Right(x: _Rational), Right(y: _Rational)) => Right(x.multiply(y, env.rationalPolicy))
      // zero short-circuit for everything scalar or unreducible: 0 * e folds to 0
      case (Right(_Number(0.0)), _) | (_, Right(_Number(0.0))) => Right(_Number(0.0))
      case (Right(_Number(x)), Right(_Number(y)))           => Right(_Number(x * y))
      // Both operands are concrete: try complex field multiplication; None → stay symbolic.
      case (Right(av: _Value), Right(bv: _Value)) =>
        _Complex.mul(av, bv).map(Right(_)).getOrElse(Left(Product(av, bv)))
      case (ra, rb) => Left(Product(ra.toExpression, rb.toExpression))


/** Division of two expressions: `a / b`.
 *
 *  `x/0` and `0/0` stay symbolic.  `M / k` scales the matrix; `k / M` computes
 *  `k · M⁻¹`; `M / N` computes `M · N⁻¹` (non-invertible or non-conforming → symbolic).
 *  Complex closure applied when both operands are concrete.
 *
 *  @param a numerator
 *  @param b denominator
 */
case class Ratio(a: _Expression, b: _Expression) extends _Operation:
  override def toString: String = s"($a / $b)"
  override def children: List[_Expression] = List(a, b)
  override def rebuild(c: List[_Expression]): _Expression = Ratio(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    (a.eval(env), b.eval(env)) match
      // Exact tier first — see the ORDER note in Sum.  A zero denominator is the same
      // domain error as below: `divide` returns None and the node stays symbolic.
      case (Right(x: _Rational), Right(y: _Rational)) =>
        x.divide(y, env.rationalPolicy).map(Right(_)).getOrElse(Left(this))
      case (Right(_Number(x)), Right(_Number(y))) =>
        val r = x / y
        // x/0 and 0/0 are domain errors: stay symbolic instead of propagating ±Infinity/NaN
        if r.isNaN || r.isInfinite then Left(this) else Right(_Number(r))
      // M / k = (1/k) · M; k = 0 yields non-finite elements → the guard stays symbolic
      case (Right(m: _MatrixValue), Right(_Number(k))) => m.scale(1.0 / k).guarded(this)
      // k / M = k · M⁻¹ (scalar over matrix); singular/non-square → stays symbolic
      case (Right(_Number(k)), Right(m: _MatrixValue)) =>
        m.inverse.map(_.scale(k).guarded(this)).getOrElse(Left(this))
      // M / N = M · N⁻¹ (matrix over matrix); non-invertible or shape mismatch → symbolic
      case (Right(x: _MatrixValue), Right(y: _MatrixValue)) =>
        y.inverse match
          case Some(yi) if x.cols == yi.rows => x.multiply(yi).guarded(this)
          case _                             => Left(this)
      // Both operands are concrete: try complex field division; None on non-numeric or zero denominator.
      case (Right(av: _Value), Right(bv: _Value)) =>
        _Complex.div(av, bv).map(Right(_)).getOrElse(Left(Ratio(av, bv)))
      case (ra, rb)                               => Left(Ratio(ra.toExpression, rb.toExpression))


/** Exponentiation: `base ^ exp`.
 *
 *  The parser keeps `^` right-associative (`2^3^2 = 2^(3^2)`).  When the real power is
 *  undefined (NaN / infinite), falls back to the principal complex value — so
 *  `(-2)^0.5 = i√2` and `(-8)^(1/3)` is the principal complex cube root.
 *
 *  For a square dense `_MatrixValue` base with an integer exponent, uses binary
 *  exponentiation (`A^0 = I`, `A^-n = (A⁻¹)^n`).  Non-square, non-integer, or negative
 *  power of singular matrix → stays symbolic.
 *
 *  @param base the base expression
 *  @param exp  the exponent expression
 */
case class Power(base: _Expression, exp: _Expression) extends _Operation:
  override def toString: String = s"($base ^ $exp)"
  override def children: List[_Expression] = List(base, exp)
  override def rebuild(c: List[_Expression]): _Expression = Power(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    (base.eval(env), exp.eval(env)) match
      // Exact tier first -- see the ORDER note in Sum.
      case (Right(b: _Rational), Right(e: _Rational)) => ratPow(b, e, env)
      case (Right(_Number(b)), Right(_Number(e))) =>
        val r = pow(b, e)
        // A non-finite real result means the real power is undefined: fall back to the
        // principal complex value ((negative)^fractional → complex; 0^negative → still
        // undefined, so _Complex.pow returns None and we stay symbolic as before).
        if r.isNaN || r.isInfinite then
          _Complex.pow(_Number(b), _Number(e)).map(Right(_)).getOrElse(Left(this))
        else Right(_Number(r))
      // Square dense matrix raised to an integer power: A^n by binary exponentiation
      // (A^0 = I, A^-n = (A⁻¹)^n). Non-square, non-integer exponent, or a singular base
      // under a negative power → stay symbolic. Precedes the generic _Value case because
      // _MatrixValue is itself a _Value and would otherwise hit the complex fallback.
      case (Right(m: _MatrixValue), Right(_Number(e))) =>
        matrixPower(m, e).getOrElse(Left(this))
      // Same, with an exact exponent: in exact mode `A^2` carries a _Rational, and without
      // this arm it would fall through to the complex kernel and lose matrix power entirely.
      // Exact matrix ENTRIES are slice B; this only keeps the existing feature working.
      case (Right(m: _MatrixValue), Right(r: _Rational)) =>
        matrixPower(m, r.toDouble).getOrElse(Left(this))
      // Both operands are concrete: try principal complex power; None → stay symbolic.
      case (Right(bv: _Value), Right(ev: _Value)) =>
        _Complex.pow(bv, ev).map(Right(_)).getOrElse(Left(Power(bv, ev)))
      case (rb, re)                               => Left(Power(rb.toExpression, re.toExpression))

  /** Raises an exact rational to an exact rational power.
   *
   *  Integer exponents are closed over the rationals and stay exact — including negative
   *  ones, which invert.  A fractional exponent is not closed (`2^(1/2)` is irrational), so
   *  it is evaluated and re-approximated to the working precision, which is the documented
   *  behaviour for every operation outside the exact set.  A non-finite real result falls
   *  back to the principal complex value exactly as the `Double` path does.
   *
   *  @param b   the exact base
   *  @param e   the exact exponent
   *  @param env supplies the working precision and the reduction policy
   *  @return the reduced power, or `Left(this)` when the result is undefined
   */
  private def ratPow(b: _Rational, e: _Rational, env: Environment): Either[_Expression, _Value] =
    e.toBigIntExact.filter(_.isValidInt) match
      case Some(k) => b.pow(k.toInt, env.rationalPolicy).map(Right(_)).getOrElse(Left(this))
      case None    =>
        val r = pow(b.toDouble, e.toDouble)
        if r.isNaN || r.isInfinite then
          _Complex.pow(_Number(b.toDouble), _Number(e.toDouble)).map(Right(_)).getOrElse(Left(this))
        else _Rational.fromApproximation(r, env.workingPrecision).map(Right(_)).getOrElse(Left(this))

  /** Raises a square dense matrix to an integer power `e`.
   *
   *  Returns `None` (→ stay symbolic) when the base is non-square, `e` is not a whole
   *  number, or `e < 0` with a singular base.
   *
   *  @param m the square dense matrix base
   *  @param e the exponent (must be an integer when this is called)
   *  @return `Some(Right(_MatrixValue))` on success, `None` to stay symbolic
   */
  private def matrixPower(m: _MatrixValue, e: Double): Option[Either[_Expression, _Value]] =
    if m.rows != m.cols || e.isInfinite || e != math.floor(e) then None
    else
      val k = e.toLong
      if k >= 0 then Some(powInt(m, k).guarded(this))
      else m.inverse.map(inv => powInt(inv, -k).guarded(this))

  /** Binary exponentiation for a positive or zero integer power; `k = 0` yields the identity.
   *
   *  @param m the matrix to raise to the power
   *  @param k the non-negative integer exponent
   *  @return `m^k` computed via repeated squaring
   */
  private def powInt(m: _MatrixValue, k: Long): _MatrixValue =
    var result = _MatrixValue.identity(m.rows)
    var factor = m
    var n      = k
    while n > 0 do
      if (n & 1L) == 1L then result = result.multiply(factor)
      n >>= 1
      if n > 0 then factor = factor.multiply(factor)
    result

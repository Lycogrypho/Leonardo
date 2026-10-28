package it.grypho.scala.leonardo
package scalar

import core.*
import scala.math.pow


/** Marker trait for the four scalar binary operations: [[Sum]], [[Product]], [[Ratio]], and [[Power]]. */
trait _Operation extends _Expression


/** Reads a symbolic matrix node as a dense `Double` one.
 *
 *  Goes through the `_MatrixShaped` marker rather than `matrix._Matrix`, which `scalar`
 *  cannot see — the same route `_Function.mapMatrixExpr` takes.
 *
 *  Needed because the exact tier stops an exactly-written matrix from collapsing into
 *  the dense carrier: operations here that used to receive a `Right(_MatrixValue)` now get a
 *  `Left(_Matrix)` instead, and the ones that cannot be exact have to demote or they would
 *  silently stop working in exact mode.
 *
 *  @param m the symbolic matrix node
 *  @return the dense matrix, or `None` if any cell is not a real number
 */
private def denseFromShaped(m: _MatrixShaped): Option[_MatrixValue] =
  val ds = m.children.collect { case _Number(d) => d }
  if ds.size == m.children.size then Some(_MatrixValue(m.rows, m.cols, ds.toArray)) else None


/** Whether either operand is an interval: the gate on the interval arms (F_0008). */
private def eitherInterval(a: _Value, b: _Value): Boolean =
  a.isInstanceOf[_Interval] || b.isInstanceOf[_Interval]


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
      // F_0008: an interval operand, outward-rounded; anything without bounds stays symbolic.
      case (Right(av: _Value), Right(bv: _Value)) if eitherInterval(av, bv) =>
        _Interval.add(av, bv, env).toRight(Sum(av, bv))
      // Both operands are concrete: try complex field addition; None → stay symbolic.
      case (Right(av: _Value), Right(bv: _Value)) =>
        _Complex.add(av, bv).toRight(Sum(av, bv))
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
      // mode; exact matrix ENTRIES ride the symbolic `_Matrix` and never reach this arm.
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
      case (Right(av: _Value), Right(bv: _Value)) if eitherInterval(av, bv) =>
        _Interval.multiply(av, bv, env).toRight(Product(av, bv))
      // Both operands are concrete: try complex field multiplication; None → stay symbolic.
      case (Right(av: _Value), Right(bv: _Value)) =>
        _Complex.mul(av, bv).toRight(Product(av, bv))
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
  /** `(a / b)`, the divisor bracketed when its text would regroup ([[core.OperandText]], F_0241). */
  override def toString: String = s"($a / ${OperandText.tight(b.toString, asBase = false)})"
  override def children: List[_Expression] = List(a, b)
  override def rebuild(c: List[_Expression]): _Expression = Ratio(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    (a.eval(env), b.eval(env)) match
      // Exact tier first — see the ORDER note in Sum.  A zero denominator is the same
      // domain error as below: `divide` returns None and the node stays symbolic.
      case (Right(x: _Rational), Right(y: _Rational)) =>
        x.divide(y, env.rationalPolicy).toRight(this)
      case (Right(_Number(x)), Right(_Number(y))) =>
        val r = x / y
        // x/0 and 0/0 are domain errors: stay symbolic instead of propagating ±Infinity/NaN
        if !r.isFinite then Left(this) else Right(_Number(r))
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
      // An exactly-written matrix stays symbolic (see `matrix.Exact`), so these three
      // shapes now arrive as `Left`s.  M / k divides cell by cell and so stays EXACT --
      // `_MatrixShaped` is a core marker, so this needs no scalar -> matrix dependency.
      case (Left(m: _MatrixShaped), Right(k @ _Number(_))) =>
        m.rebuild(m.children.map(c => Ratio(c, k))).eval(env)
      // k / M and M / N both need a matrix inverse, which `scalar` cannot build, so they
      // demote to the dense kernel rather than lose the operation.
      case (Right(_Number(k)), Left(m: _MatrixShaped)) =>
        denseFromShaped(m).flatMap(_.inverse).map(_.scale(k).guarded(this)).getOrElse(Left(this))
      case (Left(x: _MatrixShaped), Left(y: _MatrixShaped)) =>
        (denseFromShaped(x), denseFromShaped(y).flatMap(_.inverse)) match
          case (Some(a), Some(bi)) if a.cols == bi.rows => a.multiply(bi).guarded(this)
          case _                                        => Left(this)
      // An interval divisor containing zero is two rays, not an interval: stays symbolic.
      case (Right(av: _Value), Right(bv: _Value)) if eitherInterval(av, bv) =>
        _Interval.divide(av, bv, env).toRight(Ratio(av, bv))
      // Both operands are concrete: try complex field division; None on non-numeric or zero denominator.
      case (Right(av: _Value), Right(bv: _Value)) =>
        _Complex.div(av, bv).toRight(Ratio(av, bv))
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
  /** `(base ^ exp)`, each operand bracketed when its text would regroup ([[core.OperandText]],
   *  F_0241): `(-2.0) ^ x` is not `-(2.0 ^ x)`. */
  override def toString: String =
    s"(${OperandText.tight(base.toString, asBase = true)} ^ ${OperandText.tight(exp.toString, asBase = false)})"
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
        if !r.isFinite then
          _Complex.pow(_Number(b), _Number(e)).toRight(this)
        else Right(_Number(r))
      // Square dense matrix raised to an integer power: A^n by binary exponentiation
      // (A^0 = I, A^-n = (A⁻¹)^n). Non-square, non-integer exponent, or a singular base
      // under a negative power → stay symbolic. Precedes the generic _Value case because
      // _MatrixValue is itself a _Value and would otherwise hit the complex fallback.
      // An exact exponent arrives here too: the widening `_Number` reads a `_Rational`, so in
      // exact mode `A^2` keeps matrix power (a separate `_Rational` arm after this one was
      // unreachable, F_0213).  Exact matrix ENTRIES ride the symbolic `_Matrix`.
      case (Right(m: _MatrixValue), Right(_Number(e))) =>
        matrixPower(m, e).getOrElse(Left(this))
      // A matrix with exact entries stays a symbolic `_Matrix` rather than collapsing, so it
      // arrives here as a `Left`.  Every cell exact and the matrix square: the power is taken over
      // the rationals by `core.rationalMatrixPower` (F_0227), held to `MaxExactPowerBits` and
      // declined past it (the `_Rational.pow` rule), and the node rebuilt with the exact cells.
      // A mixed literal is inexact and is read as dense -- demoting beats losing `A^n`.
      case (Left(m: _MatrixShaped), Right(_Number(e))) if exactSquare(m) =>
        exactMatrixPower(m, e, env).getOrElse(Left(this))
      case (Left(m: _MatrixShaped), Right(_Number(e))) =>
        denseFromShaped(m).flatMap(mv => matrixPower(mv, e)).getOrElse(Left(this))
      case (Right(bv: _Value), Right(ev: _Value)) if eitherInterval(bv, ev) =>
        _Interval.power(bv, ev, env).toRight(Power(bv, ev))
      // Both operands are concrete: try principal complex power; None → stay symbolic.
      case (Right(bv: _Value), Right(ev: _Value)) =>
        _Complex.pow(bv, ev).toRight(Power(bv, ev))
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
      case Some(k) => b.pow(k.toInt, env.rationalPolicy, env.limits(Limit.MaxExactPowerBits))
                        .toRight(this)
      case None    => fractionalPow(b, e, env)

  /** A rational base to a genuinely fractional power.
   *
   *  Not closed over the rationals — `2^(1/2)` is irrational — so it is computed and
   *  re-approximated to the working precision, like every transcendental.  A positive base
   *  goes through spire's `Real`, which is what lets a high working precision
   *  mean something here; a negative base with a fractional exponent is the complex case and
   *  keeps the `Double` route, which owns the principal-value fallback.
   *
   *  @param b   the exact base
   *  @param e   the exact, non-integer exponent
   *  @param env supplies the working precision
   *  @return the power at the working precision, or `Left(this)` when undefined
   */
  private def fractionalPow(b: _Rational, e: _Rational, env: Environment): Either[_Expression, _Value] =
    def viaDouble: Either[_Expression, _Value] =
      val r = pow(b.toDouble, e.toDouble)
      if !r.isFinite then
        _Complex.pow(_Number(b.toDouble), _Number(e.toDouble)).toRight(this)
      else _Rational.fromApproximation(r, env.workingPrecision).toRight(this)

    if b.signum <= 0 || env.workingPrecision <= _Rational.DoubleReliableDigits then viaDouble
    else exactPow(b, e, env.workingPrecision).map(Right(_)).getOrElse(viaDouble)

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
    /* 2^62: `toLong` and its negation are exact below it. */
    val MaxMatrixExponent = 4.611686018427387904e18
    // Past 2^62 `toLong` saturates and `-k` can overflow, so `A^(-1e19)` became the identity
    // (F_0197); such a power stays symbolic.
    if m.rows != m.cols || e.isInfinite || e != math.floor(e) || math.abs(e) > MaxMatrixExponent then None
    else
      val k = e.toLong
      if k >= 0 then Some(powInt(m, k).guarded(this))
      else m.inverse.map(inv => powInt(inv, -k).guarded(this))

  /** `m^e` over the rationals when `m` is square with every cell exact and `e` whole (F_0227);
   *  `None` otherwise, or for a singular base to a negative power, or past
   *  `Limit.MaxExactPowerBits`.  A singular base declines here and again on the dense path,
   *  so the node stays unevaluated either way.
   *
   *  @param m   the exact square matrix node
   *  @param e   the exponent
   *  @param env supplies the reduction policy and the size budget
   *  @return the exact power, rebuilt on `m`'s own node */
  private def exactMatrixPower(m: _MatrixShaped, e: Double, env: Environment): Option[Either[_Expression, _Value]] =
    val cells = m.children.collect { case r: _Rational => r }.toVector
    val MaxMatrixExponent = 4.611686018427387904e18   // the dense path's bound, for the same reason
    if !Whole.isWhole(e) || math.abs(e) > MaxMatrixExponent then None
    else rationalMatrixPower(cells, m.rows, e.toLong, env.rationalPolicy, env.limits(Limit.MaxExactPowerBits))
      .map(c => Left(m.rebuild(c.toList)))

  /** Whether `m` is square with every cell exact: the operand [[exactMatrixPower]] takes. */
  private def exactSquare(m: _MatrixShaped): Boolean =
    m.rows == m.cols && m.children.nonEmpty && m.children.forall(_.isInstanceOf[_Rational])

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

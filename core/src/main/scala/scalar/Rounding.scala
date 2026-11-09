package it.grypho.scala.leonardo
package scalar

import core.*


/** The integer kernels behind `floor`, `ceil`, `round` and `mod` (F_0324, LT L_0080).
 *
 *  Every kernel is exact where its operands are: a `Double` rounds to the `Double` the hardware
 *  holds, a rational to the exact integer, a surd to the exact integer too -- decided by integer
 *  arithmetic on `a + b·√d`, never by reading the surd at the working precision, which for
 *  `√(10⁸⁰ − 1)` is `10⁴⁰` itself while the floor is one less.
 *
 *  **`round` goes half away from zero** (`round(2.5)` is `3`, `round(-2.5)` is `-3`), the convention
 *  of a mathematics course, of C's `round`, MATLAB and Excel; Python and numpy go half to even.  It
 *  is stated because it cannot be inferred and the two disagree on every tie.
 */
private[scalar] object Rounding:

  /** `⌊x⌋`; `+ 0.0` turns a negative zero (`ceil(-0.5)`) into the zero it should read as. */
  def floorD(x: Double): Double = math.floor(x) + 0.0

  /** `⌈x⌉`. */
  def ceilD(x: Double): Double = math.ceil(x) + 0.0

  /** The nearest integer to `x`, a tie going away from zero.  The fractional part is formed by
   *  subtracting the floor, which is exact, so the largest double below one half is not pushed to
   *  `1` by the rounding of `x + 0.5`. */
  def roundD(x: Double): Double =
    if !x.isFinite then x
    else
      val a = math.abs(x)
      val f = math.floor(a)
      val r = if a - f >= 0.5 then f + 1.0 else f
      (if x < 0.0 then -r else r) + 0.0

  /** `x - y·⌊x/y⌋`, the remainder with the sign of the divisor (Python's convention).
   *
   *  **Not computed from that formula**: once `x/y` passes `2⁵³` the quotient has no units digit and
   *  the difference is noise -- the conditioning rule's textbook form.  `x % y` is the exact
   *  remainder of the two doubles (representable by construction), and only its sign is adjusted.
   *  `None` for a zero divisor or a non-finite operand.
   */
  def modD(x: Double, y: Double): Option[Double] =
    if !x.isFinite || !y.isFinite || y == 0.0 then None
    else
      val r = x % y
      val m = if r != 0.0 && ((r < 0.0) != (y < 0.0)) then r + y else r
      Option.when(m.isFinite)(m + 0.0)

  /** A real kernel for [[Floor]] and its siblings: defined where `x` is finite. */
  def real(f: Double => Double)(x: Double): Option[_Value] =
    Option.when(x.isFinite)(_Number(f(x)))

  /** `⌊r⌋` of a rational.  `BigInt./%` truncates towards zero, so a negative remainder is corrected. */
  def floorOf(r: _Rational): BigInt =
    val (q, rem) = r.num /% r.den
    if rem.signum < 0 then q - 1 else q

  /** `⌈r⌉` of a rational. */
  def ceilOf(r: _Rational): BigInt = -floorOf(r.negate)

  /** The nearest integer to a rational, a tie going away from zero. */
  def roundOf(r: _Rational): BigInt =
    val (q, rem) = r.num.abs /% r.den
    val m = if rem * 2 >= r.den then q + 1 else q
    if r.num.signum < 0 then -m else m

  /** `⌊(n1 + n2·√d) / den⌋` for `n2 ≠ 0` and `d` not a perfect square, so that `n2·√d` is irrational.
   *
   *  With `t = ⌊|n2|·√d⌋ = isqrt(n2²·d)` the irrational term lies strictly between two integers, so
   *  the numerator lies strictly inside `(m, m + 1)` for the integer `m` below, and dividing by a
   *  positive `den` keeps it inside one integer's unit interval: the floor is `⌊m / den⌋`.
   */
  private def surdFloor(n1: BigInt, n2: BigInt, den: BigInt, d: BigInt): BigInt =
    val t = _Surd.isqrt(n2 * n2 * d)
    val m = if n2.signum >= 0 then n1 + t else n1 - t - 1
    val (q, rem) = m /% den
    if rem.signum < 0 then q - 1 else q

  /** `a + b·√d` as `(n1 + n2·√d) / den`, over the product of the two denominators. */
  private def split(s: _Surd): (BigInt, BigInt, BigInt) =
    (s.a.num * s.b.den, s.b.num * s.a.den, s.a.den * s.b.den)

  /** `⌊s⌋` of an exact surd. */
  def floorS(s: _Surd): BigInt =
    val (n1, n2, den) = split(s)
    surdFloor(n1, n2, den, s.d)

  /** `⌈s⌉`: a surd is never an integer, so one above the floor. */
  def ceilS(s: _Surd): BigInt = floorS(s) + 1

  /** The nearest integer to a surd.  There is no tie to break -- a surd is irrational -- so it is
   *  `⌊s + 1/2⌋`, read with the half folded into the numerator. */
  def roundS(s: _Surd): BigInt =
    val (n1, n2, den) = split(s)
    surdFloor(2 * n1 + den, 2 * n2, 2 * den, s.d)


/** The floor function `floor(e)` -- the greatest integer not above `e` (F_0324).
 *
 *  On the `Abs` template.  **Exact on exact input**: a rational or a surd answers the exact integer
 *  (a `_Rational`), the surd decided by integer arithmetic rather than by a reading at the working
 *  precision.  A real answers a `Double` that is a whole number; a dense matrix is mapped
 *  element-wise.  A complex or interval argument stays unevaluated, the `Asin` convention: neither
 *  has an order to round in.  `derive` is zero away from the integers, where the function jumps
 *  (`differentiable` excludes them).
 *
 *  @param e the argument expression
 */
case class Floor(e: _Expression) extends _Function with NamedFunction:
  override def name: String = "floor"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Floor(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    evalUnary(e, env)(Rounding.real(Rounding.floorD), exact = { case r => Right(_Rational(Rounding.floorOf(r))) })

  override protected def surdKernel: PartialFunction[_Surd, Either[_Expression, _Value]] =
    case s => Right(_Rational(Rounding.floorS(s)))


/** The ceiling function `ceil(e)` -- the least integer not below `e` (F_0324); see [[Floor]].
 *
 *  @param e the argument expression
 */
case class Ceil(e: _Expression) extends _Function with NamedFunction:
  override def name: String = "ceil"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Ceil(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    evalUnary(e, env)(Rounding.real(Rounding.ceilD), exact = { case r => Right(_Rational(Rounding.ceilOf(r))) })

  override protected def surdKernel: PartialFunction[_Surd, Either[_Expression, _Value]] =
    case s => Right(_Rational(Rounding.ceilS(s)))


/** The rounding function `round(e)` -- the nearest integer to `e`, **a tie going away from zero**
 *  (F_0324; see [[Rounding]] for why that convention); see [[Floor]] for the rest.
 *
 *  @param e the argument expression
 */
case class Round(e: _Expression) extends _Function with NamedFunction:
  override def name: String = "round"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Round(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    evalUnary(e, env)(Rounding.real(Rounding.roundD), exact = { case r => Right(_Rational(Rounding.roundOf(r))) })

  override protected def surdKernel: PartialFunction[_Surd, Either[_Expression, _Value]] =
    case s => Right(_Rational(Rounding.roundS(s)))


/** The remainder `mod(a, b) = a - b·⌊a/b⌋`, **with the sign of the divisor** (F_0324, Python's
 *  convention): `mod(-7, 3)` is `2` and `mod(7, -3)` is `-2`.
 *
 *  Exact on exact operands (evaluated from the definition, so a surd operand stays exact), and on
 *  `Double`s the exact remainder of the two values rather than the formula, which loses every digit
 *  once `a/b` passes `2⁵³`.  A zero divisor, a non-finite operand, a complex, interval or matrix
 *  operand stays unevaluated.  `derive` is `a' - b'·⌊a/b⌋`, away from the jumps.
 *
 *  @param a the dividend
 *  @param b the divisor
 */
case class Mod(a: _Expression, b: _Expression) extends _Function with NamedFunction:
  override def name: String = "mod"
  override def children: List[_Expression] = List(a, b)
  override def rebuild(c: List[_Expression]): _Expression = Mod(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    // a named constant is read at the working precision, as `floor(pi)` reads it (F_0289)
    def operand(x: _Expression): Either[_Expression, _Value] = x.eval(env).map(_Based.unwrapExact) match
      case Left(c) if c.freeVars.isEmpty && containsConstant(c) => foldConstants(c, env).eval(env).map(_Based.unwrapExact)
      case other                                                => other
    (operand(a), operand(b)) match
      case (Right(x: _Value), Right(y: _Value)) if _Surd.isExact(x) && _Surd.isExact(y) =>
        y match
          case r: _Rational if r.isZero => Left(Mod(x, y))
          case _ =>
            Sum(x, Product(_Rational(-1), Product(y, Floor(Ratio(x, y))))).eval(env) match
              case done @ Right(_) => done
              case _               => Left(Mod(x, y))
      case (Right(_Number(x)), Right(_Number(y))) =>
        Rounding.modD(x, y).map(v => _Number(v)).toRight(this)
      case (ra, rb) => Left(Mod(ra.toExpression, rb.toExpression))

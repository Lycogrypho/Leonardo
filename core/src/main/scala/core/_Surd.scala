package it.grypho.scala.leonardo
package core

import spire.math.Real
import spire.math.Rational as SpireRational


/** Companion for [[_Surd]]: the factory, the exact field arithmetic, and the readers.
 *
 *  **Every operation answers `Option`**: `None` means the result is not a quadratic surd over the
 *  same radical (`√2 + √3`, a fractional power of a surd), and the caller leaves the exact set
 *  the way it always has, through spire's `Real` to a working-precision rational
 *  ([[_Surd.approximateOp]]).  A surd is therefore never *less* exact than the rational that
 *  stood in its place before F_0231.
 */
object _Surd:

  /** Trial divisors tried when shortening a radicand: `k²` is moved out of `d` for every
   *  `k <= TrialRoot`.  **Display only** -- equality and the arithmetic never need a canonical
   *  radicand (see [[_Surd.equals]]), and full square-free extraction would need factoring, which
   *  a 30-digit discriminant puts out of reach. */
  private val TrialRoot: Int = 1000

  /** `⌊√n⌋` for `n >= 0` by Newton's iteration.  Our own because `BigInteger.sqrt` is a Java 9
   *  API the Scala.js library does not carry -- a method that compiles on one platform and is
   *  missing on the other is the `DoubleRender` trap.
   *  @param n a non-negative integer
   *  @return the integer square root
   */
  private[leonardo] def isqrt(n: BigInt): BigInt =
    if n.signum <= 0 then BigInt(0)
    else
      // Start above the root (2^ceil(bits/2) >= sqrt n); Newton then decreases monotonically.
      @annotation.tailrec
      def descend(x: BigInt): BigInt =
        val y = (x + n / x) >> 1
        if y >= x then x else descend(y)
      descend(BigInt(1) << ((n.bitLength + 1) / 2))

  /** The exact integer root of `n`, when `n` is a perfect square. */
  private[leonardo] def exactRoot(n: BigInt): Option[BigInt] =
    if n.signum < 0 then None else { val r = isqrt(n); Option.when(r * r == n)(r) }

  /** `a + b·√d` as the simplest exact value it equals: the rational `a` when `b = 0` or `d = 0`,
   *  the rational `a + b·r` when `d = r²`, else a surd with the small square factors of `d`
   *  moved into `b`.
   *
   *  @param a the rational part
   *  @param b the coefficient of the radical
   *  @param d the radicand
   *  @return the value, or `None` for a negative radicand (the complex case, not this type's)
   */
  def of(a: _Rational, b: _Rational, d: BigInt): Option[_Value] =
    if d.signum < 0 then None
    else if b.isZero || d.signum == 0 then Some(a)
    else exactRoot(d) match
      case Some(r) => Some(a.add(b.multiply(_Rational(r))))
      case None    =>
        val (coef, rest) = stripSquares(d)
        Some(new _Surd(a, b.multiply(_Rational(coef)), rest))

  /** `d` as `c²·m` with every `k²`, `k <= TrialRoot`, moved into `c`, **stopping once `k²` passes
   *  what is left** (F_0252): the loop used to run all 999 divisors whatever the radicand. */
  private def stripSquares(d: BigInt): (BigInt, BigInt) =
    @annotation.tailrec
    def loop(k: Int, c: BigInt, m: BigInt): (BigInt, BigInt) =
      val sq = BigInt(k) * k
      if k > TrialRoot || sq > m then (c, m)
      else if m % sq == 0 then loop(k, c * k, m / sq)
      else loop(k + 1, c, m)
    loop(2, BigInt(1), d)

  /** `a + b·√d` for a radicand taken from an existing surd (F_0252): already free of small squares
   *  and not a perfect square, so only the collapse when `b` vanishes is left -- [[of]]'s root test
   *  and trial division would find nothing, at the cost of an integer square root and up to 999
   *  divisions per operation.  `d = 1` is the radicand of two rationals aligned. */
  private def kept(a: _Rational, b: _Rational, d: BigInt): Option[_Value] =
    if b.isZero then Some(a)
    else if d == BigInt(1) then Some(a.add(b))
    else Some(new _Surd(a, b, d))

  /** The exact square root of a non-negative rational: `√(p/q) = √(p·q)/q`.
   *  @param r the radicand
   *  @return a rational for a perfect square, a surd otherwise, `None` when `r < 0`
   */
  def sqrtOf(r: _Rational): Option[_Value] =
    if r.signum < 0 then None
    else
      val (p, q) = _Rational.reduce(r.num, r.den)
      _Rational.of(BigInt(1), q).flatMap(inv => of(_Rational.Zero, inv, p * q))

  /** Whether `v` is exact in the sense of this type: a rational or a surd. */
  def isExact(v: _Expression): Boolean = v match
    case _: _Rational | _: _Surd => true
    case _                       => false

  /** `v` as `(a, b, d)`: a rational is `(r, 0, 1)`. */
  private def parts(v: _Value): Option[(_Rational, _Rational, BigInt)] = v match
    case r: _Rational => Some((r, _Rational.Zero, BigInt(1)))
    case s: _Surd     => Some((s.a, s.b, s.d))
    case _            => None

  /** Two exact values over one radical, `(a₁, b₁, a₂, b₂, d)`, when one exists.
   *
   *  `√d₂` is a rational multiple of `√d₁` exactly when `d₁·d₂` is a perfect square `s²`, and
   *  then `√d₂ = (s/d₁)·√d₁` -- the test that replaces a canonical radicand.
   */
  private def aligned(x: _Value, y: _Value): Option[(_Rational, _Rational, _Rational, _Rational, BigInt)] =
    for
      (a1, b1, d1) <- parts(x)
      (a2, b2, d2) <- parts(y)
      r <- if b1.isZero then Some((a1, b1, a2, b2, d2))
           else if b2.isZero then Some((a1, b1, a2, b2, d1))
           else exactRoot(d1 * d2).flatMap(s => _Rational.of(s, d1).map(k => (a1, b1, a2, b2.multiply(k), d1)))
    yield r

  /** `x + y`, exactly, when both share a radical. */
  def add(x: _Value, y: _Value): Option[_Value] =
    aligned(x, y).flatMap((a1, b1, a2, b2, d) => kept(a1.add(a2), b1.add(b2), d))

  /** `-x`. */
  def negate(x: _Value): Option[_Value] =
    parts(x).flatMap((a, b, d) => kept(a.negate, b.negate, d))

  /** `x − y`, exactly, when both share a radical. */
  def subtract(x: _Value, y: _Value): Option[_Value] = negate(y).flatMap(add(x, _))

  /** `x · y = (a₁a₂ + b₁b₂d) + (a₁b₂ + a₂b₁)√d`, exactly, when both share a radical. */
  def multiply(x: _Value, y: _Value): Option[_Value] =
    aligned(x, y).flatMap { (a1, b1, a2, b2, d) =>
      kept(a1.multiply(a2).add(b1.multiply(b2).multiply(_Rational(d))), a1.multiply(b2).add(a2.multiply(b1)), d)
    }

  /** `x / y`, by the conjugate: `x·(a₂ − b₂√d) / (a₂² − b₂²d)`.  The denominator vanishes only for
   *  `y = 0`, since `d` is not a perfect square.
   *  @return the quotient, or `None` for a zero divisor or radicals that do not combine
   */
  def divide(x: _Value, y: _Value): Option[_Value] =
    aligned(x, y).flatMap { (a1, b1, a2, b2, d) =>
      val den = a2.multiply(a2).subtract(b2.multiply(b2).multiply(_Rational(d)))
      if den.isZero then None
      else
        for
          conj <- kept(a2, b2.negate, d)
          num  <- multiply(x, conj)
          (na, nb, nd) <- parts(num)
          qa <- na.divide(den)
          qb <- nb.divide(den)
          q  <- kept(qa, qb, nd)
        yield q
    }

  /** `x^k` for an integer `k`, held to `maxBits` before any product is formed (the
   *  `_Rational.pow` rule): the estimate is the largest part's bit length times `|k|`.
   *  @return the power, or `None` past the budget or for a negative power of zero
   */
  def pow(x: _Value, k: Int, maxBits: Long): Option[_Value] =
    parts(x).flatMap { (a, b, d) =>
      val bits = math.max(math.max(a.maxBitLength, b.maxBitLength), d.bitLength).toLong
      if k != 0 && bits * math.abs(k.toLong) > maxBits then None
      else if k < 0 then pow(x, -k, maxBits).flatMap(p => divide(_Rational.One, p))
      else
        @annotation.tailrec
        def loop(base: _Value, e: Int, acc: _Value): Option[_Value] =
          if e == 0 then Some(acc)
          else
            val acc1 = if (e & 1) == 1 then multiply(acc, base) else Some(acc)
            val sq   = if e > 1 then multiply(base, base) else Some(base)
            (acc1, sq) match
              case (Some(a1), Some(s1)) => loop(s1, e >> 1, a1)
              case _                    => None
        loop(x, k, _Rational.One)
    }

  /** The sign of an exact value, decided exactly: when `a` and `b` disagree, by comparing
   *  `a²` with `b²d`, which cannot tie since `d` is not a perfect square.
   *  @return `-1`, `0` or `1`, or `None` for a value of another kind
   */
  def signum(x: _Value): Option[Int] = parts(x).map { (a, b, d) =>
    if b.isZero then a.signum
    else if a.signum == 0 || a.signum == b.signum then b.signum
    else if a.multiply(a) > b.multiply(b).multiply(_Rational(d)) then a.signum
    else b.signum
  }

  /** The order of two exact values, decided exactly: by the sign of the difference over one
   *  radical, else by reading the difference at growing precision, which ends because two
   *  exact values that are not combinable are not equal.
   *  @return `-1`, `0` or `1`, or `None` when either is not exact
   */
  def compareExact(x: _Value, y: _Value): Option[Int] =
    subtract(x, y).flatMap(signum).orElse {
      for rx <- toReal(x); ry <- toReal(y)
      yield
        val diff = rx - ry
        Iterator.iterate(256)(_ * 2).take(8).map(bits => diff.toRational(bits).signum).find(_ != 0).getOrElse(0)
    }

  /** The value as spire's `Real`, losing nothing. */
  def toReal(x: _Value): Option[Real] = parts(x).map { (a, b, d) =>
    core.toReal(a) + core.toReal(b) * Real(SpireRational(d)).sqrt
  }

  /** An exact value as a rational at `digits` places: a rational as itself, a surd as the
   *  working-precision approximation it stood for before F_0231.  What every computation that
   *  needs a rational reads a surd through, so none demotes it to `Double`.
   *  @param v      the value
   *  @param digits the working precision
   *  @return the rational, or `None` for a value that is not exact
   */
  def exactOf(v: _Expression, digits: Int): Option[_Rational] = v match
    case r: _Rational => Some(r)
    case s: _Surd     => toReal(s).flatMap(fromReal(_, digits))
    case _            => None

  /** Whether `x` and `y` are both exact and at least one is a surd: the gate on the operator
   *  arms, which keep their faster rational-rational arm first. */
  def involves(x: _Value, y: _Value): Boolean =
    isExact(x) && isExact(y) && (x.isInstanceOf[_Surd] || y.isInstanceOf[_Surd])

  /** A binary operation on two exact values: the exact field operation when it applies, else
   *  [[approximateOp]] -- so an operand pair outside one radical is answered exactly as before.
   *  @param x      the left operand
   *  @param y      the right operand
   *  @param digits the working precision of the approximation
   *  @param exact  the field operation, `None` outside one radical
   *  @param real   the same operation on `Real`s
   *  @return the result, or `None` when it is undefined (a division by zero)
   */
  def combine(x: _Value, y: _Value, digits: Int)(exact: (_Value, _Value) => Option[_Value],
                                                 real: (Real, Real) => Real): Option[_Value] =
    exact(x, y).orElse(approximateOp(x, y, digits)(real))

  /** `op` on two exact values outside the surd field, the way the exact tier always left it:
   *  through `Real`, back to a rational at `digits`.
   *  @return the approximation, or `None` when either value is not exact
   */
  def approximateOp(x: _Value, y: _Value, digits: Int)(op: (Real, Real) => Real): Option[_Rational] =
    for
      rx <- toReal(x)
      ry <- toReal(y)
      r  <- fromReal(op(rx, ry), digits)
    yield r



/** An exact real `a + b·√d` (F_0231): `a`, `b` rational, `d` a positive integer that is not a
 *  perfect square, `b != 0`.
 *
 *  The **sixth sibling** of [[_Number]], after `_Complex`, `_Truth`, `_Rational`, `_Based` and
 *  `_Interval`, and like `_Rational` it **widens**: `case _Number(x)` reads it as the real number it
 *  is, so a site that knows nothing of surds computes in `Double` -- float contagion, the safe
 *  direction -- while exactness is opt-in.  Built only through [[_Surd.of]].
 *
 *  **Why one radical, not the field of all square roots**: `a + b√d` is closed under `+ − × ÷`
 *  over a fixed `d` and covers every quadratic root and every 2-norm; `√2 + √3` leaves it, and
 *  leaves the exact set the way every irrational always did.
 *
 *  **Equality without a canonical radicand**: `a + b√d = a' + b'√d'` iff `a = a'`, `b` and `b'`
 *  share a sign, and `b²d = b'²d'` -- all exact, no factoring.  `hashCode` hashes exactly those.
 *
 *  @param a the rational part
 *  @param b the coefficient of the radical, never zero
 *  @param d the radicand, a positive non-square integer
 */
final class _Surd private (val a: _Rational, val b: _Rational, val d: BigInt) extends _Value:

  /** Returns `Right(this)` -- a concrete value needs no further reduction. */
  override def eval(env: Environment): Either[_Expression, _Value] = Right(this)
  override def children: List[_Expression] = List.empty
  override def rebuild(c: List[_Expression]): _Expression = this

  /** `b²d`, the invariant equality compares. */
  private def radicalSize: _Rational = b.multiply(b).multiply(_Rational(d))

  /** Value equality, decided exactly (see the class note). */
  override def equals(that: Any): Boolean = that match
    case s: _Surd => a == s.a && b.signum == s.b.signum && radicalSize == s.radicalSize
    case _        => false

  override def hashCode: Int = (a, b.signum, radicalSize).hashCode

  /** The value as a `Double`, through `Real` so a large radicand does not overflow on the way.
   *
   *  **Never spire's `Real.toDouble`** (F_0244): it misrounded a surd with a large radicand and a
   *  tiny coefficient, so `norm([[√2], [√2]])` read as `1.0`, and it lost relative precision on a
   *  value small only after cancellation (`√(10⁶⁰ + 1) − 10³⁰`).  The value is read instead as a
   *  rational at absolute precision `2⁻ᵇ` with `b` doubling until it carries 64 significant bits;
   *  that ends, because a surd is never zero (`√d` is irrational), and a value below the smallest
   *  subnormal stops the loop as `0.0` with its sign. */
  def toDouble: Double = _Surd.toReal(this).fold(Double.NaN) { r =>
    Iterator.iterate(128)(_ * 2).takeWhile(_ <= 4096).map(bits => (bits, r.toRational(bits)))
      .collectFirst { case (bits, q) if q.abs * SpireRational(BigInt(2).pow(bits - 64)) >= 1 => q.toDouble }
      .getOrElse(0.0)
  }

  /** Renders at [[Environment.DefaultPrecision]], as [[display]] does. */
  override def toString: String = display(Environment.DefaultPrecision)

  /** `3 + 2√13`, `√2/2`, `-√5`: the form a reader writes and the grammar's `√` prefix reads back
   *  (Decision A).  A value with a part past [[_Rational.MaxDisplayDigits]] shows as a decimal at
   *  `precision` places, the `_Rational` rule; `:save` writes [[exact]] whatever is shown.
   *  @param precision decimal places for the decimal form
   *  @return the text
   */
  def display(precision: Int): String =
    val (ra, sa) = _Rational.reduce(a.num, a.den)
    val (p, q)   = _Rational.reduce(b.num, b.den)
    val long = List(ra, sa, p, q, d).exists(n => n.abs.toString.length > _Rational.MaxDisplayDigits)
    if long then _Number.render(_Number.round(toDouble, precision))
    else
      val root = if p.abs == BigInt(1) then s"√$d" else s"${p.abs}√$d"
      val term = if q == BigInt(1) then root else s"$root/$q"
      val sign = if p.signum < 0 then "-" else ""
      if ra.signum == 0 then s"$sign$term"
      else s"(${a.display(precision)} ${if p.signum < 0 then "-" else "+"} $term)"

  /** The grammar's ASCII form, always exact: `(a + b * d^(1/2))`.  What `:save` writes (Decision
   *  A), re-read in exact mode as this same surd. */
  def exact: String =
    val term = if b == _Rational.One then s"$d^(1/2)" else s"${b.exact} * $d^(1/2)"
    if a.isZero then s"($term)" else s"(${a.exact} + $term)"

package it.grypho.scala.leonardo
package core

import scala.annotation.tailrec


/** When a [[_Rational]] reduces itself to lowest terms.
 *
 *  The policy changes **representation only, never value**: `2/6` and `1/3` are the same
 *  rational, and [[_Rational.equals]] compares by cross-multiplication precisely so that
 *  the choice stays invisible to callers.  What it does change is cost — `gcd` is the
 *  expensive step of rational arithmetic, while skipping it roughly *doubles* operand size
 *  per operation — which is why it is a measured decision (issue 4.M) rather than a guess.
 *
 *  Selected per call site rather than through [[Environment]], following the
 *  `simplifyLogic(e, symmetric, semantics)` precedent: nothing in this tier goes through
 *  `eval` yet, so an environment field would be unread.  It moves into `Environment` with
 *  the tier-1 AST integration (issue 4.L), alongside the working precision.
 */
enum GcdPolicy:
  /** Reduce on every construction — what spire's `Rational` does, and the conventional
   *  choice.  Pays one `gcd` per operation to keep operands minimal.
   */
  case Eager
  /** Never reduce.  Relies on the bounded-denominator re-approximation
   *  ([[_Rational.approximate]]) to cap growth instead of on `gcd`.
   */
  case Lazy
  /** Reduce only once an operand grows past `maxBits` bits — the middle policy, and in
   *  production rational libraries usually the one that wins.
   *
   *  @param maxBits bit-length above which either term triggers a reduction
   */
  case Threshold(maxBits: Int)


/** Companion for [[_Rational]]: smart factories, constants, and the shared reduction step. */
object _Rational:

  /** The additive identity, `0/1`. */
  val Zero: _Rational = new _Rational(BigInt(0), BigInt(1))

  /** The multiplicative identity, `1/1`. */
  val One: _Rational = new _Rational(BigInt(1), BigInt(1))

  /** Default bit-length bound for [[GcdPolicy.Threshold]], chosen by the 4.M benchmark.
   *
   *  It has to clear the operand size a *working precision* implies, or the policy fires on
   *  every operation and is [[GcdPolicy.Eager]] under another name — the benchmark shows
   *  this directly, with a 64-bit bound at 30 working digits reproducing Eager's operand
   *  sizes exactly (105 / 30 bits) and a 256-bit bound reproducing Lazy's (186 / 75).  A
   *  30-digit value needs `30 · log2 10 ≈ 100` bits and a product of two reaches ~200, so
   *  256 clears it with headroom.  See [[thresholdFor]] for the scaling rule at other
   *  precisions.
   */
  val DefaultThresholdBits: Int = 256

  /** The reduction policy chosen by the 4.M benchmark, and what a caller with no opinion
   *  should get.
   *
   *  [[GcdPolicy.Lazy]] was rejected on the exact arm — where nothing re-approximates,
   *  which is the *normal* mode for the closed operations (`Sum`, `Product`, `Ratio`,
   *  integer `Power`, and all of `matrix`) rather than a synthetic control.  There it
   *  reached 2.5 million bits and 2.65 GB on an 8×8 Hilbert solve against Eager's 30 bits,
   *  an 83 000× operand-size ratio.  Threshold keeps Lazy's cheapness wherever
   *  re-approximation is already bounding growth, and behaves as a guard where it is not.
   */
  val DefaultPolicy: GcdPolicy = GcdPolicy.Threshold(DefaultThresholdBits)

  /** The reduction bound appropriate to a given working precision.
   *
   *  `8 · digits` is a little over twice the `digits · log2 10 ≈ 3.32 · digits` bits one
   *  re-approximated operand occupies, which is what a *product* of two of them needs
   *  before reduction is worth paying for.  Floored at [[DefaultThresholdBits]] so a low
   *  working precision does not collapse the policy back onto [[GcdPolicy.Eager]].
   *
   *  @param digits the working precision in decimal digits
   *  @return a [[GcdPolicy.Threshold]] sized for that precision
   */
  def thresholdFor(digits: Int): GcdPolicy =
    GcdPolicy.Threshold(math.max(DefaultThresholdBits, 8 * digits))

  /** Builds a rational from a numerator and denominator.
   *
   *  Returns `None` for a zero denominator rather than throwing, following the house rule
   *  that an undefined result leaves the caller symbolic instead of propagating an
   *  infinity (the same contract as `SpecialFunctions`' kernels).
   *
   *  @param n      the numerator
   *  @param d      the denominator
   *  @param policy the reduction policy applied to the result
   *  @return the rational `n/d`, or `None` when `d` is zero
   */
  def of(n: BigInt, d: BigInt, policy: GcdPolicy = DefaultPolicy): Option[_Rational] =
    if d.signum == 0 then None else Some(make(n, d, policy))

  /** Builds a rational from an integer.
   *  @param n the integer value
   *  @return `n/1`
   */
  def apply(n: BigInt): _Rational = new _Rational(n, BigInt(1))

  /** Builds a rational from an `Int`.
   *  @param n the integer value
   *  @return `n/1`
   */
  def apply(n: Int): _Rational = new _Rational(BigInt(n), BigInt(1))

  /** Longest numerator or denominator, in decimal digits, still shown as a fraction.
   *
   *  Beyond this a `_Rational` displays as a decimal instead.  The bound exists because the
   *  two things a rational can be are very different to read: `1/3` is clearer as a
   *  fraction than as `0.33333`, while a 30-digit rational approximation of `pi` is a
   *  31-over-31-digit wall that tells the reader nothing.  Display only — `Session`
   *  serializes the exact fraction regardless, so `:save` never loses the value.
   */
  val MaxDisplayDigits: Int = 12

  /** Converts a `Double` to the exact rational it denotes.
   *
   *  Every finite `Double` *is* a dyadic rational, so this conversion is lossless — it
   *  recovers the value the hardware actually holds, not the decimal literal that was
   *  written.  `fromDouble(0.1)` is therefore `3602879701896397/36028797018963968`, which
   *  is the honest answer and the reason a number that was already parsed as a `Double`
   *  cannot be repaired after the fact — hence the parser's own exact mode, and hence
   *  [[fromDecimalString]] beside this.
   *
   *  @param d the value to convert
   *  @return the exact rational, or `None` when `d` is NaN or infinite
   */
  def fromDouble(d: Double): Option[_Rational] =
    if d.isNaN || d.isInfinite then None else Some(fromBigDecimal(new java.math.BigDecimal(d)))

  /** Converts a *decimal literal* to the exact rational it denotes.
   *
   *  The counterpart of [[fromDouble]], and the one the parser uses.  The distinction is
   *  the whole reason exact mode has to live at parse time: `fromDecimalString("0.1")` is
   *  `1/10`, while `fromDouble(0.1)` is the dyadic the hardware rounded it to.  Only the
   *  first makes `0.1 + 0.2 == 0.3` come out exactly.
   *
   *  Exponents are handled exactly too, so `1e-320` is `1/10^320` rather than the `Double`
   *  denormal it would otherwise become.
   *
   *  @param s the literal as written
   *  @return the exact rational, or `None` when `s` is not a decimal literal
   */
  def fromDecimalString(s: String): Option[_Rational] =
    try Some(fromBigDecimal(new java.math.BigDecimal(s)))
    catch case _: NumberFormatException => None

  /** Significant decimal digits a `Double` actually carries.
   *
   *  The ceiling on [[fromApproximation]], and an honest one: an operation that leaves the
   *  rationals — every transcendental, and any fractional power — is currently evaluated in
   *  `Double` and re-approximated, so asking for thirty digits of `sin(1/3)` would
   *  manufacture fifteen digits that are not there.  Lifting this needs an arbitrary
   *  precision engine for the functions themselves (issue 4.L tier 2, spire's `Real`); the
   *  working precision meanwhile bounds the *arithmetic* around them, which is where the
   *  cancellation lives.
   */
  val DoubleReliableDigits: Int = 15

  /** Brings a `Double` result of a non-closed operation back into the exact tier.
   *
   *  Used wherever the rationals are left and re-entered — fractional powers and the
   *  transcendental functions.  The requested precision is capped at
   *  [[DoubleReliableDigits]] so the result claims no more accuracy than its source had;
   *  without that cap the "approximation" would faithfully reproduce the `Double`'s dyadic
   *  expansion and call it thirty digits.
   *
   *  @param d      the computed value
   *  @param digits the working precision requested
   *  @return the rational approximation, or `None` when `d` is NaN or infinite
   */
  def fromApproximation(d: Double, digits: Int): Option[_Rational] =
    fromDouble(d).map(_.approximateToDigits(math.min(digits, DoubleReliableDigits)))

  /** Shared exact extraction: a `BigDecimal` is `unscaled · 10^-scale` by construction. */
  private def fromBigDecimal(bd: java.math.BigDecimal): _Rational =
    val scale = bd.scale
    val un    = BigInt(bd.unscaledValue)
    if scale >= 0 then make(un, BigInt(10).pow(scale), GcdPolicy.Eager)
    else new _Rational(un * BigInt(10).pow(-scale), BigInt(1))

  /** Reduces `n/d` to lowest terms.  `gcd` is non-negative and `gcd(0, d) == d`, so a zero
   *  numerator normalises to `0/1`.
   */
  private[core] def reduce(n: BigInt, d: BigInt): (BigInt, BigInt) =
    val g = n.gcd(d)
    if g == BigInt(1) then (n, d) else (n / g, d / g)

  /** Normalises the sign into the numerator and applies `policy`; assumes `d != 0`. */
  private[core] def make(n: BigInt, d: BigInt, policy: GcdPolicy): _Rational =
    val (sn, sd) = if d.signum < 0 then (-n, -d) else (n, d)
    val (rn, rd) = policy match
      case GcdPolicy.Eager             => reduce(sn, sd)
      case GcdPolicy.Lazy              => (sn, sd)
      case GcdPolicy.Threshold(maxBits) =>
        if sn.bitLength > maxBits || sd.bitLength > maxBits then reduce(sn, sd) else (sn, sd)
    new _Rational(rn, rd)


/** An exact rational number, held as an unreduced `BigInt` pair with the sign in the
 *  numerator and a strictly positive denominator.
 *
 *  A **sibling** [[_Value]] of [[_Number]], exactly as [[_Complex]] and [[_Truth]] are —
 *  `_Number` is not replaced and not widened, so every existing `_Number(x)` pattern match
 *  keeps firing unchanged and the `Double` path stays byte-identical.  A `_Rational` only
 *  ever enters an expression through the parser's exact mode; see `Environment` for the
 *  working precision that bounds it.
 *
 *  **The promotion lattice** (the `Int → Double` analogy `logic.asTruth` already uses):
 *  rational ⊕ rational is exact, rational ⊕ number is a `_Number` — *float contagion*.
 *  Contagion is deliberate and it is the asymmetric choice: absorbing the `_Number` into
 *  the rational would be perfectly lossless, since every finite `Double` is a dyadic
 *  rational, but it would launder representation error into an exact-looking value.  Once
 *  a value is inexact it should stay visibly inexact.  The contagion needs no code of its
 *  own: [[_Complex.parts]] reads a `_Rational` as a `Double`, so the existing complex
 *  kernels produce it for free.
 *
 *  **Why a private `BigInt` pair rather than spire's `Rational`.**  spire normalises to
 *  lowest terms on every construction, which would have settled [[GcdPolicy]] at
 *  [[GcdPolicy.Eager]] by fiat and made the 4.M benchmark unrunnable.  spire remains the
 *  intended engine for *irrationals* (`Real` / `Algebraic`, issue 4.L tier 2); the
 *  rational representation stays in-house.
 *
 *  Equality is by value, not by representation: `2/6 == 1/3` holds under every policy.
 *  `hashCode` therefore has to reduce, which means a [[GcdPolicy.Lazy]] rational used as a
 *  hash key pays the `gcd` it was avoiding.
 *
 *  @param num the numerator, carrying the sign
 *  @param den the denominator, always strictly positive
 */
final class _Rational private (val num: BigInt, val den: BigInt) extends _Value with Ordered[_Rational]:

  /** Returns `Right(this)` — a concrete rational needs no further reduction.
   *  @param env unused; a literal value is independent of the bindings
   */
  override def eval(env: Environment): Either[_Expression, _Value] = Right(this)

  override def children: List[_Expression] = List.empty
  override def rebuild(c: List[_Expression]): _Expression = this

  /** Sum: `(a·d' + a'·d) / (d·d')`.
   *
   *  Deliberately the naive cross-multiplication rather than the usual
   *  `gcd(d, d')` refinement — that refinement is an eager-flavoured optimisation and
   *  would confound the two arms of the 4.M benchmark, which must vary reduction alone.
   *
   *  @param that   the addend
   *  @param policy reduction policy for the result
   *  @return the sum
   */
  def add(that: _Rational, policy: GcdPolicy = _Rational.DefaultPolicy): _Rational =
    _Rational.make(num * that.den + that.num * den, den * that.den, policy)

  /** Difference: `(a·d' − a'·d) / (d·d')`.
   *  @param that   the subtrahend
   *  @param policy reduction policy for the result
   *  @return the difference
   */
  def subtract(that: _Rational, policy: GcdPolicy = _Rational.DefaultPolicy): _Rational =
    _Rational.make(num * that.den - that.num * den, den * that.den, policy)

  /** Product: `(a·a') / (d·d')`.
   *  @param that   the multiplier
   *  @param policy reduction policy for the result
   *  @return the product
   */
  def multiply(that: _Rational, policy: GcdPolicy = _Rational.DefaultPolicy): _Rational =
    _Rational.make(num * that.num, den * that.den, policy)

  /** Quotient: `(a·d') / (d·a')`.
   *  @param that   the divisor
   *  @param policy reduction policy for the result
   *  @return the quotient, or `None` when `that` is zero
   */
  def divide(that: _Rational, policy: GcdPolicy = _Rational.DefaultPolicy): Option[_Rational] =
    if that.isZero then None
    else Some(_Rational.make(num * that.den, den * that.num, policy))

  /** Integer power.
   *  @param k      the exponent; negative exponents invert
   *  @param policy reduction policy for the result
   *  @return `this^k`, or `None` for a negative power of zero
   */
  def pow(k: Int, policy: GcdPolicy = _Rational.DefaultPolicy): Option[_Rational] =
    if k >= 0 then Some(_Rational.make(num.pow(k), den.pow(k), policy))
    else if isZero then None
    else Some(_Rational.make(den.pow(-k), num.pow(-k), policy))

  /** The additive inverse. */
  def negate: _Rational = new _Rational(-num, den)

  /** The multiplicative inverse.
   *  @param policy reduction policy for the result
   *  @return `d/a`, or `None` when this is zero
   */
  def reciprocal(policy: GcdPolicy = _Rational.DefaultPolicy): Option[_Rational] =
    if isZero then None else Some(_Rational.make(den, num, policy))

  /** The absolute value. */
  def abs: _Rational = if num.signum < 0 then negate else this

  /** `-1`, `0` or `1` according to the sign. */
  def signum: Int = num.signum

  /** Whether this is exactly zero. */
  def isZero: Boolean = num.signum == 0

  /** This value as an exact integer, when it is one.
   *
   *  Reduces first, because a policy that defers `gcd` can leave an integer-valued rational
   *  looking like `6/3` — testing `den == 1` on the raw representation would miss it and
   *  silently downgrade an exact integer power to an approximation.
   *
   *  @return `Some(n)` when this is the integer `n`, `None` otherwise
   */
  def toBigIntExact: Option[BigInt] =
    val (rn, rd) = _Rational.reduce(num, den)
    if rd == BigInt(1) then Some(rn) else None

  /** The larger of the two terms' bit lengths — the size measure the 4.M benchmark
   *  records, since it is what explains the wall-clock and predicts behaviour at
   *  precisions that were not measured.
   */
  def maxBitLength: Int = math.max(num.bitLength, den.bitLength)

  /** Best rational approximation to this value with a denominator not exceeding `maxDen`.
   *
   *  This is the step that makes *bounded-denominator rational arithmetic* work: it is what
   *  keeps operands from growing without limit, and under [[GcdPolicy.Lazy]] it is the
   *  **only** thing that does.  The algorithm is the classical continued-fraction
   *  (Stern–Brocot) truncation — walk the convergents `hᵢ/kᵢ` until the denominator would
   *  exceed the bound, then choose between the last convergent and the best *semiconvergent*
   *  that still fits, whichever lies closer.  Both are already in lowest terms, so no
   *  further reduction is needed.
   *
   *  Depends only on the *value*, never on the representation, so every [[GcdPolicy]]
   *  approximates identically — the invariant the cross-policy equality test rests on.
   *
   *  @param maxDen the largest denominator allowed; values below `1` leave this unchanged
   *  @return the closest rational with denominator `≤ maxDen`
   *  @see [[https://en.wikipedia.org/wiki/Continued_fraction Continued fraction]]
   *  @see [[https://en.wikipedia.org/wiki/Stern%E2%80%93Brocot_tree Stern–Brocot tree]]
   */
  def approximate(maxDen: BigInt): _Rational =
    if maxDen < BigInt(1) then this
    else
      val (rn, rd) = _Rational.reduce(num, den)
      if rd <= maxDen then new _Rational(rn, rd)
      else
        val an = rn.abs
        // Closeness of h/k to an/rd, compared without leaving the integers:
        // |h/k - an/rd| < |h'/k' - an/rd|  <=>  |h*rd - an*k| * k' < |h'*rd - an*k'| * k
        def closer(h: BigInt, k: BigInt, h2: BigInt, k2: BigInt): Boolean =
          (h * rd - an * k).abs * k2 < (h2 * rd - an * k2).abs * k

        // n/d is the tail of the continued fraction still to expand; hp/kp is the previous
        // convergent and hp2/kp2 the one before it, seeded with the standard 1/0 and 0/1.
        @tailrec
        def walk(n: BigInt, d: BigInt,
                 hp: BigInt, kp: BigInt, hp2: BigInt, kp2: BigInt): (BigInt, BigInt) =
          val a = n / d
          val h = a * hp + hp2
          val k = a * kp + kp2
          if k > maxDen then
            // The convergent overshoots the bound; the best that fits is the semiconvergent
            // with the largest t < a such that t*kp + kp2 <= maxDen.  kp >= 1 here: the
            // first convergent has k = 1 <= maxDen, so this branch is never the first step.
            val t  = (maxDen - kp2) / kp
            val hs = t * hp + hp2
            val ks = t * kp + kp2
            if closer(hs, ks, hp, kp) then (hs, ks) else (hp, kp)
          else
            val r = n - a * d
            if r.signum == 0 then (h, k) else walk(d, r, h, k, hp, kp)

        val (h, k) = walk(an, rd, BigInt(1), BigInt(0), BigInt(0), BigInt(1))
        new _Rational(if rn.signum < 0 then -h else h, k)

  /** Best rational approximation with a denominator not exceeding `10^digits`.
   *
   *  The working-precision form of [[approximate]]: `digits` is the number of decimal
   *  places the approximation is good for.
   *
   *  @param digits the working precision in decimal digits
   *  @return the closest rational at that precision
   */
  def approximateToDigits(digits: Int): _Rational =
    approximate(BigInt(10).pow(math.max(digits, 0)))

  /** This value as a `BigDecimal` at `digits` significant digits.
   *  @param digits the significant digits to produce (at least 1)
   *  @return the decimal expansion
   */
  def toBigDecimal(digits: Int): BigDecimal =
    val mc = new java.math.MathContext(math.max(digits, 1))
    BigDecimal(new java.math.BigDecimal(num.bigInteger).divide(new java.math.BigDecimal(den.bigInteger), mc))

  /** This value as a `Double`, via a 17-digit decimal expansion so that a numerator or
   *  denominator too large for `Double` does not become an infinity on the way.
   */
  def toDouble: Double = toBigDecimal(17).toDouble

  /** Orders by value, valid because the denominator is always strictly positive.
   *  @param that the rational to compare against
   *  @return a negative, zero or positive `Int` as usual
   */
  override def compare(that: _Rational): Int = (num * that.den).compare(that.num * den)

  /** Value equality, by cross-multiplication — so representation never leaks.
   *  @param that the object to compare against
   */
  override def equals(that: Any): Boolean = that match
    case r: _Rational => num * r.den == r.num * den
    case _            => false

  /** Hashes the *reduced* form, as value equality requires.  Note this forces the `gcd` a
   *  [[GcdPolicy.Lazy]] rational was avoiding.
   */
  override def hashCode: Int =
    val (rn, rd) = _Rational.reduce(num, den)
    31 * rn.hashCode + rd.hashCode

  /** The exact fraction, always, in lowest terms: `n/d`, or `n` when the denominator is 1.
   *
   *  The **serialization** form, kept apart from [[display]] on purpose.  `Session` writes
   *  this into a `:save` script, so a rational that displays as a rounded decimal still
   *  round-trips exactly — the same split that lets a graded truth value display as
   *  `unknown` while saving as a portable literal.
   *
   *  Reduces first, so the reduction policy in force never leaks into a saved script.
   */
  def exact: String =
    val (rn, rd) = _Rational.reduce(num, den)
    if rd == BigInt(1) then rn.toString else s"$rn/$rd"

  /** Renders at [[Environment.DefaultPrecision]] decimal places, matching [[_Number]]. */
  override def toString: String = display(Environment.DefaultPrecision)

  /** Renders this rational for reading: the exact fraction when it is short enough to take
   *  in, a decimal at `precision` places otherwise.
   *
   *  The two things a rational can be are very different to read.  `1/3` says more as a
   *  fraction than `0.33333` does; a rational approximation of `pi` at 30 working digits
   *  says nothing at all as `31415.../10000...`, and everything as `3.14159`.  So the
   *  threshold is [[_Rational.MaxDisplayDigits]] digits on either term, applied to the
   *  *reduced* form so an unreduced representation cannot push a short value over it.
   *
   *  This is display only.  [[exact]] is what gets serialized, so nothing is lost.
   *
   *  @param precision decimal places for the decimal form
   *  @return the fraction or the decimal, whichever reads better
   */
  def display(precision: Int): String =
    val (rn, rd) = _Rational.reduce(num, den)
    // An integer prints as itself at ANY size.  The exact factorial family reaches values a
    // `Double` cannot hold (`171!` is the first), and rendering those through `toDouble`
    // would print `Infinity` for a value that is perfectly exact -- the display losing what
    // the arithmetic kept.  Long, but long is the honest answer.
    if rd == BigInt(1) then rn.toString
    else if digitsOf(rn) <= _Rational.MaxDisplayDigits && digitsOf(rd) <= _Rational.MaxDisplayDigits
    then s"$rn/$rd"
    else
      val d = toDouble
      // Out of `Double` range entirely: the fraction is the only form left that says
      // anything true.
      if d.isFinite then _Number.round(d, precision).toString else s"$rn/$rd"

  /** Decimal digit count of `n`, sign excluded. */
  private def digitsOf(n: BigInt): Int =
    val s = n.abs.toString
    s.length

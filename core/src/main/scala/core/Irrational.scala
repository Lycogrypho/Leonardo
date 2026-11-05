package it.grypho.scala.leonardo
package core

import spire.math.Real
import spire.math.Rational as SpireRational


/** The exact tier's irrational engine — the bridge between [[_Rational]] and
 *  spire's arbitrary-precision `Real`.
 *
 *  **What this closes.**  Tier 1 made the *arithmetic* exact, but everything that leaves the
 *  rationals — every transcendental, any fractional power — was still computed in `Double`
 *  and re-approximated, capped at [[_Rational.DoubleReliableDigits]].  So `exact precision 60`
 *  sharpened the arithmetic *around* a `sin()` and nothing about the `sin()` itself, and
 *  `exp(1000)` overflowed to an infinity and stayed symbolic.  Routing those kernels through
 *  `Real` instead makes the working precision mean what a user expects it to.
 *
 *  **Eager, not lazy — and deliberately so.**  The original plan was to hold irrational
 *  sub-results as unevaluated `Real`s inside the AST and materialise them at the end.  Two
 *  things argue against it.  `Real`'s equality is only *semi-decidable*, so a `_Value`
 *  wrapping one could loop forever on `==` or inside a pattern match — a hazard with no good
 *  answer.  And a fourth value type would mean a fourth pass through the promotion lattice,
 *  the widening extractor, display, serialization and the parser.  Materialising immediately
 *  at `workingPrecision + `[[GuardDigits]] costs a little accuracy across very long chains
 *  and buys the whole of both acceptance cases; where a single algorithm genuinely needs to
 *  stay exact for longer, it can use `Algebraic` locally, which `equation.Solve` does.
 *
 *  **What still uses `Double`.**  When the working precision is within `Double`'s reliable
 *  range there is nothing to gain and roughly 110× to lose, so the callers keep the `Double`
 *  kernel there; see `scalar._Function.viaExact`.
 *
 *  @see [[https://en.wikipedia.org/wiki/Computable_number Computable number]] — what `Real` is
 *  @see [[https://en.wikipedia.org/wiki/Arbitrary-precision_arithmetic Arbitrary-precision arithmetic]]
 */

/** Extra digits computed and then discarded, to absorb the rounding of the final
 *  re-approximation and of the arithmetic immediately around it.
 *
 *  Without guard digits the last place of a result would be unreliable exactly when a user
 *  raised the precision to inspect it.
 */
val GuardDigits: Int = 10

/** Lifts an exact rational into spire's `Real`, losing nothing — `Real` holds a rational
 *  exactly.
 *  @param r the exact value
 *  @return the same value as a `Real`
 */
def toReal(r: _Rational): Real =
  Real(SpireRational(r.num, r.den))

/** A spire rational as the nearest `Double`, read through [[_Rational.toDouble]] (F_0283).
 *
 *  **Never spire's own `Rational.toDouble`**: it misrounds a rational whose numerator and denominator
 *  are wide — `0.4999999999999999875` with a 124-bit numerator over a 126-bit denominator came back
 *  `0.25` — which is what read `√(10³² + 10¹⁶) − 10¹⁶` as `0.25`, and very likely F_0244's
 *  misrounded surd, there blamed on `Real.toDouble`.  Every exact kernel that hands a spire result
 *  to a `Double` goes through here.
 *  @param q the rational
 *  @return the nearest `Double` (to `_Rational.toDouble`'s 17 significant digits)
 */
private[leonardo] def spireToDouble(q: SpireRational): Double =
  _Rational.of(q.numerator.toBigInt, q.denominator.toBigInt).fold(Double.NaN)(_.toDouble)

/** How many decades below one a reading looks for a small nonzero value before it settles for
 *  the decimal places alone (F_0271).
 *
 *  A `Real` that is exactly zero reads as zero at every precision, so the search needs an end;
 *  the trivial zeros of the kernels (`sin(0)`, `ln(1)`) never reach it, being built as exact
 *  `Real`s.  The cost of an invisible value is a reading at `digits + 500` places; 500 reaches
 *  `exp(-1150)`, and the cap is what bounds the time of a value below it, which reads `0`.
 */
val MaxReadingDecades: Int = 500

/** Materialises a `Real` back into the exact tier at `digits` decimal places, and never fewer
 *  than `digits` significant digits for a nonzero value (F_0271, the owner's Decision 3).
 *
 *  Computes at `digits + `[[GuardDigits]] and then takes the best rational with denominator
 *  at most `10^digits`, which is the same bounded-denominator step the rest of the tier uses
 *  — so the result carries the working precision and no more, and its operands stay small.
 *  **A value below one gets as many more places as it lies decades below one**: by decimal
 *  places alone `exp(-100)` read as `0` and `exp(-60)` kept one digit, the units rule in the
 *  exact tier's own definition of precision.  The magnitude is found by reading at doubling
 *  precision until the value shows, up to [[MaxReadingDecades]]; a value still invisible
 *  there reads at the decimal places, as before.  An exact `Real` is read as itself.
 *
 *  @param x      the computed value
 *  @param digits the working precision in decimal digits
 *  @return the exact approximation, or `None` if the value could not be evaluated
 */
def fromReal(x: Real, digits: Int): Option[_Rational] =
  val target = math.max(digits, 1)
  def at(places: Int): SpireRational = x.toRational(Real.digitsToBits(places + GuardDigits))
  @scala.annotation.tailrec
  def decadesBelowOne(places: Int): Int =
    val q = at(places)
    if !q.isZero then decadesOf(q)
    else if places - target >= MaxReadingDecades then 0
    else decadesBelowOne(math.min(2 * places, target + MaxReadingDecades))
  try
    val (places, q) = x match
      case Real.Exact(n) => (target, n)
      case _ =>
        val p = target + decadesBelowOne(target)
        (p, at(p))
    _Rational.of(q.numerator.toBigInt, q.denominator.toBigInt, _Rational.thresholdFor(places))
      .map(_.approximateToDigits(places))
  catch
    // A `Real` is a function from precision to approximation; a pathological argument can
    // fail to converge rather than return.  Staying symbolic is the house response.
    case _: ArithmeticException | _: IllegalArgumentException => None

/** How many decades a nonzero rational lies below one, read off the bit lengths and rounded up
 *  (an extra place is harmless); `0` for a value of one or more. */
private def decadesOf(q: SpireRational): Int =
  val bits = q.denominator.toBigInt.bitLength - q.numerator.toBigInt.abs.bitLength + 1
  if bits <= 0 then 0 else math.ceil(bits * math.log10(2.0)).toInt

/** The square root of a non-negative rational, at `digits` decimal places.
 *
 *  Uses `Real` rather than spire's `Algebraic`, although an earlier plan suggested the latter
 *  for polynomial roots.  `Algebraic`'s advantage is a *decidable* sign and comparison — and
 *  the only place this is used, the quadratic solver, has already established the sign by
 *  comparing the discriminant against zero in exact rational arithmetic, so there is nothing
 *  left for that decidability to buy.
 *
 *  @param r      the radicand, which must be non-negative
 *  @param digits the working precision in decimal digits
 *  @return the root at that precision, or `None` for a negative radicand
 */
def exactSqrt(r: _Rational, digits: Int): Option[_Rational] =
  if r.signum < 0 then None
  else if r.isZero then Some(_Rational.Zero)
  else fromReal(toReal(r).sqrt, digits)

/** A positive rational raised to a rational power, at `digits` decimal places.
 *
 *  `b^e = exp(e · ln b)` in spire's terms; the base must be positive, since a negative one
 *  with a fractional exponent is the complex case and belongs to the caller's fallback.
 *
 *  @param b      the base, which must be strictly positive
 *  @param e      the exponent
 *  @param digits the working precision in decimal digits
 *  @return the power at that precision, or `None` when the base is not positive
 */
def exactPow(b: _Rational, e: _Rational, digits: Int): Option[_Rational] =
  if b.signum <= 0 then None
  else
    val r = SpireRational(e.num, e.den)
    // A root of high degree as exp(r·ln b) (F_0284): spire's fpow takes the q-th root, whose cost
    // grows with q, so an exponent typed as a long decimal (1.00001 is 100001/100000) ran for
    // minutes; exp and ln at the working precision cost the same for any q.
    if r.denominator <= MaxDirectRootDegree then fromReal(toReal(b).fpow(r), digits)
    else fromReal(Real.exp(Real(r) * Real.log(toReal(b))), digits)

/** The highest root degree [[exactPow]] takes directly; above it, `exp(r·ln b)` (F_0284).  Measured on
 *  Node, `2^((q + 1)/q)` at 30 digits: `fpow` 2–9 ms up to `q = 8`, 5 ms at 16, 15 ms at 64, 99 ms at
 *  256, 1.55 s at 1000 (`2^(1.00001)` took 38.6 s on the JVM); `exp`/`ln` 3–7 ms at every `q` past 3
 *  (14 ms at 10⁴).  The two agree at 30 digits where both run.  At or below the crossover the direct
 *  root is kept, which reads a perfect power exactly (`8^(1/3)` is `2`). */
private val MaxDirectRootDegree: Int = 16

/** `pi` as an exact rational at `digits` decimal places.
 *
 *  Genuinely `digits` correct places, where tier 1 could offer at most fifteen: the constant
 *  is now computed rather than read off a `Double`.
 *
 *  @param digits the working precision in decimal digits
 */
def piAt(digits: Int): Option[_Rational] = fromReal(Real.pi, digits)

/** `e` as an exact rational at `digits` decimal places.
 *  @param digits the working precision in decimal digits
 */
def eAt(digits: Int): Option[_Rational] = fromReal(Real.e, digits)

/** The Euler–Mascheroni constant `γ` as an exact rational at `digits` decimal places (F_0289).
 *
 *  spire has no `γ`, so it is computed by the Brent–McMillan algorithm in fixed point: with
 *  `A₀ = −ln n`, `B₀ = 1`, `Bₖ = Bₖ₋₁·n²/k²`, `Aₖ = (Aₖ₋₁·n²/k + Bₖ)/k`, `γ ≈ ΣAₖ/ΣBₖ` with an
 *  error near `π·e^(−4n)`, so `n` is chosen from the digits asked and the sums run until the terms
 *  vanish in the guard digits.
 *
 *  @param digits the working precision in decimal digits
 */
def eulerGammaAt(digits: Int): Option[_Rational] =
  val guard = 12
  val scale = BigInt(10).pow(digits + guard)
  val n     = math.ceil((digits + guard) * math.log(10) / 4).toLong.max(2)
  val n2    = BigInt(n) * n
  // ln n in the same fixed point, from spire at the guarded precision
  val lnN   = Real.log(Real(n)).toRational(Real.digitsToBits(digits + 2 * guard))
  val lnFix = lnN.numerator.toBigInt * scale / lnN.denominator.toBigInt
  @annotation.tailrec
  def sum(k: Long, a: BigInt, b: BigInt, u: BigInt, v: BigInt): (BigInt, BigInt) =
    if a == 0 && b == 0 then (u, v)
    else
      val bk = b * n2 / (BigInt(k) * k)
      val ak = (a * n2 / k + bk) / k
      sum(k + 1, ak, bk, u + ak, v + bk)
  val (u, v) = sum(1, -lnFix, scale, -lnFix, scale)
  fromReal(Real(spire.math.Rational(u, v)), digits)

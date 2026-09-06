package it.grypho.scala.leonardo
package core

import spire.math.Real
import spire.math.Rational as SpireRational


/** The exact tier's irrational engine (issue 4.N) — the bridge between [[_Rational]] and
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

/** Materialises a `Real` back into the exact tier at `digits` decimal places.
 *
 *  Computes at `digits + `[[GuardDigits]] and then takes the best rational with denominator
 *  at most `10^digits`, which is the same bounded-denominator step the rest of the tier uses
 *  — so the result carries the working precision and no more, and its operands stay small.
 *
 *  @param x      the computed value
 *  @param digits the working precision in decimal digits
 *  @return the exact approximation, or `None` if the value could not be evaluated
 */
def fromReal(x: Real, digits: Int): Option[_Rational] =
  val target = math.max(digits, 1)
  try
    val q = x.toRational(Real.digitsToBits(target + GuardDigits))
    _Rational.of(q.numerator.toBigInt, q.denominator.toBigInt, _Rational.thresholdFor(target))
      .map(_.approximateToDigits(target))
  catch
    // A `Real` is a function from precision to approximation; a pathological argument can
    // fail to converge rather than return.  Staying symbolic is the house response.
    case _: ArithmeticException | _: IllegalArgumentException => None

/** The square root of a non-negative rational, at `digits` decimal places.
 *
 *  Uses `Real` rather than spire's `Algebraic`, although the 4.N plan suggested the latter
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
  if r.signum < 0 then None else fromReal(toReal(r).sqrt, digits)

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
  else fromReal(toReal(b).fpow(SpireRational(e.num, e.den)), digits)

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

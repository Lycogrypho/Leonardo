package it.grypho.scala.leonardo
package scalar

import core.*


/** Kernels for the numeric sequences and the combinatorial functions — issue 6.28.
 *
 *  **"Numeric", not "integer" sequences.**  A linear recurrence needs only addition and
 *  multiplication, so it is closed over whatever its seeds are: `fib(5, 1.5, -pi)` is as
 *  meaningful as `fib(5)`, and with exact seeds the exact tier carries it through unrounded.
 *  Restricting these to integers would be a design choice with nothing to recommend it.
 *  (The word *series* is deliberately avoided throughout: in this codebase it already means
 *  the analytic expansions in `Series.scala` — Taylor, Fourier, Laurent.)
 *
 *  **Indexing is the standard one**: `x(0) = 0`, `x(1) = 1`, so `fib(10) = 55`, agreeing with
 *  OEIS A000045 and every published table.  The "classic rabbit" pair is available as
 *  `fib(n, 1, 1)` and is the same sequence shifted by one.  The convention cannot be inferred
 *  and an off-by-one here is silent, so it is stated wherever these appear — the rule 6.26
 *  follows for the spherical polar angle.
 *
 *  **Two caps, as in `SpecialFunctions.scala`.**  The exact path is bounded by compute cost
 *  ([[MaxExactSequenceIndex]]), the `Double` path by representability — `F(1477)` overflows,
 *  and beyond `F(78)` a `Double` can no longer hold a Fibonacci number *exactly* even though
 *  it can still approximate it, which is precisely why the exact path exists.
 */

/** Largest index computed exactly, past which a sequence node stays symbolic.
 *
 *  `F(10000)` is a 2090-digit integer and costs milliseconds by linear iteration; a few
 *  orders of magnitude more would hang the REPL.  The same "computable in principle is not
 *  should be attempted" line [[MaxExactFactorial]] draws.  Linear iteration is enough under
 *  this bound — fast doubling (`O(log n)`) would only matter if it were raised, and applies
 *  to the integer case alone, since arbitrary seeds need the plain recurrence anyway.
 */
val MaxExactSequenceIndex: Int = 10000

/** Whether `d` is a non-negative integer index small enough to iterate. */
private def isIndex(d: Double): Boolean =
  !d.isNaN && !d.isInfinite && d >= 0.0 && d == Math.floor(d) && d <= MaxExactSequenceIndex

/** The `n`-th term of `x(k) = p·x(k-1) + q·x(k-2)` in `Double`, from seeds `(a, b)`.
 *
 *  @return `Some(x(n))`, or `None` for a non-index `n` or a non-finite result (overflow)
 */
def linRecOf(n: Double, a: Double, b: Double, p: Double, q: Double): Option[Double] =
  if !isIndex(n) then None
  else
    var (x, y) = (a, b)                       // x = x(k-2), y = x(k-1)
    for _ <- 0 until n.toInt do
      val next = p * y + q * x
      x = y; y = next
    finite(x)

/** The exact `n`-th term of `x(k) = p·x(k-1) + q·x(k-2)` over the rationals.
 *
 *  Kept separate from [[linRecOf]] rather than generalised over a numeric type class: the
 *  two tiers differ in their failure modes (overflow versus a compute cap), and the exact one
 *  is the reason this function exists at all.
 *
 *  @return `Some(x(n))`, or `None` when `n` is not an index within the cap
 */
def linRecExact(n: BigInt, a: _Rational, b: _Rational,
                p: _Rational, q: _Rational): Option[_Rational] =
  if n.signum < 0 || n > BigInt(MaxExactSequenceIndex) then None
  else
    var x = a
    var y = b
    for _ <- 0 until n.toInt do
      val next = p.multiply(y).add(q.multiply(x))
      x = y; y = next
    Some(x)

/** The binomial coefficient `C(n, k)` in `Double`, generalised to a real upper index.
 *
 *  Uses the falling factorial `n(n-1)…(n-k+1)/k!`, which is the standard generalisation and
 *  handles a negative or fractional `n` (`C(-1, 3) = -1`).  `k` must be a non-negative
 *  integer; `C(n, k) = 0` for a negative `k`, and for `k > n` when `n` is a non-negative
 *  integer.
 *
 *  @return `Some(C(n, k))`, or `None` when `k` is not an integer or the result is non-finite
 */
def binomOf(n: Double, k: Double): Option[Double] =
  if k.isNaN || k.isInfinite || k != Math.floor(k) then None
  else if k < 0 then Some(0.0)
  else if n >= 0 && n == Math.floor(n) && k > n then Some(0.0)
  else if k > MaxExactSequenceIndex then None
  else
    var acc = 1.0
    for i <- 0 until k.toInt do acc = acc * (n - i) / (i + 1)
    finite(acc)

/** The exact binomial coefficient for a non-negative integer `n` and `k`.
 *
 *  Multiplicative form, dividing at every step so the intermediate values stay as small as
 *  the result — `C(2n, n)` would otherwise build `(2n)!` on the way.
 *
 *  @return `Some(C(n, k))`, or `None` when `k` exceeds the cap
 */
def binomExact(n: BigInt, k: BigInt): Option[BigInt] =
  if k.signum < 0 || (n.signum >= 0 && k > n) then Some(BigInt(0))
  else if k > BigInt(MaxExactSequenceIndex) then None
  else
    // C(n, k) = C(n, n-k): take the cheaper side when both are available
    val kk = if n.signum >= 0 && k > n - k then n - k else k
    Some((BigInt(0) until kk).foldLeft(BigInt(1))((acc, i) => acc * (n - i) / (i + 1)))

/** The `n`-th Catalan number `C(2n, n)/(n+1)`, exactly.
 *  @return `Some(Cat(n))`, or `None` outside the non-negative integers within the cap
 */
def catalanExact(n: BigInt): Option[BigInt] =
  if n.signum < 0 then None
  else binomExact(2 * n, n).map(_ / (n + 1))

/** The `n`-th Catalan number in `Double`. */
def catalanOf(n: Double): Option[Double] =
  if !isIndex(n) then None
  else binomOf(2 * n, n).flatMap(b => finite(b / (n + 1)))

/** The `n`-th harmonic number `H(n) = 1 + 1/2 + … + 1/n`, exactly (`H(0) = 0`).
 *
 *  A natural showcase for the exact tier: `H(4)` is `25/12`, not `2.0833…`.  Related to the
 *  existing `digamma` by `H(n) = ψ(n+1) + γ`, which the test suite checks rather than assumes.
 *
 *  @return `Some(H(n))`, or `None` outside the non-negative integers within the cap
 */
def harmonicExact(n: BigInt): Option[_Rational] =
  if n.signum < 0 || n > BigInt(MaxExactSequenceIndex) then None
  else
    var acc = _Rational(0)
    for i <- 1 to n.toInt do
      _Rational.of(BigInt(1), BigInt(i)).foreach(term => acc = acc.add(term))
    Some(acc)

/** The `n`-th harmonic number in `Double`. */
def harmonicOf(n: Double): Option[Double] =
  if !isIndex(n) then None
  else finite((1 to n.toInt).foldLeft(0.0)((acc, i) => acc + 1.0 / i))

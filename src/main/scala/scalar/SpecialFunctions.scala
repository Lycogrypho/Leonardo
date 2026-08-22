package it.grypho.scala.leonardo
package scalar

import scala.math.{abs, exp, log, sin, sqrt, Pi}


/** Largest `n` whose factorial is representable as a finite `Double`: `170!` is about
 *  7.26e306, while `171!` overflows to `+Infinity`.
 *
 *  Past this bound [[factorialOf]] returns `None` and the AST node stays symbolic,
 *  rather than propagating an infinity — the "domain errors stay symbolic" rule the
 *  rest of the library follows.  Use [[lgammaOf]] when a large factorial is wanted in
 *  log space.
 */
val MaxFactorial: Int = 170

/** Largest argument the *exact* factorial family will evaluate.
 *
 *  A different kind of limit from [[MaxFactorial]], and worth the distinction: 170 is where
 *  a `Double` stops being able to *represent* the answer, whereas this is where computing
 *  it stops being instant.  `10000!` is a 35 660-digit integer and takes a few
 *  milliseconds; a few orders of magnitude more would hang the REPL, and an unbounded
 *  `fact` is exactly the sort of thing a user types by accident.  Past the cap the node
 *  stays symbolic, the same give-up rule the rest of the library uses.
 */
val MaxExactFactorial: Int = 10000

/** The exact factorial `n!` as an arbitrary-precision integer.
 *
 *  The exact-tier counterpart of [[factorialOf]], which is capped at `170!` only because a
 *  `Double` overflows there.
 *
 *  @param n the argument
 *  @return `Some(n!)`, or `None` for a negative `n` or one above [[MaxExactFactorial]]
 */
def factorialExact(n: BigInt): Option[BigInt] =
  multiFactorialExact(n, BigInt(1))

/** The exact multifactorial `n!!…!` stepping down by `k`.
 *
 *  `k = 1` is the factorial and `k = 2` the double factorial, matching
 *  [[multiFactorialOf]].
 *
 *  @param n the argument
 *  @param k the step
 *  @return `Some(n!^(k))`, or `None` for a negative `n`, a non-positive `k`, or an `n`
 *          above [[MaxExactFactorial]]
 */
def multiFactorialExact(n: BigInt, k: BigInt): Option[BigInt] =
  if n.signum < 0 || k.signum <= 0 || n > BigInt(MaxExactFactorial) then None
  else
    // n <= MaxExactFactorial, so the loop count is bounded and the Int conversion is safe.
    var acc = BigInt(1)
    var i   = n
    while i.signum > 0 do
      acc *= i
      i -= k
    Some(acc)


/** Exact integer factorials `0!` through `170!`, computed once.
 *
 *  Built by iterated multiplication, so values beyond `22!` (the largest factorial that
 *  is exactly representable) carry the usual floating-point rounding.
 */
private val factorialTable: Array[Double] =
  val t = new Array[Double](MaxFactorial + 1)
  t(0) = 1.0
  var i = 1
  while i <= MaxFactorial do
    t(i) = t(i - 1) * i
    i += 1
  t


/** Lanczos coefficients for `g = 7`, `n = 9` — the standard parameter set, good to
 *  roughly 15 significant digits over the half-plane where it is applied.
 */
private val LanczosG: Double = 7.0
private val LanczosCoefficients: Array[Double] = Array(
  0.99999999999980993, 676.5203681218851, -1259.1392167224028,
  771.32342877765313, -176.61502916214059, 12.507343278686905,
  -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7)


/** Whether `d` is a whole number (and small enough for that to be meaningful). */
private def isWholeNumber(d: Double): Boolean =
  !d.isNaN && !d.isInfinite && d == Math.floor(d)

/** Wraps a computed result, rejecting non-finite values so the caller stays symbolic. */
private def finite(d: Double): Option[Double] =
  Option.when(!d.isNaN && !d.isInfinite)(d)


/** The factorial `n!`.
 *
 *  A non-negative integer up to [[MaxFactorial]] uses the exact table; a non-integer
 *  argument falls through to `Γ(n + 1)`, which is the analytic continuation (so
 *  `fact(0.5)` is `√π / 2`).  `None` — meaning the caller stays symbolic — for a
 *  negative integer (a pole of the gamma function), for an integer past the overflow
 *  bound, and for any non-finite input.
 *
 *  @param n the argument
 *  @return `Some(n!)` when defined and representable, `None` otherwise
 */
def factorialOf(n: Double): Option[Double] =
  if n.isNaN || n.isInfinite then None
  else if isWholeNumber(n) then
    // exact path for the integers: keeps small factorials bit-accurate
    if n < 0.0 then None                                  // pole of Gamma at 0, -1, -2, ...
    else if n > MaxFactorial then None                    // overflows a Double
    else Some(factorialTable(n.toInt))
  else gammaOf(n + 1.0)                                   // analytic continuation


/** The multifactorial `n!^(k)` — the product `n · (n−k) · (n−2k) · …` down to the last
 *  positive term.
 *
 *  `multiFactorialOf(n, 1)` is the ordinary factorial and `multiFactorialOf(n, 2)` the
 *  double factorial `n!!`.  Defined here for non-negative integer `n` and positive
 *  integer `k` only; anything else (including a non-integer `n`, which has no standard
 *  multifactorial) yields `None` and stays symbolic.
 *
 *  @param n the argument
 *  @param k the step
 *  @return `Some(n!^(k))` when defined and representable, `None` otherwise
 */
def multiFactorialOf(n: Double, k: Double): Option[Double] =
  if !isWholeNumber(n) || !isWholeNumber(k) || n < 0.0 || k < 1.0 then None
  else
    var acc = 1.0
    var i   = n
    while i > 1.0 && !acc.isInfinite do
      acc *= i
      i -= k
    finite(acc)


/** The gamma function `Γ(z)`, the analytic continuation of the factorial
 *  (`Γ(n) = (n−1)!`), by the Lanczos approximation.
 *
 *  Arguments below `0.5` go through the reflection formula
 *  `Γ(z) = π / (sin(πz) · Γ(1−z))`.  `None` at the poles `z = 0, −1, −2, …` and
 *  whenever the result overflows a `Double`.
 *
 *  @param z the argument
 *  @return `Some(Γ(z))` when defined and representable, `None` otherwise
 */
def gammaOf(z: Double): Option[Double] =
  if z.isNaN || z.isInfinite then None
  else if isWholeNumber(z) && z <= 0.0 then None          // poles
  else if z < 0.5 then
    // reflection: valid because the poles were already excluded above
    val s = sin(Pi * z)
    if s == 0.0 then None
    else gammaOf(1.0 - z).flatMap(g => finite(Pi / (s * g)))
  else
    val zz = z - 1.0
    var x  = LanczosCoefficients(0)
    var i  = 1
    while i < LanczosCoefficients.length do
      x += LanczosCoefficients(i) / (zz + i)
      i += 1
    val t = zz + LanczosG + 0.5
    finite(sqrt(2.0 * Pi) * Math.pow(t, zz + 0.5) * exp(-t) * x)


/** The log-gamma function `ln|Γ(z)|`.
 *
 *  Computed in log space rather than as `log(gammaOf(z))`, so it stays finite for
 *  arguments far past the factorial overflow bound — which is the whole point of
 *  having it.  `None` at the poles and for non-finite input.
 *
 *  @param z the argument
 *  @return `Some(ln|Γ(z)|)` when defined, `None` otherwise
 */
def lgammaOf(z: Double): Option[Double] =
  if z.isNaN || z.isInfinite then None
  else if isWholeNumber(z) && z <= 0.0 then None          // poles
  else if z < 0.5 then
    // reflection in log space: ln|Γ(z)| = ln(π) − ln|sin(πz)| − ln|Γ(1−z)|
    val s = abs(sin(Pi * z))
    if s == 0.0 then None
    else lgammaOf(1.0 - z).flatMap(lg => finite(log(Pi) - log(s) - lg))
  else
    val zz = z - 1.0
    var x  = LanczosCoefficients(0)
    var i  = 1
    while i < LanczosCoefficients.length do
      x += LanczosCoefficients(i) / (zz + i)
      i += 1
    val t = zz + LanczosG + 0.5
    finite(0.5 * log(2.0 * Pi) + (zz + 0.5) * log(t) - t + log(abs(x)))


/** The beta function `B(x, y) = Γ(x)Γ(y) / Γ(x+y)`.
 *
 *  For positive arguments this goes through [[lgammaOf]] and exponentiates, which keeps
 *  it finite for large `x`/`y` where the individual gammas would overflow; otherwise it
 *  falls back to the direct gamma quotient.  `None` when any factor is undefined or the
 *  result is not representable.
 *
 *  @param x the first argument
 *  @param y the second argument
 *  @return `Some(B(x, y))` when defined and representable, `None` otherwise
 */
def betaOf(x: Double, y: Double): Option[Double] =
  if x > 0.0 && y > 0.0 then
    for lx <- lgammaOf(x); ly <- lgammaOf(y); lxy <- lgammaOf(x + y)
        r <- finite(exp(lx + ly - lxy))
    yield r
  else
    for gx <- gammaOf(x); gy <- gammaOf(y); gxy <- gammaOf(x + y)
        r <- finite(gx * gy / gxy)
    yield r

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


// ---------------------------------------------------------------------------------------
// Issue 4.O: the incomplete gamma / beta family and the functions built on it.
//
// SPIRE SUPPLIES NONE OF THESE.  The 4.N dependency gives arbitrary-precision *elementary*
// functions and nothing else, so these kernels are in-house and Double-based like the
// Lanczos gamma above.  The consequence for the exact tier: they get no
// `_Function.exactKernel`, so in exact mode they fall through to `viaDouble` and are capped
// at `_Rational.DoubleReliableDigits` however high the working precision is set.  That is
// the hook for lifting it later -- implement a kernel over spire's `Real` and override it.
// ---------------------------------------------------------------------------------------

/** Iteration cap for the incomplete gamma / beta expansions.  Both converge in well under
 *  a hundred terms across the domain; the cap turns a pathological argument into a `None`
 *  rather than a hang.
 */
private val MaxSeriesIterations: Int = 300

/** Relative convergence tolerance for those expansions — a few ulps above machine epsilon,
 *  so the loop stops once further terms cannot change the `Double`.
 */
private val SeriesEpsilon: Double = 3.0e-16

/** Smallest positive value used to restart a continued fraction whose denominator
 *  underflows — the standard modified-Lentz guard.
 */
private val TinyFloor: Double = 1.0e-300

/** The regularised lower incomplete gamma function `P(a, x) = γ(a, x) / Γ(a)`.
 *
 *  The engine of this whole tier, and the reason it is implemented before `erf` rather than
 *  after: the same kernel is the chi-squared and gamma cdfs, and `erf` falls out of it at
 *  full `Double` accuracy.  The obvious alternative — the Abramowitz & Stegun 7.1.26
 *  rational approximation for `erf` — is accurate to only about `1.5e-7`, which would be
 *  seven digits in a library whose every other kernel is good to fifteen.
 *
 *  Series expansion for `x < a + 1`, continued fraction beyond it; that split is where each
 *  of the two converges quickly.
 *
 *  @param a the shape parameter, which must be strictly positive
 *  @param x the argument, which must be non-negative
 *  @return `Some(P(a, x))` in `[0, 1]`, or `None` outside the domain or on non-convergence
 *  @see [[https://en.wikipedia.org/wiki/Incomplete_gamma_function Incomplete gamma function]]
 */
def lowerGammaP(a: Double, x: Double): Option[Double] =
  if a.isNaN || x.isNaN || a <= 0.0 || x < 0.0 then None
  else if x == 0.0 then Some(0.0)
  else if x < a + 1.0 then gammaSeries(a, x)
  else gammaContinuedFraction(a, x).flatMap(q => finite(1.0 - q))

/** The regularised upper incomplete gamma function `Q(a, x) = 1 − P(a, x)`.
 *
 *  Computed directly by the continued fraction where that converges, rather than as
 *  `1 − P`: for large `x`, `P` is within an ulp of 1 and the subtraction would destroy every
 *  significant digit of a small `Q`.  This is what lets `erfc` stay accurate far out in the
 *  tail, which is exactly where a survival function is worth having.
 *
 *  @param a the shape parameter, which must be strictly positive
 *  @param x the argument, which must be non-negative
 *  @return `Some(Q(a, x))` in `[0, 1]`, or `None` outside the domain or on non-convergence
 */
def upperGammaQ(a: Double, x: Double): Option[Double] =
  if a.isNaN || x.isNaN || a <= 0.0 || x < 0.0 then None
  else if x == 0.0 then Some(1.0)
  else if x < a + 1.0 then gammaSeries(a, x).flatMap(p => finite(1.0 - p))
  else gammaContinuedFraction(a, x)

/** `P(a, x)` by its series `e^-x x^a / Γ(a) · Σ x^n / (a(a+1)…(a+n))`. */
private def gammaSeries(a: Double, x: Double): Option[Double] =
  lgammaOf(a).flatMap { lg =>
    var ap   = a
    var del  = 1.0 / a
    var sum  = del
    var i    = 0
    var done = false
    while i < MaxSeriesIterations && !done do
      ap  += 1.0
      del *= x / ap
      sum += del
      if abs(del) < abs(sum) * SeriesEpsilon then done = true
      i += 1
    if !done then None else finite(sum * exp(-x + a * log(x) - lg))
  }

/** `Q(a, x)` by its continued fraction, evaluated with the modified Lentz algorithm. */
private def gammaContinuedFraction(a: Double, x: Double): Option[Double] =
  lgammaOf(a).flatMap { lg =>
    var b    = x + 1.0 - a
    var c    = 1.0 / TinyFloor
    var d    = 1.0 / b
    var h    = d
    var i    = 1
    var done = false
    while i <= MaxSeriesIterations && !done do
      val an = -i.toDouble * (i.toDouble - a)
      b += 2.0
      d = an * d + b
      if abs(d) < TinyFloor then d = TinyFloor
      c = b + an / c
      if abs(c) < TinyFloor then c = TinyFloor
      d = 1.0 / d
      val del = d * c
      h *= del
      if abs(del - 1.0) < SeriesEpsilon then done = true
      i += 1
    if !done then None else finite(exp(-x + a * log(x) - lg) * h)
  }

/** The error function `erf(x) = 2/√π ∫₀ˣ e^(−t²) dt`.
 *
 *  Derived from [[lowerGammaP]] rather than approximated in its own right, so it carries
 *  full `Double` accuracy: `erf(x) = sign(x) · P(1/2, x²)`.
 *
 *  @param x the argument
 *  @return `Some(erf(x))` in `(-1, 1)`, or `None` for a NaN argument
 *  @see [[https://en.wikipedia.org/wiki/Error_function Error function]]
 */
def erfOf(x: Double): Option[Double] =
  if x.isNaN then None
  else if x.isInfinite then Some(if x > 0.0 then 1.0 else -1.0)
  else if x == 0.0 then Some(0.0)
  else lowerGammaP(0.5, x * x).map(p => if x > 0.0 then p else -p)

/** The complementary error function `erfc(x) = 1 − erf(x)`.
 *
 *  For a positive argument this is `Q(1/2, x²)`, computed *without* the subtraction:
 *  `erf(3)` is within `2e-5` of 1, so forming `1 − erf(x)` would leave a tail value with
 *  only a handful of correct digits.
 *
 *  @param x the argument
 *  @return `Some(erfc(x))` in `(0, 2)`, or `None` for a NaN argument
 */
def erfcOf(x: Double): Option[Double] =
  if x.isNaN then None
  else if x.isInfinite then Some(if x > 0.0 then 0.0 else 2.0)
  else if x >= 0.0 then upperGammaQ(0.5, x * x)
  else lowerGammaP(0.5, x * x).flatMap(p => finite(1.0 + p))

/** The digamma function `ψ(z) = Γ'(z) / Γ(z)`.
 *
 *  Recurrence `ψ(z) = ψ(z+1) − 1/z` pushes a small argument up into the range where the
 *  asymptotic series converges, mirroring how [[gammaOf]] handles its own domain; the
 *  reflection formula `ψ(1−z) − ψ(z) = π·cot(πz)` covers the negative half-line.
 *
 *  Its payoff is not only the cdfs: `derive` has no rule for `Γ` or `fact` without it, so
 *  both stay symbolic under differentiation.  With it, `d/dx Γ(x) = Γ(x)·ψ(x)`.
 *
 *  @param z the argument
 *  @return `Some(ψ(z))`, or `None` at the poles `0, −1, −2, …` and for non-finite input
 *  @see [[https://en.wikipedia.org/wiki/Digamma_function Digamma function]]
 */
def digammaOf(z: Double): Option[Double] =
  if z.isNaN || z.isInfinite then None
  else if isWholeNumber(z) && z <= 0.0 then None          // poles, as for Γ
  else if z < 0.0 then
    val t = Math.tan(Pi * z)
    if t == 0.0 then None else digammaOf(1.0 - z).flatMap(d => finite(d - Pi / t))
  else
    // Recur up to DigammaAsymptoticFloor, then apply the asymptotic expansion.
    var x   = z
    var acc = 0.0
    while x < DigammaAsymptoticFloor do
      acc -= 1.0 / x
      x   += 1.0
    val inv  = 1.0 / x
    val inv2 = inv * inv
    // ln x − 1/(2x) − Σ B₂ₙ/(2n x^2n); five Bernoulli terms is ample past the floor.
    val series = inv2 * (1.0 / 12.0 - inv2 * (1.0 / 120.0 - inv2 * (1.0 / 252.0 -
                 inv2 * (1.0 / 240.0 - inv2 / 132.0))))
    finite(acc + log(x) - 0.5 * inv - series)

/** Argument above which the digamma asymptotic series is used directly; below it the
 *  recurrence walks up to here first.  Ten is the usual choice — far enough out that five
 *  Bernoulli terms reach `Double` accuracy.
 */
private val DigammaAsymptoticFloor: Double = 10.0

/** The regularised incomplete beta function `I_x(a, b)`.
 *
 *  The Student-t and F cdfs, and the binomial's exact tail.  Continued fraction with the
 *  symmetry `I_x(a,b) = 1 − I_{1−x}(b,a)` used to keep the argument in the half where it
 *  converges fastest; the normalising constant goes through [[lgammaOf]], which 4.I
 *  computes in log space precisely so it survives large parameters.
 *
 *  @param x the argument, which must lie in `[0, 1]`
 *  @param a the first shape parameter, strictly positive
 *  @param b the second shape parameter, strictly positive
 *  @return `Some(I_x(a, b))` in `[0, 1]`, or `None` outside the domain
 *  @see [[https://en.wikipedia.org/wiki/Beta_function#Incomplete_beta_function Incomplete beta function]]
 */
def incompleteBetaOf(x: Double, a: Double, b: Double): Option[Double] =
  if x.isNaN || a.isNaN || b.isNaN || a <= 0.0 || b <= 0.0 || x < 0.0 || x > 1.0 then None
  else if x == 0.0 then Some(0.0)
  else if x == 1.0 then Some(1.0)
  else
    val front =
      for
        la <- lgammaOf(a + b); lb <- lgammaOf(a); lc <- lgammaOf(b)
        f  <- finite(exp(la - lb - lc + a * log(x) + b * log(1.0 - x)))
      yield f
    // The fraction converges quickly only below x = (a+1)/(a+b+2); reflect above it.
    //
    // The comparison MUST include the boundary.  With a strict `<`, an argument sitting
    // exactly on it reflects to `(1-x, b, a)`, whose own boundary is `1 - (a+1)/(a+b+2)` --
    // the same point -- so it reflects straight back and recurses until the stack dies.
    // `I_0.5(1, 1)` is exactly that case, and it is not an exotic one.
    if x <= (a + 1.0) / (a + b + 2.0) then
      front.flatMap(f => betaContinuedFraction(x, a, b).flatMap(cf => finite(f * cf / a)))
    else
      incompleteBetaOf(1.0 - x, b, a).flatMap(i => finite(1.0 - i))

/** The continued fraction of the incomplete beta, by modified Lentz. */
private def betaContinuedFraction(x: Double, a: Double, b: Double): Option[Double] =
  val qab = a + b
  val qap = a + 1.0
  val qam = a - 1.0
  var c    = 1.0
  var d    = 1.0 - qab * x / qap
  if abs(d) < TinyFloor then d = TinyFloor
  d = 1.0 / d
  var h    = d
  var m    = 1
  var done = false
  while m <= MaxSeriesIterations && !done do
    val m2 = 2 * m
    // even step
    var aa = m * (b - m) * x / ((qam + m2) * (a + m2))
    d = 1.0 + aa * d
    if abs(d) < TinyFloor then d = TinyFloor
    c = 1.0 + aa / c
    if abs(c) < TinyFloor then c = TinyFloor
    d = 1.0 / d
    h *= d * c
    // odd step
    aa = -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2))
    d = 1.0 + aa * d
    if abs(d) < TinyFloor then d = TinyFloor
    c = 1.0 + aa / c
    if abs(c) < TinyFloor then c = TinyFloor
    d = 1.0 / d
    val del = d * c
    h *= del
    if abs(del - 1.0) < SeriesEpsilon then done = true
    m += 1
  if !done then None else finite(h)


/** The gamma function over a **complex** argument, as a `(re, im)` pair.
 *
 *  The same Lanczos approximation as [[gammaOf]] — the coefficients are shared, since the
 *  series is valid over the half-plane `Re(z) ≥ 0.5` and not merely over the real ray — with
 *  the reflection formula `Γ(z) = π / (sin(πz)·Γ(1−z))` covering the rest.
 *
 *  Kept on raw `Double` pairs rather than on `core._Complex`: the companion's kernels return
 *  `Option[_Value]`, so a loop written against them would allocate and unwrap an `Option` on
 *  every one of the nine terms.  `_Complex` is the boundary type, not the arithmetic type.
 *
 *  @param re the real part of the argument
 *  @param im the imaginary part
 *  @return `Some((re, im))` of `Γ(z)`, or `None` at the real poles and for non-finite input
 *  @see [[https://en.wikipedia.org/wiki/Lanczos_approximation Lanczos approximation]]
 */
def gammaComplex(re: Double, im: Double): Option[(Double, Double)] =
  if re.isNaN || im.isNaN || re.isInfinite || im.isInfinite then None
  else if im == 0.0 then gammaOf(re).map(g => (g, 0.0))          // stay on the real kernel
  else if re < 0.5 then
    // reflection: Γ(z) = π / (sin(πz) · Γ(1−z))
    val (sr, si) = cSin(Pi * re, Pi * im)
    gammaComplex(1.0 - re, -im).flatMap { (gr, gi) =>
      val (dr, di) = cMul(sr, si, gr, gi)
      cDiv(Pi, 0.0, dr, di).flatMap(finitePair)
    }
  else
    val zr = re - 1.0
    val zi = im
    var xr = LanczosCoefficients(0)
    var xi = 0.0
    var i  = 1
    while i < LanczosCoefficients.length do
      // x += c_i / (z + i)
      cDiv(LanczosCoefficients(i), 0.0, zr + i, zi) match
        case Some((qr, qi)) => xr += qr; xi += qi
        case None           => ()      // a pole of this term; the sum simply skips it
      i += 1
    val tr = zr + LanczosG + 0.5
    val ti = zi
    // Γ(z) = √(2π) · t^(z+1/2) · e^(−t) · x
    val (pr, pi) = cPow(tr, ti, zr + 0.5, zi)
    val (er, ei) = cExp(-tr, -ti)
    val (m1r, m1i) = cMul(pr, pi, er, ei)
    val (m2r, m2i) = cMul(m1r, m1i, xr, xi)
    finitePair(sqrt(2.0 * Pi) * m2r, sqrt(2.0 * Pi) * m2i)

/** Rejects a non-finite complex result, so the caller stays symbolic. */
private def finitePair(p: (Double, Double)): Option[(Double, Double)] = finitePair(p._1, p._2)

/** Rejects a non-finite complex result, so the caller stays symbolic. */
private def finitePair(r: Double, i: Double): Option[(Double, Double)] =
  Option.when(!r.isNaN && !i.isNaN && !r.isInfinite && !i.isInfinite)((r, i))

private def cMul(ar: Double, ai: Double, br: Double, bi: Double): (Double, Double) =
  (ar * br - ai * bi, ar * bi + ai * br)

private def cDiv(ar: Double, ai: Double, br: Double, bi: Double): Option[(Double, Double)] =
  val d = br * br + bi * bi
  if d == 0.0 then None else Some(((ar * br + ai * bi) / d, (ai * br - ar * bi) / d))

private def cExp(r: Double, i: Double): (Double, Double) =
  val m = exp(r)
  (m * Math.cos(i), m * sin(i))

private def cLog(r: Double, i: Double): (Double, Double) =
  (log(sqrt(r * r + i * i)), Math.atan2(i, r))

/** `a^b` for complex `a`, `b`, as `exp(b · log a)`. */
private def cPow(ar: Double, ai: Double, br: Double, bi: Double): (Double, Double) =
  val (lr, li) = cLog(ar, ai)
  val (mr, mi) = cMul(br, bi, lr, li)
  cExp(mr, mi)

private def cSin(r: Double, i: Double): (Double, Double) =
  (sin(r) * Math.cosh(i), Math.cos(r) * Math.sinh(i))

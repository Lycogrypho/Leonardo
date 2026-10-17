package it.grypho.scala.leonardo
package scalar

import core.*


/** Dense real-coefficient polynomial helpers, shared by every tier that consumes
 *  [[collect]]'s output.
 *
 *  `collect(e, v)` yields a **dense** vector in which `cs(i)` multiplies `v^i`.  Several
 *  tiers then need the same handful of operations on it, and originally each carried
 *  its own copy: [[integrate]]'s partial-fraction tier and the inverse Laplace
 *  transform's residue rule had independently written both a root finder and a derivative
 *  helper, and the pole classification was about to add a third.
 *
 *  **The copies were not equivalent, but the difference was unreachable — and knowing why
 *  is what makes merging them safe.**  They disagreed about a coefficient vector carrying a
 *  *trailing zero*: [[polyDegree]] reads such a vector at its true degree, while the inverse
 *  transform's copy took `size - 1`, found a zero leading coefficient and gave up.  Those
 *  are different answers for `[1, 0, 1, 0]`.
 *
 *  No caller can produce one.  Every vector reaching these helpers comes from [[collect]],
 *  and `collect` finishes with `trimTrailingZeros` applied *after* `simplifyFully`, so a
 *  cancelling top term folds to a literal zero and is dropped before it is ever returned:
 *  `collect(v^3 - v^3 + v^2 + 1, v)` is `[1, 0, 1]`, not `[1, 0, 1, 0]`.  The merge is
 *  therefore behaviour-preserving, and `PolynomialTest` pins that invariant precisely
 *  because the safety argument rests on it.
 *
 *  The trimming reading is kept anyway, as the defensive one: a vector whose leading
 *  coefficient is zero *is* a lower-degree polynomial, so should a future caller hand one
 *  over directly, it is solved rather than declined.
 *
 *  Everything here is `private[leonardo]`: shared across packages, but not public API.
 */

/** Numeric tolerance for treating a polynomial coefficient or discriminant as zero. */
private[leonardo] val RationalEps = 1e-9

/** Largest polynomial degree [[rationalCoeffs]] will build, guarding against blow-up.  Fixed,
 *  not a `core.Limit` (F_0124 phase C), for the reason `collect`'s power cap is: the pure
 *  polynomial helpers take no environment, and the degree is part of what a dozen tiers agree
 *  a rational function is. */
private[leonardo] val MaxRationalDegree = 24

/** Folds an expression into `(numerator, denominator)` coefficient vectors in `v`.
 *
 *  **Decidable and total**: `+`, `*`, `/` and integer powers combine, and *anything else
 *  fails* — an `exp(v)` or an unbound parameter makes the whole normalisation return `None`,
 *  so nothing is ever half-converted.  Same principle as the Weierstrass rationality test in
 *  `IntegrateSubstitution`, which folds a substituted tree into one fraction over `t`.
 *
 *  **Why this is not [[collect]].**  `collect` answers "what are the coefficients of this
 *  *polynomial*", and returns `None` for a `Ratio`.  Callers here hold a rational *function* —
 *  a transfer function, a z-domain expression — and need it split, which no amount of
 *  `collect` will do.
 *
 *  **Why it is here rather than in its first caller.**  It began as a private helper inside
 *  `transform.InverseZTransform`, where matching `Ratio(num, den)` at the top turned
 *  out to be wrong: the z-transform's own output for `Z{n}` is `(-1*z)*(.../(z-1)^2)`, a
 *  `Product` whose left factor depends on `z`, so the inverse handled some of its own results
 *  and not others.  The control domain then needed the same split for `poles`/`zeros`, making it the third
 *  place wanting one fraction out of an arbitrary tree — the point at which this file's
 *  convention promotes a helper rather than let a third copy diverge.
 *
 *  @param e the expression to normalise
 *  @param v the variable the rational function is over
 *  @return `Some((numerator, denominator))` as dense coefficient vectors, or `None`
 */
private[leonardo] def rationalCoeffs(e: _Expression,
                                     v: _Variable): Option[(Vector[Double], Vector[Double])] =
  rationalCoeffsIn(e, v, new Environment())

/** [[rationalCoeffs]] with `env`'s bindings folded into the coefficients (`singularities` reads a
 *  bound parameter as its value).
 *  @param e   the expression
 *  @param v   the variable
 *  @param env the bindings a coefficient is evaluated in
 *  @return the numerator and denominator coefficients, or `None` */
private[leonardo] def rationalCoeffsIn(e: _Expression, v: _Variable,
                                       env: Environment): Option[(Vector[Double], Vector[Double])] =
  rationalCoeffsM(e, v, env).map((n, d) => (n.map(_._1), d.map(_._1)))

/** The relative size below which a coefficient is rounding of the terms that cancelled into it,
 *  not value (F_0068, F_0119): a coefficient computed from terms of size `m` carries an error of
 *  order `m·ε`, so one at most `1e-10·m` is indistinguishable from zero, and one above it is not. */
private[leonardo] val RoundingLevel: Double = 1e-10

/** A coefficient vector with, beside each coefficient, `Σ|terms|` that formed it (F_0119). */
private type MPoly = Vector[(Double, Double)]

private val MOne: MPoly = Vector((1.0, 1.0))

/** Drops top coefficients that are exactly zero or rounding of their terms: never a genuine one,
 *  however small. The zero polynomial is `[(0, m)]`, `m` the largest magnitude that cancelled —
 *  kept, because [[magnitudeOf]] measures a residue against exactly that. */
private def mTrim(p: MPoly): MPoly =
  val top = p.lastIndexWhere((c, m) => c != 0.0 && math.abs(c) > RoundingLevel * m)
  if top < 0 then Vector((0.0, p.map(_._2).maxOption.getOrElse(0.0))) else p.take(top + 1)

private def mAdd(a: MPoly, b: MPoly): MPoly =
  mTrim(Vector.tabulate(math.max(a.size, b.size)) { i =>
    val ((ca, ma), (cb, mb)) = (a.lift(i).getOrElse((0.0, 0.0)), b.lift(i).getOrElse((0.0, 0.0)))
    (ca + cb, ma + mb)
  })

private def mMul(a: MPoly, b: MPoly): MPoly =
  val c = Array.fill(a.size + b.size - 1)(0.0)
  val m = Array.fill(a.size + b.size - 1)(0.0)
  for i <- a.indices; j <- b.indices do
    c(i + j) += a(i)._1 * b(j)._1
    m(i + j) += a(i)._2 * b(j)._2
  mTrim(c.indices.toVector.map(k => (c(k), m(k))))

private def mIsZero(p: MPoly): Boolean = p.forall(_._1 == 0.0)

/** The polynomial fraction of `e` in `v` with magnitudes (F_0119).  **Structural throughout**,
 *  constant subexpressions included — `0.1·0.3 − 0.03` is seen as a cancellation of terms of size
 *  0.06, not as the number `3.5·10⁻¹⁸` — and each result is trimmed by [[mTrim]], so a genuine tiny
 *  coefficient survives where the absolute `RationalEps` used to drop it: `1e-12·x² + x + 1` was
 *  read as linear, losing a pole, a stationary point and the right antiderivative. */
private def rationalCoeffsM(e: _Expression, v: _Variable, env: Environment): Option[(MPoly, MPoly)] =
  def capped(p: (MPoly, MPoly)): Option[(MPoly, MPoly)] =
    Option.when(p._1.size <= MaxRationalDegree + 1 && p._2.size <= MaxRationalDegree + 1)(p)

  def numeric(x: _Expression): Option[Double] = x.eval(env) match
    case Right(_Number(d)) if !d.isNaN && !d.isInfinite => Some(d)
    case _                                             => None

  e match
    case x: _Variable if x.variable == v.variable => Some((Vector((0.0, 0.0), (1.0, 1.0)), MOne))

    case Sum(a, b) =>
      for (an, ad) <- rationalCoeffsM(a, v, env); (bn, bd) <- rationalCoeffsM(b, v, env)
          r <- capped((mAdd(mMul(an, bd), mMul(bn, ad)), mMul(ad, bd)))
      yield r

    case Product(a, b) =>
      for (an, ad) <- rationalCoeffsM(a, v, env); (bn, bd) <- rationalCoeffsM(b, v, env)
          r <- capped((mMul(an, bn), mMul(ad, bd)))
      yield r

    case Ratio(a, b) =>
      // X / 0 is refused, the zero polynomial being [(0, 0)] after mTrim.
      for (an, ad) <- rationalCoeffsM(a, v, env); (bn, bd) <- rationalCoeffsM(b, v, env)
          if !mIsZero(bn)
          r <- capped((mMul(an, bd), mMul(ad, bn)))
      yield r

    case Power(b, _Number(k)) if k.toInt.toDouble == k && math.abs(k) <= MaxRationalDegree =>
      rationalCoeffsM(b, v, env).flatMap { (bn, bd) =>
        // A negative power swaps numerator and denominator, so the zero polynomial must be
        // refused BEFORE the swap puts it underneath (the Ratio reasoning).
        // A non-negative power of zero stays legitimate: it is the zero numerator.
        if k < 0 && mIsZero(bn) then None
        else
          val (num, den) = (1 to math.abs(k.toInt)).foldLeft((MOne, MOne)) {
            case ((accN, accD), _) => (mMul(accN, bn), mMul(accD, bd))
          }
          capped(if k >= 0 then (num, den) else (den, num))
      }

    case _ if !dependsOn(e, v) => numeric(e).map(d => (Vector((d, math.abs(d))), MOne))

    case _ => None

/** The size of the terms that cancel in a numeric expression — `Σ|t|` through its sums,
 *  products, quotients and integer powers, with no folding — or `None` when `e` is not numeric
 *  in `env` (F_0119). A residue is small relative to this, not to an absolute 1.
 *  @param e   the expression, numeric once `env` is applied
 *  @param env the bindings
 *  @return the magnitude */
private[leonardo] def magnitudeOf(e: _Expression, env: Environment): Option[Double] =
  rationalCoeffsM(e, _Variable("__magnitudeOf"), env).flatMap { (num, den) =>
    (num, den) match
      case (Vector((_, m)), Vector((d, _))) if d != 0.0 => Some(m / math.abs(d))
      case _                                          => None
  }

/** The roots of a polynomial found **at its root scale** (F_0121): `x = ρy` with `ρ` =
 *  [[rootScale]], the companion matrix of the monic polynomial in `y` handed to [[polyRoots]],
 *  each root mapped back by `ρ`. The degree is the exact one ([[degreeOf]]), so a genuine tiny
 *  leading coefficient keeps its root; [[polyRoots]] on the raw vector reads degree through the
 *  absolute `RationalEps` and dropped it — `1e-12·s² + s + 1` had one pole of two.
 *
 *  One scale cannot fit roots that are themselves far apart: at `ρ = 1e12` the root near `−1`
 *  is `−1e-12` in `y`, and the companion iteration's absolute error there was a relative 9e-5
 *  (`−1.0000889`). So each root is **polished** by Newton's method on the original
 *  coefficients, a step kept only while it reduces `|p|`, which recovers every root to the
 *  accuracy its own conditioning allows.
 *  @param cs a coefficient vector, already trimmed for its provenance
 *  @return the roots, `_Number` or `_Complex`, or `None` below degree 1 or when the iteration fails */
private[leonardo] def rootsAtScale(cs: Vector[Double]): Option[Vector[_Value]] =
  val n = degreeOf(cs)
  if n < 1 then None
  else
    val rho = rootScale(cs)
    if rho == 0.0 then Some(Vector.fill(n)(_Number(0.0)))
    else
      val p = cs.take(n + 1)
      polyRoots(toRootScale(cs, rho)).map(_.map {
        case c: _Complex => _Complex.parts(c).fold(c: _Value) { (re, im) =>
          val (pr, pi) = polish(p, re * rho, im * rho)
          _Complex.of(pr, pi)
        }
        case _Number(d)  => _Number(polish(p, d * rho, 0.0)._1)
        case other       => other
      })

/** `p` and `p′` at the complex point `x + iy`, by Horner. */
private def hornerComplex(p: Vector[Double], x: Double, y: Double): (Double, Double, Double, Double) =
  p.foldRight((0.0, 0.0, 0.0, 0.0)) { case (c, (vr, vi, dr, di)) =>
    // p' accumulates before p is advanced: d ← d·z + v, v ← v·z + c.
    val ndr = dr * x - di * y + vr
    val ndi = dr * y + di * x + vi
    (vr * x - vi * y + c, vr * y + vi * x, ndr, ndi)
  }

/** Newton's method on `p` from `x + iy`, each step kept only while it reduces `|p|` (F_0121).
 *  A real start stays real, since every step from a real point on a real polynomial is real. */
private def polish(p: Vector[Double], x0: Double, y0: Double): (Double, Double) =
  @annotation.tailrec
  def go(x: Double, y: Double, size: Double, left: Int): (Double, Double) =
    if left == 0 || size == 0.0 then (x, y)
    else
      val (vr, vi, dr, di) = hornerComplex(p, x, y)
      val d2 = dr * dr + di * di
      if d2 == 0.0 || d2.isInfinite || d2.isNaN then (x, y)
      else
        // z ← z − p/p′
        val nx = x - (vr * dr + vi * di) / d2
        val ny = y - (vi * dr - vr * di) / d2
        val (wr, wi, _, _) = hornerComplex(p, nx, ny)
        val next = math.hypot(wr, wi)
        if next < size && !nx.isNaN && !ny.isNaN then go(nx, ny, next, left - 1) else (x, y)
  val (vr, vi, _, _) = hornerComplex(p, x0, y0)
  go(x0, y0, math.hypot(vr, vi), 8)

/** `collect`'s coefficients (or any vector read from `e` in `v`) cut to the degree the
 *  magnitude-tracked coefficients give, when `e` is a numeric polynomial; unchanged otherwise
 *  (F_0121, shared by F_0125). `collect` tracks no magnitudes, so a rounding-residue top
 *  coefficient would survive it; this drops that one and keeps a genuine tiny one.
 *  @param cs  the coefficients, constant term first
 *  @param e   the polynomial they were read from
 *  @param v   the variable
 *  @param env bindings
 *  @return `cs` to the magnitude-tracked degree */
private[leonardo] def atMagnitudeDegree[A](cs: Vector[A], e: _Expression, v: _Variable,
                                           env: Environment): Vector[A] =
  rationalCoeffsIn(e, v, env) match
    case Some((num, den)) if degreeOf(den) == 0 => cs.take(math.max(degreeOf(num), 0) + 1)
    case _                                     => cs

/** The exact degree of a coefficient vector: the last non-zero coefficient, `-1` for none.  For
 *  vectors already trimmed by magnitude (from [[rationalCoeffs]]), where [[polyDegree]]'s
 *  absolute `RationalEps` would drop a genuine tiny top coefficient (F_0119).
 *  @param cs the coefficients
 *  @return the degree */
private[leonardo] def degreeOf(cs: Vector[Double]): Int = cs.lastIndexWhere(_ != 0.0)

/** `cs` up to its exact degree, `[0.0]` for the zero polynomial: [[polyTrim]] without the
 *  absolute `RationalEps`, for vectors already trimmed by magnitude (F_0121).
 *  @param cs the coefficients
 *  @return the trimmed coefficients */
private[leonardo] def trimExact(cs: Vector[Double]): Vector[Double] =
  val d = degreeOf(cs)
  if d < 0 then Vector(0.0) else cs.take(d + 1)

/** Degree of a coefficient vector: the highest index carrying a non-negligible
 *  coefficient, or `-1` for the zero polynomial.
 *
 *  This is the *true* degree, which is **not** `cs.size - 1` whenever the vector carries
 *  trailing zeros — see the note in the file overview.
 *
 *  @param cs dense coefficient vector, `cs(i)` multiplying the `i`-th power
 *  @return the degree, or `-1` when every coefficient is negligible
 */
private[leonardo] def polyDegree(cs: Vector[Double]): Int =
  cs.lastIndexWhere(c => math.abs(c) > RationalEps)

/** Derivative coefficient vector `[c1, 2*c2, ..., n*cn]`.
 *
 *  Returns `[0.0]` for a constant or empty input rather than an empty vector, so a caller
 *  may evaluate the result unconditionally.  (The inverse transform's former copy omitted
 *  that guard and threw on an empty input; it never reached one, but the guard is free.)
 *
 *  @param cs dense coefficient vector
 *  @return the coefficient vector of the derivative
 */
private[leonardo] def derivCoeffs(cs: Vector[Double]): Vector[Double] =
  if cs.sizeIs <= 1 then Vector(0.0) else cs.zipWithIndex.tail.map((c, i) => i.toDouble * c)

/** Roots of a polynomial, as the eigenvalues of its [[https://en.wikipedia.org/wiki/Companion_matrix Frobenius companion matrix]].
 *
 *  Each root is a real [[core._Number]] or a non-real [[core._Complex]].  The companion
 *  matrix is built at the *true* degree reported by [[polyDegree]], so a vector with
 *  trailing zeros is solved at the degree it actually has.
 *
 *  @param cs dense coefficient vector, `cs(i)` multiplying the `i`-th power
 *  @return the roots, or `None` when the true degree is below 1 or the QR iteration fails
 */
private[leonardo] def polyRoots(cs: Vector[Double]): Option[Vector[_Value]] =
  val n = polyDegree(cs)
  if n < 1 then None
  else
    val cn  = cs(n)
    val mat = Array.fill(n * n)(0.0)
    for i <- 0 until n do mat(i * n + (n - 1)) = -cs(i) / cn  // last column
    for i <- 1 until n do mat(i * n + (i - 1)) = 1.0          // sub-diagonal
    _MatrixValue(n, n, mat).eigenDecompose


// ── Arithmetic over coefficient vectors ─────────────────────────────
//
// These grew inside `Integrate.scala` as the integration tiers were built, which recreated
// exactly the divergence this file exists to merge away. They live here now, so the
// three `polyRoots` consumers — the integrator, `Singularity` and the inverse Laplace
// transform — share one implementation rather than one each.

/** Trims trailing (high-order) negligible coefficients to the true degree; `[0.0]` for zero. */
private[leonardo] def polyTrim(a: Vector[Double]): Vector[Double] =
  val d = polyDegree(a)
  if d < 0 then Vector(0.0) else a.take(d + 1)

/** Coefficient-wise combination of two polynomials under `f`, trimmed — the shared body of
 *  [[polyAdd]] and [[polySub]], which were near-duplicates before being merged. */
private def polyZipWith(a: Vector[Double], b: Vector[Double])(f: (Double, Double) => Double): Vector[Double] =
  val n = math.max(a.length, b.length)
  polyTrim(Vector.tabulate(n)(i => f(a.lift(i).getOrElse(0.0), b.lift(i).getOrElse(0.0))))

/** Coefficient-wise sum `a + b`, trimmed. */
private[leonardo] def polyAdd(a: Vector[Double], b: Vector[Double]): Vector[Double] =
  polyZipWith(a, b)(_ + _)

/** Coefficient-wise difference `a - b`, trimmed. */
private[leonardo] def polySub(a: Vector[Double], b: Vector[Double]): Vector[Double] =
  polyZipWith(a, b)(_ - _)

/** Real polynomial multiplication (convolution). */
private[leonardo] def polyMul(a: Vector[Double], b: Vector[Double]): Vector[Double] =
  val out = Array.fill(a.length + b.length - 1)(0.0)
  for i <- a.indices; j <- b.indices do out(i + j) += a(i) * b(j)
  out.toVector

/** Normalises to a monic polynomial (leading coefficient 1); unchanged for the zero/constant. */
private[leonardo] def polyMonic(a: Vector[Double]): Vector[Double] =
  val d = polyDegree(a)
  if d < 0 then a else a.take(d + 1).map(_ / a(d))

/** Polynomial long division `num / den` -> `(quotient, remainder)` coefficient vectors
 *  (`num = quotient * den + remainder`, `deg remainder < deg den`). */
private[leonardo] def polyDivide(num: Vector[Double], den: Vector[Double],
                                 degree: Vector[Double] => Int = polyDegree): (Vector[Double], Vector[Double]) =
  val dd   = degree(den)
  val lc   = den(dd)
  val dnum = degree(num)
  if dnum < dd then (Vector(0.0), num)
  else
    val rr = num.toArray.clone()
    val q  = Array.fill(dnum - dd + 1)(0.0)
    var k  = dnum - dd
    while k >= 0 do
      val coef = rr(dd + k) / lc
      q(k) = coef
      var i = 0
      while i <= dd do
        rr(k + i) -= coef * den(i)
        i += 1
      k -= 1
    (q.toVector, rr.toVector)

/** Horner evaluation of a real-coefficient polynomial at a real point. */
private[leonardo] def evalCoeffsReal(cs: Vector[Double], x: Double): Double =
  cs.foldRight(0.0)((c, acc) => acc * x + c)

/** Monic polynomial GCD by the Euclidean algorithm (tolerance via [[polyDegree]]). */
private[leonardo] def polyGcd(a0: Vector[Double], b0: Vector[Double]): Vector[Double] =
  var a = polyTrim(a0)
  var b = polyTrim(b0)
  while polyDegree(b) >= 0 do
    val (_, r) = polyDivide(a, b)
    a = b
    b = polyTrim(r)
  polyMonic(a)

/** [[https://en.wikipedia.org/wiki/Square-free_polynomial Square-free factorisation]] ([[https://en.wikipedia.org/wiki/Square-free_polynomial#Yun's_algorithm Yun]]): each returned `(poly, k)` is the product of the distinct
 *  factors of `d0` that occur with multiplicity exactly `k`, so `poly` has only simple roots.
 *
 *  **This is how multiplicity is read anywhere in the library**, and it exists because
 *  [[polyRoots]] cannot do the job: the QR iteration does not merely *scatter* a repeated
 *  root, it can fail to converge on one outright (a pure `(x−1)³` returns nothing).  Splitting
 *  the polynomial by multiplicity is purely arithmetic — only the square-free parts, whose
 *  roots are simple and well-conditioned, are ever handed to a root finder.
 *
 *  @param d0 dense coefficient vector
 *  @return `(square-free factor, multiplicity)` pairs, ascending in multiplicity
 */
private[leonardo] def squareFreeFactors(d0: Vector[Double]): Vector[(Vector[Double], Int)] =
  // Only exact zeros are trimmed here (F_0119): a caller hands over a polynomial already trimmed
  // for its provenance, and the absolute RationalEps would drop a genuine tiny top coefficient.
  // Yun's own arithmetic runs at the root scale, where the absolute trim is effectively relative.
  val dd = d0.take(degreeOf(d0) + 1)
  if degreeOf(dd) < 1 then Vector.empty
  else
    // At the roots' own scale (F_0118): Yun's gcds trim with an absolute tolerance, so near the
    // origin every Euclidean remainder read as zero and distinct roots merged into a repeated one.
    val rho = rootScale(dd)
    if rho == 0.0 || rho == 1.0 then yun(dd)
    else yun(toRootScale(dd, rho)).map((p, k) => (fromRootScale(p, rho), k))

/** Whether a root, among one factor's roots, is real: its imaginary part at most `tol` times the
 *  **largest modulus among them**, the factor's own root scale.  It replaced three tests that
 *  were absolute below unit scale — `1e-9·max(1, |re|)` in `optimize` (F_0109), the same with
 *  `RootClusterTolerance` in `Singularity`, and `|im| < 1e-6` in the partial-fraction tier
 *  (F_0118): `10⁻⁶ ± 10⁻¹⁰i` read as real at those floors, and its copy in units 10⁶ larger did not.
 *
 *  @param roots the roots of one factor, as `(re, im)`
 *  @param tol   the relative tolerance
 *  @return the test for one of them
 */
private[leonardo] def realAmong(roots: Seq[(Double, Double)], tol: Double): ((Double, Double)) => Boolean =
  val scale = roots.map((re, im) => math.hypot(re, im)).maxOption.getOrElse(0.0)
  (_, im) => math.abs(im) <= tol * scale

/** The real parts of the roots that are real by [[realAmong]].
 *  @param roots the roots of one factor, as `(re, im)`
 *  @param tol   the relative tolerance
 *  @return those real roots */
private[leonardo] def realParts(roots: Seq[(Double, Double)], tol: Double = 1e-9): Seq[Double] =
  roots.filter(realAmong(roots, tol)).map(_._1)

/** The scale of the roots, `M = maxₖ |cₙ₋ₖ/cₙ|^(1/k)`: between half the largest root's modulus
 *  (Fujiwara's bound is `2M`) and `n` times it, and exactly proportional to the variable's units.
 *  `0` for a pure power of the variable, whose roots are all `0`. This is the scale the
 *  polynomial's own arithmetic happens at (F_0118).
 *
 *  **Why `M` and not Fujiwara's `2M`**: the first version used `2M`, which put a root pair at
 *  `10⁻⁶(1 ± 10⁻⁴i)` near `y = 0.25` and shrank the Euclidean remainder that tells the pair apart
 *  sixteen-fold, to `6·10⁻¹⁰` — below the absolute trim, so the pair still merged. Within a
 *  constant factor every choice is units-invariant; the one nearest the roots loses least.
 *
 *  @param cs a trimmed coefficient vector of degree at least 1
 *  @return the scale
 */
private[leonardo] def rootScale(cs: Vector[Double]): Double =
  val n = degreeOf(cs)
  if n < 1 then 0.0
  else (1 to n).map(k => math.pow(math.abs(cs(n - k) / cs(n)), 1.0 / k)).max

/** `p(ρ·y)` made monic: the polynomial in `y = x/ρ`, whose roots are of order one. Written as
 *  `(cᵢ/cₙ)·ρ^(i−n)` so neither `ρⁿ` nor `cₙ` can overflow on its own. */
private[leonardo] def toRootScale(cs: Vector[Double], rho: Double): Vector[Double] =
  val n = degreeOf(cs)
  Vector.tabulate(n + 1)(i => cs(i) / cs(n) * math.pow(rho, (i - n).toDouble))

/** The inverse of [[toRootScale]] on a monic factor `q(y)` of degree `d`: `ρᵈ·q(x/ρ)`, monic in `x`. */
private[leonardo] def fromRootScale(q: Vector[Double], rho: Double): Vector[Double] =
  val d = polyDegree(q)
  Vector.tabulate(d + 1)(i => q(i) / q(d) * math.pow(rho, (d - i).toDouble))

/** Yun's algorithm itself, on a trimmed polynomial of degree at least 1 (see [[squareFreeFactors]]).
 *
 *  **Limit, inherent to one scale**: roots at `10⁻⁶` and `10⁶` in the *same* polynomial are
 *  separated at the large one's scale, so the small ones still meet the absolute tolerance. */
private def yun(dd: Vector[Double]): Vector[(Vector[Double], Int)] =
  if polyDegree(dd) < 1 then Vector.empty
  else
    val g  = polyGcd(dd, polyTrim(derivCoeffs(dd)))
    var c  = polyTrim(polyDivide(dd, g)._1)
    var w  = polySub(polyTrim(polyDivide(polyTrim(derivCoeffs(dd)), g)._1), polyTrim(derivCoeffs(c)))
    val out = scala.collection.mutable.ListBuffer[(Vector[Double], Int)]()
    var k  = 1
    while polyDegree(c) >= 1 do
      val p = polyGcd(c, w)
      if polyDegree(p) >= 1 then out += ((polyMonic(p), k))
      c = polyTrim(polyDivide(c, p)._1)
      w = polySub(polyTrim(polyDivide(w, p)._1), polyTrim(derivCoeffs(c)))
      k += 1
    out.toVector

/** Roots of a square-free polynomial as `(re, im)` pairs; degree 1 and 2 are solved
 *  analytically (robust), an even polynomial by the `s = t²` reduction (the QR iteration
 *  does not converge on purely imaginary conjugate pairs, and the Weierstrass denominators
 *  are exactly that shape), degree ≥ 3 otherwise via [[polyRoots]].
 *
 *  @param poly a square-free coefficient vector (simple roots)
 *  @return the roots as `(real, imaginary)` pairs, or `None` when none can be found
 */
private[leonardo] def rootsOfSquareFree(poly: Vector[Double]): Option[Vector[(Double, Double)]] =
  polyDegree(poly) match
    case 1 => Some(Vector((-poly(0) / poly(1), 0.0)))
    case 2 =>
      val a = poly(2); val b = poly(1); val c = poly(0)
      val disc = b * b - 4 * a * c
      if disc >= 0 then
        val s = math.sqrt(disc)
        Some(Vector(((-b + s) / (2 * a), 0.0), ((-b - s) / (2 * a), 0.0)))
      else
        val s = math.sqrt(-disc)
        Some(Vector((-b / (2 * a), s / (2 * a)), (-b / (2 * a), -s / (2 * a))))
    case d if d >= 3 && poly.indices.forall(i => i % 2 == 0 || math.abs(poly(i)) <= RationalEps) =>
      // even polynomial p(t) = q(t²): solve q, then each root s of q yields t = ±√s
      // (the principal complex square root and its negation)
      rootsOfSquareFree(Vector.tabulate(d / 2 + 1)(i => poly(2 * i))).map(_.flatMap { (a, b) =>
        val r  = math.sqrt(math.hypot(a, b))
        val th = math.atan2(b, a) / 2.0
        val (re, im) = (r * math.cos(th), r * math.sin(th))
        Vector((re, im), (-re, -im))
      })
    case d if d >= 3 => polyRoots(poly).map(_.flatMap(_Complex.parts))
    case _           => None

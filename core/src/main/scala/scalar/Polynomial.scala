package it.grypho.scala.leonardo
package scalar

import core.*


/** Dense real-coefficient polynomial helpers, shared by every tier that consumes
 *  [[collect]]'s output.
 *
 *  `collect(e, v)` yields a **dense** vector in which `cs(i)` multiplies `v^i`.  Several
 *  tiers then need the same handful of operations on it, and until issue 3.1 each carried
 *  its own copy: [[integrate]]'s partial-fraction tier (4.C) and the inverse Laplace
 *  transform's residue rule had independently written both a root finder and a derivative
 *  helper, and 6.13's pole classification was about to add a third.
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

/** Largest polynomial degree [[rationalCoeffs]] will build, guarding against blow-up. */
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
 *  `transform.InverseZTransform` (6.33), where matching `Ratio(num, den)` at the top turned
 *  out to be wrong: the z-transform's own output for `Z{n}` is `(-1*z)*(.../(z-1)^2)`, a
 *  `Product` whose left factor depends on `z`, so the inverse handled some of its own results
 *  and not others.  6.29 then needed the same split for `poles`/`zeros`, making it the third
 *  place wanting one fraction out of an arbitrary tree — the point at which issues 3.1 and 2.5
 *  each promoted a helper into this file rather than let a third copy diverge.
 *
 *  @param e the expression to normalise
 *  @param v the variable the rational function is over
 *  @return `Some((numerator, denominator))` as dense coefficient vectors, or `None`
 */
private[leonardo] def rationalCoeffs(e: _Expression,
                                     v: _Variable): Option[(Vector[Double], Vector[Double])] =
  def capped(p: (Vector[Double], Vector[Double])): Option[(Vector[Double], Vector[Double])] =
    Option.when(p._1.size <= MaxRationalDegree + 1 && p._2.size <= MaxRationalDegree + 1)(p)

  def numeric(x: _Expression): Option[Double] = x.eval(new Environment()) match
    case Right(_Number(d)) => Some(d)
    case _                 => None

  e match
    case _ if !dependsOn(e, v) => numeric(e).map(d => (Vector(d), Vector(1.0)))

    case x: _Variable if x.variable == v.variable => Some((Vector(0.0, 1.0), Vector(1.0)))

    case Sum(a, b) =>
      for (an, ad) <- rationalCoeffs(a, v); (bn, bd) <- rationalCoeffs(b, v)
          r <- capped((polyAdd(polyMul(an, bd), polyMul(bn, ad)), polyMul(ad, bd)))
      yield r

    case Product(a, b) =>
      for (an, ad) <- rationalCoeffs(a, v); (bn, bd) <- rationalCoeffs(b, v)
          r <- capped((polyMul(an, bn), polyMul(ad, bd)))
      yield r

    case Ratio(a, b) =>
      for (an, ad) <- rationalCoeffs(a, v); (bn, bd) <- rationalCoeffs(b, v)
          if polyTrim(bn).nonEmpty
          r <- capped((polyMul(an, bd), polyMul(ad, bn)))
      yield r

    case Power(b, _Number(k)) if k.toInt.toDouble == k && math.abs(k) <= MaxRationalDegree =>
      rationalCoeffs(b, v).flatMap { (bn, bd) =>
        val (num, den) = (1 to math.abs(k.toInt)).foldLeft((Vector(1.0), Vector(1.0))) {
          case ((accN, accD), _) => (polyMul(accN, bn), polyMul(accD, bd))
        }
        capped(if k >= 0 then (num, den) else (den, num))
      }

    case _ => None

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

/** Roots of a polynomial, as the eigenvalues of its Frobenius companion matrix.
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


// ── Arithmetic over coefficient vectors (issue 2.5) ─────────────────────────────
//
// These grew inside `Integrate.scala` while issues 3.12–3.16 were built, which recreated
// exactly the divergence issue 3.1 merged away once before. They live here now, so the
// three `polyRoots` consumers — the integrator, `Singularity` and the inverse Laplace
// transform — share one implementation rather than one each.

/** Trims trailing (high-order) negligible coefficients to the true degree; `[0.0]` for zero. */
private[leonardo] def polyTrim(a: Vector[Double]): Vector[Double] =
  val d = polyDegree(a)
  if d < 0 then Vector(0.0) else a.take(d + 1)

/** Coefficient-wise combination of two polynomials under `f`, trimmed — the shared body of
 *  [[polyAdd]] and [[polySub]], which were near-duplicates before issue 2.5. */
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
private[leonardo] def polyDivide(num: Vector[Double], den: Vector[Double]): (Vector[Double], Vector[Double]) =
  val dd   = polyDegree(den)
  val lc   = den(dd)
  val dnum = polyDegree(num)
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

/** Square-free factorisation (Yun): each returned `(poly, k)` is the product of the distinct
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
  val dd = polyTrim(d0)
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

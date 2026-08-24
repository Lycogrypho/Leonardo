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

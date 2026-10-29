package it.grypho.scala.leonardo
package statistics

import core.*
import matrix._Matrix


/** The descriptive kernels.
 *
 *  Each takes the sample as already-extracted values and returns a `_Value`, so the exact
 *  and inexact paths share one signature: an all-rational sample comes back exact, anything
 *  else comes back as a `_Number`.
 *
 *  @see [[https://en.wikipedia.org/wiki/Variance#Sample_variance Sample variance]]
 *  @see [[https://en.wikipedia.org/wiki/Bessel%27s_correction Bessel's correction]] — the `n − 1`
 */

/** Reads an evaluated expression as a sample: every entry of a matrix, row-major.
 *
 *  Accepts both carriers, which matters for exact input: an exactly-written matrix stays a
 *  symbolic `_Matrix` rather than collapsing to the dense one, and that is precisely the case
 *  where an exact statistic is worth having.
 *
 *  @param r the evaluated argument
 *  @return the entries, or `None` when the argument is not a matrix of values
 */
def sampleOf(r: Either[_Expression, _Value]): Option[Vector[_Value]] =
  _MatrixValue.cellsOf(r).map(_._3).filter(_.nonEmpty)

/** Whether every entry is exact -- a rational or, since F_0231, a surd -- so the field operations
 *  can stay exact. */
private def allExact(xs: Vector[_Value]): Boolean = xs.forall(_Surd.isExact)

/** The exact entries as rationals, when they all are exact: a surd is read as its
 *  working-precision rational (F_0231), the value it stood for before, never as a `Double`. */
private def exactly(xs: Vector[_Value], env: Environment): Option[Vector[_Rational]] =
  if !allExact(xs) then None
  else
    val rs = xs.flatMap(_Surd.exactOf(_, env.workingPrecision))
    Option.when(rs.size == xs.size)(rs)

/** The entries as `Double`s, reading through the widening `_Number` extractor (the one copy
 *  since F_0147; `_Statistic.scala` carried a byte-identical `asDoubles`). */
private[statistics] def doubles(xs: Vector[_Value]): Option[Vector[Double]] =
  val ds = xs.collect { case _Number(d) => d }
  Option.when(ds.size == xs.size)(ds)

/** The arithmetic mean.
 *
 *  @param xs  the sample, which must be non-empty
 *  @param env supplies the reduction policy for the exact path
 *  @return the mean — exact when the sample is
 */
def sampleMean(xs: Vector[_Value], env: Environment): Option[_Value] =
  if xs.isEmpty then None
  else
    exactly(xs, env) match
      case Some(rs) =>
        val p   = env.rationalPolicy
        val sum = rs.reduceLeft((a, b) => a.add(b, p))
        sum.divide(_Rational(rs.size), p)
      case None => doubles(xs).flatMap(ds => finite(ds.sum / ds.size))

/** The variance, dividing by `n − ddof`.
 *
 *  **Two-pass, deliberately.**  The textbook one-pass form `E[X²] − E[X]²` subtracts two
 *  nearly equal quantities whenever the sample has a large mean relative to its spread, and
 *  loses most of its significant digits doing so — the same catastrophic cancellation that
 *  already cost this project the quadratic formula's small root.  Computing the mean
 *  first and then summing squared deviations has no such subtraction in it.
 *
 *  @param xs   the sample
 *  @param ddof `1` for the unbiased sample variance, `0` for the population variance
 *  @param env  supplies the reduction policy for the exact path
 *  @return the variance — exact when the sample is
 */
def sampleVariance(xs: Vector[_Value], ddof: Int, env: Environment): Option[_Value] =
  val n = xs.size
  if n <= ddof then None      // n = 1 has no unbiased variance, and n = 0 has none at all
  else
    exactly(xs, env) match
      case Some(rs) =>
        val p = env.rationalPolicy
        for
          m  <- sampleMean(xs, env).collect { case r: _Rational => r }
          ss  = rs.map(x => { val d = x.subtract(m, p); d.multiply(d, p) })
                  .reduceLeft((a, b) => a.add(b, p))
          v  <- ss.divide(_Rational(n - ddof), p)
        yield v
      case None =>
        doubles(xs).flatMap { ds =>
          val m = ds.sum / n
          finite(ds.map(x => (x - m) * (x - m)).sum / (n - ddof))
        }

/** The standard deviation — the square root of [[sampleVariance]].
 *
 *  A square root is not closed over the rationals, so an exact sample gives an exact
 *  *variance* but only a working-precision standard deviation.  That is the same contract
 *  every irrational result in the library carries, and it goes through the same kernel.
 *
 *  @param xs   the sample
 *  @param ddof `1` for the sample standard deviation, `0` for the population one
 *  @param env  supplies the working precision
 */
def sampleStdDev(xs: Vector[_Value], ddof: Int, env: Environment): Option[_Value] =
  sampleVariance(xs, ddof, env).flatMap(rootOf(_, env))

/** The covariance of two equally long samples, dividing by `n − 1`.
 *
 *  @param xs  the first sample
 *  @param ys  the second, which must be the same length
 *  @param env supplies the reduction policy
 *  @return the covariance — exact when both samples are
 */
def sampleCovariance(xs: Vector[_Value], ys: Vector[_Value], env: Environment): Option[_Value] =
  val n = xs.size
  if n != ys.size || n < 2 then None
  else
    (exactly(xs, env), exactly(ys, env)) match
      case (Some(rx), Some(ry)) =>
        val p = env.rationalPolicy
        for
          mx <- sampleMean(xs, env).collect { case r: _Rational => r }
          my <- sampleMean(ys, env).collect { case r: _Rational => r }
          ss  = rx.zip(ry).map((a, b) => a.subtract(mx, p).multiply(b.subtract(my, p), p))
                  .reduceLeft((a, b) => a.add(b, p))
          v  <- ss.divide(_Rational(n - 1), p)
        yield v
      case _ =>
        for
          dx <- doubles(xs); dy <- doubles(ys)
          mx  = dx.sum / n; my = dy.sum / n
          v  <- finite(dx.zip(dy).map((a, b) => (a - mx) * (b - my)).sum / (n - 1))
        yield v

/** The Pearson correlation coefficient.
 *
 *  Scale-invariant, so the `n − 1` cancels between the covariance and the two standard
 *  deviations — this is the one statistic here for which the population/sample choice makes
 *  no difference at all.
 *
 *  @param xs  the first sample
 *  @param ys  the second, which must be the same length
 *  @param env supplies the working precision
 *  @return the correlation in `[-1, 1]`, or `None` if either sample is constant
 */
def sampleCorrelation(xs: Vector[_Value], ys: Vector[_Value], env: Environment): Option[_Value] =
  for
    dx  <- doubles(xs); dy <- doubles(ys)
    if dx.size == dy.size && dx.size >= 2
    n    = dx.size
    mx   = dx.sum / n; my = dy.sum / n
    cov  = dx.zip(dy).map((a, b) => (a - mx) * (b - my)).sum
    sx   = math.sqrt(dx.map(a => (a - mx) * (a - mx)).sum)
    sy   = math.sqrt(dy.map(b => (b - my) * (b - my)).sum)
    if spreadResolved(sx / math.sqrt(n), mx, n) && spreadResolved(sy / math.sqrt(n), my, n)
    raw  = cov / (sx * sy)
    // Clamp: the division can land a hair outside [-1, 1] on a perfectly correlated sample,
    // and a correlation of 1.0000000000000002 is worse than useless to a caller testing it.
    r   <- finite(math.max(-1.0, math.min(1.0, raw)))
  yield r

/** Whether a sample's spread is more than the rounding left by a constant sample (F_0196).  The
 *  deviations of `[[0.1, 0.1, 0.1]]` from its computed mean are `1.4e-17`, not `0`, so a test
 *  against exactly zero let a constant sample through as `t = 1.41`; rounding leaves a spread of
 *  order `n·ε·|mean|`, and four times that is the refusal line.
 *
 *  @param sd     a standard deviation of the sample
 *  @param center the sample's mean
 *  @param n      the sample size
 *  @return `true` when the spread is genuine
 */
private[statistics] def spreadResolved(sd: Double, center: Double, n: Int): Boolean =
  sd > 0.0 && sd > 4.0 * n * Math.ulp(1.0) * (math.abs(center) + sd)

/** The sample in ascending order (F_0129) — compared exactly when every entry is exact, else as
 *  `Double`s.  Selection is not arithmetic, so an exact sample stays exact.
 *
 *  @param xs the sample
 *  @return the entries sorted, or `None` when an entry is not a real number
 */
def sortedSample(xs: Vector[_Value]): Option[Vector[_Value]] =
  // Exact entries are sorted by the exact order and kept as they are, a surd included (F_0231):
  // selection is not arithmetic, so the median of an exact sample is one of its entries.
  if allExact(xs) then Some(xs.sortWith((a, b) => _Surd.compareExact(a, b).exists(_ < 0)))
  else doubles(xs).map(_.sorted.map(d => _Number(d)))

/** The `p`-quantile of a sample by Hyndman and Fan's type 7 or type 6 (F_0129, Decision 3).
 *
 *  Both interpolate linearly between neighbouring order statistics; they differ in where `p`
 *  falls.  **Type 7** (the default, R's and numpy's) puts it at the 0-based position `(n − 1)p`,
 *  so `p = 0` and `p = 1` are the extremes.  **Type 6** puts it at the 1-based position
 *  `(n + 1)p`, the percentile formula of *Introductory Statistics 2e*, clamped to the extremes
 *  below 1 and past `n`.  The median is the same under both.
 *
 *  @param xs     the sample, non-empty
 *  @param p      the probability, in `[0, 1]`
 *  @param method `7` or `6`
 *  @param env    supplies the reduction policy for the exact path
 *  @return the quantile — exact when the sample and `p` are — or `None` for an empty sample, a
 *          `p` outside `[0, 1]` or another method
 */
def sampleQuantile(xs: Vector[_Value], p: _Value, method: Int, env: Environment): Option[_Value] =
  if xs.isEmpty || (method != 6 && method != 7) then None
  else
    val n = xs.size
    (exactly(xs, env), p) match
      case (Some(rs), q: _Rational) =>
        Option.when(q.signum >= 0 && q <= _Rational(1)) {
          val pol    = env.rationalPolicy
          val sorted = rs.sorted
          val raw    = if method == 7 then _Rational(n - 1).multiply(q, pol)
                       else _Rational(n + 1).multiply(q, pol).subtract(_Rational(1), pol)
          val h      = if raw.signum < 0 then _Rational(0) else if raw > _Rational(n - 1) then _Rational(n - 1) else raw
          val j      = (h.num / h.den).toInt       // floor, since h >= 0
          val g      = h.subtract(_Rational(j), pol)
          if g.isZero || j + 1 >= n then sorted(j)
          else sorted(j).add(g.multiply(sorted(j + 1).subtract(sorted(j), pol), pol), pol)
        }
      case _ =>
        for
          ds <- doubles(xs)
          pd <- p match { case _Number(d) => Some(d); case _ => None }
          if pd >= 0.0 && pd <= 1.0
          sorted = ds.sorted
          raw    = if method == 7 then (n - 1) * pd else (n + 1) * pd - 1.0
          h      = math.max(0.0, math.min((n - 1).toDouble, raw))
          j      = math.floor(h).toInt
          g      = h - j
          v     <- finite(if g == 0.0 || j + 1 >= n then sorted(j) else sorted(j) + g * (sorted(j + 1) - sorted(j)))
        yield v

/** The counts of a sample in `k` equal bins over `[lo, hi]` (F_0135).
 *
 *  Each bin is half-open, `[lo + i·w, lo + (i+1)·w)`, except the last, which is closed so that
 *  `hi` itself — typically the sample's maximum — is counted.  Values outside `[lo, hi]` are left
 *  out.  A value's bin is computed, not searched for, so the cost is one pass.
 *
 *  @param xs the sample
 *  @param lo the lower edge
 *  @param hi the upper edge, above `lo`
 *  @param k  the number of bins, at least 1
 *  @return the `k` counts, or `None` for `lo ≥ hi`, `k < 1` or a non-finite edge
 */
def binCounts(xs: Vector[Double], lo: Double, hi: Double, k: Int): Option[Vector[Int]] =
  Option.when(k >= 1 && lo < hi && !lo.isInfinite && !hi.isInfinite) {
    val counts = new Array[Int](k)
    // In halves (F_0150): `hi − lo` overflows to Inf for edges near ±1.8e308, which put every
    // value in the first bin. Halving numerator and width alike is exact, so every bin a finite
    // span gave is unchanged.
    val halfWidth = (hi / 2 - lo / 2) / k
    xs.foreach { x =>
      if x >= lo && x <= hi then counts(math.min(k - 1, ((x / 2 - lo / 2) / halfWidth).toInt)) += 1
    }
    counts.toVector
  }

/** The square root of a value, exact-tier aware. */
private def rootOf(v: _Value, env: Environment): Option[_Value] = v match
  // F_0231: the root of a rational is its exact surd; of a surd, not closed, so the
  // working-precision rational it always was.
  case r: _Rational => _Surd.sqrtOf(r)
  case s: _Surd     => _Surd.exactOf(s, env.workingPrecision).flatMap(exactSqrt(_, env.workingPrecision))
  case _Number(d)   => if d < 0.0 then None else finite(math.sqrt(d))
  case _            => None

private def finite(d: Double): Option[_Value] =
  Option.when(d.isFinite)(_Number(d))

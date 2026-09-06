package it.grypho.scala.leonardo
package statistics

import core.*
import matrix._Matrix


/** The descriptive kernels — issue 4.Q slice A.
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
 *  Accepts both carriers, which matters since 4.L slice B: an exactly-written matrix stays a
 *  symbolic `_Matrix` rather than collapsing to the dense one, and that is precisely the case
 *  where an exact statistic is worth having.
 *
 *  @param r the evaluated argument
 *  @return the entries, or `None` when the argument is not a matrix of values
 */
def sampleOf(r: Either[_Expression, _Value]): Option[Vector[_Value]] = r match
  case Right(mv: _MatrixValue) => Some(mv.toVector.map(d => _Number(d)))
  case Left(m: _Matrix) =>
    val vs = m.elems.collect { case v: _Value => v }
    Option.when(vs.size == m.elems.size && vs.nonEmpty)(vs)
  case _ => None

/** Whether every entry is exact, so the field operations can stay exact. */
private def allExact(xs: Vector[_Value]): Boolean = xs.forall(_.isInstanceOf[_Rational])

/** The exact entries, when they all are. */
private def exactly(xs: Vector[_Value]): Option[Vector[_Rational]] =
  Option.when(allExact(xs))(xs.collect { case r: _Rational => r })

/** The entries as `Double`s, reading through the widening `_Number` extractor. */
private def doubles(xs: Vector[_Value]): Option[Vector[Double]] =
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
    exactly(xs) match
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
 *  issue 1.1 already cost this project once, in the quadratic formula.  Computing the mean
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
    exactly(xs) match
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
    (exactly(xs), exactly(ys)) match
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
    if sx > 0.0 && sy > 0.0
    raw  = cov / (sx * sy)
    // Clamp: the division can land a hair outside [-1, 1] on a perfectly correlated sample,
    // and a correlation of 1.0000000000000002 is worse than useless to a caller testing it.
    r   <- finite(math.max(-1.0, math.min(1.0, raw)))
  yield r

/** The square root of a value, exact-tier aware. */
private def rootOf(v: _Value, env: Environment): Option[_Value] = v match
  case r: _Rational => exactSqrt(r, env.workingPrecision)
  case _Number(d)   => if d < 0.0 then None else finite(math.sqrt(d))
  case _            => None

private def finite(d: Double): Option[_Value] =
  Option.when(!d.isNaN && !d.isInfinite)(_Number(d))

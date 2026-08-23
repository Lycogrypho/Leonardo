package it.grypho.scala.leonardo
package statistics

import core.*
import matrix._Matrix
import probability.{_Distribution, DistKind}


/** The single-sample statistics. */
enum StatKind:
  /** `mean(x)` */                                   case Mean
  /** `variance(x)` — unbiased, `n − 1` for a sample */ case Variance
  /** `pvariance(x)` — population, `n` */            case PVariance
  /** `stddev(x)` — the root of `variance` */        case StdDev
  /** `pstddev(x)` — the root of `pvariance` */      case PStdDev

object StatKind:
  /** The grammar keyword. */
  def keyword(k: StatKind): String = k match
    case Mean      => "mean"
    case Variance  => "variance"
    case PVariance => "pvariance"
    case StdDev    => "stddev"
    case PStdDev   => "pstddev"


/** A statistic of one argument, which may be a **sample or a distribution**.
 *
 *  Dispatching on the argument is what lets `mean` and `variance` mean one thing each.  A
 *  distribution delegates to `probability`, which owns its own moments; a matrix is treated
 *  as a sample.  This is why `statistics` imports `probability` and not the reverse.
 *
 *  Note the `n`/`n − 1` distinction that follows from the dispatch: `variance(sample)` is the
 *  unbiased estimator, while `variance(distribution)` is the population variance, because a
 *  distribution is not a sample and has no `n` to correct for.  See the package overview.
 *
 *  @param kind the statistic
 *  @param arg  the sample or distribution
 */
case class _Statistic(kind: StatKind, arg: _Expression) extends _Expression:
  override def toString: String = s"${StatKind.keyword(kind)}($arg)"
  override def children: List[_Expression] = List(arg)
  override def rebuild(c: List[_Expression]): _Expression = _Statistic(kind, c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    val r = arg.eval(env)
    val result = r match
      case Right(d: _Distribution) => fromDistribution(d, env)
      case other                   => sampleOf(other).flatMap(fromSample(_, env))
    result.map(Right(_)).getOrElse(Left(_Statistic(kind, r.toExpression)))

  /** A distribution's own moment; the population/sample distinction does not arise.
   *
   *  Routed through `probability.Moments` rather than reading `d.mean` directly, so the
   *  one-argument branches of `_Expectation`/`_Variance` stay the single definition of what a
   *  distribution's moments are, instead of being left as dead code with a copy here.
   */
  private def fromDistribution(d: _Distribution, env: Environment): Option[_Value] = kind match
    case StatKind.Mean =>
      probability.Moments.expectation(d, None, env).flatMap(_.toOption)
    case StatKind.Variance | StatKind.PVariance =>
      probability.Moments.variance(d, None, env).flatMap(_.toOption)
    case StatKind.StdDev | StatKind.PStdDev =>
      probability.Moments.variance(d, None, env).flatMap(_.toOption).collect {
        case _Number(v) if v >= 0.0 => _Number(math.sqrt(v))
      }

  /** The statistic of a sample. */
  private def fromSample(xs: Vector[_Value], env: Environment): Option[_Value] = kind match
    case StatKind.Mean      => sampleMean(xs, env)
    case StatKind.Variance  => sampleVariance(xs, 1, env)
    case StatKind.PVariance => sampleVariance(xs, 0, env)
    case StatKind.StdDev    => sampleStdDev(xs, 1, env)
    case StatKind.PStdDev   => sampleStdDev(xs, 0, env)


/** The two-sample statistics. */
enum PairStatKind:
  /** `covariance(x, y)` — unbiased, `n − 1` */ case Covariance
  /** `correlation(x, y)` — Pearson's r */      case Correlation

object PairStatKind:
  /** The grammar keyword. */
  def keyword(k: PairStatKind): String = k.toString.toLowerCase


/** A statistic of two equally long samples.
 *
 *  @param kind the statistic
 *  @param a    the first sample
 *  @param b    the second
 */
case class _PairStatistic(kind: PairStatKind, a: _Expression, b: _Expression) extends _Expression:
  override def toString: String = s"${PairStatKind.keyword(kind)}($a, $b)"
  override def children: List[_Expression] = List(a, b)
  override def rebuild(c: List[_Expression]): _Expression = _PairStatistic(kind, c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    val (ra, rb) = (a.eval(env), b.eval(env))
    val result =
      for
        xs <- sampleOf(ra)
        ys <- sampleOf(rb)
        v  <- kind match
                case PairStatKind.Covariance  => sampleCovariance(xs, ys, env)
                case PairStatKind.Correlation => sampleCorrelation(xs, ys, env)
      yield v
    result.map(Right(_)).getOrElse(Left(_PairStatistic(kind, ra.toExpression, rb.toExpression)))


/** Ordinary least squares: `regress(X, y)` — issue 4.Q slice B.
 *
 *  Returns the `p × 1` coefficient vector.  **No intercept column is added**: a constant term
 *  is a column of ones the caller supplies, because a silently inserted column would make
 *  `regress` compute something other than what was written, with no way to opt out.
 *
 *  @param x the design matrix
 *  @param y the response vector
 */
case class _Regress(x: _Expression, y: _Expression) extends _Expression:
  override def toString: String = s"regress($x, $y)"
  override def children: List[_Expression] = List(x, y)
  override def rebuild(c: List[_Expression]): _Expression = _Regress(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    (denseOf(x.eval(env)), denseOf(y.eval(env))) match
      case (Some(xm), Some(ym)) =>
        leastSquares(xm, ym).map(Right(_)).getOrElse(Left(this))
      case _ => Left(_Regress(x.eval(env).toExpression, y.eval(env).toExpression))


/** The inference tests. */
enum TestKind:
  /** `ttest(sample, mu0)` — two-sided one-sample t-test, returning the p-value. */ case TTest
  /** `confint(sample, level)` — a two-sided confidence interval, as `[[lo, hi]]`. */ case ConfInt
  /** `chisqtest(observed, expected)` — Pearson's goodness-of-fit p-value. */        case ChiSqTest

object TestKind:
  /** The grammar keyword. */
  def keyword(k: TestKind): String = k match
    case TTest     => "ttest"
    case ConfInt   => "confint"
    case ChiSqTest => "chisqtest"


/** An inference test — issue 4.Q slice C.
 *
 *  Thin wrappers, exactly as the plan predicted: the work was in 4.O's incomplete beta and
 *  gamma and in 4.P's distribution carrier, both of which already existed.  What this adds is
 *  the test statistics themselves and the routing.
 *
 *  @param kind the test
 *  @param a    the sample (or observed counts)
 *  @param b    the null mean, the confidence level, or the expected counts
 */
case class _Test(kind: TestKind, a: _Expression, b: _Expression) extends _Expression:
  override def toString: String = s"${TestKind.keyword(kind)}($a, $b)"
  override def children: List[_Expression] = List(a, b)
  override def rebuild(c: List[_Expression]): _Expression = _Test(kind, c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    val (ra, rb) = (a.eval(env), b.eval(env))
    val result = kind match
      case TestKind.TTest     => tTest(ra, rb, env)
      case TestKind.ConfInt   => confidenceInterval(ra, rb, env)
      case TestKind.ChiSqTest => chiSquaredTest(ra, rb, env)
    result.getOrElse(Left(_Test(kind, ra.toExpression, rb.toExpression)))


/** Reads an evaluated operand as a dense matrix, demoting an exact one.
 *
 *  Regression and the tests are `Double` algorithms — QR iteration and the cdf kernels are
 *  not exact whatever their input — so an exact matrix demotes here rather than losing the
 *  operation, the pattern 4.L slice B established for the decompositions.
 */
private def denseOf(r: Either[_Expression, _Value]): Option[_MatrixValue] = r match
  case Right(mv: _MatrixValue) => Some(mv)
  case Left(m: _Matrix) =>
    val ds = m.elems.collect { case _Number(d) => d }
    Option.when(ds.size == m.elems.size && ds.nonEmpty)(_MatrixValue(m.rows, m.cols, ds.toArray))
  case _ => None

/** A plain number from an evaluated operand. */
private def numberOf(r: Either[_Expression, _Value]): Option[Double] = r match
  case Right(_Number(d)) => Some(d)
  case _                 => None

/** The two-sided one-sample t-test, returning the p-value.
 *
 *  `t = (x̄ − μ₀) / (s / √n)` on `n − 1` degrees of freedom, where `s` is the **unbiased**
 *  sample standard deviation — which is why `variance(sample)` divides by `n − 1`.  The
 *  p-value is `2·(1 − F(|t|))`, computed from the far tail so the subtraction does not eat
 *  the answer for a strongly significant result.
 */
private def tTest(ra: Either[_Expression, _Value], rb: Either[_Expression, _Value],
                  env: Environment): Option[Either[_Expression, _Value]] =
  for
    xs   <- sampleOf(ra)
    mu0  <- numberOf(rb)
    n     = xs.size
    if n >= 2
    mean <- sampleMean(xs, env).collect { case _Number(d) => d }
    sd   <- sampleStdDev(xs, 1, env).collect { case _Number(d) => d }
    if sd > 0.0
    t     = (mean - mu0) / (sd / math.sqrt(n.toDouble))
    d    <- _Distribution.of(DistKind.StudentT, Vector((n - 1).toDouble))
    // 2 * cdf(-|t|) rather than 2 * (1 - cdf(|t|)): the second form subtracts two nearly
    // equal quantities far out in the tail, which is exactly where a p-value matters most.
    p    <- d.cdf(-math.abs(t))
  yield Right(_Number(math.min(1.0, 2.0 * p)))

/** A two-sided confidence interval for the mean, returned as a `1 × 2` matrix `[[lo, hi]]`.
 *
 *  @param rb the confidence level in `(0, 1)` — `0.95`, not `95`
 */
private def confidenceInterval(ra: Either[_Expression, _Value], rb: Either[_Expression, _Value],
                               env: Environment): Option[Either[_Expression, _Value]] =
  for
    xs    <- sampleOf(ra)
    level <- numberOf(rb)
    if level > 0.0 && level < 1.0
    n      = xs.size
    if n >= 2
    mean  <- sampleMean(xs, env).collect { case _Number(d) => d }
    sd    <- sampleStdDev(xs, 1, env).collect { case _Number(d) => d }
    d     <- _Distribution.of(DistKind.StudentT, Vector((n - 1).toDouble))
    crit  <- quantileOfDistribution(d, 1.0 - (1.0 - level) / 2.0)
    half   = crit * sd / math.sqrt(n.toDouble)
    m     <- _MatrixValue(1, 2, Array(mean - half, mean + half)).guarded(_Number(0)).toOption
  yield Right(m)

/** Pearson's chi-squared goodness-of-fit p-value.
 *
 *  `X² = Σ (O − E)² / E` on `k − 1` degrees of freedom.  A zero expected count makes the
 *  statistic undefined rather than infinite, so it stays symbolic.
 */
private def chiSquaredTest(ra: Either[_Expression, _Value], rb: Either[_Expression, _Value],
                           env: Environment): Option[Either[_Expression, _Value]] =
  for
    obs <- sampleOf(ra).flatMap(asDoubles)
    exp <- sampleOf(rb).flatMap(asDoubles)
    if obs.size == exp.size && obs.size >= 2 && exp.forall(_ > 0.0)
    stat = obs.zip(exp).map((o, e) => (o - e) * (o - e) / e).sum
    d   <- _Distribution.of(DistKind.ChiSquared, Vector((obs.size - 1).toDouble))
    c   <- d.cdf(stat)
  yield Right(_Number(math.max(0.0, 1.0 - c)))

/** The inverse cdf, reached through the `probability` grammar node's own kernel. */
private def quantileOfDistribution(d: _Distribution, p: Double): Option[Double] =
  // Bracket-and-bisect on the cdf, which is monotone for every family.
  if p <= 0.0 || p >= 1.0 then None
  else
    val centre = d.mean.getOrElse(0.0)
    val spread = d.variance.map(math.sqrt).filter(_ > 0.0).getOrElse(1.0)
    var lo = centre - spread
    var hi = centre + spread
    var i  = 0
    while i < 200 && d.cdf(lo).exists(_ > p) do { lo -= spread * 2.0; i += 1 }
    i = 0
    while i < 200 && d.cdf(hi).exists(_ < p) do { hi += spread * 2.0; i += 1 }
    var k = 0
    while k < 200 && (hi - lo) > 1e-13 * math.max(1.0, math.abs(lo)) do
      val m = 0.5 * (lo + hi)
      if d.cdf(m).exists(_ < p) then lo = m else hi = m
      k += 1
    Some(0.5 * (lo + hi))

private def asDoubles(xs: Vector[_Value]): Option[Vector[Double]] =
  val ds = xs.collect { case _Number(d) => d }
  Option.when(ds.size == xs.size)(ds)

package it.grypho.scala.leonardo
package statistics

import core.*
import matrix._Matrix
import probability.{_Distribution, _DistributionQuery, DistKind, Query}


/** The single-sample statistics. */
enum StatKind extends Keyworded:
  /** The grammar keyword (F_0148); the table is the companion's. */
  def keyword: String = StatKind.keyword(this)
  /** `mean(x)` */                                   case Mean
  /** `variance(x)` — unbiased, `n − 1` for a sample */ case Variance
  /** `pvariance(x)` — population, `n` */            case PVariance
  /** `stddev(x)` — the root of `variance` */        case StdDev
  /** `pstddev(x)` — the root of `pvariance` */      case PStdDev
  /** `median(x)` — the middle order statistic (F_0129); a distribution's 0.5 quantile */ case Median

object StatKind:
  /** The grammar keyword. */
  def keyword(k: StatKind): String = k match
    case Mean      => "mean"
    case Variance  => "variance"
    case PVariance => "pvariance"
    case StdDev    => "stddev"
    case PStdDev   => "pstddev"
    case Median    => "median"


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
    result.toRight(_Statistic(kind, r.toExpression))

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
    // The distribution's own quantile query, so the two cannot disagree about where 0.5 falls.
    case StatKind.Median =>
      _DistributionQuery(Query.Quantile, d, List(_Number(0.5))).eval(env).toOption

  /** The statistic of a sample. */
  private def fromSample(xs: Vector[_Value], env: Environment): Option[_Value] = kind match
    case StatKind.Mean      => sampleMean(xs, env)
    case StatKind.Variance  => sampleVariance(xs, 1, env)
    case StatKind.PVariance => sampleVariance(xs, 0, env)
    case StatKind.StdDev    => sampleStdDev(xs, 1, env)
    case StatKind.PStdDev   => sampleStdDev(xs, 0, env)
    // The same under type 6 and type 7; one half is exact, so an exact sample stays exact.
    case StatKind.Median    => _Rational(2).reciprocal(env.rationalPolicy).flatMap(sampleQuantile(xs, _, 7, env))


/** `bincounts(x, lo, hi, k)` — [[binCounts]] at the prompt (F_0135): a `k × 1` column of counts,
 *  the data behind a histogram.  An exact sample is read as the numbers it holds.  `k` is capped
 *  by the environment's `Limit.MaxMatrixCells`, the size of the column it builds.
 *
 *  @param x  the sample
 *  @param lo the lower edge
 *  @param hi the upper edge
 *  @param k  the number of bins
 */
case class _BinCounts(x: _Expression, lo: _Expression, hi: _Expression, k: _Expression) extends _Expression:
  override def toString: String = s"bincounts($x, $lo, $hi, $k)"
  override def children: List[_Expression] = List(x, lo, hi, k)
  override def rebuild(c: List[_Expression]): _Expression = _BinCounts(c.head, c(1), c(2), c(3))

  override def eval(env: Environment): Either[_Expression, _Value] =
    val rs = List(x, lo, hi, k).map(_.eval(env))
    val counts = for
      xs <- sampleOf(rs.head).flatMap(doubles)
      l  <- numberOf(rs(1))
      h  <- numberOf(rs(2))
      n  <- numberOf(rs(3))
      if n == math.floor(n) && n >= 1 && n <= env.limits(Limit.MaxMatrixCells)
      cs <- binCounts(xs, l, h, n.toInt)
    yield _MatrixValue(cs.size, 1, cs.map(_.toDouble).toArray)
    counts.toRight(_BinCounts(rs.head.toExpression, rs(1).toExpression, rs(2).toExpression, rs(3).toExpression))


/** `quantile(x, p)` or `quantile(x, p, method)` — dispatching on its first argument like
 *  [[_Statistic]] (F_0129): a distribution answers its inverse cdf, exactly as before, and a
 *  sample its order-statistic quantile by [[sampleQuantile]], type 7 unless `method` is `6`.
 *  A method given with a distribution is refused: there is nothing for it to choose between.
 *
 *  @param x      the sample or distribution
 *  @param p      the probability
 *  @param method `6` or `7`, when given
 */
case class _Quantile(x: _Expression, p: _Expression, method: Option[_Expression]) extends _Expression:
  override def toString: String = s"quantile($x, $p${method.fold("")(m => s", $m")})"
  override def children: List[_Expression] = x :: p :: method.toList
  override def rebuild(c: List[_Expression]): _Expression = _Quantile(c.head, c(1), method.map(_ => c(2)))

  override def eval(env: Environment): Either[_Expression, _Value] =
    val (rx, rp) = (x.eval(env), p.eval(env))
    val rm       = method.map(_.eval(env))
    val answer: Option[Either[_Expression, _Value]] = rx match
      case Right(d: _Distribution) =>
        Option.when(rm.isEmpty)(_DistributionQuery(Query.Quantile, d, List(rp.toExpression)).eval(env))
      case other =>
        for
          xs <- sampleOf(other)
          pv <- rp.toOption
          m  <- rm.fold(Option(7)) {
                  case Right(_Number(k)) if k == 6.0 || k == 7.0 => Some(k.toInt)
                  case _                                          => None
                }
          q  <- sampleQuantile(xs, pv, m, env)
        yield Right(q)
    answer.getOrElse(Left(_Quantile(rx.toExpression, rp.toExpression, rm.map(_.toExpression))))


/** `sort(x)` — the entries of a sample in ascending order (F_0129): a row as a row, a column as
 *  a column, any other matrix as a column of all its entries.  An exact sample stays exact, on
 *  the symbolic `_Matrix` carrier exact matrices use.
 *
 *  @param x the sample
 */
case class _Sort(x: _Expression) extends _Expression:
  override def toString: String = s"sort($x)"
  override def children: List[_Expression] = List(x)
  override def rebuild(c: List[_Expression]): _Expression = _Sort(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    val r = x.eval(env)
    val shape = r match
      case Right(m: _MatrixValue) => Some((m.rows, m.cols))
      case Left(m: _Matrix)       => Some((m.rows, m.cols))
      case _                      => None
    val sorted = for
      (rows, cols) <- shape
      xs           <- sampleOf(r)
      ys           <- sortedSample(xs)
    yield
      val (rr, cc) = if rows == 1 then (1, ys.size) else (ys.size, 1)
      ys.collect { case _Number(d) => d } match
        case ds if ds.size == ys.size && !ys.exists(_.isInstanceOf[_Rational]) => Right(_MatrixValue(rr, cc, ds.toArray))
        case _                                                                  => Left(_Matrix(rr, cc, ys))
    sorted.getOrElse(Left(_Sort(r.toExpression)))


/** The two-sample statistics. */
enum PairStatKind extends Keyworded:
  /** The grammar keyword (F_0148); the table is the companion's. */
  def keyword: String = PairStatKind.keyword(this)
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
    result.toRight(_PairStatistic(kind, ra.toExpression, rb.toExpression))


/** Ordinary least squares: `regress(X, y)`.
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
    (_MatrixValue.ofEvaluated(x.eval(env)), _MatrixValue.ofEvaluated(y.eval(env))) match
      case (Some(xm), Some(ym)) =>
        leastSquares(xm, ym).toRight(this)
      case _ => Left(_Regress(x.eval(env).toExpression, y.eval(env).toExpression))


/** Ridge regression: `regress(X, y, lambda)`, minimising `‖Xβ − y‖² + λ‖β‖²` (F_0062).
 *
 *  The three-argument spelling of `regress`, as a node of its own rather than a third field on
 *  [[_Regress]]: that is a published case class, and widening it would be a binary break for a
 *  spelling. Solved by [[ridgeLeastSquares]] (augmented QR). Stays unevaluated for a `λ` that is
 *  not a number `≥ 0`, and wherever `regress` would.
 *
 *  @param x      the design matrix
 *  @param y      the response vector
 *  @param lambda the penalty
 */
case class _RidgeRegress(x: _Expression, y: _Expression, lambda: _Expression) extends _Expression:
  override def toString: String = s"regress($x, $y, $lambda)"
  override def children: List[_Expression] = List(x, y, lambda)
  override def rebuild(c: List[_Expression]): _Expression = _RidgeRegress(c.head, c(1), c(2))

  override def eval(env: Environment): Either[_Expression, _Value] =
    val (rx, ry, rl) = (x.eval(env), y.eval(env), lambda.eval(env))
    (_MatrixValue.ofEvaluated(rx), _MatrixValue.ofEvaluated(ry), rl) match
      case (Some(xm), Some(ym), Right(_Number(l))) =>
        ridgeLeastSquares(xm, ym, l).toRight(this)
      case _ => Left(_RidgeRegress(rx.toExpression, ry.toExpression, rl.toExpression))


/** Weighted least squares: `wregress(X, y, w)`, minimising `Σ wᵢ(xᵢβ − yᵢ)²` (F_0062).
 *
 *  Solved by [[weightedLeastSquares]] (row scaling by `√wᵢ`, then QR). Stays unevaluated for a
 *  negative weight, a length mismatch, or a design the weights leave rank-deficient.
 *
 *  @param x the design matrix
 *  @param y the response vector
 *  @param w the weights, one per observation
 */
case class _WeightedRegress(x: _Expression, y: _Expression, w: _Expression) extends _Expression:
  override def toString: String = s"wregress($x, $y, $w)"
  override def children: List[_Expression] = List(x, y, w)
  override def rebuild(c: List[_Expression]): _Expression = _WeightedRegress(c.head, c(1), c(2))

  override def eval(env: Environment): Either[_Expression, _Value] =
    val (rx, ry, rw) = (x.eval(env), y.eval(env), w.eval(env))
    (_MatrixValue.ofEvaluated(rx), _MatrixValue.ofEvaluated(ry), _MatrixValue.ofEvaluated(rw)) match
      case (Some(xm), Some(ym), Some(wm)) =>
        weightedLeastSquares(xm, ym, wm).toRight(this)
      case _ => Left(_WeightedRegress(rx.toExpression, ry.toExpression, rw.toExpression))


/** The inference tests. */
enum TestKind extends Keyworded:
  /** The grammar keyword (F_0148); the table is the companion's. */
  def keyword: String = TestKind.keyword(this)
  /** `ttest(sample, mu0)` — two-sided one-sample t-test, returning the p-value. */ case TTest
  /** `confint(sample, level)` — a two-sided confidence interval, as `[[lo, hi]]`. */ case ConfInt
  /** `chisqtest(observed, expected)` — Pearson's goodness-of-fit p-value. */        case ChiSqTest

object TestKind:
  /** The grammar keyword. */
  def keyword(k: TestKind): String = k match
    case TTest     => "ttest"
    case ConfInt   => "confint"
    case ChiSqTest => "chisqtest"


/** An inference test.
 *
 *  Thin wrappers: the work was in the incomplete beta and
 *  gamma kernels and in the distribution carrier, both of which already existed.  What this adds is
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



/** The alternative hypothesis of a test (F_0130), with the slot word that names it.  The words
 *  are matched only in a test's trailing slots and are **not** reserved (the `c2d` precedent). */
enum Alternative(val word: String) extends Keyworded:
  /** The slot word (F_0148). */
  def keyword: String = word
  /** `≠`: both tails, the default and what the two-argument forms have always answered. */
  case TwoSided extends Alternative("two.sided")
  /** `<`: the lower tail. */
  case Less extends Alternative("less")
  /** `>`: the upper tail. */
  case Greater extends Alternative("greater")


/** The p-value of a statistic whose null distribution is symmetric about zero with cdf `cdf`.
 *
 *  Each tail is read directly — `cdf(−|t|)` and `cdf(−t)`, never `1 − cdf(…)`, which subtracts two
 *  nearly equal quantities exactly where a p-value matters (the conditioning rule). */
private def symmetricTail(stat: Double, alt: Alternative, cdf: Double => Option[Double]): Option[Double] =
  alt match
    case Alternative.TwoSided => cdf(-math.abs(stat)).map(p => math.min(1.0, 2.0 * p))
    case Alternative.Less     => cdf(stat)
    case Alternative.Greater  => cdf(-stat)

/** The p-value of a t statistic on `df` degrees of freedom. */
private def tTail(t: Double, df: Double, alt: Alternative): Option[Double] =
  _Distribution.of(DistKind.StudentT, Vector(df)).flatMap(d => symmetricTail(t, alt, d.cdf))

/** The p-value of a standard-normal statistic. */
private def zTail(z: Double, alt: Alternative): Option[Double] =
  _Distribution.of(DistKind.Normal, Vector(0.0, 1.0)).flatMap(d => symmetricTail(z, alt, d.cdf))

/** The mean, the unbiased variance and the size of a sample of plain numbers. */
private def moments(xs: Vector[_Value], env: Environment): Option[(Double, Double, Int)] =
  for
    m <- sampleMean(xs, env).collect { case _Number(d) => d }
    v <- sampleVariance(xs, 1, env).collect { case _Number(d) => d }
  yield (m, v, xs.size)

/** The t-test (F_0130 widens it): one sample against `μ₀` when `rb` is a number, two samples
 *  when it is a matrix — Welch's test, or the pooled-variance test on `n₁ + n₂ − 2` degrees of
 *  freedom when `pooled`.  Welch's degrees of freedom are Welch-Satterthwaite's, not an integer.
 *
 *  `t = (x̄ − μ₀) / (s / √n)` for one sample, `s` the **unbiased** sample standard deviation —
 *  which is why `variance(sample)` divides by `n − 1`.  A constant sample has no t statistic.
 */
private def tTest(ra: Either[_Expression, _Value], rb: Either[_Expression, _Value], env: Environment,
                  pooled: Boolean = false, alt: Alternative = Alternative.TwoSided): Option[Either[_Expression, _Value]] =
  val p = numberOf(rb) match
    case Some(mu0) if !pooled =>
      for
        xs        <- sampleOf(ra)
        (m, v, n) <- moments(xs, env)
        if n >= 2 && spreadResolved(math.sqrt(v), m, n)
        p         <- tTail((m - mu0) / math.sqrt(v / n), n - 1.0, alt)
      yield p
    case Some(_) => None
    case None =>
      for
        xs           <- sampleOf(ra)
        ys           <- sampleOf(rb)
        (m1, v1, n1) <- moments(xs, env)
        (m2, v2, n2) <- moments(ys, env)
        if n1 >= 2 && n2 >= 2
        p            <- twoSampleTail(m1, v1, n1, m2, v2, n2, pooled, alt)
      yield p
  p.map(v => Right(_Number(v)))

/** The two-sample t-test's p-value: pooled on `n₁ + n₂ − 2`, or Welch's on the
 *  Welch-Satterthwaite degrees of freedom. */
private def twoSampleTail(m1: Double, v1: Double, n1: Int, m2: Double, v2: Double, n2: Int,
                          pooled: Boolean, alt: Alternative): Option[Double] =
  // At least one sample must vary by more than rounding (F_0196).
  val varies = spreadResolved(math.sqrt(v1), m1, n1) || spreadResolved(math.sqrt(v2), m2, n2)
  if pooled then
    val sp2 = ((n1 - 1) * v1 + (n2 - 1) * v2) / (n1 + n2 - 2)
    Option.when(varies && sp2 > 0.0)((m1 - m2) / math.sqrt(sp2 * (1.0 / n1 + 1.0 / n2)))
      .flatMap(tTail(_, n1 + n2 - 2.0, alt))
  else
    val (a, b) = (v1 / n1, v2 / n2)
    Option.when(varies && a + b > 0.0)((a, b)).flatMap { (a, b) =>
      tTail((m1 - m2) / math.sqrt(a + b), (a + b) * (a + b) / (a * a / (n1 - 1) + b * b / (n2 - 1)), alt)
    }


/** `ttest(x, m_or_y [, pooled] [, less | greater])` — a t-test with its options written (F_0130).
 *  A node of its own because the published [[_Test]] has two fields; `ttest(a, b)` with no
 *  option still builds `_Test`, which answers the two-sample test too.
 *
 *  **`pooled` is a two-sample option**: with a null mean in place of a second sample it has
 *  nothing to pool, and the call stays unevaluated rather than silently dropping a word the user
 *  wrote (F_0150) — `ttest(x, 3)` is the one-sample test.
 *
 *  @param a      the sample
 *  @param b      the null mean, or the second sample
 *  @param pooled whether two samples share a variance
 *  @param alt    the alternative hypothesis
 */
case class _TTest(a: _Expression, b: _Expression, pooled: Boolean, alt: Alternative) extends _Expression:
  override def toString: String =
    s"ttest($a, $b${if pooled then ", pooled" else ""}${if alt == Alternative.TwoSided then "" else s", ${alt.word}"})"
  override def children: List[_Expression] = List(a, b)
  override def rebuild(c: List[_Expression]): _Expression = _TTest(c.head, c(1), pooled, alt)
  override def eval(env: Environment): Either[_Expression, _Value] =
    val (ra, rb) = (a.eval(env), b.eval(env))
    tTest(ra, rb, env, pooled, alt).getOrElse(Left(_TTest(ra.toExpression, rb.toExpression, pooled, alt)))


/** `proptest(x, n, p0 [, alt])` or `proptest(x1, n1, x2, n2 [, alt])` — the one- and two-proportion
 *  z-tests of *Introductory Statistics 2e* (F_0130): the normal approximation, the two-sample
 *  form on the pooled proportion `(x₁ + x₂)/(n₁ + n₂)`.  Counts must be whole and within their
 *  trial counts, `p0` strictly inside `(0, 1)`; otherwise the node stays unevaluated.
 *
 *  @param args `x, n, p0` or `x1, n1, x2, n2`
 *  @param alt  the alternative hypothesis
 */
case class _PropTest(args: List[_Expression], alt: Alternative) extends _Expression:
  override def toString: String =
    s"proptest(${args.mkString(", ")}${if alt == Alternative.TwoSided then "" else s", ${alt.word}"})"
  override def children: List[_Expression] = args
  override def rebuild(c: List[_Expression]): _Expression = _PropTest(c, alt)

  override def eval(env: Environment): Either[_Expression, _Value] =
    val rs = args.map(_.eval(env))
    def count(x: Double, n: Double): Boolean = x == math.floor(x) && n == math.floor(n) && x >= 0 && n > 0 && x <= n
    val p = rs.map(numberOf) match
      case List(Some(x), Some(n), Some(p0)) if count(x, n) && p0 > 0.0 && p0 < 1.0 =>
        zTail((x / n - p0) / math.sqrt(p0 * (1 - p0) / n), alt)
      case List(Some(x1), Some(n1), Some(x2), Some(n2)) if count(x1, n1) && count(x2, n2) =>
        val pool = (x1 + x2) / (n1 + n2)
        Option.when(pool > 0.0 && pool < 1.0) {
          (x1 / n1 - x2 / n2) / math.sqrt(pool * (1 - pool) * (1 / n1 + 1 / n2))
        }.flatMap(zTail(_, alt))
      case _ => None
    p.map(v => _Number(v)).toRight(_PropTest(rs.map(_.toExpression), alt))


/** `chisqtest(table)` — Pearson's test of independence (or homogeneity, the same computation) on
 *  an `r × c` table of observed counts (F_0130): expected counts from the margins,
 *  `(r − 1)(c − 1)` degrees of freedom.  A single row or column, or a zero margin, has no test.
 *
 *  @param table the observed counts
 */
case class _ChiSqTable(table: _Expression) extends _Expression:
  override def toString: String = s"chisqtest($table)"
  override def children: List[_Expression] = List(table)
  override def rebuild(c: List[_Expression]): _Expression = _ChiSqTable(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    val r = table.eval(env)
    val p = for
      m <- _MatrixValue.ofEvaluated(r)
      if m.rows >= 2 && m.cols >= 2
      rows   = (0 until m.rows).map(i => (0 until m.cols).map(m(i, _)).sum)
      cols   = (0 until m.cols).map(j => (0 until m.rows).map(m(_, j)).sum)
      total  = rows.sum
      // A count is never negative (F_0196: `[[5, -1], [3, 4]]` answered a p-value).
      if m.toVector.forall(_ >= 0.0) && rows.forall(_ > 0.0) && cols.forall(_ > 0.0)
      stat   = (for i <- 0 until m.rows; j <- 0 until m.cols yield
                  val e = rows(i) * cols(j) / total
                  (m(i, j) - e) * (m(i, j) - e) / e).sum
      p     <- chiSquaredTail(stat, (m.rows - 1.0) * (m.cols - 1.0))
    yield p
    p.map(v => _Number(v)).toRight(_ChiSqTable(r.toExpression))


/** `P(F ≤ f)` and `P(F > f)` for Fisher's F on `(d1, d2)`, each read directly from the
 *  regularised incomplete beta — the upper tail as `I_{d2/(d2+d1·f)}(d2/2, d1/2)` rather than
 *  `1 − cdf` (F_0131, the conditioning rule). */
private def fTails(f: Double, d1: Double, d2: Double): Option[(Double, Double)] =
  for
    lower <- scalar.incompleteBetaOf(d1 * f / (d1 * f + d2), d1 / 2.0, d2 / 2.0)
    upper <- scalar.incompleteBetaOf(d2 / (d2 + d1 * f), d2 / 2.0, d1 / 2.0)
  yield (lower, upper)


/** `vartest(x, y[, less | greater])` — the F-test of two variances (F_0131): `F = s₁²/s₂²` on
 *  `(n₁ − 1, n₂ − 1)`; `greater` is `P(F ≥ f)`, `less` is `P(F ≤ f)`, and the two-sided p-value is
 *  twice the smaller tail.
 *
 *  @param a   the first sample
 *  @param b   the second sample
 *  @param alt the alternative hypothesis
 */
case class _VarTest(a: _Expression, b: _Expression, alt: Alternative) extends _Expression:
  override def toString: String =
    s"vartest($a, $b${if alt == Alternative.TwoSided then "" else s", ${alt.word}"})"
  override def children: List[_Expression] = List(a, b)
  override def rebuild(c: List[_Expression]): _Expression = _VarTest(c.head, c(1), alt)

  override def eval(env: Environment): Either[_Expression, _Value] =
    val (ra, rb) = (a.eval(env), b.eval(env))
    val p = for
      xs             <- sampleOf(ra)
      ys             <- sampleOf(rb)
      (m1, v1, n1)   <- moments(xs, env)
      (m2, v2, n2)   <- moments(ys, env)
      if n1 >= 2 && n2 >= 2 && spreadResolved(math.sqrt(v1), m1, n1) && spreadResolved(math.sqrt(v2), m2, n2)
      (lower, upper) <- fTails(v1 / v2, n1 - 1.0, n2 - 1.0)
    yield alt match
      case Alternative.TwoSided => math.min(1.0, 2.0 * math.min(lower, upper))
      case Alternative.Less     => lower
      case Alternative.Greater  => upper
    p.map(v => _Number(v)).toRight(_VarTest(ra.toExpression, rb.toExpression, alt))


/** `anova(g1, g2, …)` — one-way analysis of variance over two or more samples (F_0131),
 *  answering the p-value of `F = MSB/MSW` on `(k − 1, N − k)`, the upper tail read directly.  With
 *  two groups it is the pooled two-sample t-test (`F = t²`), which the tests pin.  A single group,
 *  or groups with no spread inside them, has no test.
 *
 *  @param groups the samples, one per group
 */
case class _Anova(groups: List[_Expression]) extends _Expression:
  override def toString: String = s"anova(${groups.mkString(", ")})"
  override def children: List[_Expression] = groups
  override def rebuild(c: List[_Expression]): _Expression = _Anova(c)

  override def eval(env: Environment): Either[_Expression, _Value] =
    val rs = groups.map(_.eval(env))
    val samples = rs.map(r => sampleOf(r).flatMap(doubles))
    val p = for
      gs <- Option.when(samples.forall(_.isDefined))(samples.flatten)
      k   = gs.size
      n   = gs.map(_.size).sum
      if k >= 2 && n > k
      grand = gs.flatten.sum / n
      ssb   = gs.map(g => { val m = g.sum / g.size; g.size * (m - grand) * (m - grand) }).sum
      ssw   = gs.map(g => { val m = g.sum / g.size; g.map(x => (x - m) * (x - m)).sum }).sum
      // Some group must vary by more than rounding (F_0196), not merely `ssw > 0`.
      if gs.exists(g => g.size >= 2 && {
           val m = g.sum / g.size
           spreadResolved(math.sqrt(g.map(x => (x - m) * (x - m)).sum / (g.size - 1)), m, g.size)
         })
      (_, upper) <- fTails((ssb / (k - 1)) / (ssw / (n - k)), k - 1.0, n - k.toDouble)
    yield upper
    p.map(v => _Number(v)).toRight(_Anova(rs.map(_.toExpression)))


/** `P(X > stat)` for a chi-squared variable on `df` degrees of freedom: the upper incomplete
 *  gamma `Q(df/2, stat/2)` directly, never `1 − cdf`, which cancels to zero far in the tail
 *  (F_0130; the conditioning rule's `erfc` row). */
private def chiSquaredTail(stat: Double, df: Double): Option[Double] =
  if stat <= 0.0 then Some(1.0) else scalar.upperGammaQ(df / 2.0, stat / 2.0)

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
    crit  <- quantileOfDistribution(d, 1.0 - (1.0 - level) / 2.0, env)
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
    obs <- sampleOf(ra).flatMap(doubles)
    exp <- sampleOf(rb).flatMap(doubles)
    if obs.size == exp.size && obs.size >= 2 && exp.forall(_ > 0.0)
    stat = obs.zip(exp).map((o, e) => (o - e) * (o - e) / e).sum
    p   <- chiSquaredTail(stat, obs.size - 1.0)
  yield Right(_Number(p))

/** The inverse cdf, read from the distribution's own quantile query (F_0130): this file carried a
 *  private copy of the bisection with the `max(1, |x|)` floor F_0125 removed from `quantileOf`,
 *  a second definition that had already drifted from the first. */
private def quantileOfDistribution(d: _Distribution, p: Double, env: Environment): Option[Double] =
  _DistributionQuery(Query.Quantile, d, List(_Number(p))).eval(env) match
    case Right(_Number(q)) => Some(q)
    case _                 => None

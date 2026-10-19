package it.grypho.scala.leonardo
package probability

import core.*
import scalar.{lowerGammaP, upperGammaQ, erfOf, erfcOf, incompleteBetaOf, factorialOf, lgammaOf}

import scala.math.{exp, log, sqrt, Pi}


/** The distribution families this package knows.
 *
 *  An enum rather than one case class per family: every family shares the same shape — a
 *  short parameter vector plus four kernels — so a single carrier keeps `Session` display,
 *  `:save` serialization and the parser to one case each instead of five.
 */
enum DistKind extends Keyworded:
  /** The grammar keyword (F_0148); the table is the companion's. */
  def keyword: String = DistKind.keyword(this)
  /** `normal(mu, sigma)` — parameters `(mu, sigma)`, with `sigma > 0`. */
  case Normal
  /** `uniform(a, b)` — parameters `(a, b)`, with `b > a`. */
  case Uniform
  /** `exponential(lambda)` — parameter `(lambda)`, with `lambda > 0`. */
  case Exponential
  /** `binomial(n, p)` — parameters `(n, p)`, with `n` a non-negative integer, `p` in `[0,1]`. */
  case Binomial
  /** `poisson(lambda)` — parameter `(lambda)`, with `lambda > 0`. */
  case Poisson
  /** `studentt(nu)` — parameter `(nu)`, the degrees of freedom, with `nu > 0`. */
  case StudentT
  /** `chisq(k)` — parameter `(k)`, the degrees of freedom, with `k > 0`. */
  case ChiSquared
  /** `betadist(a, b)` — parameters `(a, b)`, both strictly positive (F_0050).
   *
   *  Spelled `betadist`, not `beta`: `beta` is kept free as a variable name, which is the
   *  reason the *function* is capitalised `Beta`, and a distribution keyword could not take
   *  the lower-case spelling either.  The conjugate prior of the binomial.
   */
  case BetaDist
  /** `gammadist(k, rate)` — **shape and rate**, both strictly positive (F_0050).
   *
   *  Shape–rate rather than shape–scale, stated because the literature splits and a silent
   *  choice makes every result wrong for half its readers: rate is the form in which the
   *  Gamma–Poisson update is additive, `(k, rate) → (k + Σx, rate + n)`.  The conjugate prior
   *  of the Poisson.  `gammadist`, not `gamma`, for the `betadist` reason.
   */
  case GammaDist
  /** `geometric(p)` (F_0132) — the number of **trials up to and including the first success**,
   *  support `1, 2, …`, with `0 < p ≤ 1`.  Stated because the literature splits: some books
   *  count the failures before it, from 0; this one counts as *Introductory Statistics 2e* does.
   */
  case Geometric
  /** `hypergeometric(N, K, n)` (F_0132) — the successes in `n` draws without replacement from
   *  `N` items of which `K` are successes; whole numbers with `0 ≤ K ≤ N` and `0 ≤ n ≤ N`.
   */
  case Hypergeometric
  /** `fdist(d1, d2)` (F_0131) — Fisher-Snedecor's F, the ratio of two independent chi-squared
   *  variables over their degrees of freedom, `d1, d2 > 0`.  `fdist`, not `f`: a one-letter name
   *  is far too plausible a variable to reserve. */
  case FDist

object DistKind:
  /** The grammar keyword for a family.
   *
   *  Usually the lower-cased enum name, but `ChiSquared` is spelled `chisq` — and the
   *  difference matters: `toString` prints this, so an enum name that is not the keyword
   *  produces output that re-parses as something else entirely (`chisquared(x)` reads as
   *  `chisquared * x`).  Same trap `Query.ProbOf` has.
   */
  def keyword(k: DistKind): String = k match
    case ChiSquared => "chisq"
    case other      => other.toString.toLowerCase

  /** Number of parameters a family takes. */
  def arity(k: DistKind): Int = k match
    case Normal | Uniform | Binomial        => 2
    case Exponential | Poisson              => 1
    case StudentT | ChiSquared              => 1
    case BetaDist | GammaDist               => 2
    case Geometric                          => 1
    case Hypergeometric                     => 3
    case FDist                              => 2

  /** Looks a family up by its grammar keyword. */
  def fromKeyword(s: String): Option[DistKind] = Keyworded.byKeyword(values, s)


/** Companion for [[_Distribution]]: the validating factory. */
object _Distribution:

  /** The largest support a hypergeometric distribution may have (F_0132).
   *
   *  Its cdf has no closed form, so the masses are tabulated once per distribution (log-space
   *  ratios, normalised) and every query reads the table: `quantile` and `draw` call the cdf
   *  dozens of times per answer, so recomputing a sum each time would multiply the cost by that.
   *  A table of a million doubles is 8 MB and is built by a million ratios.  **Fixed, not a
   *  `core.Limit`**: the factory and the kernels take no environment, the integrator-parameter
   *  precedent recorded in the F_0124 inventory.
   */
  val MaxHypergeometricSupport: Int = 1000000

  /** The largest population a hypergeometric distribution may have, `2⁵³` (F_0142): past it
   *  consecutive whole numbers are not distinct doubles, so a support point cannot be named, and
   *  the table's bounds rounded until it had no points at all — the first query then threw. */
  private val MaxHypergeometricPopulation: Double = 9007199254740992.0

  /** Whether `d` is a whole number. */
  private def whole(d: Double): Boolean = Whole.isWhole(d)

  /** A hypergeometric support's first point, its masses and their running totals. */
  private[probability] final class HyperTable(val lo: Double, val masses: Array[Double], val cumul: Array[Double])

  private[probability] object HyperTable:
    /** The table of `hypergeometric(N, K, n)`, built from log-space ratios and normalised in place. */
    def of(params: Vector[Double]): HyperTable =
      val (n, k, draws) = (params(0), params(1), params(2))
      // draws - (n - k), not draws + k - n: the sum may pass 2^53 and round where the
      // difference of two values below it is exact.
      val lo   = math.max(0.0, draws - (n - k))
      val logs = new Array[Double]((math.min(draws, k) - lo).toInt + 1)
      var j = 1
      while j < logs.length do
        val i = lo + (j - 1)
        logs(j) = logs(j - 1) + math.log(k - i) + math.log(draws - i) - math.log(i + 1) - math.log((n - k) - (draws - i) + 1)
        j += 1
      val top = logs.max
      var total = 0.0
      j = 0
      while j < logs.length do
        logs(j) = math.exp(logs(j) - top)   // the array becomes the masses, unnormalised
        total += logs(j)
        j += 1
      val cumul = new Array[Double](logs.length)
      var acc = 0.0
      j = 0
      while j < logs.length do
        logs(j) /= total
        acc += logs(j)
        cumul(j) = acc
        j += 1
      HyperTable(lo, logs, cumul)

  /** The tables already built, keyed by the parameters (F_0146).  A `_Distribution` is built
   *  afresh by every evaluation of its constructor node, so a table held per instance was rebuilt
   *  by every term of `tabulate(pdf(hypergeometric(…), k), …)`: 40 terms at the support cap took
   *  5.8 s on the JVM.  Four entries: a cap-sized table holds two arrays of a million doubles, so
   *  the bound is the memory, ~64 MB, and a course rarely mixes more distributions in one sum. */
  private[probability] val hyperTables = new scalar.Memo[Vector[Double], HyperTable](4)

  /** Builds a distribution, or `None` when the parameters are outside the family's domain.
   *
   *  Validation lives here rather than at the call sites so an invalid distribution simply
   *  cannot be constructed — `normal(0, -1)` leaves its node symbolic instead of producing
   *  an object whose every kernel would return `NaN`.  The same "domain errors stay
   *  symbolic" rule the rest of the library follows.
   *
   *  @param kind   the family
   *  @param params the parameter vector, whose length must match the family's arity
   *  @return the distribution, or `None` if the parameters are invalid
   */
  def of(kind: DistKind, params: Vector[Double]): Option[_Distribution] =
    if params.length != DistKind.arity(kind) || params.exists(p => !p.isFinite) then None
    else
      val ok = kind match
        case DistKind.Normal      => params(1) > 0.0
        case DistKind.Uniform     => params(1) > params(0)
        case DistKind.Exponential => params(0) > 0.0
        case DistKind.Poisson     => params(0) > 0.0
        case DistKind.StudentT    => params(0) > 0.0
        case DistKind.ChiSquared  => params(0) > 0.0
        case DistKind.BetaDist    => params(0) > 0.0 && params(1) > 0.0
        case DistKind.GammaDist   => params(0) > 0.0 && params(1) > 0.0
        case DistKind.Binomial    =>
          params(0) >= 0.0 && params(0) == Math.floor(params(0)) &&
          params(1) >= 0.0 && params(1) <= 1.0
        case DistKind.Geometric   => params(0) > 0.0 && params(0) <= 1.0
        case DistKind.FDist       => params(0) > 0.0 && params(1) > 0.0
        case DistKind.Hypergeometric =>
          val (n, k, draws) = (params(0), params(1), params(2))
          // N >= 1 (F_0150): an empty population has no draws to describe, and its mean was 0/0.
          whole(n) && n >= 1.0 && whole(k) && whole(draws) && k >= 0.0 && k <= n && draws >= 0.0 && draws <= n &&
          n <= MaxHypergeometricPopulation &&
          math.min(draws, k) - math.max(0.0, draws - (n - k)) + 1 <= MaxHypergeometricSupport
      Option.when(ok)(new _Distribution(kind, params))


/** A probability distribution: a concrete `core._Value`, bindable in an `Environment` and
 *  round-trippable through `:save`.
 *
 *  Construction routes through [[_Distribution.of]], which validates the parameters, so
 *  every instance that exists is a well-formed distribution.
 *
 *  @param kind   the family
 *  @param params the family's parameters, in the order the grammar takes them
 */
final case class _Distribution private (kind: DistKind, params: Vector[Double]) extends _Value:

  /** Returns `Right(this)` — a distribution is already fully reduced. */
  override def eval(env: Environment): Either[_Expression, _Value] = Right(this)
  override def children: List[_Expression] = List.empty
  override def rebuild(c: List[_Expression]): _Expression = this

  /** Whether this family is supported on the integers rather than on an interval.
   *
   *  Decides which of [[pdf]] and `pmf` is meaningful, and makes `prob` sum rather than
   *  subtract cdfs at the lower endpoint — for a discrete family `P(lo ≤ X ≤ hi)` has to
   *  include `lo` itself.
   */
  def isDiscrete: Boolean = kind match
    case DistKind.Binomial | DistKind.Poisson | DistKind.Geometric | DistKind.Hypergeometric => true
    case _                                                                                  => false

  /** The hypergeometric support's first point and its masses and running totals, built once
   *  (F_0132).  Log-space ratios `p(k+1)/p(k) = (K−k)(n−k) / ((k+1)(N−K−n+k+1))` from the first
   *  point, then normalised by their sum, so no `lgamma` difference has to be accurate to the
   *  last digit and no mass underflows on the way.  Empty for every other family.
   *
   *  **The first point is kept a `Double` and only the offset into the table is an `Int`**
   *  (F_0142): `of` bounds the support's width, not its position, and `Double.toInt` saturates,
   *  so a support past `Int.MaxValue` became a one-point table at `2147483647` and every mass
   *  was read as `0`.  Each ratio is a sum of four logarithms, never a product of two factors,
   *  which overflowed to `Inf` past `≈ 10³⁰²`; every factor is at least `1` on the support. */
  private def hyperTable: (Double, Array[Double], Array[Double]) =
    if kind != DistKind.Hypergeometric then (0.0, Array.empty, Array.empty)
    else
      val t = _Distribution.hyperTables.getOrElseUpdate(params)(_Distribution.HyperTable.of(params))
      (t.lo, t.masses, t.cumul)

  /** The density at `x` (continuous families) or the mass at `x` (discrete ones).
   *
   *  @param x the point
   *  @return the density or mass, or `None` where the family is undefined
   */
  def pdf(x: Double): Option[Double] =
    if x.isNaN then None
    else kind match
      case DistKind.Normal =>
        val (mu, s) = (params(0), params(1))
        val z = (x - mu) / s
        finiteOption(exp(-0.5 * z * z) / (s * sqrt(2.0 * Pi)))
      case DistKind.Uniform =>
        val (a, b) = (params(0), params(1))
        Some(if x >= a && x <= b then 1.0 / (b - a) else 0.0)
      case DistKind.Exponential =>
        val l = params(0)
        Some(if x < 0.0 then 0.0 else l * exp(-l * x))
      case DistKind.Binomial =>
        val (n, p) = (params(0), params(1))
        if x < 0.0 || x > n || x != Math.floor(x) then Some(0.0)
        else binomialPmf(n, p, x)
      case DistKind.Poisson =>
        val l = params(0)
        if x < 0.0 || x != Math.floor(x) then Some(0.0)
        else finiteOption(exp(-l + x * log(l) - lgammaOf(x + 1.0).getOrElse(Double.NaN)))
      case DistKind.StudentT =>
        val nu = params(0)
        // Through lgamma rather than gamma: the ratio Γ((ν+1)/2)/Γ(ν/2) overflows for a
        // large ν long before the ratio itself becomes large.
        for
          ln <- lgammaOf((nu + 1.0) / 2.0)
          ld <- lgammaOf(nu / 2.0)
          r  <- finiteOption(exp(ln - ld) / sqrt(nu * Pi) * Math.pow(1.0 + x * x / nu, -(nu + 1.0) / 2.0))
        yield r
      case DistKind.ChiSquared =>
        val k = params(0)
        if x <= 0.0 then Some(0.0)
        else
          // Assembled in log space for the same reason.
          for
            lg <- lgammaOf(k / 2.0)
            r  <- finiteOption(exp((k / 2.0 - 1.0) * log(x) - x / 2.0 - (k / 2.0) * log(2.0) - lg))
          yield r
      case DistKind.BetaDist =>
        val (a, b) = (params(0), params(1))
        if x < 0.0 || x > 1.0 then Some(0.0)
        // The endpoints are decided explicitly: `(a - 1) * log(0)` is `0 * -inf = NaN` at
        // a = 1, where the density is the finite value b, and infinite below it.
        else if x == 0.0 then betaEdge(a, b)
        else if x == 1.0 then betaEdge(b, a)
        else
          for
            la  <- lgammaOf(a); lb <- lgammaOf(b); lab <- lgammaOf(a + b)
            r   <- finiteOption(exp((a - 1.0) * log(x) + (b - 1.0) * log(1.0 - x) + lab - la - lb))
          yield r
      case DistKind.GammaDist =>
        val (k, rate) = (params(0), params(1))
        if x < 0.0 then Some(0.0)
        else if x == 0.0 then
          if k > 1.0 then Some(0.0) else if k == 1.0 then Some(rate) else None
        else
          for
            lg <- lgammaOf(k)
            r  <- finiteOption(exp(k * log(rate) + (k - 1.0) * log(x) - rate * x - lg))
          yield r
      case DistKind.Geometric =>
        val p = params(0)
        if x < 1.0 || x != Math.floor(x) then Some(0.0)
        else if p == 1.0 then Some(if x == 1.0 then 1.0 else 0.0)
        else finiteOption(p * exp((x - 1.0) * math.log1p(-p)))
      case DistKind.Hypergeometric =>
        val (lo, masses, _) = hyperTable
        if x != Math.floor(x) || x < lo || x >= lo + masses.length then Some(0.0)
        else Some(masses((x - lo).toInt))
      case DistKind.FDist =>
        val (d1, d2) = (params(0), params(1))
        if x < 0.0 then Some(0.0)
        // At 0 the density is infinite below d1 = 2, the finite 1 at it, and 0 above (betaEdge's shape).
        else if x == 0.0 then
          if d1 > 2.0 then Some(0.0) else if d1 == 2.0 then Some(1.0) else None
        else
          // In log space, the Student-t reason: the beta function overflows long before the density.
          for
            la <- lgammaOf(d1 / 2.0); lb <- lgammaOf(d2 / 2.0); lab <- lgammaOf((d1 + d2) / 2.0)
            r  <- finiteOption(exp(0.5 * (d1 * log(d1 * x) + d2 * log(d2) - (d1 + d2) * log(d1 * x + d2)) -
                             log(x) - (la + lb - lab)))
          yield r

  /** The cumulative distribution function `P(X ≤ x)`.
   *
   *  Every one of these is a closed form built on the special-function kernels rather than a numeric
   *  integral of [[pdf]]: the normal is `erfc`, the gamma-family tails are the regularised
   *  incomplete gamma, and the binomial tail is the regularised incomplete beta.  Numeric
   *  integration is available through `_DefIntegral` for anything added later that has no
   *  closed form, but using it here would be slower and less accurate for no reason.
   *
   *  @param x the point
   *  @return `P(X ≤ x)` in `[0, 1]`, or `None` where undefined
   */
  def cdf(x: Double): Option[Double] =
    if x.isNaN then None
    else kind match
      case DistKind.Normal =>
        val (mu, s) = (params(0), params(1))
        // 1/2 erfc(-z/sqrt 2) rather than 1/2 (1 + erf(z/sqrt 2)): in the left tail the
        // second form subtracts two nearly equal quantities and loses the answer.
        erfcOf(-(x - mu) / (s * sqrt(2.0))).flatMap(e => finiteOption(0.5 * e))
      case DistKind.Uniform =>
        val (a, b) = (params(0), params(1))
        Some(if x <= a then 0.0 else if x >= b then 1.0 else (x - a) / (b - a))
      case DistKind.Exponential =>
        val l = params(0)
        Some(if x <= 0.0 then 0.0 else 1.0 - exp(-l * x))
      case DistKind.Binomial =>
        val (n, p) = (params(0), params(1))
        val k = Math.floor(x)
        if k < 0.0 then Some(0.0)
        else if k >= n then Some(1.0)
        // P(X <= k) = I_{1-p}(n-k, k+1)
        else incompleteBetaOf(1.0 - p, n - k, k + 1.0)
      case DistKind.Poisson =>
        val l = params(0)
        val k = Math.floor(x)
        if k < 0.0 then Some(0.0) else upperGammaQ(k + 1.0, l)   // P(X <= k) = Q(k+1, lambda)
      case DistKind.StudentT =>
        // P(T <= t) = 1 - I_z(nu/2, 1/2)/2 for t > 0, with z = nu/(nu+t^2); mirrored below
        // zero by symmetry.  Exactly what the incomplete beta exists for.
        val nu = params(0)
        val z  = nu / (nu + x * x)
        incompleteBetaOf(z, nu / 2.0, 0.5).flatMap { ib =>
          finiteOption(if x > 0.0 then 1.0 - 0.5 * ib else 0.5 * ib)
        }
      case DistKind.ChiSquared =>
        // P(X <= x) = P(k/2, x/2), the regularised lower incomplete gamma.
        if x <= 0.0 then Some(0.0) else lowerGammaP(params(0) / 2.0, x / 2.0)
      case DistKind.BetaDist =>
        // The regularised incomplete beta IS the beta cdf.
        if x <= 0.0 then Some(0.0) else if x >= 1.0 then Some(1.0)
        else incompleteBetaOf(x, params(0), params(1))
      case DistKind.GammaDist =>
        // P(X <= x) = P(k, rate * x), the regularised lower incomplete gamma in the rate form.
        if x <= 0.0 then Some(0.0) else lowerGammaP(params(0), params(1) * x)
      case DistKind.Geometric =>
        // 1 - (1 - p)^k as -expm1(k·log1p(-p)): the textbook form subtracts two numbers near 1
        // and keeps only a few digits once k·p is small (the conditioning rule).
        val (p, k) = (params(0), Math.floor(x))
        if k < 1.0 then Some(0.0) else if p == 1.0 then Some(1.0) else finiteOption(-math.expm1(k * math.log1p(-p)))
      case DistKind.FDist =>
        // The regularised incomplete beta at d1·x/(d1·x + d2) IS the F cdf.
        val (d1, d2) = (params(0), params(1))
        if x <= 0.0 then Some(0.0) else incompleteBetaOf(d1 * x / (d1 * x + d2), d1 / 2.0, d2 / 2.0)
      case DistKind.Hypergeometric =>
        val (lo, masses, cumul) = hyperTable
        val k = Math.floor(x)
        if k < lo then Some(0.0) else if k >= lo + masses.length - 1 then Some(1.0) else Some(cumul((k - lo).toInt))

  /** The mean `E[X]`.
   *  @return the mean, or `None` if it does not exist
   */
  def mean: Option[Double] = kind match
    case DistKind.Normal      => Some(params(0))
    case DistKind.Uniform     => finiteOption(0.5 * (params(0) + params(1)))
    case DistKind.Exponential => finiteOption(1.0 / params(0))
    case DistKind.Binomial    => finiteOption(params(0) * params(1))
    case DistKind.Poisson     => Some(params(0))
    // The t mean does not exist for nu <= 1 — the defining integral does not converge —
    // so `None` here is a fact about the distribution, not a computational give-up.
    case DistKind.StudentT    => Option.when(params(0) > 1.0)(0.0)
    case DistKind.ChiSquared  => Some(params(0))
    case DistKind.BetaDist    => finiteOption(params(0) / (params(0) + params(1)))
    case DistKind.GammaDist   => finiteOption(params(0) / params(1))
    case DistKind.Geometric   => finiteOption(1.0 / params(0))
    case DistKind.Hypergeometric => finiteOption(params(2) * params(1) / params(0))
    // Exists only for d2 > 2: below it the defining integral diverges, a fact about the family.
    case DistKind.FDist       => Option.when(params(1) > 2.0)(params(1) / (params(1) - 2.0))

  /** The variance `Var(X)`.
   *  @return the variance, or `None` if it does not exist
   */
  def variance: Option[Double] = kind match
    case DistKind.Normal      => finiteOption(params(1) * params(1))
    case DistKind.Uniform     => val d = params(1) - params(0); finiteOption(d * d / 12.0)
    case DistKind.Exponential => finiteOption(1.0 / (params(0) * params(0)))
    case DistKind.Binomial    => finiteOption(params(0) * params(1) * (1.0 - params(1)))
    case DistKind.Poisson     => Some(params(0))
    // Undefined for nu <= 2, and infinite on (1, 2] — neither of which is a Double we
    // should hand back, so both give `None`.
    case DistKind.StudentT    => Option.when(params(0) > 2.0)(params(0) / (params(0) - 2.0))
    case DistKind.ChiSquared  => Some(2.0 * params(0))
    case DistKind.BetaDist    =>
      val s = params(0) + params(1)
      finiteOption(params(0) * params(1) / (s * s * (s + 1.0)))
    case DistKind.GammaDist   => finiteOption(params(0) / (params(1) * params(1)))
    case DistKind.Geometric   => finiteOption((1.0 - params(0)) / (params(0) * params(0)))
    case DistKind.Hypergeometric =>
      val (n, k, draws) = (params(0), params(1), params(2))
      if n <= 1.0 then Some(0.0)
      else finiteOption(draws * (k / n) * (1.0 - k / n) * (n - draws) / (n - 1.0))
    case DistKind.FDist =>
      val (d1, d2) = (params(0), params(1))
      if d2 <= 4.0 then None
      else finiteOption(2.0 * d2 * d2 * (d1 + d2 - 2.0) / (d1 * (d2 - 2.0) * (d2 - 2.0) * (d2 - 4.0)))

  /** `P(lo ≤ X ≤ hi)`.
   *
   *  For a discrete family the lower endpoint is *included*, which `cdf(hi) − cdf(lo)`
   *  would silently exclude — `P(1 ≤ X ≤ 3)` on a binomial must count `X = 1`.
   *
   *  @param lo the lower bound
   *  @param hi the upper bound
   *  @return the probability, or `None` where undefined
   */
  def probability(lo: Double, hi: Double): Option[Double] =
    if lo > hi then Some(0.0)
    else
      val lower = if isDiscrete then cdf(Math.ceil(lo) - 1.0) else cdf(lo)
      for a <- lower; b <- cdf(hi); r <- finiteOption(b - a) yield math.max(0.0, math.min(1.0, r))

  /** Renders as the grammar call that rebuilds it, so a bound distribution round-trips. */
  override def toString: String = display(Environment.DefaultPrecision)

  /** Renders at `precision` decimal places, as the grammar call that rebuilds it.
   *  @param precision decimal places for the parameters
   */
  def display(precision: Int): String =
    s"${DistKind.keyword(kind)}(${params.map(p => _Number(p).display(precision)).mkString(", ")})"

  /** The binomial mass `C(n, k) p^k (1−p)^(n−k)`, computed in log space so a large `n`
   *  does not overflow the binomial coefficient before the small `p^k` can shrink it.
   */
  private def binomialPmf(n: Double, p: Double, k: Double): Option[Double] =
    if p == 0.0 then Some(if k == 0.0 then 1.0 else 0.0)
    else if p == 1.0 then Some(if k == n then 1.0 else 0.0)
    else
      for
        ln  <- lgammaOf(n + 1.0)
        lk  <- lgammaOf(k + 1.0)
        lnk <- lgammaOf(n - k + 1.0)
        r   <- finiteOption(exp(ln - lk - lnk + k * log(p) + (n - k) * log(1.0 - p)))
      yield r

  /** The beta density at the endpoint where the exponent `a - 1` applies: zero above
   *  `a = 1`, the finite value `b` at it (`1/B(1, b)`), and infinite — so `None` — below. */
  private def betaEdge(a: Double, b: Double): Option[Double] =
    if a > 1.0 then Some(0.0) else if a == 1.0 then Some(b) else None


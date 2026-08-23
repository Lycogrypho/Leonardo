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
enum DistKind:
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
  /** `studentt(nu)` — parameter `(nu)`, the degrees of freedom, with `nu > 0` (added by 4.Q). */
  case StudentT
  /** `chisq(k)` — parameter `(k)`, the degrees of freedom, with `k > 0` (added by 4.Q). */
  case ChiSquared

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

  /** Looks a family up by its grammar keyword. */
  def fromKeyword(s: String): Option[DistKind] = values.find(k => keyword(k) == s)


/** Companion for [[_Distribution]]: the validating factory. */
object _Distribution:

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
    if params.length != DistKind.arity(kind) || params.exists(p => p.isNaN || p.isInfinite) then None
    else
      val ok = kind match
        case DistKind.Normal      => params(1) > 0.0
        case DistKind.Uniform     => params(1) > params(0)
        case DistKind.Exponential => params(0) > 0.0
        case DistKind.Poisson     => params(0) > 0.0
        case DistKind.StudentT    => params(0) > 0.0
        case DistKind.ChiSquared  => params(0) > 0.0
        case DistKind.Binomial    =>
          params(0) >= 0.0 && params(0) == Math.floor(params(0)) &&
          params(1) >= 0.0 && params(1) <= 1.0
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
    case DistKind.Binomial | DistKind.Poisson => true
    case _                                    => false

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
        finite(exp(-0.5 * z * z) / (s * sqrt(2.0 * Pi)))
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
        else finite(exp(-l + x * log(l) - lgammaOf(x + 1.0).getOrElse(Double.NaN)))
      case DistKind.StudentT =>
        val nu = params(0)
        // Through lgamma rather than gamma: the ratio Γ((ν+1)/2)/Γ(ν/2) overflows for a
        // large ν long before the ratio itself becomes large.
        for
          ln <- lgammaOf((nu + 1.0) / 2.0)
          ld <- lgammaOf(nu / 2.0)
          r  <- finite(exp(ln - ld) / sqrt(nu * Pi) * Math.pow(1.0 + x * x / nu, -(nu + 1.0) / 2.0))
        yield r
      case DistKind.ChiSquared =>
        val k = params(0)
        if x <= 0.0 then Some(0.0)
        else
          // Assembled in log space for the same reason.
          for
            lg <- lgammaOf(k / 2.0)
            r  <- finite(exp((k / 2.0 - 1.0) * log(x) - x / 2.0 - (k / 2.0) * log(2.0) - lg))
          yield r

  /** The cumulative distribution function `P(X ≤ x)`.
   *
   *  Every one of these is a closed form built on 4.O's kernels rather than a numeric
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
        erfcOf(-(x - mu) / (s * sqrt(2.0))).flatMap(e => finite(0.5 * e))
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
        // zero by symmetry.  Exactly the incomplete beta 4.O added, as the plan predicted.
        val nu = params(0)
        val z  = nu / (nu + x * x)
        incompleteBetaOf(z, nu / 2.0, 0.5).flatMap { ib =>
          finite(if x > 0.0 then 1.0 - 0.5 * ib else 0.5 * ib)
        }
      case DistKind.ChiSquared =>
        // P(X <= x) = P(k/2, x/2), the regularised lower incomplete gamma.
        if x <= 0.0 then Some(0.0) else lowerGammaP(params(0) / 2.0, x / 2.0)

  /** The mean `E[X]`.
   *  @return the mean, or `None` if it does not exist
   */
  def mean: Option[Double] = kind match
    case DistKind.Normal      => Some(params(0))
    case DistKind.Uniform     => finite(0.5 * (params(0) + params(1)))
    case DistKind.Exponential => finite(1.0 / params(0))
    case DistKind.Binomial    => finite(params(0) * params(1))
    case DistKind.Poisson     => Some(params(0))
    // The t mean does not exist for nu <= 1 — the defining integral does not converge —
    // so `None` here is a fact about the distribution, not a computational give-up.
    case DistKind.StudentT    => Option.when(params(0) > 1.0)(0.0)
    case DistKind.ChiSquared  => Some(params(0))

  /** The variance `Var(X)`.
   *  @return the variance, or `None` if it does not exist
   */
  def variance: Option[Double] = kind match
    case DistKind.Normal      => finite(params(1) * params(1))
    case DistKind.Uniform     => val d = params(1) - params(0); finite(d * d / 12.0)
    case DistKind.Exponential => finite(1.0 / (params(0) * params(0)))
    case DistKind.Binomial    => finite(params(0) * params(1) * (1.0 - params(1)))
    case DistKind.Poisson     => Some(params(0))
    // Undefined for nu <= 2, and infinite on (1, 2] — neither of which is a Double we
    // should hand back, so both give `None`.
    case DistKind.StudentT    => Option.when(params(0) > 2.0)(params(0) / (params(0) - 2.0))
    case DistKind.ChiSquared  => Some(2.0 * params(0))

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
      for a <- lower; b <- cdf(hi); r <- finite(b - a) yield math.max(0.0, math.min(1.0, r))

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
        r   <- finite(exp(ln - lk - lnk + k * log(p) + (n - k) * log(1.0 - p)))
      yield r

  private def finite(d: Double): Option[Double] =
    Option.when(!d.isNaN && !d.isInfinite)(d)

package it.grypho.scala.leonardo
package probability

import core.*


/** Bayes' theorem over a finite set of hypotheses: `bayes(prior, likelihood)` (F_0050).
 *
 *  Two vectors of the same shape — a `1×n` row or an `n×1` column — give the normalised
 *  posterior `pᵢ·Lᵢ / Σⱼ pⱼ·Lⱼ`, in that same shape.  No new carrier was needed: a row is what
 *  `tabulate` and `eigen` already return, and it is the form every textbook opens with.
 *
 *  **Exact on exact input.**  When every entry of both vectors is a [[core._Rational]] the
 *  posterior is computed over the rationals and returned as the prior's own node rebuilt with
 *  exact cells, so a posterior of fractions stays a posterior of fractions; one inexact entry
 *  demotes the whole computation to `Double`, the float-contagion rule.  The rebuild goes
 *  through the prior's [[core._MatrixShaped]] node, which is how this package yields a symbolic
 *  matrix without importing `matrix`.
 *
 *  **Refuses**, staying symbolic: a shape mismatch, an operand that is not a row or a column,
 *  a negative entry (neither a probability nor a likelihood), and **zero evidence**, `Σ = 0`,
 *  which has no posterior at all rather than a degenerate one.
 *
 *  @param prior      the prior weights over the hypotheses, a row or a column
 *  @param likelihood the likelihood of the observation under each hypothesis, the same shape
 */
case class _Bayes(prior: _Expression, likelihood: _Expression) extends _Expression:
  override def toString: String = s"bayes($prior, $likelihood)"
  override def children: List[_Expression] = List(prior, likelihood)
  override def rebuild(c: List[_Expression]): _Expression = _Bayes(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    val rp = prior.eval(env)
    val rl = likelihood.eval(env)
    val fallback = Left(_Bayes(rp.toExpression, rl.toExpression))
    (vectorCells(rp), vectorCells(rl)) match
      case (Some(p), Some(l)) if p.rows == l.rows && p.cols == l.cols =>
        (exactCells(p), exactCells(l), rp) match
          // The exact tier: both operands rational, and the prior still the symbolic node
          // whose rebuild carries the result -- an exact matrix never collapses to a dense one.
          case (Some(ep), Some(el), Left(shape: _MatrixShaped)) =>
            exactPosterior(ep, el).map(cells => Left(shape.rebuild(cells))).getOrElse(fallback)
          case _ =>
            densePosterior(p, l).map(Right(_)).getOrElse(fallback)
      case _ => fallback


/** A row or column of number-valued cells, exact cells kept as they are. */
private final case class Cells(rows: Int, cols: Int, cells: Vector[_Expression])

/** Reads an evaluated operand as a vector of numbers, or `None` when it is neither a dense
 *  nor a fully numeric symbolic vector. */
private def vectorCells(r: Either[_Expression, _Value]): Option[Cells] = r match
  case Right(m: _MatrixValue) if m.rows == 1 || m.cols == 1 =>
    Some(Cells(m.rows, m.cols, m.toVector.map(_Number(_))))
  case Left(m: _MatrixShaped) if (m.rows == 1 || m.cols == 1) && m.children.forall(isNumber) =>
    Some(Cells(m.rows, m.cols, m.children.toVector))
  case _ => None

private def isNumber(e: _Expression): Boolean = e match
  case _Number(_) => true
  case _          => false

/** The cells as rationals when every one of them is exact, else `None`. */
private def exactCells(c: Cells): Option[Vector[_Rational]] =
  val rs = c.cells.collect { case r: _Rational => r }
  Option.when(rs.size == c.cells.size)(rs)

/** The exact posterior cells, or `None` for a negative entry or zero evidence. */
private def exactPosterior(p: Vector[_Rational], l: Vector[_Rational]): Option[List[_Expression]] =
  if p.exists(_.signum < 0) || l.exists(_.signum < 0) then None
  else
    val joint    = p.zip(l).map((a, b) => a.multiply(b))
    val evidence = joint.reduce(_.add(_))
    if evidence.isZero then None
    else
      val cells = joint.flatMap(_.divide(evidence))
      Option.when(cells.size == joint.size)(cells.toList)

/** The `Double` posterior, or `None` for a negative or non-finite entry or zero evidence. */
private def densePosterior(p: Cells, l: Cells): Option[_MatrixValue] =
  val ps = p.cells.collect { case _Number(d) => d }
  val ls = l.cells.collect { case _Number(d) => d }
  if ps.exists(d => d.isNaN || d < 0.0) || ls.exists(d => d.isNaN || d < 0.0) then None
  else
    val joint    = ps.zip(ls).map(_ * _)
    val evidence = joint.sum
    if !(evidence > 0.0) || evidence.isInfinite then None
    else
      val m = _MatrixValue(p.rows, p.cols, joint.map(_ / evidence).toArray)
      Option.when(m.isFinite)(m)


/** Conjugate updating: `posterior(prior, likelihood, data)` (F_0050).
 *
 *  The likelihood is written as the family with its unknown parameter left **free** —
 *  `posterior(betadist(2, 2), binomial(10, p), 7)` — where `p` is the one free name, the
 *  same exactly-one-free-name rule `Predicate` applies to a random variable.  The answer is an
 *  ordinary [[_Distribution]], so `pdf`/`cdf`/`quantile`/`expect`/`variance` apply to it
 *  unchanged and nothing downstream learns a new type.
 *
 *  The table, each arm validating its data:
 *
 *  | prior | likelihood (free) | data | posterior |
 *  |---|---|---|---|
 *  | `betadist(a, b)` | `binomial(n, p)` | a count `k` in `0..n` | `betadist(a + k, b + n − k)` |
 *  | `gammadist(k, rate)` | `poisson(l)` | observations, non-negative integers | `gammadist(k + Σx, rate + n)` |
 *  | `normal(μ₀, σ₀)` | `normal(μ, σ)`, `σ` known | observations | precision-weighted `normal(μₙ, σₙ)` |
 *
 *  Observations are a `1×n` row or an `n×1` column; a single number is a sample of one.
 *
 *  **Everything else stays symbolic**: a pair outside the table, two free names or none, a
 *  likelihood whose free slot is not the conjugate parameter (`binomial(n, 0.5)`), the
 *  Normal–Normal case with unknown `σ` (that needs the Normal-inverse-gamma family, a
 *  two-parameter carrier this package does not have), and invalid data.  A general posterior
 *  — an unnormalised density with no closed form — is deliberately not attempted: it needs
 *  numerical integration over the parameter space or sampling, which is a package rather than
 *  a function, and declining beats a confident approximation.
 *
 *  **Exactness**: a `_Distribution` stores `Double` parameters, so a conjugate posterior is a
 *  `Double` result even from exact input — the `lu`/`qr` precedent, demote rather than refuse.
 *  Only [[_Bayes]] is exact.
 *
 *  @param prior      a distribution expression, the prior
 *  @param likelihood the sampling family with its unknown parameter free
 *  @param data       the observation: a count, a number, or a vector of observations
 */
case class _Posterior(prior: _Expression, likelihood: _Expression, data: _Expression)
    extends _Expression:
  override def toString: String = s"posterior($prior, $likelihood, $data)"
  override def children: List[_Expression] = List(prior, likelihood, data)
  override def rebuild(c: List[_Expression]): _Expression = _Posterior(c.head, c(1), c(2))

  override def eval(env: Environment): Either[_Expression, _Value] =
    val rp = prior.eval(env)
    val rd = data.eval(env)
    val updated =
      for
        pd <- rp.toOption.collect { case d: _Distribution => d }
        lk <- likelihoodShape(likelihood, env)
        d  <- conjugateUpdate(pd, lk, rd)
      yield d
    updated.map(Right(_)).getOrElse(Left(_Posterior(rp.toExpression, likelihood, rd.toExpression)))


/** A likelihood as the table reads it: the family, which parameter slot is free, and the
 *  numeric value of every other slot. */
private final case class Likelihood(kind: DistKind, freeSlot: Int, fixed: Map[Int, Double])

/** Reads `e` as a family with exactly one free, unbound parameter and every other one numeric.
 *
 *  A name bound in the environment is not free: `p := 0.3` makes `binomial(10, p)` a plain
 *  distribution with nothing to learn, and the call stays symbolic rather than guessing which
 *  parameter was meant.
 */
private def likelihoodShape(e: _Expression, env: Environment): Option[Likelihood] = e match
  case _DistributionOf(kind, args) =>
    val reduced = args.map(_.eval(env))
    val free    = reduced.zipWithIndex.collect { case (Left(_: _Variable), i) => i }
    val fixed   = reduced.zipWithIndex.collect { case (Right(_Number(d)), i) => i -> d }.toMap
    free match
      case slot :: Nil if fixed.size == args.size - 1 => Some(Likelihood(kind, slot, fixed))
      case _                                          => None
  case _ => None

/** The conjugate table.  Each arm validates its data and builds the result through the
 *  validating factory, so an update that lands outside the family's domain stays symbolic. */
private def conjugateUpdate(prior: _Distribution, lk: Likelihood,
                            data: Either[_Expression, _Value]): Option[_Distribution] =
  (prior.kind, lk.kind, lk.freeSlot) match
    // Beta-Binomial: k successes in n trials, p free.
    case (DistKind.BetaDist, DistKind.Binomial, 1) =>
      for
        n <- lk.fixed.get(0)
        k <- numberOf(data)
        if k == Math.floor(k) && k >= 0.0 && k <= n
        d <- _Distribution.of(DistKind.BetaDist, Vector(prior.params(0) + k, prior.params(1) + n - k))
      yield d
    // Gamma-Poisson: counts, lambda free.  Additive in the rate form, which is why gammadist
    // is shape-rate.
    case (DistKind.GammaDist, DistKind.Poisson, 0) =>
      for
        xs <- sampleOf(data)
        if xs.forall(x => x >= 0.0 && x == Math.floor(x))
        d  <- _Distribution.of(DistKind.GammaDist,
                               Vector(prior.params(0) + xs.sum, prior.params(1) + xs.size))
      yield d
    // Normal-Normal with KNOWN sigma, mu free: precision-weighted mean, precisions add.
    case (DistKind.Normal, DistKind.Normal, 0) =>
      for
        sigma <- lk.fixed.get(1)
        if sigma > 0.0
        xs    <- sampleOf(data)
        d     <- normalUpdate(prior.params(0), prior.params(1), sigma, xs)
      yield d
    case _ => None

/** `μₙ = (τ₀μ₀ + τΣx) / (τ₀ + nτ)`, `σₙ = (τ₀ + nτ)^(-1/2)`, with `τ = 1/σ²`. */
private def normalUpdate(mu0: Double, s0: Double, sigma: Double, xs: Vector[Double]): Option[_Distribution] =
  val tau0 = 1.0 / (s0 * s0)
  val tau  = 1.0 / (sigma * sigma)
  val prec = tau0 + xs.size * tau
  _Distribution.of(DistKind.Normal, Vector((tau0 * mu0 + tau * xs.sum) / prec, 1.0 / math.sqrt(prec)))

/** A single number, when the data is one. */
private def numberOf(r: Either[_Expression, _Value]): Option[Double] = r match
  case Right(_Number(d)) => Some(d)
  case _                 => None

/** The observations: a number is a sample of one, otherwise a row or column of numbers. */
private def sampleOf(r: Either[_Expression, _Value]): Option[Vector[Double]] =
  numberOf(r).map(Vector(_)).orElse(
    vectorCells(r).map(_.cells.collect { case _Number(d) => d }).filter(_.nonEmpty))

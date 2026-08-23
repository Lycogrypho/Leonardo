package it.grypho.scala.leonardo
package probability

import core.*
import scalar.{Sum, Product, Ratio, Power, dependsOn}


/** Builds a distribution from parameter *expressions*: `normal(mu, sigma)` and friends.
 *
 *  The node/value split `_Matrix` has with `_MatrixValue`: this is the symbolic form, whose
 *  `eval` folds to a [[_Distribution]] once every parameter reduces to a number.  It keeps
 *  `normal(m, s)` meaningful while `m` and `s` are still free variables.
 *
 *  @param kind the family
 *  @param args the parameter expressions, in the family's declared order
 */
case class _DistributionOf(kind: DistKind, args: List[_Expression]) extends _Expression:
  override def toString: String = s"${DistKind.keyword(kind)}(${args.mkString(", ")})"
  override def children: List[_Expression] = args
  override def rebuild(c: List[_Expression]): _Expression = _DistributionOf(kind, c)

  override def eval(env: Environment): Either[_Expression, _Value] =
    val reduced = args.map(_.eval(env))
    val numbers = reduced.collect { case Right(_Number(d)) => d }
    if numbers.size != args.size then Left(_DistributionOf(kind, reduced.map(_.toExpression)))
    else _Distribution.of(kind, numbers.toVector) match
      case Some(d) => Right(d)
      case None    => Left(_DistributionOf(kind, reduced.map(_.toExpression)))   // invalid parameters


/** What a [[_DistributionQuery]] asks of a distribution. */
enum Query:
  /** `pdf(d, x)` — density or mass at a point. */
  case Pdf
  /** `cdf(d, x)` — `P(X ≤ x)`. */
  case Cdf
  /** `prob(d, lo, hi)` — `P(lo ≤ X ≤ hi)`. */
  case Prob
  /** `quantile(d, p)` — the inverse cdf. */
  case Quantile

object Query:
  /** The grammar keyword for a query. */
  def keyword(q: Query): String = q.toString.toLowerCase


/** A numeric question about a distribution: `pdf`, `cdf`, `prob` or `quantile`.
 *
 *  One node for all four rather than four nodes, for the same reason [[DistKind]] is one
 *  enum: they differ only in which kernel they call.
 *
 *  **Why `prob(d, lo, hi)` and not `P(X < 2)`.**  The grammar has no comparison operators —
 *  only `=` and `==`, which build equations — so a predicate like `X < 2` cannot be
 *  expressed.  Adding `<` and `>` would be a change to the expression language, not to this
 *  domain.  An interval is the honest shape available today.
 *
 *  @param query the question
 *  @param dist  the distribution expression
 *  @param args  the query's own arguments (one for `pdf`/`cdf`/`quantile`, two for `prob`)
 */
case class _DistributionQuery(query: Query, dist: _Expression, args: List[_Expression])
    extends _Expression:
  override def toString: String =
    s"${Query.keyword(query)}(${(dist :: args).mkString(", ")})"
  override def children: List[_Expression] = dist :: args
  override def rebuild(c: List[_Expression]): _Expression =
    _DistributionQuery(query, c.head, c.tail)

  override def eval(env: Environment): Either[_Expression, _Value] =
    val rd = dist.eval(env)
    val ra = args.map(_.eval(env))
    val xs = ra.collect { case Right(_Number(d)) => d }
    (rd, xs.size == args.size) match
      case (Right(d: _Distribution), true) =>
        val result = (query, xs) match
          case (Query.Pdf, List(x))          => d.pdf(x)
          case (Query.Cdf, List(x))          => d.cdf(x)
          case (Query.Prob, List(lo, hi))    => d.probability(lo, hi)
          case (Query.Quantile, List(p))     => quantileOf(d, p)
          case _                             => None      // wrong arity: stays symbolic
        result.map(v => Right(_Number(v)))
              .getOrElse(Left(_DistributionQuery(query, rd.toExpression, ra.map(_.toExpression))))
      case _ => Left(_DistributionQuery(query, rd.toExpression, ra.map(_.toExpression)))


/** The expectation `expect(e, X)`, or `expect(d)` for a distribution's own mean.
 *
 *  **Why not `E[·]`.**  Square brackets are the matrix literal, so `E[X]` would parse as `E`
 *  times a one-element matrix.  `expect` is unambiguous and needs no grammar change.
 *
 *  The one-argument form is the mean of a distribution; the two-argument form is `E[e]`
 *  where `x` names the random variable inside `e`, and is where the linearity rules live.
 *
 *  @param e the expression whose expectation is wanted, or the distribution itself
 *  @param x the random variable, or `None` for the one-argument form
 */
case class _Expectation(e: _Expression, x: Option[_Variable]) extends _Expression:
  override def toString: String =
    x match
      case Some(v) => s"expect($e, $v)"
      case None    => s"expect($e)"
  // The random variable is a BINDER: it names the thing being averaged over, so `substitute`
  // must not rewrite it -- the same convention `_Derivative` and `_Integral` follow.
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = _Expectation(c.head, x)

  override def eval(env: Environment): Either[_Expression, _Value] =
    Moments.expectation(e, x, env) match
      case Some(r) => r
      case None    => Left(this)


/** The variance `variance(e, X)`, or `variance(d)` for a distribution's own variance.
 *
 *  @param e the expression whose variance is wanted, or the distribution itself
 *  @param x the random variable, or `None` for the one-argument form
 */
case class _Variance(e: _Expression, x: Option[_Variable]) extends _Expression:
  override def toString: String =
    x match
      case Some(v) => s"variance($e, $v)"
      case None    => s"variance($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = _Variance(c.head, x)

  override def eval(env: Environment): Either[_Expression, _Value] =
    Moments.variance(e, x, env) match
      case Some(r) => r
      case None    => Left(this)


/** Numerically inverts a cdf by bisection.
 *
 *  Brackets outward from the mean before bisecting, so it needs no per-family quantile
 *  formula and works for any distribution whose cdf is monotone — which is all of them.
 *
 *  @param d the distribution
 *  @param p the probability, in `(0, 1)`
 *  @return the smallest `x` with `cdf(x) ≥ p`, or `None` outside `(0, 1)`
 */
private def quantileOf(d: _Distribution, p: Double): Option[Double] =
  if p.isNaN || p <= 0.0 || p >= 1.0 then None
  else
    val centre = d.mean.getOrElse(0.0)
    val spread = d.variance.map(v => math.sqrt(v)).filter(_ > 0.0).getOrElse(1.0)
    // Expand the bracket until it straddles p; 60 doublings covers any reachable tail.
    var lo   = centre - spread
    var hi   = centre + spread
    var i    = 0
    while i < 60 && d.cdf(lo).exists(_ > p) do { lo -= spread * (1 << math.min(i, 20)); i += 1 }
    i = 0
    while i < 60 && d.cdf(hi).exists(_ < p) do { hi += spread * (1 << math.min(i, 20)); i += 1 }
    if !d.cdf(lo).exists(_ <= p) || !d.cdf(hi).exists(_ >= p) then None
    else
      var a = lo
      var b = hi
      var k = 0
      while k < 200 && (b - a) > 1e-14 * math.max(1.0, math.abs(a)) do
        val m = 0.5 * (a + b)
        if d.cdf(m).exists(_ < p) then a = m else b = m
        k += 1
      Some(if d.isDiscrete then math.ceil(b - 1e-9) else 0.5 * (a + b))

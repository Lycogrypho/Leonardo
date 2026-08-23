package it.grypho.scala.leonardo
package probability

import core.*
import equation.{_Comparison, CompareOp, _EqualityCheck, _Equation}
import logic.And


/** Turns a predicate over a random variable into a probability — issue 4.R slice B.
 *
 *  `prob(X < 2)` is how the notation is actually written, and until comparison operators
 *  existed the language could not say it; 4.P had to offer `prob(d, lo, hi)` instead.  Both
 *  forms are kept: an interval is still the clearer way to write a two-sided bound, and
 *  removing it would break saved sessions.
 *
 *  **The give-up rule matters more than usual here.**  A predicate this cannot interpret
 *  must leave its node symbolic, because the bad failure mode is not an error — it is
 *  silently answering the probability of a *different* event.
 */

/** A one-sided or two-sided bound on a random variable, in the reals.
 *
 *  Inclusivity is tracked because it is invisible for a continuous family and decisive for
 *  a discrete one: `P(X < 2)` and `P(X <= 2)` differ by the whole mass at 2.
 */
private case class Bounds(lo: Double, loIncl: Boolean, hi: Double, hiIncl: Boolean):
  /** Intersects two bounds, which is what `and` means. */
  def meet(that: Bounds): Bounds =
    val (l, li) =
      if that.lo > lo then (that.lo, that.loIncl)
      else if that.lo < lo then (lo, loIncl)
      else (lo, loIncl && that.loIncl)
    val (h, hi2) =
      if that.hi < hi then (that.hi, that.hiIncl)
      else if that.hi > hi then (hi, hiIncl)
      else (hi, hiIncl && that.hiIncl)
    Bounds(l, li, h, hi2)

private object Bounds:
  /** The unconstrained bound, the identity of [[Bounds.meet]]. */
  val Unbounded: Bounds =
    Bounds(Double.NegativeInfinity, false, Double.PositiveInfinity, false)


/** The probability that `pred` holds, when `pred` is a recognised predicate over exactly one
 *  random variable.
 *
 *  Recognised shapes: `X < c`, `X <= c`, `X > c`, `X >= c` (in either operand order), `X == c`,
 *  `X != c`, and any `and` of those.  Anything else — `or`, a predicate over two random
 *  variables, a non-constant bound — yields `None`.
 *
 *  @param pred the predicate expression
 *  @param env  supplies the bindings that say which name is random
 *  @return the probability, or `None` when the predicate is not recognised
 */
private[probability] def probabilityOfPredicate(pred: _Expression,
                                                env: Environment): Option[Double] =
  for
    v <- randomVariableOf(pred, env)
    d <- env.get(v).collect { case dist: _Distribution => dist }
    p <- evaluatePredicate(pred, v, d, env)
  yield p

/** The single random variable a predicate is about, or `None` if there is not exactly one.
 *
 *  "Random" means bound to a `_Distribution` — the same test `Moments.isConstant` uses, so
 *  the two agree about what a random variable is.  Requiring *exactly* one is what keeps
 *  `prob(X < Y)` from being answered as though `Y` were a constant.
 */
private def randomVariableOf(pred: _Expression, env: Environment): Option[String] =
  val randoms = pred.freeVars.filter(n => env.get(n).exists(_.isInstanceOf[_Distribution]))
  Option.when(randoms.size == 1)(randoms.head)

/** Evaluates a recognised predicate against the distribution. */
private def evaluatePredicate(pred: _Expression, v: String, d: _Distribution,
                              env: Environment): Option[Double] = pred match
  // Equality and inequality are point events, not intervals: for a continuous family the
  // probability of a single point is zero, for a discrete one it is the mass there.
  case _EqualityCheck(l, r) => pointMass(l, r, v, d, env)
  case _Equation(l, r)      => pointMass(l, r, v, d, env)
  case _Comparison(l, CompareOp.Ne, r) =>
    pointMass(l, r, v, d, env).flatMap(p => finite(1.0 - p))
  case _ => boundsOf(pred, v, env).flatMap(b => probabilityOfBounds(b, d))

/** `P(X = c)`, given the two sides of an equality. */
private def pointMass(l: _Expression, r: _Expression, v: String,
                      d: _Distribution, env: Environment): Option[Double] =
  constantSide(l, r, v, env).flatMap { (c, _) =>
    if d.isDiscrete then d.pdf(c) else Some(0.0)
  }

/** Reads a predicate as a bound on `v`. */
private def boundsOf(pred: _Expression, v: String, env: Environment): Option[Bounds] =
  pred match
    // `and` intersects, which is what makes `0 < X and X < 1` work.  `or` is deliberately
    // absent: a union of intervals is not a Bounds, and answering it as anything else would
    // be wrong rather than merely incomplete.
    case And(a, b) =>
      for ba <- boundsOf(a, v, env); bb <- boundsOf(b, v, env) yield ba.meet(bb)
    case _Comparison(l, op, r) =>
      constantSide(l, r, v, env).flatMap { (c, varOnLeft) =>
        // `c < X` is `X > c`: flip the operator when the variable is on the right.
        val effective = if varOnLeft then op else flip(op)
        effective match
          case CompareOp.Lt => Some(Bounds(Double.NegativeInfinity, false, c, false))
          case CompareOp.Le => Some(Bounds(Double.NegativeInfinity, false, c, true))
          case CompareOp.Gt => Some(Bounds(c, false, Double.PositiveInfinity, false))
          case CompareOp.Ge => Some(Bounds(c, true, Double.PositiveInfinity, false))
          case CompareOp.Ne => None      // handled as a point event, not a bound
      }
    case _ => None

/** The constant side of a two-sided relation, plus which side the variable was on.
 *
 *  Requires the variable side to be the *bare* variable: `2*X < 6` is not recognised, because
 *  solving for `X` would mean dividing by a coefficient whose sign decides whether the
 *  inequality flips — the same trap that keeps `_Comparison` out of `_ElementWise`.
 */
private def constantSide(l: _Expression, r: _Expression, v: String,
                         env: Environment): Option[(Double, Boolean)] =
  def bare(e: _Expression): Boolean = e match
    case w: _Variable => w.variable == v
    case _            => false
  def constant(e: _Expression): Option[Double] =
    if e.freeVars.contains(v) then None
    else e.eval(env) match
      case Right(_Number(d)) => Some(d)
      case _                 => None

  if bare(l) then constant(r).map((_, true))
  else if bare(r) then constant(l).map((_, false))
  else None

/** Mirrors a comparison, for when the variable sits on the right. */
private def flip(op: CompareOp): CompareOp = op match
  case CompareOp.Lt => CompareOp.Gt
  case CompareOp.Gt => CompareOp.Lt
  case CompareOp.Le => CompareOp.Ge
  case CompareOp.Ge => CompareOp.Le
  case CompareOp.Ne => CompareOp.Ne

/** The probability mass inside a bound.
 *
 *  For a discrete family the real bounds are first snapped to the integers they actually
 *  admit — `X < 2` means `X <= 1` — which is where the inclusivity flags earn their keep.
 */
private def probabilityOfBounds(b: Bounds, d: _Distribution): Option[Double] =
  if d.isDiscrete then
    val hiEff = if b.hiIncl then Math.floor(b.hi) else Math.ceil(b.hi) - 1.0
    val loEff = if b.loIncl then Math.ceil(b.lo) else Math.floor(b.lo) + 1.0
    if hiEff < loEff then Some(0.0)
    else
      for
        upper <- d.cdf(hiEff)
        lower <- if loEff.isNegInfinity then Some(0.0) else d.cdf(loEff - 1.0)
        r     <- finite(upper - lower)
      yield clamp(r)
  else
    // Continuous: the endpoints carry no mass, so inclusivity does not matter.
    for
      upper <- d.cdf(b.hi)
      lower <- d.cdf(b.lo)
      r     <- finite(upper - lower)
    yield clamp(r)

private def clamp(d: Double): Double = math.max(0.0, math.min(1.0, d))

private def finite(d: Double): Option[Double] =
  Option.when(!d.isNaN && !d.isInfinite)(d)

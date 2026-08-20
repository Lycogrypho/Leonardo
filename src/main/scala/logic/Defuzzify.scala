package it.grypho.scala.leonardo
package logic

import core.*
import scalar.compile


/** Default number of grid points used when defuzzifying; odd so the midpoint of the
 *  range is always sampled.
 */
val DefaultDefuzzSamples: Int = 201


/** Samples a membership expression over a uniform grid of `n` points in `[lo, hi]`.
 *
 *  Fast path: when the expression is pure scalar arithmetic it `scalar.compile`s to a
 *  `Double => Double` closure — the same fast path `_DefIntegral.eval` uses for Simpson's
 *  rule.  A membership or connective node is not something `compile` can lower, so it
 *  returns `None` and the fallback evaluates the tree per point, reading the result with
 *  [[asDegree]] so a truth-valued degree counts (`scalar.sample` would drop it, since it
 *  keeps only `_Number` results).
 *
 *  A point is kept only when its value is an actual membership degree — finite and
 *  inside `[0, 1]`.  The range test matters for the fast path: the tree path already
 *  reads through [[asDegree]], which rejects anything outside the interval, so without
 *  it the same curve would be treated differently depending on whether `compile`
 *  happened to lower it.  Points outside the interval are dropped, following the
 *  `sample` convention for values that are not results.
 *
 *  @param membership the membership expression in `v`
 *  @param v          the variable ranging over the grid
 *  @param lo         left endpoint
 *  @param hi         right endpoint
 *  @param n          number of grid points
 *  @param env        environment for resolving any other free variables
 *  @return `(x, degree)` pairs in ascending `x` order
 */
def sampleDegrees(
    membership: _Expression,
    v:          _Variable,
    lo:         Double,
    hi:         Double,
    n:          Int          = DefaultDefuzzSamples,
    env:        Environment  = new Environment()
): Vector[(Double, Double)] =
  val step = if n <= 1 then 0.0 else (hi - lo) / (n - 1)
  val xs   = Vector.tabulate(n)(i => lo + i * step)
  val at: Double => Option[Double] = compile(membership, v, env) match
    case Some(f) => x => Some(f(x))
    case None    => x =>
      membership.eval(env.withBinding(v.variable, _Number(x))) match
        case Right(value) => asDegree(value, env.symmetricLogic)
        case _            => None
  xs.flatMap(x => at(x).filter(isDegree).map(d => (x, d)))


/** Whether `d` is an actual membership degree: finite and inside `[0, 1]`. */
private def isDegree(d: Double): Boolean =
  !d.isNaN && !d.isInfinite && d >= 0.0 && d <= 1.0


/** Defuzzifies by the centre of gravity: `Σ x·μ(x) / Σ μ(x)` over the sampled curve.
 *
 *  The most common defuzzifier, and the one `defuzz(...)` uses in the grammar.
 *
 *  @param membership the membership expression in `v`
 *  @param v          the variable ranging over the universe of discourse
 *  @param lo         left endpoint of the universe
 *  @param hi         right endpoint
 *  @param n          number of grid points
 *  @param env        environment for resolving any other free variables
 *  @return the crisp representative, or `None` when nothing was sampled or the total
 *          membership is zero (no centroid is defined)
 */
def centroid(
    membership: _Expression,
    v:          _Variable,
    lo:         Double,
    hi:         Double,
    n:          Int          = DefaultDefuzzSamples,
    env:        Environment  = new Environment()
): Option[Double] =
  val points = sampleDegrees(membership, v, lo, hi, n, env)
  val total  = points.map(_._2).sum
  Option.when(points.nonEmpty && total > 0.0)(points.map((x, d) => x * d).sum / total)


/** Defuzzifies by the mean of maxima: the average of the sampled points that attain the
 *  greatest membership.
 *
 *  Points within `1e-9` of the maximum count as maximal, so a plateau is averaged rather
 *  than resolved by floating-point noise.
 *
 *  @param membership the membership expression in `v`
 *  @param v          the variable ranging over the universe of discourse
 *  @param lo         left endpoint of the universe
 *  @param hi         right endpoint
 *  @param n          number of grid points
 *  @param env        environment for resolving any other free variables
 *  @return the crisp representative, or `None` when nothing was sampled or the curve is
 *          everywhere zero
 */
def meanOfMaxima(
    membership: _Expression,
    v:          _Variable,
    lo:         Double,
    hi:         Double,
    n:          Int          = DefaultDefuzzSamples,
    env:        Environment  = new Environment()
): Option[Double] =
  val points = sampleDegrees(membership, v, lo, hi, n, env)
  Option.when(points.nonEmpty)(points.map(_._2).max).filter(_ > 0.0).map { peak =>
    val maximal = points.filter((_, d) => math.abs(d - peak) <= 1e-9).map(_._1)
    maximal.sum / maximal.size
  }


/** Defuzzifies by the bisector: the sampled point at which the cumulative membership
 *  first reaches half of the total, splitting the area in two.
 *
 *  @param membership the membership expression in `v`
 *  @param v          the variable ranging over the universe of discourse
 *  @param lo         left endpoint of the universe
 *  @param hi         right endpoint
 *  @param n          number of grid points
 *  @param env        environment for resolving any other free variables
 *  @return the crisp representative, or `None` when nothing was sampled or the total
 *          membership is zero
 */
def bisector(
    membership: _Expression,
    v:          _Variable,
    lo:         Double,
    hi:         Double,
    n:          Int          = DefaultDefuzzSamples,
    env:        Environment  = new Environment()
): Option[Double] =
  val points = sampleDegrees(membership, v, lo, hi, n, env)
  val total  = points.map(_._2).sum
  Option.when(points.nonEmpty && total > 0.0) {
    val half = total / 2.0
    // scanLeft gives the running area; the first index reaching `half` is the bisector
    val cumulative = points.map(_._2).scanLeft(0.0)(_ + _).tail
    val idx = cumulative.indexWhere(_ >= half)
    points(if idx < 0 then points.size - 1 else idx)._1
  }


/** Defuzzification of a membership expression to a crisp value: `defuzz(e, v, lo, hi)`.
 *
 *  Evaluates by [[centroid]] once `lo` and `hi` fold to numbers; stays symbolic otherwise
 *  (the fixpoint convention shared with the transforms and `_ODE`).  `v` is a binder, so
 *  it is excluded from `children` — the same convention as `_DefIntegral`.
 *
 *  @param e  the membership expression
 *  @param v  the variable ranging over the universe of discourse
 *  @param lo left endpoint of the universe
 *  @param hi right endpoint
 */
case class _Defuzzify(e: _Expression, v: _Variable, lo: _Expression, hi: _Expression)
    extends _Expression:
  override def toString: String = s"defuzz($e, $v, $lo, $hi)"
  override def children: List[_Expression] = List(e, lo, hi)
  override def rebuild(cs: List[_Expression]): _Expression = _Defuzzify(cs.head, v, cs(1), cs(2))

  override def eval(env: Environment): Either[_Expression, _Value] =
    (lo.eval(env), hi.eval(env)) match
      case (Right(_Number(l)), Right(_Number(h))) if l < h =>
        centroid(e, v, l, h, DefaultDefuzzSamples, env) match
          case Some(x) => Right(_Number(x))
          case None    => Left(this)
      case _ => Left(this)

package it.grypho.scala.leonardo
package control

import core.*
import optimize.qpPrepare
import scalar.RoundingLevel


// The steady-state target layer (F_0358, AIACE L_0079).
//
// With more inputs than setpoints an MPC has a whole family of steady states that meet the setpoints;
// the target layer of industrial MPC (Muske and Rawlings, 1993; Rao and Rawlings, 1999) chooses one by a
// priority order: the goals of the first level are met as closely as the bounds allow, those of the
// next as closely as possible without giving up anything the first achieved, and so on, the inputs'
// resting values after every output goal and the smallest move from the current input last, so the
// answer is unique.  Each level is one small `qpSolve`; its optimum is held for the levels after it.

/** A steady-state model the target layer reads (F_0358). */
sealed trait SteadyStateModel

/** The state-space steady state (F_0358): `x_s = A·x_s + B·u_s + Bd·d`, `y_s = C·x_s + Cd·d` — the
 *  relation [[steadyStateTarget]] solves, the disturbance estimate `d` included.  `I − A` may be
 *  singular (a plant with an integrator).
 *
 *  @param a  the state matrix, `n×n`
 *  @param b  the input matrix, `n×m`
 *  @param c  the output matrix, `p×n`
 *  @param bd the disturbance's entry into the state, `n×nd`
 *  @param cd the disturbance's entry into the output, `p×nd`
 *  @param d  the disturbance estimate, `nd` entries
 */
final case class StateSpaceSteadyState(a: _MatrixValue, b: _MatrixValue, c: _MatrixValue, bd: _MatrixValue,
                                       cd: _MatrixValue, d: Vector[Double]) extends SteadyStateModel

/** The steady state through a gain matrix (F_0358): `y_s = y0 + G·(u_s − u0)`, the form an increment-input
 *  model gives most naturally — `y0` the outputs measured at the input `u0`.  It has no state, so the
 *  target's `x` is `None`.
 *
 *  @param g  the steady-state gain, `p×m`
 *  @param y0 the outputs at `u0`, `p` entries
 *  @param u0 the input `y0` was measured at, `m` entries
 */
final case class GainSteadyState(g: _MatrixValue, y0: Vector[Double], u0: Vector[Double]) extends SteadyStateModel

/** One goal of a priority level of the target layer (F_0358), on output `output` (zero-based). */
enum TargetGoal:
  /** Bring the output to `value`; the level minimises `weight·(y − value)²`.
   *  @param output the output, zero-based
   *  @param value  the setpoint
   *  @param weight its weight within the level, `> 0` */
  case Setpoint(output: Int, value: Double, weight: Double = 1.0)
  /** Keep the output within `[lower, upper]` (either side may be infinite); the level minimises
   *  `weight·excess²`, so a bound that cannot hold is exceeded as little as possible.  Called an
   *  **output bound**, as in [[MpcBounds]], and not a limit, a word the library already spends on
   *  `limit(…)` and the work budgets.
   *  @param output the output, zero-based
   *  @param lower  the lower bound, `-∞` for none
   *  @param upper  the upper bound, `+∞` for none
   *  @param weight its weight within the level, `> 0` */
  case OutputBound(output: Int, lower: Double, upper: Double, weight: Double = 1.0)

/** The inputs' resting values (F_0358): among the targets every output level allows, the one minimising
 *  `‖u_s − target‖²_weight`.
 *
 *  @param target `u_rest`, `m` entries
 *  @param weight `S`, `m×m`, symmetric positive semidefinite; a zero row leaves that input free
 */
final case class RestingValues(target: Vector[Double], weight: _MatrixValue)

/** What the target layer is asked for (F_0358): `levels` in priority order, the first met as closely as
 *  possible and each later one within what the earlier ones achieved, then the resting values.
 *
 *  @param levels the priority levels, each a group of goals weighed together
 *  @param rest   the inputs' resting values, or `None`
 */
final case class TargetSpec(levels: Vector[Vector[TargetGoal]], rest: Option[RestingValues] = None)

/** The usual specification. */
object TargetSpec:
  /** Every output bound first, as one level, then every setpoint, as one level of unit weights (F_0358
   *  Decision C: a bound such as a pressure drop gives way after no setpoint).
   *
   *  @param setpoints    one setpoint per output
   *  @param outputBounds `(lower, upper)`, one entry per output, `±∞` for none, or `None`
   *  @param rest         the inputs' resting values, or `None`
   *  @return the specification
   */
  def of(setpoints: Vector[Double], outputBounds: Option[(Vector[Double], Vector[Double])] = None,
         rest: Option[RestingValues] = None): TargetSpec =
    val bounds = outputBounds.toVector.flatMap((lo, hi) =>
      lo.indices.collect { case i if lo(i).isFinite || hi(i).isFinite => TargetGoal.OutputBound(i, lo(i), hi(i)) })
    val points = setpoints.indices.map(i => TargetGoal.Setpoint(i, setpoints(i))).toVector
    TargetSpec(Vector(bounds, points).filter(_.nonEmpty), rest)

/** The target the layer chose (F_0358).
 *
 *  @param x     `x_s`, for a state-space model; `None` for a gain model
 *  @param u     `u_s`
 *  @param y     `y_s`
 *  @param unmet the goals the target does not meet, each to `0.5·10⁻ᵖ` of its own terms; empty when every
 *               goal is met
 */
final case class TargetLayer(x: Option[Vector[Double]], u: Vector[Double], y: Vector[Double], unmet: Vector[TargetGoal]):
  /** Whether every goal is met.
   *  @return `unmet.isEmpty` */
  def reached: Boolean = unmet.isEmpty

/** The steady-state target by priority (F_0358, AIACE L_0079): the levels of `spec` in order, then the
 *  resting values, then the smallest move from the current input `u`.
 *
 *  **Each level is one `qpSolve`**, over the steady-state unknowns and a slack per output bound, under
 *  the input bounds and everything the levels before it achieved.  **What a level achieved is held**
 *  (Decision D): with positive weights a level's optimal outputs and excesses are unique, so the
 *  levels after it keep each setpoint's output where it was and each bound's excess no larger, relaxed
 *  only by `RoundingLevel` of the output's own terms, so no later level gives up anything an earlier
 *  one achieved — never by a tolerance on a squared cost, which would be a claim about its units and
 *  hold nothing back at a met setpoint.  (Two inequalities, not an equality: an equality computed in
 *  floating point is exactly inconsistent with the steady-state equations whenever it depends on
 *  them, and a semidefinite QP starts from an exact LP.)  The resting values are
 *  held the same way (`S·u_s` unchanged).  **The last level is exact** (Decision E): the smallest
 *  move `Σₖ ((u_s,k − u_k)/sₖ)²`, `sₖ` an input's bound width, else `|u_k|`, else the move that changes
 *  some output by its own size through the steady-state gain (F_0368), else 1, so the target is
 *  unique and does not jump between equivalent points from cycle to cycle; no level is regularised,
 *  the `H + εI` the QP tier refuses.
 *
 *  A goal not met is an answer, reported in [[TargetLayer.unmet]], not a failure; a level the bounds
 *  make infeasible, or a QP that declines, is.  Every QP is semidefinite in general, so it inherits
 *  the exact LP's caps for its feasible start ([[optimize.qpSolve]]).
 *
 *  @param model   the steady-state relation: state space or a gain
 *  @param spec    the priority levels and the resting values
 *  @param uBounds `(lower, upper)` on `u_s`, `m` entries each, or `None`
 *  @param u       the current input, `m` entries, which the last level moves least from
 *  @param env     the precision of the report and of every QP's certificate, and the budgets
 *  @return the target, or why there is none
 */
def steadyStateTargets(model: SteadyStateModel, spec: TargetSpec, uBounds: Option[(Vector[Double], Vector[Double])],
                       u: Vector[Double], env: Environment): Either[MpcFailure, TargetLayer] =
  steadyRelation(model).flatMap { rel =>
    targetSpecError(rel, spec, uBounds, u) match
      case Some(why) => Left(MpcFailure.InvalidInput(why))
      case None      => targetLayer(rel, spec, uBounds, u, env)
  }

/** `y = F·w + g` over the unknowns `w`, with `E·w = e`; `u_s` is `w(uAt until uAt + m)`.  `gain` is the
 *  steady-state gain `∂y/∂u`, `p×m`, when the model states one: the gain itself, or `C(I − A)⁻¹B` when
 *  `I − A` is invertible (a plant with an integrator has none). */
private final class SteadyRelation(val nw: Int, val uAt: Int, val m: Int, val p: Int,
                                   val f: Array[Array[Double]], val g: Array[Double],
                                   val eq: Option[(Array[Array[Double]], Array[Double])], val hasState: Boolean,
                                   val gain: Option[_MatrixValue])

/** `lo ≤ row·w ≤ hi`, what a level achieved, held for the levels after it. */
private final case class Held(row: Array[Double], lo: Double, hi: Double)

/** The relation a model states, or why it does not conform. */
private def steadyRelation(model: SteadyStateModel): Either[MpcFailure, SteadyRelation] =
  def bad(s: String) = Left(MpcFailure.InvalidInput(s))
  def times(mv: _MatrixValue, v: Vector[Double]) = Array.tabulate(mv.rows)(i => (0 until mv.cols).map(j => mv(i, j) * v(j)).sum)
  model match
    case StateSpaceSteadyState(a, b, c, bd, cd, d) =>
      val (n, m, p, nd) = (a.rows, b.cols, c.rows, bd.cols)
      if a.cols != n || b.rows != n || c.cols != n || bd.rows != n || cd.rows != p || cd.cols != nd || d.size != nd then
        bad("the shapes of A, B, C, Bd, Cd and d do not conform")
      else if !(Seq(a, b, c, bd, cd).forall(_.isFinite) && d.forall(_.isFinite)) then bad("every entry must be finite")
      else
        val nw = n + m
        // (A − I)·x + B·u = −Bd·d, y = C·x + Cd·d
        val e = Array.tabulate(n)(i => Array.tabulate(nw)(j =>
          if j < n then a(i, j) - (if i == j then 1.0 else 0.0) else b(i, j - n)))
        val f = Array.tabulate(p)(i => Array.tabulate(nw)(j => if j < n then c(i, j) else 0.0))
        val gain = _MatrixValue.identity(n).add(a.scale(-1.0)).inverse.map(inv => c.multiply(inv).multiply(b)).filter(_.isFinite)
        Right(SteadyRelation(nw, n, m, p, f, times(cd, d), Some((e, times(bd, d).map(-_))), hasState = true, gain))
    case GainSteadyState(g, y0, u0) =>
      val (p, m) = (g.rows, g.cols)
      if y0.size != p || u0.size != m then bad(s"y0 needs $p entries and u0 $m")
      else if !(g.isFinite && (y0 ++ u0).forall(_.isFinite)) then bad("every entry must be finite")
      else
        val gu0 = times(g, u0)
        Right(SteadyRelation(m, 0, m, p, Array.tabulate(p, m)((i, j) => g(i, j)), Array.tabulate(p)(i => y0(i) - gu0(i)),
                             None, hasState = false, Some(g)))

/** Why the specification, the bounds or the current input do not fit the model, or `None`. */
private def targetSpecError(rel: SteadyRelation, spec: TargetSpec, uBounds: Option[(Vector[Double], Vector[Double])],
                            u: Vector[Double]): Option[String] =
  import TargetGoal.*
  def goalOk(gl: TargetGoal) = gl match
    case Setpoint(i, v, w)        => i >= 0 && i < rel.p && v.isFinite && w > 0 && w.isFinite
    case OutputBound(i, lo, hi, w) => i >= 0 && i < rel.p && !lo.isNaN && !hi.isNaN && lo <= hi &&
                                      lo != Double.PositiveInfinity && hi != Double.NegativeInfinity && w > 0 && w.isFinite
  val m = rel.m
  if !spec.levels.forall(_.forall(goalOk)) then
    Some(s"every goal needs an output in 0..${rel.p - 1}, a finite setpoint or lower <= upper, and a weight > 0")
  else if uBounds.exists((lo, hi) => lo.size != m || hi.size != m || lo.zip(hi).exists((l, h) => !(l <= h))) then
    Some(s"the input bounds need $m entries each, lower below upper")
  else if u.size != m || !u.forall(_.isFinite) then Some(s"the current input needs $m finite entries")
  else spec.rest.flatMap { r =>
    val s = r.weight
    if r.target.size != m || !r.target.forall(_.isFinite) then Some(s"the resting values need $m finite entries")
    else if s.rows != m || s.cols != m || !s.isFinite then Some(s"the resting weight must be a finite ${m}x$m")
    else if (0 until m).exists(i => (0 until i).exists(j => s(i, j) != s(j, i))) then Some("the resting weight must be symmetric")
    else None
  }

/** The levels in order, the resting values, the tie-break; the report at the final target. */
private def targetLayer(rel: SteadyRelation, spec: TargetSpec, uBounds: Option[(Vector[Double], Vector[Double])],
                        u: Vector[Double], env: Environment): Either[MpcFailure, TargetLayer] =
  import TargetGoal.*
  val (nw, uAt, m) = (rel.nw, rel.uAt, rel.m)
  /** A held output's relaxation, relative to its terms: the rounding level. */
  val hold = RoundingLevel
  val lbW = Vector.tabulate(nw)(j => if j >= uAt && j < uAt + m then uBounds.fold(Double.NegativeInfinity)(_._1(j - uAt)) else Double.NegativeInfinity)
  val ubW = Vector.tabulate(nw)(j => if j >= uAt && j < uAt + m then uBounds.fold(Double.PositiveInfinity)(_._2(j - uAt)) else Double.PositiveInfinity)
  def terms(row: Array[Double], w: Array[Double]) = row.indices.map(j => math.abs(row(j) * w(j))).sum
  def dot(row: Array[Double], w: Array[Double]) = row.indices.map(j => row(j) * w(j)).sum
  def zeroCost = (Array.ofDim[Double](nw, nw), new Array[Double](nw))
  /** Adds `weight·(row·w + c)²` to the cost. */
  def square(h: Array[Array[Double]], f: Array[Double], row: Array[Double], c: Double, weight: Double): Unit =
    for j <- 0 until nw; k <- 0 until nw do h(j)(k) += 2.0 * weight * row(j) * row(k)
    for j <- 0 until nw do f(j) += 2.0 * weight * c * row(j)
  def onU(row: Int => Double) = Array.tabulate(nw)(j => if j >= uAt && j < uAt + m then row(j - uAt) else 0.0)

  /** One output level: its cost, a slack per finite side of each output bound, then what it achieved. */
  def outputLevel(goals: Vector[TargetGoal], held: Vector[Held], from: Option[Array[Double]]): Either[MpcFailure, (Vector[Held], Array[Double])] =
    val (h, f) = zeroCost
    // a slack's side: (output, upper?, bound)
    val sides = goals.flatMap {
      case OutputBound(i, lo, hi, wt) =>
        Vector(Option.when(hi.isFinite)((i, true, hi, wt)), Option.when(lo.isFinite)((i, false, lo, wt))).flatten
      case _ => Vector.empty
    }
    goals.foreach {
      case Setpoint(i, v, wt) => square(h, f, rel.f(i), rel.g(i) - v, wt)
      case _                  => ()
    }
    // F·w + g − s ≤ hi, and −(F·w + g) − s ≤ −lo
    val slacks = sides.map { (i, upper, bound, wt) =>
      if upper then (rel.f(i), bound - rel.g(i), wt) else (rel.f(i).map(-_), rel.g(i) - bound, wt)
    }
    solveLevel(rel, h, f, slacks, held, lbW, ubW, from, env).map { (w, s) =>
      val kept = goals.collect { case Setpoint(i, _, _) =>
        val v = dot(rel.f(i), w)
        val delta = hold * (terms(rel.f(i), w) + math.abs(rel.g(i)))
        Held(rel.f(i), v - delta, v + delta)
      } ++ sides.indices.map { k =>
        val (i, upper, bound, _) = sides(k)
        val delta = hold * (terms(rel.f(i), w) + math.abs(rel.g(i)) + math.abs(bound))
        if upper then Held(rel.f(i), Double.NegativeInfinity, bound - rel.g(i) + s(k) + delta)
        else Held(rel.f(i), bound - rel.g(i) - s(k) - delta, Double.PositiveInfinity)
      }
      (held ++ kept, w)
    }

  /** The resting values: `‖u − u_rest‖²_S`, then `S·u` held where it ended. */
  def restLevel(r: RestingValues, held: Vector[Held], from: Option[Array[Double]]): Either[MpcFailure, (Vector[Held], Array[Double])] =
    val (h, f) = zeroCost
    val s = r.weight
    for j <- 0 until m; k <- 0 until m do h(uAt + j)(uAt + k) += 2.0 * s(j, k)
    for j <- 0 until m do f(uAt + j) -= 2.0 * (0 until m).map(k => s(j, k) * r.target(k)).sum
    solveLevel(rel, h, f, Vector.empty, held, lbW, ubW, from, env).map { (w, _) =>
      val kept = (0 until m).flatMap { j =>
        val row = onU(k => s(j, k))
        Option.when(row.exists(_ != 0.0)) {
          val v = dot(row, w)
          val delta = hold * terms(row, w)
          Held(row, v - delta, v + delta)
        }
      }
      (held ++ kept, w)
    }

  // Each output's size, in its own units: the largest setpoint or finite bound the levels give it.
  val outputSize = Vector.tabulate(rel.p) { i =>
    spec.levels.flatten.collect {
      case Setpoint(`i`, v, _)          => math.abs(v)
      case OutputBound(`i`, lo, hi, _) => Seq(lo, hi).filter(_.isFinite).map(math.abs).maxOption.getOrElse(0.0)
    }.maxOption.getOrElse(0.0)
  }
  /** An input's scale from the model (F_0368, the owner's choice): the move of input `k` that changes some
   *  output by its own size, `min_i r_i/|G_ik|` — in the input's units, each output read in its own; `None`
   *  without a gain (an integrator), or when no output with a size depends on the input. */
  def modelScale(k: Int): Option[Double] = rel.gain.flatMap { gm =>
    (0 until rel.p).collect { case i if outputSize(i) > 0.0 && gm(i, k) != 0.0 => outputSize(i) / math.abs(gm(i, k)) }
      .filter(s => s > 0.0 && s.isFinite).minOption
  }

  /** The smallest move from `u`, each input in its own scale: its bound width, else `|u_k|`, else the scale
   *  the model gives it ([[modelScale]], F_0368; a unit scale was a claim about the input's units, and
   *  `steadyStateTarget` always passes `u = 0`), else 1.  The weights `1/scale²` are taken relative to the
   *  largest, `(s_min/s_k)² ≤ 1` (F_0368): absolute, a scale below `~10⁻¹⁵⁴` overflowed the weight to `∞`
   *  (the QP declined its `H`).  Only the weights' ratios choose the target, so that changes no answer. */
  def tieBreak(held: Vector[Held], from: Option[Array[Double]]): Either[MpcFailure, Array[Double]] =
    val (h, f) = zeroCost
    val scales = Vector.tabulate(m) { k =>
      val width = uBounds.fold(Double.PositiveInfinity)((lo, hi) => hi(k) - lo(k))
      if width.isFinite && width > 0.0 then width
      else if u(k) != 0.0 then math.abs(u(k))
      else modelScale(k).getOrElse(1.0)
    }
    val smallest = scales.min
    for k <- 0 until m do
      val ratio = smallest / scales(k)
      val c = ratio * ratio
      h(uAt + k)(uAt + k) += 2.0 * c
      f(uAt + k) -= 2.0 * c * u(k)
    solveLevel(rel, h, f, Vector.empty, held, lbW, ubW, from, env).map(_._1)

  // Each level starts from the one before it (F_0359): that point meets every held band and every bound,
  // so only the first level's semidefinite QP needs an exact LP to find a feasible start.
  type Reached = (Vector[Held], Option[Array[Double]])
  val outputs = spec.levels.filter(_.nonEmpty).foldLeft[Either[MpcFailure, Reached]](Right((Vector.empty, None))) {
    (acc, goals) => acc.flatMap((held, from) => outputLevel(goals, held, from).map((h, w) => (h, Some(w))))
  }
  for
    afterOutputs <- outputs
    afterRest    <- spec.rest.fold[Either[MpcFailure, Reached]](Right(afterOutputs))(r =>
                      restLevel(r, afterOutputs._1, afterOutputs._2).map((h, w) => (h, Some(w))))
    w            <- tieBreak(afterRest._1, afterRest._2)
  yield
    val tol = 0.5 * math.pow(10.0, -env.precision)
    val y   = Vector.tabulate(rel.p)(i => dot(rel.f(i), w) + rel.g(i))
    def size(i: Int, extra: Double) = terms(rel.f(i), w) + math.abs(rel.g(i)) + math.abs(extra)
    val unmet = spec.levels.flatten.filter {
      case Setpoint(i, v, _)          => math.abs(y(i) - v) > tol * size(i, v)
      case OutputBound(i, lo, hi, _)  =>
        (hi.isFinite && y(i) - hi > tol * size(i, hi)) || (lo.isFinite && lo - y(i) > tol * size(i, lo))
    }
    TargetLayer(Option.when(rel.hasState)(w.take(uAt).toVector), w.slice(uAt, uAt + m).toVector, y, unmet)

/** One level's QP over `z = [w; s]`: `½zᵀHz + fᵀz`, the slacks' rows `row·w − sₖ ≤ rhs` costing
 *  `weightₖ·sₖ²`, `sₖ ≥ 0`, under the steady-state equalities, the held rows and the bounds of `w`;
 *  from the previous level's `w` when there is one, each slack at the excess it leaves there.
 *  @return `(w, s)` at the certified optimum, or why the QP declined */
private def solveLevel(rel: SteadyRelation, hw: Array[Array[Double]], fw: Array[Double],
                       slacks: Vector[(Array[Double], Double, Double)], held: Vector[Held],
                       lbW: Vector[Double], ubW: Vector[Double], from: Option[Array[Double]],
                       env: Environment): Either[MpcFailure, (Array[Double], Array[Double])] =
  val nw = rel.nw
  val ns = slacks.size
  val nz = nw + ns
  def padded(row: Array[Double], slack: Int, sign: Double) =
    Array.tabulate(nz)(j => if j < nw then row(j) else if j - nw == slack then sign else 0.0)
  val h = _MatrixValue(nz, nz, Array.tabulate(nz * nz) { idx =>
    val (i, j) = (idx / nz, idx % nz)
    if i < nw && j < nw then hw(i)(j) else if i >= nw && i == j then 2.0 * slacks(i - nw)._3 else 0.0
  })
  val f = Vector.tabulate(nz)(i => if i < nw then fw(i) else 0.0)
  val ineq = Vector.newBuilder[(Array[Double], Double)]
  val eqs  = Vector.newBuilder[(Array[Double], Double)]
  slacks.indices.foreach { k => ineq += ((padded(slacks(k)._1, k, -1.0), slacks(k)._2)) }
  held.foreach { hd =>
    if hd.lo == hd.hi then eqs += ((padded(hd.row, -1, 0.0), hd.lo))
    else
      if hd.hi < Double.PositiveInfinity then ineq += ((padded(hd.row, -1, 0.0), hd.hi))
      if hd.lo > Double.NegativeInfinity then ineq += ((padded(hd.row.map(-_), -1, 0.0), -hd.lo))
  }
  rel.eq.foreach((e, rhs) => e.indices.foreach(i => eqs += ((padded(e(i), -1, 0.0), rhs(i)))))
  def block(rows: Vector[(Array[Double], Double)]) =
    Option.when(rows.nonEmpty)((_MatrixValue(rows.size, nz, rows.flatMap(_._1).toArray), rows.map(_._2)))
  val lb = lbW ++ Vector.fill(ns)(0.0)
  val ub = ubW ++ Vector.fill(ns)(Double.PositiveInfinity)
  val (ineqs, eqRows) = (block(ineq.result()), block(eqs.result()))
  val start = from.map { w =>
    val excess = slacks.map((row, rhs, _) => math.max(0.0, row.indices.map(j => row(j) * w(j)).sum - rhs))
    w ++ excess
  }
  qpPrepare(h, ineqs.map(_._1), eqRows.map(_._1), lb, ub)
    .solve(f, ineqs.map(_._2), eqRows.map(_._2), lb, ub, Set.empty, env, start)
    .left.map(MpcFailure.Qp(_))
    .map(r => (r.z.take(nw).toArray, r.z.drop(nw).toArray))

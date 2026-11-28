package it.grypho.scala.leonardo
package optimize

import core.*

import scala.annotation.tailrec

/** One constraint of a [[qpSolve]] problem, named the way a reader names it: a row of `A`, or
 *  the lower or upper bound of one variable.  Indices are zero-based; the REPL prints them from
 *  one.  The name is what makes a warm start survive a perturbed problem: it does not depend on
 *  which bounds happen to be finite this time. */
enum QpConstraint:
  /** Row `k` of the inequality block `A z ≤ b`. */
  case Row(k: Int)
  /** The lower bound `lb(i) ≤ z(i)`. */
  case Lower(i: Int)
  /** The upper bound `z(i) ≤ ub(i)`. */
  case Upper(i: Int)

/** Why [[qpSolve]] gave no answer (F_0052 Decision A: a reason at the Scala level, which the
 *  REPL appends as a note).  An MPC reacts differently to each — relax the targets, fix the
 *  model, hold the last input — which is why `Left(this)` alone would not do. */
enum QpFailure:
  /** An operand has the wrong shape or is not numeric; the detail says which. */
  case InvalidInput(detail: String)
  /** `H` has a negative eigenvalue: the problem is not convex. */
  case NotConvex
  /** `H` is positive semidefinite but singular, so the Goldfarb–Idnani method does not apply
   *  (F_0053 Decision B).  **Since F_0074 `qpSolve` no longer answers with it**: it is the signal
   *  that hands the problem to the primal active-set method instead.  Never regularised —
   *  `H + εI` would answer a different problem. */
  case NotStrictlyConvex
  /** The constraints admit no point. */
  case Infeasible
  /** The objective falls without bound: a direction of zero curvature (a null direction of a
   *  semidefinite `H`) along which the cost decreases and no constraint blocks (F_0074). */
  case Unbounded
  /** No optimum within the iteration cap. */
  case MaxIterations(limit: Int)
  /** The final point failed the KKT certificate (X-02); `residual` is the relative residual. */
  case Uncertified(residual: Double)
  /** A semidefinite `H` starts from an exact [[lpSolve]] feasible point (F_0074), and that LP
   *  declined for a reason of its own (F_0106) — typically its caps, which are far below
   *  `qp`'s: [[MaxLpVariables]], [[MaxLpConstraints]] and [[MaxLpBits]].  `Infeasible` is never
   *  carried here; it is reported as [[Infeasible]], since the LP proves it. */
  case FeasibleStart(why: LpFailure)

/** A certified optimum of [[qpSolve]].
 *
 *  @param z           the minimiser
 *  @param objective   `½ zᵀHz + fᵀz` at `z`
 *  @param iterations  active-set changes (additions and drops) the solve took
 *  @param kktResidual the relative KKT residual the certificate measured, at most the tolerance
 *  @param active      the inequalities and bounds holding with equality — pass it back as
 *                     `warm` for the next, similar problem (X-04: state in, state out)
 *  @param multipliers the Lagrange multipliers at `z` — what an SQP iteration built on this
 *                     solver reads as its multiplier estimate (F_0066)
 */
case class QpResult(z: Vector[Double], objective: Double, iterations: Int, kktResidual: Double,
                    active: Vector[QpConstraint], multipliers: QpMultipliers)

/** The Lagrange multipliers at a QP's optimum (F_0066), in the convention of
 *  `L = ½ zᵀHz + fᵀz + μᵀ(Az − b) + λᵀ(Aeq·z − beq) + νₗᵀ(lb − z) + νᵤᵀ(z − ub)`: an
 *  inequality or bound multiplier is `≥ 0` and zero when its constraint is inactive, an equality
 *  multiplier has either sign, and `Hz + f + Aᵀμ + Aeqᵀλ − νₗ + νᵤ = 0` at the optimum.
 *
 *  @param inequality one per row of `A`
 *  @param equality   one per row of `Aeq`
 *  @param lower      one per variable, for `lb`
 *  @param upper      one per variable, for `ub`
 */
case class QpMultipliers(inequality: Vector[Double], equality: Vector[Double],
                         lower: Vector[Double], upper: Vector[Double])

/** The iteration cap: additions plus drops of active constraints.  Measured (F_0053, JVM, random
 *  problems with half the variables' worth of constraints active): 35 iterations at 50 variables
 *  and 200 constraints, 170 at 200 / 800, 368 at 500 / 2000 — roughly 0.7 per variable — so 2000
 *  leaves a wide margin while bounding the worst case at the largest size to tens of seconds.
 *
 *  This and the two caps below are the **defaults** of their [[core.Limit]]s (F_0124); a caller
 *  raises one through [[core.Environment.withLimit]]. */
val MaxQpIterations: Int = Limit.MaxQpIterations.default.toInt

/** The largest number of variables accepted, checked before any work.  Measured: a cold solve
 *  takes 9.6 ms at 50 variables, 51 ms at 100, 173 ms at 200 and 4.3 s at 500, the setup growing
 *  as `m·n²` — 500 is where it stops being interactive, the `MaxSampleCount` rule in time. */
val MaxQpVariables: Int = Limit.MaxQpVariables.default.toInt

/** The largest number of constraint rows (inequalities, equalities and finite bounds), the
 *  companion of [[MaxQpVariables]]: 2000 rows at 500 variables is the 4.3 s point above. */
val MaxQpConstraints: Int = Limit.MaxQpConstraints.default.toInt

/** Minimises `½ zᵀHz + fᵀz` subject to `A z ≤ b`, `Aeq z = beq` and `lb ≤ z ≤ ub` (F_0053).
 *
 *  **Goldfarb–Idnani dual active set** (Decision B): it starts at the unconstrained minimum,
 *  needs no feasible starting point, and adds the most violated constraint each step, dropping
 *  any whose multiplier would turn negative — so it stays dual-feasible and detects
 *  infeasibility by itself (a violated constraint that no step can reach).  It requires `H`
 *  **positive definite**, the case of every MPC Hessian `ΓᵀQΓ + R` with `R ≻ 0`; a merely
 *  semidefinite `H` is never regularised but goes to the primal active-set method from an exact
 *  [[lpSolve]] feasible point (F_0074), and so inherits that LP's caps (F_0106).  Ties — equally
 *  violated constraints, equal step ratios — go to the lowest index, so a run is deterministic.
 *
 *  Each step solves the reduced system afresh rather than updating a factorisation: at the
 *  stated size an `O(n³)` recompute is affordable, and code without incremental updates has no
 *  drift to repair.
 *
 *  **A prepare followed by one solve** (F_0357): everything that does not depend on `f` or a
 *  right-hand side — `H` checked, its inverse, every constraint's normal — is fixed by a
 *  `QpPrepared`, which a caller solving one problem per cycle with new data (an MPC) keeps
 *  between solves; the two paths are one.
 *
 *  **Certified before it answers** (X-02): stationarity, primal and dual feasibility and
 *  complementarity, each relative to the problem's scale (the 2.11 units rule), against the `=`
 *  tolerance `0.5·10^-precision` — the `minimize` arrangement.  A point that fails is never
 *  returned.
 *
 *  @param h    the Hessian, `n×n`, symmetric positive definite
 *  @param f    the linear term, `n` entries
 *  @param ineq `A` (`m×n`) and `b` (`m` entries) of `A z ≤ b`, or `None`
 *  @param eq   `Aeq` and `beq` of `Aeq z = beq`, or `None`
 *  @param lb   lower bounds, `n` entries, `-∞` for none
 *  @param ub   upper bounds, `n` entries, `+∞` for none
 *  @param warm constraints believed active, typically the previous solve's [[QpResult.active]];
 *              only the iteration count depends on it, never the answer
 *  @param env  supplies the precision the certificate is held to, and the budgets (F_0124):
 *              [[core.Limit.MaxQpVariables]], `MaxQpConstraints`, `MaxQpIterations`, and the
 *              LP's caps for a semidefinite `H`'s feasible start
 *  @return the certified optimum, or why there is none
 */
def qpSolve(h: _MatrixValue, f: Vector[Double],
            ineq: Option[(_MatrixValue, Vector[Double])], eq: Option[(_MatrixValue, Vector[Double])],
            lb: Vector[Double], ub: Vector[Double],
            warm: Set[QpConstraint], env: Environment): Either[QpFailure, QpResult] =
  qpPrepare(h, ineq.map(_._1), eq.map(_._1), lb, ub).solve(f, ineq.map(_._2), eq.map(_._2), lb, ub, warm, env)

/** Fixes the parts of a [[qpSolve]] problem that do not change between solves (F_0357, AIACE
 *  L_0076): `H`, the constraint matrices, and which bounds are finite.  See [[QpPrepared]].
 *
 *  @param h   the Hessian
 *  @param a   `A` of `A z ≤ b`, or `None`
 *  @param aeq `Aeq` of `Aeq z = beq`, or `None`
 *  @param lb  lower bounds, of which only which entries are finite is kept
 *  @param ub  upper bounds, likewise
 *  @return the prepared problem; nothing is checked until [[QpPrepared.solve]]
 */
private[leonardo] def qpPrepare(h: _MatrixValue, a: Option[_MatrixValue], aeq: Option[_MatrixValue],
                                lb: Vector[Double], ub: Vector[Double]): QpPrepared =
  QpPrepared(h, a, aeq, lb.map(_ > Double.NegativeInfinity), ub.map(_ < Double.PositiveInfinity))

/** A [[qpSolve]] problem with everything fixed that does not depend on `f` or a right-hand side
 *  (F_0357, AIACE L_0076): `H` checked, its inverse, and every constraint's normal with its size and
 *  `H⁻¹·n`, each computed once, on first use.  An MPC solves one such problem per cycle with a new
 *  state, so it prepares once (`control.mpcQpPrepared`); [[qpSolve]] is a prepare followed by one
 *  solve, so the two cannot disagree.  [[solve]] runs the checks in the order and with the messages
 *  `qpSolve` always used, so a prepared problem declines exactly as the one-shot call would.
 *  Immutable, its lazy parts thread-safe, so one may be shared.
 *
 *  @param h           the Hessian
 *  @param a           `A`, or `None`
 *  @param aeq         `Aeq`, or `None`
 *  @param lowerFinite which lower bounds are finite, so are rows
 *  @param upperFinite which upper bounds are finite
 */
private[leonardo] final class QpPrepared(h: _MatrixValue, a: Option[_MatrixValue], aeq: Option[_MatrixValue],
                                         lowerFinite: Vector[Boolean], upperFinite: Vector[Boolean],
                                         jacobi: Boolean = true, smallestStart: Boolean = true):
  private val n = h.rows
  private lazy val maxH: Double = h.toVector.map(math.abs).maxOption.getOrElse(0.0)
  private lazy val hFinite: Boolean = h.isFinite
  private lazy val symmetric: Boolean =
    !(0 until n).exists(i => (0 until i).exists(j => math.abs(h(i, j) - h(j, i)) > 1e-12 * maxH))
  private lazy val aFinite: Boolean   = a.forall(_.isFinite)
  private lazy val aeqFinite: Boolean = aeq.forall(_.isFinite)

  /** The rows in the solver's order — equalities, inequalities, finite lower then upper bounds —
   *  each with a placeholder right-hand side that [[solve]] fills. */
  private lazy val templates: Vector[QpRow] =
    def unit(i: Int, s: Double) = Array.tabulate(n)(j => if j == i then s else 0.0)
    aeq.fold(Vector.empty[QpRow])(m => Vector.tabulate(m.rows)(k => QpRow(Array.tabulate(n)(j => m(k, j)), 0.0, None))) ++
      a.fold(Vector.empty[QpRow])(m =>
        Vector.tabulate(m.rows)(k => QpRow(Array.tabulate(n)(j => -m(k, j)), 0.0, Some(QpConstraint.Row(k))))) ++
      (0 until n).filter(lowerFinite).map(i => QpRow(unit(i, 1.0), 0.0, Some(QpConstraint.Lower(i)))) ++
      (0 until n).filter(upperFinite).map(i => QpRow(unit(i, -1.0), 0.0, Some(QpConstraint.Upper(i))))

  private lazy val inverse: Either[QpFailure, Array[Array[Double]]] = qpInverseHessian(h)

  /** Each variable's length `dⱼ` (F_0377, F_0378, the owner's decision: Jacobi, a flat variable's from its rows),
   *  or `None` when every length is 1.  The problem is solved in `y = D⁻¹z`, where `H` becomes `DHD`, so a
   *  variable in small units no longer reads as flat beside one in large units: the Cholesky test, the null-curvature
   *  threshold and the iterations' tolerances are relative to the largest entry of `H`, and with the variables in
   *  units `10⁻⁴`, `1`, `10⁴` they certified non-optimal points.
   *
   *  A curved variable's length is `√(H_max/Hⱼⱼ)`, `H_max` the largest diagonal entry: Jacobi up to one common
   *  factor, which leaves the largest-curvature variable's unit alone.  A flat variable (`Hⱼⱼ = 0`) takes the
   *  smallest length its rows give it beside the curved variables in them, `minᵣ maxₖ |nᵣₖ|·dₖ / |nᵣⱼ|`, and 1 when no
   *  row joins it to one (the `nlp` reading, F_0120).  When every variable is flat (an LP, `H = 0`), each takes its
   *  length from its own column of the rows, each row read relative to its own largest entry, so that the columns'
   *  largest entries agree: in mixed units an LP was certified 10 % from its optimum.  **Every length is a power of two**, so scaling and mapping
   *  back are exact, a bound coordinate stays exactly on its bound, and a problem whose diagonal spans less than
   *  about a factor 2 is solved exactly as before. */
  private lazy val lengths: Option[Array[Double]] =
    if !jacobi || n == 0 || !hFinite then None
    else
      def pow2(x: Double): Double =
        val e = math.getExponent(x)
        math.scalb(1.0, if x / math.scalb(1.0, e) > math.sqrt(2.0) then e + 1 else e)
      val diag = Array.tabulate(n)(j => h(j, j))
      val top  = diag.max
      val general = templates.filter(r => r.id.forall { case QpConstraint.Row(_) => true; case _ => false })
      if !(top > 0.0) then
        // Every variable flat (an LP): each length from its own column of the rows, each row read relative to its
        // largest entry so that a row's units do not choose the lengths, the columns' largest entries made to agree;
        // a variable in no row keeps its unit.
        val col = Array.tabulate(n)(j => general.map(r => math.abs(r.normal(j)) / normInfA(r.normal)).filter(_.isFinite).maxOption.getOrElse(0.0))
        val ref = col.max
        val d = Array.tabulate(n)(j => if col(j) > 0.0 && ref > 0.0 then pow2(ref / col(j)) else 1.0)
        Option.when(d.exists(_ != 1.0))(d)
      else
        val d = Array.tabulate(n)(j => if diag(j) > 0.0 then pow2(math.sqrt(top / diag(j))) else 0.0)
        for j <- 0 until n if d(j) == 0.0 do
          val candidates = general.flatMap { r =>
            val nj = math.abs(r.normal(j))
            val beside = (0 until n).filter(k => diag(k) > 0.0 && r.normal(k) != 0.0).map(k => math.abs(r.normal(k)) * d(k))
            Option.when(nj > 0.0 && beside.nonEmpty)(beside.max / nj)
          }
          d(j) = candidates.minOption.filter(c => c > 0.0 && c.isFinite).map(pow2).getOrElse(1.0)
        Option.when(d.exists(_ != 1.0))(d)

  /** Each constraint's row in [[templates]]' order, which every solve's rows keep. */
  private lazy val rowOf: Map[QpConstraint, Int] = templates.indices.flatMap(k => templates(k).id.map(_ -> k)).toMap

  /** The equalities' rows, always active. */
  private lazy val equalityRows: Vector[Int] = templates.indices.filter(templates(_).isEquality).toVector

  /** The problem in `y = D⁻¹z`, prepared once with its own lengths switched off. */
  private lazy val scaled: Option[(Array[Double], QpPrepared)] = lengths.map { d =>
    def cols(m: _MatrixValue) = _MatrixValue(m.rows, m.cols, Array.tabulate(m.rows * m.cols)(k => m(k / m.cols, k % m.cols) * d(k % m.cols)))
    val hy = _MatrixValue(n, n, Array.tabulate(n * n)(k => d(k / n) * h(k / n, k % n) * d(k % n)))
    (d, QpPrepared(hy, a.map(cols), aeq.map(cols), lowerFinite, upperFinite, jacobi = false, smallestStart = smallestStart))
  }

  /** `H⁻¹·nⱼ` for every row. */
  private lazy val directions: Vector[Array[Double]] =
    inverse.fold(_ => Vector.empty, hinv => templates.map(r => matTimes(hinv, r.normal)))

  /** Solves the prepared problem for one linear term and one set of right-hand sides.
   *
   *  @param f    the linear term, `n` entries
   *  @param b    the right-hand side of `A z ≤ b`, given exactly when `A` was
   *  @param beq  the right-hand side of `Aeq z = beq`, likewise
   *  @param lb   lower bounds, finite exactly where the prepared ones were
   *  @param ub   upper bounds, likewise
   *  @param warm constraints believed active
   *  @param env  the precision and the budgets, as for [[qpSolve]]
   *  @return the certified optimum, or why there is none
   */
  def solve(f: Vector[Double], b: Option[Vector[Double]], beq: Option[Vector[Double]],
            lb: Vector[Double], ub: Vector[Double],
            warm: Set[QpConstraint], env: Environment): Either[QpFailure, QpResult] =
    solve(f, b, beq, lb, ub, warm, env, None)

  /** [[solve]] from a starting point (F_0359): a semidefinite problem's primal active-set method starts
   *  from `start` when it is feasible to that method's own iteration tolerance, and from an exact LP
   *  vertex otherwise; the definite path is dual and needs none.  A caller solving a sequence of
   *  problems whose previous answer is feasible for the next (the target layer's levels) saves an LP
   *  per problem.  Only the iteration's path depends on the start, never the certificate.
   *
   *  @param start a point of `n` entries, or `None` for the LP's
   *  @return as [[solve]]
   */
  def solve(f: Vector[Double], b: Option[Vector[Double]], beq: Option[Vector[Double]],
            lb: Vector[Double], ub: Vector[Double],
            warm: Set[QpConstraint], env: Environment, start: Option[Array[Double]]): Either[QpFailure, QpResult] =
    val (maxVars, maxRows) = (env.limits.count(Limit.MaxQpVariables), env.limits.count(Limit.MaxQpConstraints))
    def bad(detail: String): Either[QpFailure, QpResult] = Left(QpFailure.InvalidInput(detail))
    def finite(xs: Iterable[Double]) = xs.forall(d => d.isFinite)
    def block(name: String, m: Option[_MatrixValue], rhs: Option[Vector[Double]], cellsFinite: => Boolean): Option[QpFailure] =
      (m, rhs) match
        case (None, None) => None
        case (Some(mm), Some(r)) =>
          if mm.cols != n || mm.rows != r.size then
            Some(QpFailure.InvalidInput(s"$name must have $n columns and as many rows as its right-hand side has entries"))
          else if !cellsFinite || !finite(r) then Some(QpFailure.InvalidInput(s"$name and its right-hand side must be finite"))
          else None
        case _ => Some(QpFailure.InvalidInput(s"$name and its right-hand side must be given together"))
    if h.rows != h.cols then bad("H must be square")
    else if n > maxVars then bad(s"at most $maxVars variables are accepted, not $n")
    else if f.size != n || lb.size != n || ub.size != n then bad(s"f, lb and ub must each have $n entries")
    else if !hFinite || !finite(f) then bad("H and f must be finite")
    else if lb.exists(_.isNaN) || ub.exists(_.isNaN) then bad("a bound is not a number")
    else if !symmetric then bad("H must be symmetric")
    else if (0 until n).exists(i => lb(i) > ub(i) || lb(i) == Double.PositiveInfinity || ub(i) == Double.NegativeInfinity) then
      Left(QpFailure.Infeasible)
    else block("A", a, b, aFinite).orElse(block("Aeq", aeq, beq, aeqFinite)) match
      case Some(why) => Left(why)
      case None =>
        if lowerFinite.size != n || upperFinite.size != n || (0 until n).exists(i =>
             (lb(i) > Double.NegativeInfinity) != lowerFinite(i) || (ub(i) < Double.PositiveInfinity) != upperFinite(i)) then
          bad("the bounds' finite entries differ from the prepared ones")
        else if templates.size > maxRows then bad(s"at most $maxRows constraints are accepted, not ${templates.size}")
        else
          val prob = QpProblem(n, h, f.toArray, filled(b, beq, lb, ub), maxH, 0.5 * math.pow(10.0, -env.precision))
          scaled.flatMap((d, inner) => inOwnLengths(d, inner, prob, f, b, beq, lb, ub, warm, env, start))
            .getOrElse(unscaled(prob, b, beq, lb, ub, warm, env, start))

  /** The prepared problem solved in each variable's own length ([[lengths]]), mapped back and **certified again in
   *  `z`** (the `nlp` arrangement, F_0120): the answer is `z = D·y`; the general rows' multipliers are unchanged,
   *  since their normals in `y` are `nᵣ·D`, and a bound's is divided by `dⱼ`, its normal in `y` being the unit
   *  vector.  `None` when the scaled data leave the finite numbers, which the unscaled path then answers.  A
   *  decline in `y` is the answer. */
  private def inOwnLengths(d: Array[Double], inner: QpPrepared, prob: QpProblem, f: Vector[Double],
                           b: Option[Vector[Double]], beq: Option[Vector[Double]], lb: Vector[Double], ub: Vector[Double],
                           warm: Set[QpConstraint], env: Environment,
                           start: Option[Array[Double]]): Option[Either[QpFailure, QpResult]] =
    val fy  = Vector.tabulate(n)(i => f(i) * d(i))
    val lby = Vector.tabulate(n)(i => lb(i) / d(i))
    val uby = Vector.tabulate(n)(i => ub(i) / d(i))
    val sameBounds = (0 until n).forall(i => (lby(i) > Double.NegativeInfinity) == lowerFinite(i) &&
                                             (uby(i) < Double.PositiveInfinity) == upperFinite(i))
    Option.when(fy.forall(_.isFinite) && sameBounds) {
      inner.solve(fy, b, beq, lby, uby, warm, env, start.map(z => Array.tabulate(z.length)(i => z(i) / d(i)))).flatMap { r =>
        val x = Array.tabulate(n)(i => r.z(i) * d(i))
        val act = equalityRows ++ r.active.flatMap(rowOf.get)
        val m = r.multipliers
        val u = act.map(k => prob.rows(k).id match
          case None                        => -m.equality(k)
          case Some(QpConstraint.Row(j))   => m.inequality(j)
          case Some(QpConstraint.Lower(i)) => m.lower(i) / d(i)
          case Some(QpConstraint.Upper(i)) => m.upper(i) / d(i)).toArray
        qpCertificate(prob, x, act, u, r.iterations, prob.certTol)
      }
    }

  /** The prepared problem solved as it is written. */
  private def unscaled(prob: QpProblem, b: Option[Vector[Double]], beq: Option[Vector[Double]],
                       lb: Vector[Double], ub: Vector[Double], warm: Set[QpConstraint], env: Environment,
                       start: Option[Array[Double]]): Either[QpFailure, QpResult] =
          // The primal active-set method (F_0074), from a feasible start: the caller's when it is feasible and not
          // far beyond the data (F_0380: a previous target 1e12 away was taken as it was), else the exact LP's
          // vertex, else the smallest feasible point when that vertex is far (F_0362).  **Past the smallest-point
          // LP's caps a far vertex declines** (F_0380): from a corner 1e12 away the method's tests no longer see a
          // step of the data's size, and a 30-variable problem was certified at a corner, objective 0 for -1.
          // Only a vertex the smallest LP could not replace is refused: where the bounds put every feasible point
          // far, the smallest point is legitimately far too.
          lazy val primal = PrimalActiveSet(prob, env.precision, env.limits)
          def primalPath: Either[QpFailure, QpResult] =
            start.filter(z => primal.feasible(z) && !primal.far(z)).orElse(smallestPoint(prob, primal, b, beq, lb, ub, env)) match
              case Some(z) => primal.solve(z.clone(), warm)
              case None    =>
                val weights = prob.dataScale.map(s => if s > 0.0 then 1.0 / s else 1.0).toVector
                val rows    = a.fold(0)(_.rows) + aeq.fold(0)(_.rows)
                val smallestFits = 2 * n <= env.limits.count(Limit.MaxLpVariables) &&
                                   rows + 2 * n <= env.limits.count(Limit.MaxLpConstraints)
                feasiblePoint(n, a.zip(b), aeq.zip(beq), lb, ub, env.limits).flatMap { z0 =>
                  if !primal.far(z0) then Right(z0)
                  else if smallestFits then
                    feasiblePoint(n, a.zip(b), aeq.zip(beq), lb, ub, env.limits, Some(weights)).orElse(Right(z0))
                  else Left(QpFailure.FeasibleStart(LpFailure.InvalidInput(
                    s"every feasible start found is far beyond the data, and the smallest feasible point needs at most " +
                    s"${env.limits.count(Limit.MaxLpVariables) / 2} variables")))
                }.flatMap(primal.solve(_, warm))
          inverse match
            // F_0379: the dual method's `Infeasible` is a numerical verdict (a dependent constraint no step reaches,
            // which a nearly singular H that passed the Cholesky test can produce); the exact LP decides it, and the
            // primal method continues from the LP's point when it finds one
            case Right(hinv) => QpSolver(prob, hinv, directions, env.precision, env.limits).solve(warm) match
              case Left(QpFailure.Infeasible) => primalPath
              case other                      => other
            case Left(QpFailure.NotStrictlyConvex) => primalPath
            case Left(other) => Left(other)

  /** The smallest feasible point as the semidefinite path's start (F_0388): `min ½Σ(zⱼ/sⱼ)²` under this problem's own
   *  rows and bounds, `sⱼ` each variable's [[QpProblem.dataScale]], a **definite** QP the dual method solves with no
   *  start, at a precision two digits finer than the caller's (to 15) so that its point is feasible to the primal
   *  method's own tolerance.  It replaced an exact LP per semidefinite solve: the zero-cost vertex, then when that was
   *  far a weighted LP over doubled variables, which declined past 20 variables (F_0380) and cost a target layer an LP
   *  per cycle.  `None` when it declines or its point is infeasible to the primal method or far, and the exact LP's
   *  path then runs unchanged, which also proves an `Infeasible`.  Its own semidefinite fallback, should `H`'s
   *  diagonal underflow, never asks for another smallest point. */
  private def smallestPoint(prob: QpProblem, primal: PrimalActiveSet, b: Option[Vector[Double]], beq: Option[Vector[Double]],
                            lb: Vector[Double], ub: Vector[Double], env: Environment): Option[Array[Double]] =
    Option.when(smallestStart && n > 0)(()).flatMap { _ =>
      val w = prob.dataScale.map(s => if s > 0.0 && s.isFinite then 1.0 / s else 1.0)
      val hs = _MatrixValue(n, n, Array.tabulate(n * n)(k => if k / n == k % n then w(k / n) * w(k / n) else 0.0))
      val fine = new Environment(math.min(15, math.max(env.precision + 2, 11)), limits = env.limits)
      Option.when(hs.isFinite && hs.toVector.exists(_ > 0.0))(()).flatMap { _ =>
        QpPrepared(hs, a, aeq, lowerFinite, upperFinite, smallestStart = false)
          .solve(Vector.fill(n)(0.0), b, beq, lb, ub, Set.empty, fine).toOption
          .map(_.z.toArray).filter(z => primal.feasible(z) && !primal.far(z))
      }
    }

  /** The rows with this solve's right-hand sides, in [[templates]]' order. */
  private def filled(b: Option[Vector[Double]], beq: Option[Vector[Double]],
                     lb: Vector[Double], ub: Vector[Double]): Vector[QpRow] =
    val rhs = new Array[Double](templates.size)
    var r = 0
    for v <- beq; k <- v.indices do { rhs(r) = v(k); r += 1 }
    for v <- b; k <- v.indices do { rhs(r) = -v(k); r += 1 }
    for i <- 0 until n if lowerFinite(i) do { rhs(r) = lb(i); r += 1 }
    for i <- 0 until n if upperFinite(i) do { rhs(r) = -ub(i); r += 1 }
    Vector.tabulate(templates.size)(k => templates(k).withRhs(rhs(k)))

/** One constraint row in the solver's form `n·z ≥ rhs`; `id` is `None` for an equality. */
private final class QpRow(val normal: Array[Double], val rhs: Double, val id: Option[QpConstraint]):
  def isEquality: Boolean = id.isEmpty
  def slack(z: Array[Double]): Double = dotA(normal, z) - rhs
  /** The same row with another right-hand side, its normal shared (F_0357). */
  def withRhs(r: Double): QpRow = new QpRow(normal, r, id)

/** `a / b`, reading `0 / 0` as 0: a quantity whose terms are all zero is exactly zero (the
 *  `Nlp` convention).  A non-zero `a` over a zero `b` is infinite, with `a`'s sign. */
private def qpRelative(a: Double, b: Double): Double =
  if a == 0.0 then 0.0 else a / b

// The three kernels below are the solver's inner loops, written as `while` loops over arrays the
// way `_MatrixValue`'s kernels are: a `foldLeft` over an index range boxes a `Double` per step,
// which measured roughly 10x slower at the larger sizes (F_0053, Done.md).  Each allocates its
// result and mutates nothing it was given.

private def dotA(a: Array[Double], b: Array[Double]): Double =
  var s = 0.0
  var i = 0
  while i < a.length do
    s += a(i) * b(i)
    i += 1
  s

/** `Σ aᵢ` from 0.0 in order; for the non-negative terms it is used on, the same as a reduce. */
private def sumA(a: Array[Double]): Double =
  var s = 0.0
  var i = 0
  while i < a.length do
    s += a(i)
    i += 1
  s

private def normInfA(a: Array[Double]): Double =
  var m = 0.0
  var i = 0
  while i < a.length do
    m = math.max(m, math.abs(a(i)))
    i += 1
  m

/** `base + sign · Σ cₐ vₐ`, the one linear combination the solver forms. */
private def combine(base: Array[Double], cs: Array[Double], vs: Vector[Array[Double]], sign: Double): Array[Double] =
  val out = base.clone()
  var a = 0
  while a < cs.length do
    val c = sign * cs(a)
    val v = vs(a)
    var k = 0
    while k < out.length do
      out(k) += c * v(k)
      k += 1
    a += 1
  out

/** An array of `n` doubles from `f`, with no boxing: `Array.tabulate` is generic in its element
 *  type and boxes each one (F_0357, AIACE's allocation profile). */
private inline def tabD(n: Int)(inline f: Int => Double): Array[Double] =
  val out = new Array[Double](n)
  var i = 0
  while i < n do
    out(i) = f(i)
    i += 1
  out

/** A `rows×cols` array of arrays from `f`, with no boxing. */
private inline def tabD2(rows: Int, cols: Int)(inline f: (Int, Int) => Double): Array[Array[Double]] =
  val out = new Array[Array[Double]](rows)
  var i = 0
  while i < rows do
    val row = new Array[Double](cols)
    var j = 0
    while j < cols do
      row(j) = f(i, j)
      j += 1
    out(i) = row
    i += 1
  out

/** `m·v` for a matrix held as its rows. */
private def matTimes(m: Array[Array[Double]], v: Array[Double]): Array[Double] = tabD(m.length)(i => dotA(m(i), v))

/** The array with `x` appended, and with the entry at `k` removed (`k` clamped at 0, the way
 *  `Vector.patch` reads it): the active set's multipliers change one entry at a time. */
private def appended(a: Array[Double], x: Double): Array[Double] =
  val out = java.util.Arrays.copyOf(a, a.length + 1)
  out(a.length) = x
  out

private def removed(a: Array[Double], k: Int): Array[Double] =
  val i = math.max(k, 0)
  if i >= a.length then a.clone()
  else
    val out = new Array[Double](a.length - 1)
    System.arraycopy(a, 0, out, 0, i)
    System.arraycopy(a, i + 1, out, i, a.length - i - 1)
    out

/** A validated problem: `H` checked, every constraint rewritten as a [[QpRow]]; `maxH` is
 *  `max|Hᵢⱼ|`, which a prepared problem computes once. */
private final case class QpProblem(n: Int, h: _MatrixValue, f: Array[Double], rows: Vector[QpRow], maxH: Double,
                                   certTol: Double = 0.5e-5):

  /** Each variable's size, read from its own data (F_0102, F_0362): the largest `|rhs|/‖n‖∞` over the general
   *  rows and equalities it appears in, and `|fⱼ|/maxₖ|Hⱼₖ|`.  Both are in the variable's units, so a scale built
   *  from them moves with it.  **Per variable, and with the bounds left out** (F_0362): one problem-wide scale
   *  let a far bound or one large `f` entry inflate every row's test, and `qpSolve` certified points violating
   *  `z₂ ≥ 1` beside `z₁ ≥ −10¹²`, or an MPC's `y ≤ 1` with its inputs bounded by `±10¹²`.  A bound only says how
   *  large a variable may be, not how large it is: with each move's own bound in its scale the MPC case still
   *  failed.  It replaced a `1 +` floor in every relative test (F_0102), a claim about the units. */
  lazy val ownScale: Array[Double] = Array.tabulate(n) { j =>
    var s = 0.0
    for r <- rows if r.normal(j) != 0.0 && r.id.forall { case QpConstraint.Row(_) => true; case _ => false } do
      s = math.max(s, math.abs(r.rhs) / normInfA(r.normal))
    var hj = 0.0
    var k = 0
    while k < n do
      hj = math.max(hj, math.abs(h(j, k)))
      k += 1
    if hj > 0.0 then math.max(s, math.abs(f(j)) / hj) else s
  }

  /** Each variable's size as the starts and the step test read it (F_0377): its own [[floorSize]], and [[ownScale]]
   *  only for a variable with no cost or curvature of its own (an LP's), whose rows are all that says how large it
   *  is.  With the rows read for every variable, rows `≤ 10¹²` made a corner `10¹²` away "near" the data, the
   *  zero-cost LP vertex there started the primal method, and a semidefinite problem was certified at objective `0`
   *  for `−1`, its gradient's terms `2·10¹³` cancelling. */
  lazy val dataScale: Array[Double] = Array.tabulate(n)(j => if floorSize(j) > 0.0 then floorSize(j) else ownScale(j))

  /** The largest of the variables' [[dataScale]]s: the data's scale, which the primal method's step test reads. */
  lazy val ownScaleMax: Double = dataScale.maxOption.getOrElse(0.0)

  /** The rounding of a quantity whose terms add up to `scale`: `16·n·ε` of it.  What a multiplier must exceed
   *  to have a sign at all (F_0362). */
  def roundingOf(scale: Double): Double = 16.0 * n * math.ulp(1.0) * scale

  /** Each variable's size as the certificate's floors read it (F_0377): `|fⱼ|/maxₖ|Hⱼₖ|`, from its own cost and
   *  curvature, and `0` for a flat variable.  **A row's right-hand side is not a variable's size**: `|rhs|/‖n‖∞` is
   *  how large a variable would have to be to reach the row alone, and an MPC's late output rows, whose normals are
   *  `10⁻⁹` beside a right-hand side near 2, gave every variable a size of `5·10⁸`, under which a violation of `0.1`
   *  read as `10⁻¹⁰`.  The row's right-hand side still enters that row's own scale ([[rowScale]]) and the rounding
   *  a variable carries ([[roundingFloors]]); [[ownScale]], which keeps the rows, remains the data's scale the
   *  starts and the step test read. */
  lazy val floorSize: Array[Double] = Array.tabulate(n) { j =>
    var hj = 0.0
    var k = 0
    while k < n do
      hj = math.max(hj, math.abs(h(j, k)))
      k += 1
    if hj > 0.0 then math.abs(f(j)) / hj else 0.0
  }

  /** Each variable's rounding floor (F_0377): the rounding of the largest [[floorSize]] among the variables it is
   *  **coupled** to, over the certificate's tolerance, so that a residual at that rounding (a component whose optimum is
   *  0, F_0358) reads as at most `certTol`.  The rows' rounding enters at the point, through the rows that bind
   *  there ([[floorsAt]]): read from the data, an inactive row `≤ 10¹²` gave variables of size `0.1` a floor of
   *  `7·10³`.  Two variables are coupled when an entry of `H` or a general row or
   *  equality joins them, transitively: a computed value carries the rounding of the equations it is solved in, and
   *  near an SQP iterate's convergence a variable whose own data are rounding read `z₃ ≥ 0` at `5.6·10⁻¹⁷` as `0.125`
   *  without the floor its neighbours give it (F_0362).  **One problem-wide floor** (F_0362 until F_0377) gave a
   *  variable the rounding of an unrelated one: with `f₁ = −10¹⁰` the uncoupled `z₂` had a floor of 14 and its bound
   *  `z₂ ≥ 10⁻⁹` could not be seen.  The bounds couple nothing. */
  lazy val roundingFloors: Array[Double] =
    val parent = Array.tabulate(n)(identity)
    def root(i: Int): Int =
      var r = i
      while parent(r) != r do r = parent(r)
      r
    def join(i: Int, j: Int): Unit =
      val (a, b) = (root(i), root(j))
      if a != b then parent(math.max(a, b)) = math.min(a, b)
    for i <- 0 until n; j <- 0 until i if h(i, j) != 0.0 || h(j, i) != 0.0 do join(i, j)
    for r <- rows if r.id.forall { case QpConstraint.Row(_) => true; case _ => false } do
      var first = -1
      var k = 0
      while k < n do
        if r.normal(k) != 0.0 then
          if first < 0 then first = k else join(first, k)
        k += 1
    val top = new Array[Double](n)
    for j <- 0 until n do top(root(j)) = math.max(top(root(j)), floorSize(j))
    Array.tabulate(n)(j => roundingOf(top(root(j))) / certTol)

  /** Each active row's pull on the gradient, `|uₐ|·‖nₐ‖∞`: what the certificate weighs a multiplier by. */
  def multiplierWeights(act: Vector[Int], u: Array[Double]): Array[Double] =
    tabD(act.size)(a => math.abs(u(a)) * normInfA(rows(act(a)).normal))

  /** The active inequalities whose multiplier has the wrong sign as the certificate reads it: negative by more than
   *  the rounding of the gradient's terms (F_0362).  The primal method drops these at its last step
   *  ([[PrimalActiveSet]]), since its own drop test reads `iterTol` and would otherwise leave one for the
   *  certificate to refuse.
   *  @return the positions in `act`, lowest row first */
  def wrongSigns(x: Array[Double], act: Vector[Int], u: Array[Double]): Vector[Int] =
    val weight = multiplierWeights(act, u)
    val rounding = roundingOf(gradientScale(x) + sumA(weight))
    act.indices.filter(a => !rows(act(a)).isEquality && u(a) < 0.0 && weight(a) > rounding).sortBy(act(_)).toVector

  /** The primal method's iteration tolerance, which [[near]] reads; `PrimalActiveSet` holds the same formula. */
  private def iterTol: Double = math.max(1e-14, math.min(1e-10, certTol / 10))

  /** Whether a size, in a variable's units, lies where the step test can still see a step of the data's own size
   *  (F_0362): a tenth of `(certTol/iterTol)·max sⱼ`.  What `far` refuses as a start, and where a row's rounding at
   *  the point is accounted ([[floorsAt]]). */
  def near(size: Double): Boolean = size <= 0.1 * (certTol / iterTol) * math.max(ownScaleMax, Double.MinPositiveValue)

  /** Every variable's floor at `z`: its own size ([[floorSize]]), its group's rounding ([[roundingFloors]]), and the
   *  rounding of each general row it is in that **binds at this point** (its slack within `certTol` of its terms),
   *  `16·n·ε·(|rhs| + Σₖ|nₖzₖ|)/(|nⱼ|·certTol)`: the primal method moves in the binding rows' null space, so every
   *  variable of such a row carries its rounding, divided by its coefficient (a slack at `5.7·10⁻¹⁴` beside variables
   *  near `95`; an input of size `10⁻²⁰⁰` in a row held at `2`), and a row that does not bind moves nothing.  **Only
   *  while the row's terms are [[near]] the data**: at a point beyond what the step test can see the rounding cannot be
   *  accounted, and an `x` sharing an equality with two free variables at `10¹⁶` would have had a floor of `6·10⁷`,
   *  its bound's violation of `1.25` passing (F_0361's case, through the rows).  The data's scale alone
   *  undercounted it where the point is larger than the data: a target layer's slack at `5.7·10⁻¹⁴`, its row's terms
   *  at `186` from variables near `95`, was declined `Uncertified(7·10⁻⁶)` once the rows left the variables' sizes
   *  (F_0377).  The point is near the data, since a far start is never used (F_0380).  Computed once per point:
   *  the rows and the certificate read it for every row, and no method writes into a point it has made. */
  def floorsAt(z: Array[Double]): Array[Double] = lastFloors match
    case Some((at, fl)) if at eq z => fl
    case _ =>
      val fl = Array.tabulate(n)(j => math.max(floorSize(j), roundingFloors(j)))
      for r <- rows if r.id.forall { case QpConstraint.Row(_) => true; case _ => false } do
        var terms = math.abs(r.rhs)
        var k = 0
        while k < n do
          terms += math.abs(r.normal(k) * z(k))
          k += 1
        if math.abs(r.slack(z)) <= certTol * terms && near(terms / normInfA(r.normal)) then
          val rounding = roundingOf(terms) / certTol
          k = 0
          while k < n do
            if r.normal(k) != 0.0 then fl(k) = math.max(fl(k), rounding / math.abs(r.normal(k)))
            k += 1
      lastFloors = Some((z, fl))
      fl

  /** The last point [[floorsAt]] was read at, with its floors. */
  private var lastFloors: Option[(Array[Double], Array[Double])] = None

  /** The size a slack of `r` is measured against at `z`, by the iterations and the certificate alike: the
   *  constraint's terms, `|rhs| + Σⱼ|nⱼ|·max(|zⱼ|, floorⱼ)` — scale-free, with no `1 +` floor (F_0102).  **Each
   *  term at its own variable's size** (F_0361): at `‖z‖∞` a large value of a variable the row does not involve
   *  scaled the test of the others, and a point with two free variables at `10¹⁶` and a bound on a third
   *  violated by `0.34` was certified at `5·10⁻¹⁷`.  **One function for both** (F_0362): the iterations held
   *  slacks to a looser scale until then, which a far bound inflated too. */
  def rowScale(r: QpRow, z: Array[Double]): Double =
    val fl = floorsAt(z)
    var s = math.abs(r.rhs)
    var j = 0
    while j < n do
      if r.normal(j) != 0.0 then s += math.abs(r.normal(j)) * math.max(math.abs(z(j)), fl(j))
      j += 1
    s

  /** The size of the gradient `Hz + f` at `z`, from its terms: `‖f‖∞ + maxᵢ Σⱼ|Hᵢⱼ|·max(|zⱼ|, floorⱼ)`.  The
   *  terms rather than `‖Hz‖` itself, which cancels to nothing where `z` lies near `H`'s null space while its
   *  rounding does not.
   *
   *  **One scale for every component, with a floor** (F_0358): a component whose optimum is 0 is reached only
   *  to the iteration's tolerance, and against its own `|Hᵢⱼ zⱼ|` the gradient it leaves read as 100 % of
   *  its size, so `min u₁²` over a slab of `u₁ + u₂` was declined `Uncertified(1)`; reading each component
   *  against its own terms was measured again by F_0361 and declined 133 of 300 target-layer problems, a
   *  component with no terms of its own carrying only the multipliers' rounding.  **Each `H` term at its own
   *  variable's size** (F_0361), floored per variable ([[floorOf]], F_0362), so neither a large variable `H`
   *  does not touch nor a far bound raises the test of the others. */
  def gradientScale(z: Array[Double]): Double =
    val fl = floorsAt(z)
    var m = 0.0
    var i = 0
    while i < n do
      var s = 0.0
      var j = 0
      while j < n do
        s += math.abs(h(i, j)) * math.max(math.abs(z(j)), fl(j))
        j += 1
      m = math.max(m, s)
      i += 1
    normInfA(f) + m

  /** The gradient's size as the ITERATIONS read it, `‖f‖∞ + ‖z‖∞·maxᵢ Σⱼ|Hᵢⱼ|` (F_0358, F_0359): the ray
   *  test and the multiplier-drop test, never the certificate, which reads [[gradientScale]] (F_0361). */
  def iterationGradientScale(z: Array[Double]): Double =
    val zMax = normInfA(z)
    var m = 0.0
    var i = 0
    while i < n do
      var s = 0.0
      var j = 0
      while j < n do
        s += math.abs(h(i, j)) * zMax
        j += 1
      m = math.max(m, s)
      i += 1
    normInfA(f) + m


/** `H⁻¹`, through the Cholesky factor; `H` not positive definite is the failure. */
private def qpInverseHessian(h: _MatrixValue): Either[QpFailure, Array[Array[Double]]] =
    val n = h.rows
    h.cholesky match
      case Some(l) =>
        // L⁻¹ by forward substitution on the identity (lower triangular, so k runs from col),
        // then H⁻¹ = L⁻ᵀ L⁻¹ with each row of L⁻¹ read as a column of L⁻ᵀ.
        val lr   = Array.tabulate(n)(i => Array.tabulate(n)(k => l(i, k)))
        val linv = Array.ofDim[Double](n, n)
        for col <- 0 until n; i <- col until n do
          var acc = if i == col then 1.0 else 0.0
          var k = col
          while k < i do
            acc -= lr(i)(k) * linv(k)(col)
            k += 1
          linv(i)(col) = acc / lr(i)(i)
        val cols = Array.tabulate(n)(c => Array.tabulate(n)(k => linv(k)(c)))
        Right(Array.tabulate(n, n)((i, j) => dotA(cols(i), cols(j))))
      case None =>
        val negative = h.eigenDecompose.exists { evs =>
          val reals = evs.collect { case _Number(d) => d }
          val top   = reals.map(math.abs).maxOption.getOrElse(0.0)
          reals.exists(_ < -1e-12 * math.max(top, Double.MinPositiveValue))
        }
        Left(if negative then QpFailure.NotConvex else QpFailure.NotStrictlyConvex)

/** The Goldfarb–Idnani iteration over one validated problem; see [[qpSolve]].  `g` holds
 *  `H⁻¹·nⱼ` for every row, computed once by the [[QpPrepared]] problem. */
private final class QpSolver(prob: QpProblem, hinv: Array[Array[Double]], g: Vector[Array[Double]],
                             precision: Int, limits: Limits):
  import prob.{n, rows}

  private val maxIterations = limits.count(Limit.MaxQpIterations)

  private val certTol = 0.5 * math.pow(10.0, -precision)
  /** Decisions inside the iteration are held tighter than the certificate, so a point the
   *  iteration accepts is one the certificate can pass. */
  private val iterTol = math.max(1e-14, math.min(1e-10, certTol / 10))
  /** A constraint whose normal lies in the span of the active ones, relative to its own size. */
  private val depTol  = 1e-12

  /** The unconstrained minimiser `−H⁻¹f`. */
  private val x0: Array[Double] =
    val hf = matTimes(hinv, prob.f)
    tabD(hf.length)(i => -hf(i))

  /** State between steps: the iterate, the active rows and their multipliers (aligned). */
  private final case class State(x: Array[Double], act: Vector[Int], u: Array[Double], iters: Int)

  /** The active rows' Gram matrix `nₐ·H⁻¹nᵦ`. */
  private def gram(act: Vector[Int]): Array[Array[Double]] =
    tabD2(act.size, act.size)((a, b) => dotA(rows(act(a)).normal, g(act(b))))

  /** `(r, z, curvature)` for adding row `p` to `act`: the dual and primal directions. */
  private def direction(act: Vector[Int], p: Int): Option[(Array[Double], Array[Double], Double)] =
    val np = rows(p).normal
    val r  = if act.isEmpty then Some(Array.emptyDoubleArray)
             else spdSolve(gram(act), tabD(act.size)(a => dotA(rows(act(a)).normal, g(p))))
    r.map { rr =>
      val z = combine(g(p), rr, act.map(g), -1.0)
      (rr, z, dotA(np, z))
    }

  private def dependent(p: Int, curvature: Double): Boolean =
    curvature <= depTol * dotA(rows(p).normal, g(p))

  /** The minimiser with `act` held as equalities: `x = x₀ + Σ uᵢ gᵢ`, `u = M⁻¹ (b − Nᵀx₀)`. */
  private def onFace(act: Vector[Int]): Option[(Array[Double], Array[Double])] =
    if act.isEmpty then Some((x0, Array.emptyDoubleArray))
    else
      spdSolve(gram(act), tabD(act.size)(a => rows(act(a)).rhs - dotA(rows(act(a)).normal, x0)))
        .map(u => (combine(x0, u, act.map(g), 1.0), u))

  /** The starting point: equalities, then the warm rows, each kept only if independent of
   *  those before it; then inequality multipliers that come out negative are dropped, most
   *  negative first, so the start is dual-feasible as the method requires. */
  private def start(warm: Set[QpConstraint]): Either[QpFailure, State] =
    val eqs   = rows.indices.filter(rows(_).isEquality).toList
    val warms = rows.indices.filter(i => rows(i).id.exists(warm.contains)).toList
    val (act, skipped) = (eqs ++ warms).foldLeft((Vector.empty[Int], List.empty[Int])) { case ((acc, sk), p) =>
      direction(acc, p) match
        case Some((_, _, c)) if !dependent(p, c) => (acc :+ p, sk)
        case _                                   => (acc, p :: sk)
    }
    @tailrec def dropNegative(act: Vector[Int]): Option[(Array[Double], Vector[Int], Array[Double])] =
      onFace(act) match
        case None => None
        case Some((x, u)) =>
          val negatives = act.indices.filter(a => !rows(act(a)).isEquality && u(a) < 0)
          if negatives.isEmpty then Some((x, act, u))
          else dropNegative(act.patch(negatives.minBy(a => (u(a), act(a))), Nil, 1))
    dropNegative(act) match
      case None => Left(QpFailure.Uncertified(Double.PositiveInfinity))
      case Some((x, act1, u)) =>
        // A dependent equality was skipped above; it must still hold, or there is no point at all.
        val broken = skipped.exists(p => rows(p).isEquality &&
                                         math.abs(rows(p).slack(x)) > certTol * prob.rowScale(rows(p), x))
        if broken then Left(QpFailure.Infeasible) else Right(State(x, act1, u, 0))

  def solve(warm: Set[QpConstraint]): Either[QpFailure, QpResult] = start(warm).flatMap(outer)

  /** Adds the most violated inactive inequality, or certifies when none is violated. */
  @tailrec private def outer(s: State): Either[QpFailure, QpResult] =
    if s.iters > maxIterations then Left(QpFailure.MaxIterations(maxIterations))
    else
      // The most violated inactive inequality, the lowest index on a tie: the order of the
      // tuples `(relative slack, row)` this was written as, in `Double`'s total order.
      var best    = -1
      var bestRel = 0.0
      var j = 0
      while j < rows.size do
        if !rows(j).isEquality && !s.act.contains(j) then
          val rel = qpRelative(rows(j).slack(s.x), prob.rowScale(rows(j), s.x))
          if rel < -iterTol && (best < 0 || java.lang.Double.compare(rel, bestRel) < 0) then
            best = j
            bestRel = rel
        j += 1
      if best < 0 then certify(s)
      else step(s, best, 0.0) match
        case Right(next) => outer(next)
        case Left(why)   => Left(why)

  /** One constraint `p` is brought into the active set by full and partial steps. */
  @tailrec private def step(s: State, p: Int, up: Double): Either[QpFailure, State] =
    if s.iters > maxIterations then Left(QpFailure.MaxIterations(maxIterations))
    else direction(s.act, p) match
      case None => Left(QpFailure.Uncertified(Double.PositiveInfinity))
      case Some((r, z, curvature)) =>
        // The blocking active inequality: the least ratio u/r, then the lowest row, as the
        // tuples `(ratio, row, position)` this was written as ordered them.
        var k  = -1
        var t1 = Double.PositiveInfinity
        var a  = 0
        while a < s.act.size do
          if !rows(s.act(a)).isEquality && r(a) > 0 then
            val ta = s.u(a) / r(a)
            val c  = if k < 0 then -1 else java.lang.Double.compare(ta, t1)
            if c < 0 || (c == 0 && s.act(a) < s.act(k)) then
              k = a
              t1 = ta
          a += 1
        val dep = dependent(p, curvature)
        val t2  = if dep then Double.PositiveInfinity else -rows(p).slack(s.x) / curvature
        val t   = math.min(t1, t2)
        if t.isInfinite then Left(QpFailure.Infeasible)
        else
          val u1  = tabD(s.u.length)(a => s.u(a) - t * r(a))
          val x1  = if dep then s.x else combine(s.x, Array(t), Vector(z), 1.0)
          if !dep && t2 <= t1 then Right(State(x1, s.act :+ p, appended(u1, up + t), s.iters + 1))
          else step(State(x1, s.act.patch(k, Nil, 1), removed(u1, k), s.iters + 1), p, up + t)

  /** The KKT certificate (X-02); the answer only when every part passes. */
  private def certify(s: State): Either[QpFailure, QpResult] =
    qpCertificate(prob, s.x, s.act, s.u, s.iters, certTol)


/** The KKT certificate both QP methods answer through (X-02): stationarity, primal and dual
 *  feasibility and complementarity, each relative to the problem's own magnitudes (the 2.11
 *  units rule, in `Nlp.kktResidual`'s shape), against `certTol`; the answer only when every
 *  part passes.  `act` are row indices and `u` their multipliers in the rows' `n·z ≥ rhs`
 *  convention, stationarity being `∇ = Σ uₐ nₐ`.
 *
 *  **No part carries a `1 +` floor** (F_0102): each did until 2026-10-12, a claim about the
 *  problem's units under which `min ½x² − 1e-11·x` on `x ≥ 2e-11` was certified at the
 *  unconstrained `1e-11`.  Stationarity and the multipliers are measured against the gradient's
 *  terms and the active normals' pull; a slack against its row's terms at the problem's scale
 *  ([[QpProblem.rowScale]]); a quantity whose terms are all zero reads as zero. */
private def qpCertificate(prob: QpProblem, x: Array[Double], act: Vector[Int], u: Array[Double],
                          iters: Int, certTol: Double): Either[QpFailure, QpResult] =
  import prob.{n, rows}
  // Each sum runs from 0.0 in index order, the folds this was written as, on unboxed arrays.
  val hx = tabD(n) { i =>
    var acc = 0.0
    var j = 0
    while j < n do
      acc = acc + prob.h(i, j) * x(j)
      j += 1
    acc
  }
  val grad = tabD(n)(i => hx(i) + prob.f(i))
  val pull = tabD(n) { i =>
    var acc = 0.0
    var a = 0
    while a < act.size do
      acc = acc + u(a) * rows(act(a)).normal(i)
      a += 1
    acc
  }
  val weight = prob.multiplierWeights(act, u)
  val gScale = prob.gradientScale(x) + sumA(weight)
  val stationarity = qpRelative(normInfA(tabD(n)(i => grad(i) - pull(i))), gScale)
  // The largest relative violation, in `Double`'s total order (a NaN wins), 0 for no rows.
  var primal = 0.0
  var k = 0
  while k < rows.size do
    val r  = rows(k)
    val sl = r.slack(x)
    val v  = qpRelative(if r.isEquality then math.abs(sl) else math.max(0.0, -sl), prob.rowScale(r, x))
    if k == 0 || java.lang.Double.compare(v, primal) > 0 then primal = v
    k += 1
  val inequalities = act.indices.filterNot(a => rows(act(a)).isEquality)
  // F_0362: a multiplier's size against the rounding of the gradient's terms, scaled so that the rounding
  // itself reads as certTol
  def multiplierSize(a: Int): Double =
    qpRelative(weight(a), prob.roundingOf(gScale)) * certTol
  val dual = inequalities
    .map(a => if u(a) < 0.0 then multiplierSize(a) else 0.0).maxOption.getOrElse(0.0)
  // Small when either the multiplier or the slack is negligible on its own scale.
  val complementarity = inequalities
    .map(a => math.min(multiplierSize(a),
                       qpRelative(math.abs(rows(act(a)).slack(x)), prob.rowScale(rows(act(a)), x))))
    .maxOption.getOrElse(0.0)
  val residual = Seq(stationarity, primal, dual, complementarity).max
  if residual.isNaN || residual > certTol then Left(QpFailure.Uncertified(residual))
  else
    val objective = 0.5 * dotA(x, hx) + dotA(prob.f, x)
    Right(QpResult(x.toVector, objective, iters, residual, act.flatMap(rows(_).id), qpMultipliers(prob, act, u)))

/** The active multipliers in [[QpMultipliers]]' convention. The rows are `n·z ≥ rhs` with
 *  stationarity `∇ = Σ uₐ nₐ`: an inequality's normal is `−aₖ`, a lower bound's `eᵢ` and an
 *  upper bound's `−eᵢ`, so their `u` is the multiplier as it stands; an equality is `a·z = b`
 *  with normal `a`, so its multiplier is `−u`. The equality rows come first, in order. */
private def qpMultipliers(prob: QpProblem, act: Vector[Int], u: Array[Double]): QpMultipliers =
  import prob.{n, rows}
  val eqCount   = rows.count(_.isEquality)
  val ineqCount = rows.count(_.id.exists { case QpConstraint.Row(_) => true; case _ => false })
  val ineq  = Array.fill(ineqCount)(0.0)
  val eqs   = Array.fill(eqCount)(0.0)
  val lower = Array.fill(n)(0.0)
  val upper = Array.fill(n)(0.0)
  act.indices.foreach { a =>
    val r = act(a)
    rows(r).id match
      case None                        => eqs(r) = -u(a)
      case Some(QpConstraint.Row(k))   => ineq(k) = u(a)
      case Some(QpConstraint.Lower(i)) => lower(i) = u(a)
      case Some(QpConstraint.Upper(i)) => upper(i) = u(a)
  }
  QpMultipliers(ineq.toVector, eqs.toVector, lower.toVector, upper.toVector)

/** Solves `M u = rhs` for a small symmetric positive-definite `M` by Cholesky; `None` when a
 *  pivot is not positive, which here means the active normals became numerically dependent. */
private def spdSolve(m: Array[Array[Double]], rhs: Array[Double]): Option[Array[Double]] =
  val q = m.length
  val l = Array.ofDim[Double](q, q)
  /** `Σ_{k<upto} a(k)·b(k)` over the leading entries of two rows. */
  def head(a: Array[Double], b: Array[Double], upto: Int): Double =
    var s = 0.0
    var k = 0
    while k < upto do
      s += a(k) * b(k)
      k += 1
    s
  var ok = true
  var j  = 0
  while ok && j < q do
    val d = m(j)(j) - head(l(j), l(j), j)
    if d <= 0 || d.isNaN then ok = false
    else
      l(j)(j) = math.sqrt(d)
      var i = j + 1
      while i < q do
        l(i)(j) = (m(i)(j) - head(l(i), l(j), j)) / l(j)(j)
        i += 1
    j += 1
  Option.when(ok) {
    val y = new Array[Double](q)
    var i = 0
    while i < q do
      y(i) = (rhs(i) - head(l(i), y, i)) / l(i)(i)
      i += 1
    val u = new Array[Double](q)
    i = q - 1
    while i >= 0 do
      var s = 0.0
      var k = i + 1
      while k < q do
        s += l(k)(i) * u(k)
        k += 1
      u(i) = (y(i) - s) / l(i)(i)
      i -= 1
    u
  }

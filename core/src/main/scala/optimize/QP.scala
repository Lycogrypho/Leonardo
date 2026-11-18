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
                                         lowerFinite: Vector[Boolean], upperFinite: Vector[Boolean]):
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
          val prob = QpProblem(n, h, f.toArray, filled(b, beq, lb, ub), maxH)
          inverse match
            case Right(hinv) => QpSolver(prob, hinv, directions, env.precision, env.limits).solve(warm)
            // F_0074: a semidefinite H goes to the primal active-set method, from an exact LP vertex.
            case Left(QpFailure.NotStrictlyConvex) =>
              feasiblePoint(n, a.zip(b), aeq.zip(beq), lb, ub, env.limits)
                .flatMap(PrimalActiveSet(prob, env.precision, env.limits).solve(_, warm))
            case Left(other) => Left(other)

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

/** A point satisfying the constraints, from [[lpSolve]] with zero cost (F_0074): exact, so an
 *  infeasible set is proved so.  Any other decline is the LP's, and is reported as
 *  [[QpFailure.FeasibleStart]] carrying it (F_0106): the LP checks its own dimension caps before
 *  any work, so they are not restated here, where a second copy could drift from them. */
private def feasiblePoint(n: Int, ineq: Option[(_MatrixValue, Vector[Double])], eq: Option[(_MatrixValue, Vector[Double])],
                          lb: Vector[Double], ub: Vector[Double], limits: Limits): Either[QpFailure, Array[Double]] =
  def group(g: Option[(_MatrixValue, Vector[Double])]) =
    g.map((a, b) => (Vector.tabulate(a.rows)(i => Vector.tabulate(a.cols)(j => _Number(a(i, j)): _Value)), b.map(_Number(_): _Value)))
  lpSolve(Vector.fill(n)(_Number(0.0)), group(ineq), group(eq), lb.map(_Number(_)), ub.map(_Number(_)), limits) match
    case Right(r) => Right(r.x.map { case _Number(d) => d; case _ => Double.NaN }.toArray)
    case Left(LpFailure.Infeasible) => Left(QpFailure.Infeasible)
    case Left(why) => Left(QpFailure.FeasibleStart(why))


/** One constraint row in the solver's form `n·z ≥ rhs`; `id` is `None` for an equality. */
private final class QpRow(val normal: Array[Double], val rhs: Double, val id: Option[QpConstraint], val absSum: Double):
  def isEquality: Boolean = id.isEmpty
  def slack(z: Array[Double]): Double = dotA(normal, z) - rhs
  /** The same row with another right-hand side, its normal and size shared (F_0357). */
  def withRhs(r: Double): QpRow = new QpRow(normal, r, id, absSum)

private object QpRow:
  /** A row with its normal's size `Σ|nᵢ|`, the units a slack is measured in, computed once. */
  def apply(normal: Array[Double], rhs: Double, id: Option[QpConstraint]): QpRow =
    var s = 0.0
    var i = 0
    while i < normal.length do
      s += math.abs(normal(i))
      i += 1
    new QpRow(normal, rhs, id, s)

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
private final case class QpProblem(n: Int, h: _MatrixValue, f: Array[Double], rows: Vector[QpRow], maxH: Double):

  /** The size of the problem's variables, read from its own data (F_0102): each constraint's
   *  `|rhs|/‖n‖∞` and, where `H` has an entry, `‖f‖∞/max|Hᵢⱼ|`.  Both are in the units of `z`,
   *  so a scale built from them moves with the variables.  It replaced a `1 +` floor in every
   *  relative test, which was a claim about those units: below unit scale it let a point
   *  violating a bound by half be certified. */
  lazy val zScale: Double =
    val fromH   = if maxH > 0.0 then normInfA(f) / maxH else 0.0
    val fromRows = rows.map(r => normInfA(r.normal)).zip(rows).collect { case (s, r) if s > 0.0 => math.abs(r.rhs) / s }
    (fromRows :+ fromH).max

  /** The size a slack of `r` is measured against at `z`: the constraint's terms, `|rhs| +
   *  Σ|nᵢ|·max(‖z‖∞, zScale)` — scale-free, with no floor (F_0102). */
  def rowScale(r: QpRow, z: Array[Double]): Double =
    math.abs(r.rhs) + r.absSum * math.max(normInfA(z), zScale)

  /** The size of the gradient `Hz + f` at `z`, from its terms: `‖f‖∞ + maxᵢ Σⱼ|Hᵢⱼ zⱼ|`.  The
   *  terms rather than `‖Hz‖` itself, which cancels to nothing where `z` lies near `H`'s null
   *  space while its rounding does not. */
  def gradientScale(z: Array[Double]): Double =
    var m = 0.0
    var i = 0
    while i < n do
      var s = 0.0
      var j = 0
      while j < n do
        s += math.abs(h(i, j) * z(j))
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
  val weight = tabD(act.size)(a => math.abs(u(a)) * normInfA(rows(act(a)).normal))
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
  val dual = inequalities
    .map(a => if u(a) < 0.0 then qpRelative(weight(a), gScale) else 0.0).maxOption.getOrElse(0.0)
  // Small when either the multiplier or the slack is negligible on its own scale.
  val complementarity = inequalities
    .map(a => math.min(qpRelative(weight(a), gScale),
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

/** The primal active-set method (Nocedal–Wright §16.3) for a positive **semi**definite `H`
 *  (F_0074), from a feasible point.
 *
 *  Each iteration minimises the quadratic on the working set's face by the **null-space method**:
 *  with `Z` an orthonormal basis of the face's directions, the reduced Hessian `ZᵀHZ` is only
 *  semidefinite, so the reduced system is solved through its eigen-decomposition (the SVD of a
 *  symmetric positive-semidefinite matrix).  A reduced gradient with a component along a null
 *  direction gives a **ray** — zero curvature, falling cost — followed until a constraint blocks;
 *  none blocking is `Unbounded`.  Otherwise the minimum-norm step is taken, cut by the first
 *  blocking constraint, which joins the working set.  At a zero step the multipliers decide:
 *  all non-negative is optimal (certified by the shared [[qpCertificate]]), otherwise the
 *  lowest-index negative one leaves.  Lowest-index choices throughout, against cycling. */
private final class PrimalActiveSet(prob: QpProblem, precision: Int, limits: Limits):
  import prob.{n, rows}

  private val maxIterations = limits.count(Limit.MaxQpIterations)

  private val certTol = 0.5 * math.pow(10.0, -precision)
  private val iterTol = math.max(1e-14, math.min(1e-10, certTol / 10))

  // `H`'s rows as arrays, read once: `hTimes` runs once per face direction per move, and a boxed
  // fold over `prob.h(i, j)` there broke the file's own rule for inner loops (F_0108).
  private val hRows: Array[Array[Double]] = Array.tabulate(n)(i => Array.tabulate(n)(j => prob.h(i, j)))
  private def hTimes(v: Array[Double]): Array[Double] = Array.tabulate(n)(i => dotA(hRows(i), v))
  private def gradient(x: Array[Double]): Array[Double] =
    val hx = hTimes(x)
    Array.tabulate(n)(i => hx(i) + prob.f(i))

  /** Gram–Schmidt (twice) of `vs` against `basis`, keeping each vector that adds a direction. */
  private def extend(basis: Vector[Array[Double]], vs: Seq[Array[Double]]): Vector[Array[Double]] =
    vs.foldLeft(basis) { (acc, v0) =>
      val size = math.sqrt(dotA(v0, v0))
      var v = v0.clone
      for _ <- 0 until 2; b <- acc do v = combine(v, Array(dotA(v, b)), Vector(b), -1.0)
      val norm = math.sqrt(dotA(v, v))
      if norm > 1e-9 * math.max(size, Double.MinPositiveValue) then acc :+ v.map(_ / norm) else acc
    }

  /** An orthonormal basis of the directions the working set leaves free. */
  private def faceBasis(w: Vector[Int]): Vector[Array[Double]] =
    val span = extend(Vector.empty, w.map(rows(_).normal))
    extend(span, (0 until n).map(k => Array.tabulate(n)(i => if i == k then 1.0 else 0.0))).drop(span.size)

  private enum Move:
    case Step(p: Array[Double])
    case Ray(d: Array[Double])

  /** The move from `x` on the working set's face; `None` when the reduced system cannot be read. */
  private def move(x: Array[Double], w: Vector[Int]): Option[Move] =
    val g = gradient(x)
    val z = faceBasis(w)
    if z.isEmpty then Some(Move.Step(Array.fill(n)(0.0)))
    else
      val k  = z.size
      val hz = z.map(hTimes)
      val m  = _MatrixValue(k, k, Array.tabulate(k * k)(idx => dotA(z(idx / k), hz(idx % k))))
      val r  = Array.tabulate(k)(j => dotA(z(j), g))
      // A zero reduced Hessian (H = 0: a linear program) has every direction null; its
      // decomposition is the identity, written directly, since the SVD's certificate is relative
      // to the matrix's size and a zero matrix has none.
      val decomposition =
        if m.toVector.forall(_ == 0.0) then Some((Vector.fill(k)(0.0), _MatrixValue.identity(k)))
        else m.svd(limits).map((_, sigma, v) => (sigma, v))
      decomposition.map { (sigma, v) =>
        val top  = sigma.headOption.getOrElse(0.0)
        val gNorm = math.sqrt(dotA(g, g))
        val cols = sigma.indices.map(i => Array.tabulate(k)(row => v(row, i)))
        val coef = cols.map(c => dotA(c, r))
        val nul  = sigma.indices.filter(i => sigma(i) <= 1e-12 * top || top == 0.0)
        val falling = nul.filter(i => math.abs(coef(i)) > 1e-10 * math.max(gNorm, Double.MinPositiveValue))
        def lift(w: Array[Double]): Array[Double] = combine(new Array[Double](n), w, z, 1.0)
        if falling.nonEmpty then
          Move.Ray(lift(falling.foldLeft(Array.fill(k)(0.0))((acc, i) => combine(acc, Array(coef(i)), Vector(cols(i)), -1.0))))
        else
          val range = sigma.indices.filterNot(nul.contains)
          Move.Step(lift(range.foldLeft(Array.fill(k)(0.0))((acc, i) => combine(acc, Array(coef(i) / sigma(i)), Vector(cols(i)), -1.0))))
      }

  /** The working-set multipliers, `Nᵂᵀ u = ∇`, by the normal equations (the rows are independent). */
  private def multipliers(x: Array[Double], w: Vector[Int]): Option[Array[Double]] =
    if w.isEmpty then Some(Array.emptyDoubleArray)
    else
      val g = gradient(x)
      spdSolve(tabD2(w.size, w.size)((a, b) => dotA(rows(w(a)).normal, rows(w(b)).normal)),
               tabD(w.size)(a => dotA(rows(w(a)).normal, g)))

  /** The first inactive inequality a move along `d` reaches: `(step, row)`, lowest index on a tie. */
  private def blocking(x: Array[Double], d: Array[Double], w: Vector[Int]): Option[(Double, Int)] =
    rows.indices.filterNot(w.contains).filterNot(rows(_).isEquality).flatMap { i =>
      val slope = dotA(rows(i).normal, d)
      Option.when(slope < -iterTol * normInfA(rows(i).normal) * math.max(normInfA(d), Double.MinPositiveValue))(
        (math.max(0.0, rows(i).slack(x)) / -slope, i))
    }.minOption

  def solve(x0: Array[Double], warm: Set[QpConstraint]): Either[QpFailure, QpResult] =
    // The starting working set: every equality, then the warm rows, then the other inequalities
    // active at x0 -- each kept only if independent of those before it.
    val active = rows.indices.filter(i => rows(i).isEquality || math.abs(rows(i).slack(x0)) <= iterTol * prob.rowScale(rows(i), x0))
    val order  = active.filter(rows(_).isEquality) ++ active.filter(i => rows(i).id.exists(warm.contains)) ++ active
    val w0 = order.distinct.foldLeft((Vector.empty[Int], Vector.empty[Array[Double]])) { case ((ws, basis), i) =>
      val grown = extend(basis, Seq(rows(i).normal))
      if grown.size > basis.size then (ws :+ i, grown) else (ws, basis)
    }._1

    @annotation.tailrec
    def loop(x: Array[Double], w: Vector[Int], it: Int): Either[QpFailure, QpResult] =
      if it > maxIterations then Left(QpFailure.MaxIterations(maxIterations))
      else move(x, w) match
        case None => Left(QpFailure.Uncertified(Double.PositiveInfinity))
        case Some(Move.Ray(d)) =>
          blocking(x, d, w) match
            case None         => Left(QpFailure.Unbounded)
            case Some((t, j)) => loop(combine(x, Array(t), Vector(d), 1.0), w :+ j, it + 1)
        // A step negligible beside the point and the problem's own scale, and a multiplier
        // negative beyond the gradient's: both relative, with no `1 +` floor (F_0102).
        case Some(Move.Step(p)) if normInfA(p) <= iterTol * math.max(normInfA(x), prob.zScale) =>
          multipliers(x, w) match
            case None => Left(QpFailure.Uncertified(Double.PositiveInfinity))
            case Some(u) =>
              val scale = prob.gradientScale(x)
              val drop  = w.indices.filter(a => !rows(w(a)).isEquality && u(a) * normInfA(rows(w(a)).normal) < -iterTol * scale)
              drop.minByOption(w(_)) match
                case None    => qpCertificate(prob, x, w, u, it, certTol)
                case Some(a) => loop(x, w.patch(a, Nil, 1), it + 1)
        case Some(Move.Step(p)) =>
          blocking(x, p, w) match
            case Some((t, j)) if t < 1.0 => loop(combine(x, Array(t), Vector(p), 1.0), w :+ j, it + 1)
            case _                       => loop(combine(x, Array(1.0), Vector(p), 1.0), w, it + 1)

    loop(x0, w0, 0)


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

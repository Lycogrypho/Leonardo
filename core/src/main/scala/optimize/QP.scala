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
  /** `H` is positive semidefinite but singular: some direction costs nothing, and the
   *  Goldfarb–Idnani method needs `H` positive definite (F_0053 Decision B).  Not regularised —
   *  `H + εI` would answer a different problem. */
  case NotStrictlyConvex
  /** The constraints admit no point. */
  case Infeasible
  /** No optimum within the iteration cap. */
  case MaxIterations(limit: Int)
  /** The final point failed the KKT certificate (X-02); `residual` is the relative residual. */
  case Uncertified(residual: Double)

/** A certified optimum of [[qpSolve]].
 *
 *  @param z           the minimiser
 *  @param objective   `½ zᵀHz + fᵀz` at `z`
 *  @param iterations  active-set changes (additions and drops) the solve took
 *  @param kktResidual the relative KKT residual the certificate measured, at most the tolerance
 *  @param active      the inequalities and bounds holding with equality — pass it back as
 *                     `warm` for the next, similar problem (X-04: state in, state out)
 */
case class QpResult(z: Vector[Double], objective: Double, iterations: Int, kktResidual: Double,
                    active: Vector[QpConstraint])

/** The iteration cap: additions plus drops of active constraints.  Measured (F_0053, JVM, random
 *  problems with half the variables' worth of constraints active): 35 iterations at 50 variables
 *  and 200 constraints, 170 at 200 / 800, 368 at 500 / 2000 — roughly 0.7 per variable — so 2000
 *  leaves a wide margin while bounding the worst case at the largest size to tens of seconds. */
val MaxQpIterations: Int = 2000

/** The largest number of variables accepted, checked before any work.  Measured: a cold solve
 *  takes 9.6 ms at 50 variables, 51 ms at 100, 173 ms at 200 and 4.3 s at 500, the setup growing
 *  as `m·n²` — 500 is where it stops being interactive, the `MaxSampleCount` rule in time. */
val MaxQpVariables: Int = 500

/** The largest number of constraint rows (inequalities, equalities and finite bounds), the
 *  companion of [[MaxQpVariables]]: 2000 rows at 500 variables is the 4.3 s point above. */
val MaxQpConstraints: Int = 2000

/** Minimises `½ zᵀHz + fᵀz` subject to `A z ≤ b`, `Aeq z = beq` and `lb ≤ z ≤ ub` (F_0053).
 *
 *  **Goldfarb–Idnani dual active set** (Decision B): it starts at the unconstrained minimum,
 *  needs no feasible starting point, and adds the most violated constraint each step, dropping
 *  any whose multiplier would turn negative — so it stays dual-feasible and detects
 *  infeasibility by itself (a violated constraint that no step can reach).  It requires `H`
 *  **positive definite**, the case of every MPC Hessian `ΓᵀQΓ + R` with `R ≻ 0`; a merely
 *  semidefinite `H` is declined with its own reason rather than regularised.  Ties — equally
 *  violated constraints, equal step ratios — go to the lowest index, so a run is deterministic.
 *
 *  Each step solves the reduced system afresh rather than updating a factorisation: at the
 *  stated size an `O(n³)` recompute is affordable, and code without incremental updates has no
 *  drift to repair.
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
 *  @param env  supplies the precision the certificate is held to
 *  @return the certified optimum, or why there is none
 */
def qpSolve(h: _MatrixValue, f: Vector[Double],
            ineq: Option[(_MatrixValue, Vector[Double])], eq: Option[(_MatrixValue, Vector[Double])],
            lb: Vector[Double], ub: Vector[Double],
            warm: Set[QpConstraint], env: Environment): Either[QpFailure, QpResult] =
  for
    prob <- QpProblem.build(h, f, ineq, eq, lb, ub)
    hinv <- prob.inverseHessian
    res  <- QpSolver(prob, hinv, env.precision).solve(warm)
  yield res


/** One constraint row in the solver's form `n·z ≥ rhs`; `id` is `None` for an equality. */
private final case class QpRow(normal: Array[Double], rhs: Double, id: Option[QpConstraint]):
  def isEquality: Boolean = id.isEmpty
  def slack(z: Array[Double]): Double = dotA(normal, z) - rhs
  /** The size a slack is measured against: the constraint's own scale at `z`. */
  def scale(z: Array[Double]): Double = 1.0 + math.abs(rhs) + absSum * normInfA(z)
  private lazy val absSum: Double = normal.map(math.abs).sum

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

private def normInfA(a: Array[Double]): Double =
  var m = 0.0
  var i = 0
  while i < a.length do
    m = math.max(m, math.abs(a(i)))
    i += 1
  m

/** `base + sign · Σ cₐ vₐ`, the one linear combination the solver forms. */
private def combine(base: Array[Double], cs: Vector[Double], vs: Vector[Array[Double]], sign: Double): Array[Double] =
  val out = base.clone()
  var a = 0
  while a < cs.size do
    val c = sign * cs(a)
    val v = vs(a)
    var k = 0
    while k < out.length do
      out(k) += c * v(k)
      k += 1
    a += 1
  out

/** A validated problem: `H` checked, every constraint rewritten as a [[QpRow]]. */
private final case class QpProblem(n: Int, h: _MatrixValue, f: Array[Double], rows: Vector[QpRow]):

  /** `H⁻¹`, through the Cholesky factor; `H` not positive definite is the failure. */
  def inverseHessian: Either[QpFailure, Array[Array[Double]]] =
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

private object QpProblem:

  def build(h: _MatrixValue, f: Vector[Double],
            ineq: Option[(_MatrixValue, Vector[Double])], eq: Option[(_MatrixValue, Vector[Double])],
            lb: Vector[Double], ub: Vector[Double]): Either[QpFailure, QpProblem] =
    val n = h.rows
    def bad(detail: String) = Left(QpFailure.InvalidInput(detail))
    def finite(xs: Iterable[Double]) = xs.forall(d => !d.isNaN && !d.isInfinite)
    def block(name: String, g: Option[(_MatrixValue, Vector[Double])]): Either[QpFailure, Vector[(Array[Double], Double)]] =
      g match
        case None => Right(Vector.empty)
        case Some((a, b)) =>
          if a.cols != n || a.rows != b.size then
            bad(s"$name must have $n columns and as many rows as its right-hand side has entries")
          else if !finite(a.toVector) || !finite(b) then bad(s"$name and its right-hand side must be finite")
          else Right(Vector.tabulate(a.rows)(k => (Array.tabulate(n)(j => a(k, j)), b(k))))
    val maxH  = h.toVector.map(math.abs).maxOption.getOrElse(0.0)
    if h.rows != h.cols then bad("H must be square")
    else if n > MaxQpVariables then bad(s"at most $MaxQpVariables variables are accepted, not $n")
    else if f.size != n || lb.size != n || ub.size != n then bad(s"f, lb and ub must each have $n entries")
    else if !finite(h.toVector) || !finite(f) then bad("H and f must be finite")
    else if lb.exists(_.isNaN) || ub.exists(_.isNaN) then bad("a bound is not a number")
    else if (0 until n).exists(i => (0 until i).exists(j => math.abs(h(i, j) - h(j, i)) > 1e-12 * maxH)) then
      bad("H must be symmetric")
    else if (0 until n).exists(i => lb(i) > ub(i) || lb(i) == Double.PositiveInfinity || ub(i) == Double.NegativeInfinity) then
      Left(QpFailure.Infeasible)
    else
      for
        ins <- block("A", ineq)
        eqs <- block("Aeq", eq)
        unit = (i: Int, s: Double) => Array.tabulate(n)(j => if j == i then s else 0.0)
        rows = eqs.map((a, b) => QpRow(a, b, None)) ++
               ins.zipWithIndex.map { case ((a, b), k) => QpRow(a.map(-_), -b, Some(QpConstraint.Row(k))) } ++
               (0 until n).filter(i => lb(i) > Double.NegativeInfinity).map(i => QpRow(unit(i, 1.0), lb(i), Some(QpConstraint.Lower(i)))) ++
               (0 until n).filter(i => ub(i) < Double.PositiveInfinity).map(i => QpRow(unit(i, -1.0), -ub(i), Some(QpConstraint.Upper(i))))
        _ <- if rows.size > MaxQpConstraints then bad(s"at most $MaxQpConstraints constraints are accepted, not ${rows.size}")
             else Right(())
      yield QpProblem(n, h, f.toArray, rows)


/** The Goldfarb–Idnani iteration over one validated problem; see [[qpSolve]]. */
private final class QpSolver(prob: QpProblem, hinv: Array[Array[Double]], precision: Int):
  import prob.{n, rows}

  private val certTol = 0.5 * math.pow(10.0, -precision)
  /** Decisions inside the iteration are held tighter than the certificate, so a point the
   *  iteration accepts is one the certificate can pass. */
  private val iterTol = math.max(1e-14, math.min(1e-10, certTol / 10))
  /** A constraint whose normal lies in the span of the active ones, relative to its own size. */
  private val depTol  = 1e-12

  private def hinvTimes(v: Array[Double]): Array[Double] = Array.tabulate(n)(i => dotA(hinv(i), v))
  /** `H⁻¹ nⱼ` for every row, computed once. */
  private val g: Vector[Array[Double]] = rows.map(r => hinvTimes(r.normal))
  private val x0: Array[Double] = hinvTimes(prob.f).map(-_)

  /** State between steps: the iterate, the active rows and their multipliers (aligned). */
  private final case class State(x: Array[Double], act: Vector[Int], u: Vector[Double], iters: Int)

  /** `(r, z, curvature)` for adding row `p` to `act`: the dual and primal directions. */
  private def direction(act: Vector[Int], p: Int): Option[(Vector[Double], Array[Double], Double)] =
    val np = rows(p).normal
    val r  = if act.isEmpty then Some(Vector.empty[Double])
             else spdSolve(Vector.tabulate(act.size, act.size)((a, b) => dotA(rows(act(a)).normal, g(act(b)))),
                           act.map(i => dotA(rows(i).normal, g(p))))
    r.map { rr =>
      val z = combine(g(p), rr, act.map(g), -1.0)
      (rr, z, dotA(np, z))
    }

  private def dependent(p: Int, curvature: Double): Boolean =
    curvature <= depTol * dotA(rows(p).normal, g(p))

  /** The minimiser with `act` held as equalities: `x = x₀ + Σ uᵢ gᵢ`, `u = M⁻¹ (b − Nᵀx₀)`. */
  private def onFace(act: Vector[Int]): Option[(Array[Double], Vector[Double])] =
    if act.isEmpty then Some((x0, Vector.empty))
    else
      spdSolve(Vector.tabulate(act.size, act.size)((a, b) => dotA(rows(act(a)).normal, g(act(b)))),
               act.map(i => rows(i).rhs - dotA(rows(i).normal, x0))).map(u => (combine(x0, u, act.map(g), 1.0), u))

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
    @tailrec def dropNegative(act: Vector[Int]): Option[(Array[Double], Vector[Int], Vector[Double])] =
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
                                         math.abs(rows(p).slack(x)) > certTol * rows(p).scale(x))
        if broken then Left(QpFailure.Infeasible) else Right(State(x, act1, u, 0))

  def solve(warm: Set[QpConstraint]): Either[QpFailure, QpResult] = start(warm).flatMap(outer)

  /** Adds the most violated inactive inequality, or certifies when none is violated. */
  @tailrec private def outer(s: State): Either[QpFailure, QpResult] =
    if s.iters > MaxQpIterations then Left(QpFailure.MaxIterations(MaxQpIterations))
    else
      val candidates = rows.indices.filterNot(s.act.contains).filterNot(rows(_).isEquality)
        .map(j => (rows(j).slack(s.x) / rows(j).scale(s.x), j))
        .filter(_._1 < -iterTol)
      if candidates.isEmpty then certify(s)
      else step(s, candidates.min._2, 0.0) match
        case Right(next) => outer(next)
        case Left(why)   => Left(why)

  /** One constraint `p` is brought into the active set by full and partial steps. */
  @tailrec private def step(s: State, p: Int, up: Double): Either[QpFailure, State] =
    if s.iters > MaxQpIterations then Left(QpFailure.MaxIterations(MaxQpIterations))
    else direction(s.act, p) match
      case None => Left(QpFailure.Uncertified(Double.PositiveInfinity))
      case Some((r, z, curvature)) =>
        val droppable = s.act.indices.filter(a => !rows(s.act(a)).isEquality && r(a) > 0)
        val (t1, k) =
          if droppable.isEmpty then (Double.PositiveInfinity, -1)
          else droppable.map(a => (s.u(a) / r(a), s.act(a), a)).min match { case (t, _, a) => (t, a) }
        val dep = dependent(p, curvature)
        val t2  = if dep then Double.PositiveInfinity else -rows(p).slack(s.x) / curvature
        val t   = math.min(t1, t2)
        if t.isInfinite then Left(QpFailure.Infeasible)
        else
          val u1  = s.u.indices.map(a => s.u(a) - t * r(a)).toVector
          val x1  = if dep then s.x else combine(s.x, Vector(t), Vector(z), 1.0)
          if !dep && t2 <= t1 then Right(State(x1, s.act :+ p, u1 :+ (up + t), s.iters + 1))
          else step(State(x1, s.act.patch(k, Nil, 1), u1.patch(k, Nil, 1), s.iters + 1), p, up + t)

  /** The KKT certificate (X-02); the answer only when every part passes. */
  private def certify(s: State): Either[QpFailure, QpResult] =
    val x = s.x
    val hx = Array.tabulate(n)(i => (0 until n).foldLeft(0.0)((acc, j) => acc + prob.h(i, j) * x(j)))
    val grad = Array.tabulate(n)(i => hx(i) + prob.f(i))
    val pull = Array.tabulate(n)(i => s.act.indices.foldLeft(0.0)((acc, a) => acc + s.u(a) * rows(s.act(a)).normal(i)))
    val uMax = s.u.map(math.abs).maxOption.getOrElse(0.0)
    val stationarity =
      normInfA(Array.tabulate(n)(i => grad(i) - pull(i))) /
        (1.0 + normInfA(prob.f) + normInfA(hx) + s.act.indices.map(a => math.abs(s.u(a)) * normInfA(rows(s.act(a)).normal)).sum)
    val primal = rows.map { r =>
      val sl = r.slack(x)
      (if r.isEquality then math.abs(sl) else math.max(0.0, -sl)) / r.scale(x)
    }.maxOption.getOrElse(0.0)
    val dual = s.act.indices.filterNot(a => rows(s.act(a)).isEquality)
      .map(a => math.max(0.0, -s.u(a))).maxOption.getOrElse(0.0) / (1.0 + uMax)
    val complementarity = s.act.indices.filterNot(a => rows(s.act(a)).isEquality)
      .map(a => math.abs(s.u(a) * rows(s.act(a)).slack(x)) / (rows(s.act(a)).scale(x) * (1.0 + uMax)))
      .maxOption.getOrElse(0.0)
    val residual = Seq(stationarity, primal, dual, complementarity).max
    if residual.isNaN || residual > certTol then Left(QpFailure.Uncertified(residual))
    else
      val objective = 0.5 * dotA(x, hx) + dotA(prob.f, x)
      Right(QpResult(x.toVector, objective, s.iters, residual, s.act.flatMap(rows(_).id)))


/** Solves `M u = rhs` for a small symmetric positive-definite `M` by Cholesky; `None` when a
 *  pivot is not positive, which here means the active normals became numerically dependent. */
private def spdSolve(m: Vector[Vector[Double]], rhs: Vector[Double]): Option[Vector[Double]] =
  val q = m.size
  val l = Array.ofDim[Double](q, q)
  /** `Σ_{k<upto} a(k)·b(k)` over the leading entries of two rows. */
  def head(a: Array[Double], b: Array[Double], upto: Int): Double =
    var s = 0.0
    var k = 0
    while k < upto do
      s += a(k) * b(k)
      k += 1
    s
  val ok = (0 until q).forall { j =>
    val d = m(j)(j) - head(l(j), l(j), j)
    if d <= 0 || d.isNaN then false
    else
      l(j)(j) = math.sqrt(d)
      for i <- j + 1 until q do l(i)(j) = (m(i)(j) - head(l(i), l(j), j)) / l(j)(j)
      true
  }
  Option.when(ok) {
    val y = new Array[Double](q)
    for i <- 0 until q do y(i) = (rhs(i) - head(l(i), y, i)) / l(i)(i)
    val u = new Array[Double](q)
    for i <- (q - 1) to 0 by -1 do
      var s = 0.0
      var k = i + 1
      while k < q do
        s += l(k)(i) * u(k)
        k += 1
      u(i) = (y(i) - s) / l(i)(i)
    u.toVector
  }

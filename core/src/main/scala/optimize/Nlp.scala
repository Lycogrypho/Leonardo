package it.grypho.scala.leonardo
package optimize

import core.*
import scalar.*
import matrix._Matrix
import vector._Jacobian

import scala.annotation.tailrec


/** The constrained nonlinear programming methods, always named by the caller (F_0066).
 *
 *  **Never a hidden default**, the `c2d` rule: on a non-convex problem the two can stop at
 *  different KKT points, so a silent choice would make two correct-looking answers disagree.
 */
enum NlpMethod:
  /** Sequential quadratic programming: one [[qpSolve]] per iteration on the linearised
   *  constraints, a damped-BFGS Hessian, an `ℓ₁` merit line search with a second-order
   *  correction. */
  case SQP
  /** The Powell–Hestenes–Rockafellar augmented Lagrangian, each subproblem minimised under the
   *  bounds by a projected BFGS. */
  case AugLag

object NlpMethod:
  /** The keyword a method is written with. */
  def keyword(m: NlpMethod): String = m match
    case SQP    => "sqp"
    case AugLag => "auglag"

  /** Looks a method up by its keyword. */
  def fromKeyword(s: String): Option[NlpMethod] = values.find(m => keyword(m) == s)


/** Why [[nlpSolve]] declined (F_0052 Decision A: a reason at the Scala level, which the REPL
 *  appends as a note).  The node itself stays symbolic. */
enum NlpFailure:
  /** The problem as written is malformed; `detail` says how. */
  case InvalidInput(detail: String)
  /** The objective, a constraint or a derivative is not a real number at an iterate. */
  case NotEvaluable
  /** No feasible point was found: SQP's linearised constraints admit no step at `iteration`,
   *  or the augmented Lagrangian's penalty grew past [[MaxNlpPenalty]] with the constraints
   *  still violated.  Neither proves the problem infeasible — a local method cannot — but no
   *  answer is given. */
  case Infeasible(iteration: Int)
  /** SQP's quadratic subproblem declined at `iteration` for a reason other than infeasibility. */
  case Subproblem(iteration: Int, why: QpFailure)
  /** The line search found no acceptable step at `iteration`. */
  case LineSearchFailed(iteration: Int)
  /** The iteration cap was reached; `kktResidual` is what the certificate measured there. */
  case MaxIterations(limit: Int, kktResidual: Double)
  /** Progress stopped short of the certificate; `kktResidual` is what was left. */
  case Stalled(kktResidual: Double)

/** A certified KKT point of `min f` subject to `g ≤ 0`, `h = 0`, `lb ≤ x ≤ ub`.
 *
 *  @param x                     the point
 *  @param value                 `f` there
 *  @param iterations            SQP iterations, or augmented-Lagrangian rounds
 *  @param kktResidual           the relative KKT residual the certificate measured, at most
 *                               the tolerance
 *  @param inequalityMultipliers `μ ≥ 0`, one per inequality, in `L = f + μᵀg + λᵀh`
 *  @param equalityMultipliers   `λ`, one per equality, in the same Lagrangian
 *  @param active                the inequalities with a positive multiplier, ascending
 *  @param bounded               the variables held at a bound, ascending
 */
case class NlpResult(x: Vector[Double], value: Double, iterations: Int, kktResidual: Double,
                     inequalityMultipliers: Vector[Double], equalityMultipliers: Vector[Double],
                     active: Vector[Int], bounded: Vector[Int])


/** SQP's iteration cap.  **Measured** (F_0066, JVM): the test problems converge in 1–42
 *  iterations — 28 for Rosenbrock with a bound, 42 for an objective scaled by `10⁶` — and a
 *  50-variable problem in 10; 200 is almost five times the slowest. */
val MaxNlpIterations: Int = 200

/** The augmented Lagrangian's cap on rounds, each one a full inner minimisation. */
val MaxAugLagRounds: Int = 50

/** The penalty beyond which the augmented Lagrangian gives up on feasibility.  Ten orders
 *  above the start: past it the inner problems are conditioned beyond what a `Double` BFGS
 *  resolves, so continuing would only manufacture an answer. */
val MaxNlpPenalty: Double = 1e12

/** The largest number of variables accepted, checked before any work.  **Measured** on a
 *  quadratic objective with two nonlinear constraints and a bound, JVM / Node: 0.34 / 0.68 s at
 *  50 variables, 0.39 / 1.6 s at 75, for either method — the symbolic Jacobians and their
 *  compilation dominate. At the prompt the expression-depth limit (`core.MaxExpressionDepth`)
 *  stops a sum of more than about 200 terms first, so this cap binds only library callers. */
val MaxNlpVariables: Int = 100


/** Minimises `f` over `vars` subject to `g ≤ 0`, `h = 0` and `lb ≤ x ≤ ub` (F_0066, MPC T3-02).
 *
 *  **The convention is minimisation with `g ≤ 0`** (the `kkt` convention), stated because the
 *  literature also writes `g ≥ 0`, and the sign of every multiplier depends on which.
 *
 *  Two methods, always named ([[NlpMethod]]); both answer only a point that passes **one KKT
 *  certificate** (X-02) against the `=` tolerance `0.5·10⁻ᵖ`:
 *
 *  - *stationarity*, `∇f + Σμⱼ∇gⱼ + Σλₖ∇hₖ`, each component relative to the magnitudes of the
 *    terms that cancel in it — so the verdict does not move when `f` or a variable is rescaled
 *    (the units rule; an absolute bound on a gradient is the bug F_0095 fixed in `minimize`).
 *    A component at an active bound is absorbed when its sign is the bound multiplier's;
 *  - *primal feasibility*, each violation relative to its constraint's terms;
 *  - *dual feasibility*, `μ ≥ 0`, and *complementarity*, `μⱼ·gⱼ = 0`.
 *
 *  **A local KKT point is answered**, the `minimize` precedent: on a non-convex problem it may
 *  be a local minimum other than the global one, or (rarely) a saddle of the Lagrangian.
 *  The variables are renamed to fresh names first, so a session binding of a variable's name
 *  cannot fold the problem (the `lsqfit` rule).
 *
 *  @param f      the objective
 *  @param vars   the variables, in the order of `x0`
 *  @param x0     the starting point; projected into the bounds
 *  @param g      the inequality constraints, each meaning `gⱼ ≤ 0`
 *  @param h      the equality constraints, each meaning `hₖ = 0`
 *  @param bounds lower and upper bounds, entries may be infinite; `None` when unbounded
 *  @param method the method
 *  @param env    bindings and precision
 *  @return the certified KKT point, or why there is none
 */
def nlpSolve(f: _Expression, vars: Vector[_Variable], x0: Vector[Double],
             g: Vector[_Expression], h: Vector[_Expression],
             bounds: Option[(Vector[Double], Vector[Double])], method: NlpMethod,
             env: Environment): Either[NlpFailure, NlpResult] =
  val n = vars.size
  val (lo, hi) = bounds.getOrElse((Vector.fill(n)(Double.NegativeInfinity), Vector.fill(n)(Double.PositiveInfinity)))
  val invalid: Option[String] =
    if !distinctVariables(vars) then Some("the variables must be distinct names")
    else if n > MaxNlpVariables then Some(s"at most $MaxNlpVariables variables are accepted, not $n")
    else if x0.size != n then Some(s"the starting point has ${x0.size} entries for $n variables")
    else if x0.exists(d => d.isNaN || d.isInfinite) then Some("the starting point must be finite")
    else if lo.size != n || hi.size != n then Some(s"the bounds must have $n entries each")
    else if lo.zip(hi).exists((a, b) => a.isNaN || b.isNaN || a > b) then Some("a lower bound exceeds its upper bound")
    else None
  invalid match
    case Some(detail) => Left(NlpFailure.InvalidInput(detail))
    case None =>
      NlpSystem.build(f, vars, g, h, env).flatMap { sys =>
        val (l, u) = (lo.toArray, hi.toArray)
        val start  = Array.tabulate(n)(i => math.min(math.max(x0(i), l(i)), u(i)))
        method match
          case NlpMethod.SQP    => sqp(sys, start, l, u, env)
          case NlpMethod.AugLag => augLag(sys, start, l, u, env)
      }


/** The problem at one point: the values, the derivatives, and the magnitudes of the terms
 *  each of them sums — what the certificate measures a residual against.  Jacobians are
 *  row-major, one row per constraint. */
private final case class NlpPoint(x: Array[Double], f: Double, g: Array[Double], h: Array[Double],
                                  grad: Array[Double], jg: Array[Double], jh: Array[Double],
                                  gTerms: Array[Double], hTerms: Array[Double],
                                  gradTerms: Array[Double], jgTerms: Array[Double], jhTerms: Array[Double])

/** The problem with its symbolic derivatives, every cell compiled once (F_0058) or
 *  tree-evaluated where the compiler's table does not reach.  `f`, `gs` and `hs` are written in
 *  the fresh variables `vars`. */
private final class NlpSystem(val n: Int, val mg: Int, val mh: Int,
                              val f: _Expression, val gs: Vector[_Expression], val hs: Vector[_Expression],
                              val vars: Vector[_Variable],
                              val values: Array[Double] => Option[Array[Double]],
                              val derivatives: Array[Double] => Option[Array[Double]],
                              constraintTerms: Array[Double] => Option[Array[Double]],
                              derivTerms: Array[Double] => Option[Array[Double]]):

  /** Everything at `x`, or `None` when a cell is not a finite real there. */
  def at(x: Array[Double]): Option[NlpPoint] =
    val (jgEnd, jhEnd) = (n + mg * n, n + mg * n + mh * n)
    for
      v  <- values(x)
      d  <- derivatives(x)
      ct <- constraintTerms(x)
      dt <- derivTerms(x)
    yield NlpPoint(x, v(0), v.slice(1, 1 + mg), v.slice(1 + mg, 1 + mg + mh),
                   d.slice(0, n), d.slice(n, jgEnd), d.slice(jgEnd, jhEnd),
                   ct.slice(0, mg), ct.slice(mg, mg + mh),
                   dt.slice(0, n), dt.slice(n, jgEnd), dt.slice(jgEnd, jhEnd))

private object NlpSystem:

  def build(f: _Expression, vars: Vector[_Variable], g: Vector[_Expression], h: Vector[_Expression],
            env: Environment): Either[NlpFailure, NlpSystem] =
    val n       = vars.size
    val fresh   = freshNames(n, "__x", reservedNames(f +: (g ++ h), vars)).toVector
    val rename  = vars.map(_.variable).zip(fresh).toMap
    // Renamed, then evaluated once: a bound parameter folds now, before anything is compiled.
    def prepared(e: _Expression): _Expression = substitute(e, rename).eval(env).toExpression
    val (fr, gr, hr) = (prepared(f), g.map(prepared), h.map(prepared))
    val names   = unbound(fr +: (gr ++ hr), fresh, env)
    if names.nonEmpty then
      Left(NlpFailure.InvalidInput(s"free names that are not variables: ${names.toList.sorted.mkString(", ")}"))
    else
      def jacobian(cells: Vector[_Expression]): Option[Vector[_Expression]] =
        if cells.isEmpty then Some(Vector.empty)
        else gridCells(_Jacobian(_Matrix(cells.size, 1, cells), fresh).eval(env).toExpression, cells.size, n)
      val derived = for
        grad <- jacobian(Vector(fr))
        jg   <- jacobian(gr)
        jh   <- jacobian(hr)
      yield grad ++ jg ++ jh
      derived match
        case None => Left(NlpFailure.NotEvaluable)
        case Some(dcells) =>
          Right(NlpSystem(n, gr.size, hr.size, fr, gr, hr, fresh,
            cellsAt(fr +: (gr ++ hr), fresh, env), cellsAt(dcells, fresh, env),
            termSums(gr ++ hr, fresh, env, expandedTerms), termSums(dcells, fresh, env, expandedTerms)))

  /** An evaluator that also accepts no cells. */
  private def cellsAt(cells: Vector[_Expression], vars: Vector[_Variable],
                      env: Environment): Array[Double] => Option[Array[Double]] =
    if cells.isEmpty then _ => Some(Array.empty[Double]) else evaluator(cells, vars, env)

  /** The terms of a cell's expanded form: what the KKT certificate measures a residual against.
   *  `lsqfit` reads its residuals' terms as written; see [[termSums]]. */
  private def expandedTerms(c: _Expression): List[_Expression] = summands(expand(c))


// ── the certificate ──

/** `aᵀb`, as a `while` loop (the QP measurement: boxed folds are several times slower). */
private def nlpDot(a: Array[Double], b: Array[Double]): Double =
  var s = 0.0
  var i = 0
  while i < a.length do
    s += a(i) * b(i)
    i += 1
  s

/** `maxᵢ |aⱼᵢ|` over row `j` of a row-major `rows×n` array. */
private def rowNorm(a: Array[Double], j: Int, n: Int): Double =
  var m = 0.0
  var i = 0
  while i < n do
    m = math.max(m, math.abs(a(j * n + i)))
    i += 1
  m

/** `a / b`, reading `0 / 0` as 0: a residual whose terms are all zero is exactly zero. */
private def ratio(a: Double, b: Double): Double =
  if a == 0.0 then 0.0 else if b > 0.0 then a / b else Double.PositiveInfinity

/** The size a constraint's value is measured against: its terms, or its gradient times the
 *  size of the point when the terms themselves vanish there (a constraint `x ≤ 0` at `x = 0`). */
private def constraintScale(terms: Double, jac: Array[Double], j: Int, n: Int, xNorm: Double): Double =
  math.max(terms, rowNorm(jac, j, n) * xNorm)

/** The relative primal violation at `p`: the largest `max(0, gⱼ)` and `|hₖ|`, each against its
 *  constraint's scale. */
private def primalViolation(sys: NlpSystem, p: NlpPoint): Double =
  val xNorm = p.x.map(math.abs).maxOption.getOrElse(0.0)
  val ineq = (0 until sys.mg).map(j => ratio(math.max(0.0, p.g(j)), constraintScale(p.gTerms(j), p.jg, j, sys.n, xNorm)))
  val eq   = (0 until sys.mh).map(k => ratio(math.abs(p.h(k)), constraintScale(p.hTerms(k), p.jh, k, sys.n, xNorm)))
  (ineq ++ eq).maxOption.getOrElse(0.0)

/** The relative KKT residual at `p` with multipliers `mu` (inequalities) and `lam`
 *  (equalities): the largest of stationarity, primal feasibility, dual feasibility and
 *  complementarity, each scale-free (see [[nlpSolve]]). */
private def kktResidual(sys: NlpSystem, p: NlpPoint, mu: Array[Double], lam: Array[Double],
                        lo: Array[Double], hi: Array[Double]): Double =
  val n = sys.n
  var stationarity = 0.0
  var tMax = 0.0
  var i = 0
  while i < n do
    var r = p.grad(i)
    var t = p.gradTerms(i)
    var j = 0
    while j < sys.mg do
      r += mu(j) * p.jg(j * n + i)
      t += math.abs(mu(j)) * p.jgTerms(j * n + i)
      j += 1
    var k = 0
    while k < sys.mh do
      r += lam(k) * p.jh(k * n + i)
      t += math.abs(lam(k)) * p.jhTerms(k * n + i)
      k += 1
    tMax = math.max(tMax, t)
    // At an active bound, a component pushing outward is the bound's multiplier, not a residual.
    val held = (p.x(i) <= lo(i) && r > 0.0) || (p.x(i) >= hi(i) && r < 0.0)
    if !held then stationarity = math.max(stationarity, ratio(math.abs(r), t))
    i += 1
  val xNorm = p.x.map(math.abs).maxOption.getOrElse(0.0)
  val dualAndSlack = (0 until sys.mg).map { j =>
    val weight = math.abs(mu(j)) * rowNorm(p.jg, j, n)
    val dual   = if mu(j) < 0.0 then ratio(weight, tMax) else 0.0
    val slack  = math.min(ratio(weight, tMax),
                          ratio(math.abs(p.g(j)), constraintScale(p.gTerms(j), p.jg, j, n, xNorm)))
    math.max(dual, slack)
  }.maxOption.getOrElse(0.0)
  Seq(stationarity, primalViolation(sys, p), dualAndSlack).max

private def certificateTolerance(env: Environment): Double = 0.5 * math.pow(10.0, -env.precision)

/** The certified answer at `p`. */
private def nlpAnswer(p: NlpPoint, k: Int, residual: Double, mu: Array[Double], lam: Array[Double],
                      lo: Array[Double], hi: Array[Double]): NlpResult =
  NlpResult(p.x.toVector, p.f, k, residual, mu.toVector, lam.toVector, mu.indices.filter(mu(_) > 0.0).toVector,
            p.x.indices.filter(i => p.x(i) <= lo(i) || p.x(i) >= hi(i)).toVector)

/** `Σ max(0, gⱼ) + Σ |hₖ|`, the `ℓ₁` infeasibility the merit function penalises. */
private def l1Violation(g: Array[Double], h: Array[Double]): Double =
  g.map(math.max(0.0, _)).sum + h.map(math.abs).sum


// ── SQP ──

/** Sequential quadratic programming (Nocedal–Wright §18.3).
 *
 *  Each iteration solves `min ½ dᵀBd + ∇fᵀd` subject to the constraints linearised at `x` and
 *  the bounds shifted to `lb − x ≤ d ≤ ub − x`, by [[qpSolve]]; its multipliers are the
 *  estimates the certificate and the next Hessian read.  `B` is **Powell's damped BFGS** on the
 *  Lagrangian, which stays positive definite where the Lagrangian's Hessian is not, so the
 *  Goldfarb–Idnani solver always applies.  The step is accepted by an Armijo test on the `ℓ₁`
 *  merit `f + ν·(Σ max(0, g) + Σ|h|)`, `ν` kept above the multipliers; a rejected full step
 *  first tries a **second-order correction** — the Maratos effect would otherwise reject the
 *  very steps that converge fast near a curved constraint — then backtracks.  Iterates are
 *  projected into the bounds, so they are never violated.
 */
private def sqp(sys: NlpSystem, x0: Array[Double], lo: Array[Double], hi: Array[Double],
                env: Environment): Either[NlpFailure, NlpResult] =
  val n      = sys.n
  val tol    = certificateTolerance(env)
  val target = math.min(1e-10, tol / 100)

  def clamp(x: Array[Double]): Array[Double] = Array.tabulate(n)(i => math.min(math.max(x(i), lo(i)), hi(i)))
  def identity: Array[Array[Double]] = Array.tabulate(n, n)((i, j) => if i == j then 1.0 else 0.0)

  def subproblem(p: NlpPoint, b: Array[Array[Double]], gRhs: Array[Double], hRhs: Array[Double],
                 warm: Set[QpConstraint]): Either[QpFailure, QpResult] =
    qpSolve(_MatrixValue(n, n, b.flatten), p.grad.toVector,
            Option.when(sys.mg > 0)((_MatrixValue(sys.mg, n, p.jg), gRhs.toVector)),
            Option.when(sys.mh > 0)((_MatrixValue(sys.mh, n, p.jh), hRhs.toVector)),
            Vector.tabulate(n)(i => lo(i) - p.x(i)), Vector.tabulate(n)(i => hi(i) - p.x(i)), warm, env)

  /** The QP at `p`, retried once from `B = I` if rounding has cost `B` its definiteness. */
  def step(p: NlpPoint, b: Array[Array[Double]], warm: Set[QpConstraint]): Either[QpFailure, (QpResult, Array[Array[Double]])] =
    subproblem(p, b, p.g.map(-_), p.h.map(-_), warm) match
      case Left(QpFailure.NotConvex | QpFailure.NotStrictlyConvex) =>
        val reset = identity
        subproblem(p, reset, p.g.map(-_), p.h.map(-_), warm).map((_, reset))
      case other => other.map((_, b))

  def lagrangianGradient(p: NlpPoint, mu: Array[Double], lam: Array[Double]): Array[Double] =
    Array.tabulate(n) { i =>
      var s = p.grad(i)
      var j = 0
      while j < sys.mg do { s += mu(j) * p.jg(j * n + i); j += 1 }
      var k = 0
      while k < sys.mh do { s += lam(k) * p.jh(k * n + i); k += 1 }
      s
    }

  /** Powell's damped BFGS update: `y` is blended with `Bs` until `sᵀy ≥ 0.2·sᵀBs`, which keeps
   *  `B` positive definite whatever the Lagrangian's curvature.  The first update rescales the
   *  identity by `yᵀy/sᵀy` (Shanno–Phua), so the initial step length carries the problem's units. */
  def updated(b0: Array[Array[Double]], p: NlpPoint, q: NlpPoint, mu: Array[Double], lam: Array[Double],
              first: Boolean): Array[Array[Double]] =
    val s  = Array.tabulate(n)(i => q.x(i) - p.x(i))
    val gp = lagrangianGradient(p, mu, lam)
    val gq = lagrangianGradient(q, mu, lam)
    val y  = Array.tabulate(n)(i => gq(i) - gp(i))
    val sy = nlpDot(s, y)
    val yy = nlpDot(y, y)
    val b  = if first && sy > 0.0 then Array.tabulate(n, n)((i, j) => if i == j then yy / sy else 0.0) else b0
    val bs  = Array.tabulate(n)(i => nlpDot(b(i), s))
    val sBs = nlpDot(s, bs)
    if !(sBs > 0.0) || sBs.isInfinite then b
    else
      val theta = if sy >= 0.2 * sBs then 1.0 else 0.8 * sBs / (sBs - sy)
      val r     = Array.tabulate(n)(i => theta * y(i) + (1 - theta) * bs(i))
      val sr    = nlpDot(s, r)
      if !(sr > 0.0) || sr.isInfinite then b
      else
        val next = Array.tabulate(n, n)((i, j) => b(i)(j) - bs(i) * bs(j) / sBs + r(i) * r(j) / sr)
        if next.exists(_.exists(d => d.isNaN || d.isInfinite)) then b
        else Array.tabulate(n, n)((i, j) => 0.5 * (next(i)(j) + next(j)(i)))

  /** The accepted next point, or `None` when no step down to `2⁻⁴⁰` passes the Armijo test. */
  def search(p: NlpPoint, b: Array[Array[Double]], d: Array[Double], nu: Double, slope: Double,
             warm: Set[QpConstraint]): Option[Array[Double]] =
    val phi0 = p.f + nu * l1Violation(p.g, p.h)
    def merit(x: Array[Double]): Option[Double] =
      sys.values(x).map(v => v(0) + nu * l1Violation(v.slice(1, 1 + sys.mg), v.slice(1 + sys.mg, v.length)))
    def accepts(x: Array[Double], alpha: Double): Boolean =
      merit(x).exists(m => m <= phi0 + 1e-4 * alpha * slope)
    val full = clamp(Array.tabulate(n)(i => p.x(i) + d(i)))
    if accepts(full, 1.0) then Some(full)
    else
      // Second-order correction: re-solve with the constraints' values at x + d in place of
      // their linearisation, which bends the step back onto a curved constraint.
      val corrected = sys.values(full).flatMap { v =>
        val gRhs = Array.tabulate(sys.mg)(j => -v(1 + j) + (0 until n).map(i => p.jg(j * n + i) * d(i)).sum)
        val hRhs = Array.tabulate(sys.mh)(k => -v(1 + sys.mg + k) + (0 until n).map(i => p.jh(k * n + i) * d(i)).sum)
        subproblem(p, b, gRhs, hRhs, warm).toOption
          .map(q => clamp(Array.tabulate(n)(i => p.x(i) + q.z(i))))
          .filter(accepts(_, 1.0))
      }
      corrected.orElse(
        Iterator.iterate(0.5)(_ / 2).takeWhile(_ >= math.pow(2, -40))
          .map(alpha => (alpha, clamp(Array.tabulate(n)(i => p.x(i) + alpha * d(i)))))
          .find((alpha, x) => accepts(x, alpha))
          .map(_._2))

  @tailrec
  def loop(p: NlpPoint, b: Array[Array[Double]], nu: Double, warm: Set[QpConstraint], k: Int,
           first: Boolean): Either[NlpFailure, NlpResult] =
    step(p, b, warm) match
      case Left(QpFailure.Infeasible) => Left(NlpFailure.Infeasible(k))
      case Left(why)                  => Left(NlpFailure.Subproblem(k, why))
      case Right((qp, bk)) =>
        val d   = qp.z.toArray
        val mu  = qp.multipliers.inequality.toArray
        val lam = qp.multipliers.equality.toArray
        val res = kktResidual(sys, p, mu, lam, lo, hi)
        def answer = Right(nlpAnswer(p, k, res, mu, lam, lo, hi))
        if res <= target then answer
        else if k >= MaxNlpIterations then
          if res <= tol then answer else Left(NlpFailure.MaxIterations(MaxNlpIterations, res))
        else
          val m     = (mu ++ lam).map(math.abs).maxOption.getOrElse(0.0)
          val nuK   = if nu >= 1.1 * m then nu else 2.0 * m
          val slope = nlpDot(p.grad, d) - nuK * l1Violation(p.g, p.h)
          if !(slope < 0.0) then
            if res <= tol then answer else Left(NlpFailure.Stalled(res))
          else search(p, bk, d, nuK, slope, qp.active.toSet) match
            case None =>
              if res <= tol then answer else Left(NlpFailure.LineSearchFailed(k))
            case Some(x) =>
              val moved = (0 until n).map(i => math.abs(x(i) - p.x(i))).max
              val size  = p.x.map(math.abs).maxOption.getOrElse(0.0)
              if moved <= 1e-15 * size || moved == 0.0 then
                if res <= tol then answer else Left(NlpFailure.Stalled(res))
              else sys.at(x) match
                case None    => Left(NlpFailure.NotEvaluable)
                case Some(q) => loop(q, updated(bk, p, q, mu, lam, first), nuK, qp.active.toSet, k + 1, false)

  sys.at(x0) match
    case None    => Left(NlpFailure.NotEvaluable)
    case Some(p) => loop(p, identity, 0.0, Set.empty, 0, true)


// ── the augmented Lagrangian ──

/** The Powell–Hestenes–Rockafellar augmented Lagrangian (Nocedal–Wright §17.4).
 *
 *  Each round minimises, under the bounds,
 *  `φ = f/s + λᵀh + (ρ/2)‖h‖² + (1/2ρ)Σ(max(0, μⱼ + ρgⱼ)² − μⱼ²)` — continuously
 *  differentiable, with the closed-form gradient
 *  `∇f/s + Σ(λₖ + ρhₖ)∇hₖ + Σ max(0, μⱼ + ρgⱼ)∇gⱼ` — then updates `μ ← max(0, μ + ρg)`,
 *  `λ ← λ + ρh`, and multiplies `ρ` by ten when the violation did not fall by a factor of four.
 *
 *  **`s` is the size of `∇f` at the start**, so the penalty meets an objective of unit slope
 *  whatever the units of `f` (a fixed `ρ` against `10⁻⁶·f` left the subproblem flat for
 *  thousands of iterations); the multipliers are scaled back by `s` before anything reads them.
 *
 *  **The inner solve is only a stopping rule, never a certificate**: a projected BFGS on the
 *  compiled functions ([[innerMinimum]]), stopped at a projected gradient that tightens by ten
 *  each round. `minimize` was the first inner solver and failed here: its scale-free
 *  certificate measures progress from its own start, and a round warm-started near the
 *  previous optimum stalls on it. The outer KKT certificate decides; it answers once the
 *  residual reaches the tight target, or passes the certificate and has stopped improving.
 */
private def augLag(sys: NlpSystem, x0: Array[Double], lo: Array[Double], hi: Array[Double],
                   env: Environment): Either[NlpFailure, NlpResult] =
  val tol    = certificateTolerance(env)
  val target = math.min(1e-10, tol / 100)
  val fScale = sys.at(x0).map(_.grad.map(math.abs).maxOption.getOrElse(0.0))
                 .filter(s => s > 0.0 && !s.isInfinite).getOrElse(1.0)

  /** One round from `x` with the scaled multipliers `mu`, `lam`; `previous` is the last round's
   *  violation and `lastResidual` its KKT residual. */
  @tailrec
  def round(x: Array[Double], mu: Array[Double], lam: Array[Double], rho: Double, previous: Double,
            lastResidual: Double, k: Int): Either[NlpFailure, NlpResult] =
    val omega = math.max(1e-13, math.pow(0.1, k))
    innerMinimum(sys, x, lo, hi, fScale, mu, lam, rho, omega).flatMap(sys.at) match
      case None => Left(NlpFailure.NotEvaluable)
      case Some(p) =>
        val mu1  = Array.tabulate(sys.mg)(j => math.max(0.0, mu(j) + rho * p.g(j)))
        val lam1 = Array.tabulate(sys.mh)(k => lam(k) + rho * p.h(k))
        val (muF, lamF) = (mu1.map(_ * fScale), lam1.map(_ * fScale))
        val res       = kktResidual(sys, p, muF, lamF, lo, hi)
        val violation = primalViolation(sys, p)
        def answer    = Right(nlpAnswer(p, k, res, muF, lamF, lo, hi))
        if res <= target || (res <= tol && res > 0.5 * lastResidual) then answer
        else if k >= MaxAugLagRounds then
          if res <= tol then answer else Left(NlpFailure.MaxIterations(MaxAugLagRounds, res))
        else if rho >= MaxNlpPenalty && violation > tol then Left(NlpFailure.Infeasible(k))
        else
          val rho1 = if violation > 0.25 * previous then math.min(rho * 10, MaxNlpPenalty) else rho
          round(p.x, mu1, lam1, rho1, violation, res, k + 1)

  round(x0, Array.fill(sys.mg)(0.0), Array.fill(sys.mh)(0.0), 10.0, Double.PositiveInfinity,
        Double.PositiveInfinity, 1)

/** The inner iteration cap of the augmented Lagrangian: reaching it ends the round, it does not
 *  fail it — the outer certificate decides. */
private val MaxInnerIterations: Int = 500

/** A minimiser of the augmented Lagrangian under the bounds, to projected gradient `omega`: a
 *  projected BFGS with an Armijo search along the projected path, the inverse Hessian reset
 *  when its direction is not a descent one.  `None` when the functions are not real somewhere
 *  the iteration must evaluate them. */
private def innerMinimum(sys: NlpSystem, x0: Array[Double], lo: Array[Double], hi: Array[Double],
                         fScale: Double, mu: Array[Double], lam: Array[Double], rho: Double,
                         omega: Double): Option[Array[Double]] =
  val (n, mg, mh) = (sys.n, sys.mg, sys.mh)
  def clamp(x: Array[Double]): Array[Double] = Array.tabulate(n)(i => math.min(math.max(x(i), lo(i)), hi(i)))
  def value(x: Array[Double]): Option[Double] = sys.values(x).map { v =>
    var s = v(0) / fScale
    var k = 0
    while k < mh do
      val hk = v(1 + mg + k)
      s += lam(k) * hk + 0.5 * rho * hk * hk
      k += 1
    var j = 0
    while j < mg do
      val t = math.max(0.0, mu(j) + rho * v(1 + j))
      s += (t * t - mu(j) * mu(j)) / (2 * rho)
      j += 1
    s
  }
  def gradient(x: Array[Double]): Option[Array[Double]] =
    for v <- sys.values(x); d <- sys.derivatives(x) yield
      val out = Array.tabulate(n)(i => d(i) / fScale)
      var j = 0
      while j < mg do
        val w = math.max(0.0, mu(j) + rho * v(1 + j))
        var i = 0
        while i < n do { out(i) += w * d(n + j * n + i); i += 1 }
        j += 1
      var k = 0
      while k < mh do
        val w = lam(k) + rho * v(1 + mg + k)
        var i = 0
        while i < n do { out(i) += w * d(n + mg * n + k * n + i); i += 1 }
        k += 1
      out
  /** The gradient with the components at an active bound, pushing outward, removed. */
  def projected(x: Array[Double], g: Array[Double]): Array[Double] =
    Array.tabulate(n)(i => if (x(i) <= lo(i) && g(i) > 0.0) || (x(i) >= hi(i) && g(i) < 0.0) then 0.0 else g(i))
  def unit: Array[Array[Double]] = Array.tabulate(n, n)((i, j) => if i == j then 1.0 else 0.0)

  @tailrec
  def loop(x: Array[Double], fx: Double, gx: Array[Double], hInv: Array[Array[Double]], fresh: Boolean,
           it: Int): Option[Array[Double]] =
    val pg = projected(x, gx)
    if it >= MaxInnerIterations || pg.forall(c => math.abs(c) <= omega) then Some(x)
    else
      // Bertsekas' ε-active set: a coordinate within ε of a bound it is pushed against takes a
      // plain gradient step, which the projection then lands exactly on the bound. Without it
      // the quasi-Newton step creeps towards the bound and never reaches it, and the outer
      // certificate cannot read that bound's multiplier.
      val proj  = clamp(Array.tabulate(n)(k => x(k) - gx(k)))     // once, not once per coordinate (F_0108)
      val width = (0 until n).map(i => math.abs(proj(i) - x(i))).max
      val eps   = math.min(1e-3, width)
      val near  = Array.tabulate(n)(i => (x(i) - lo(i) <= eps && gx(i) > 0.0) || (hi(i) - x(i) <= eps && gx(i) < 0.0))
      val free  = Array.tabulate(n)(i => if near(i) then 0.0 else pg(i))
      val quasi = Array.tabulate(n)(i => if near(i) then -gx(i) else -nlpDot(hInv(i), free))
      val descent = nlpDot(quasi, gx) < 0.0
      val d       = if descent then quasi else pg.map(-_)
      val step = Iterator.iterate(1.0)(_ / 2).takeWhile(_ >= math.pow(2, -40))
        .map(alpha => clamp(Array.tabulate(n)(i => x(i) + alpha * d(i))))
        .map(xn => (xn, value(xn)))
        .find { case (xn, v) => v.exists(fn => fn <= fx + 1e-4 * nlpDot(gx, Array.tabulate(n)(i => xn(i) - x(i)))) }
      step match
        case None => Some(x)
        case Some((xn, v)) =>
          if (0 until n).forall(i => xn(i) == x(i)) then Some(x)
          else gradient(xn) match
            case None => None
            case Some(gn) =>
              val s  = Array.tabulate(n)(i => xn(i) - x(i))
              val y  = Array.tabulate(n)(i => gn(i) - gx(i))
              val sy = nlpDot(s, y)
              val base = if descent then hInv else unit
              val next =
                if !(sy > 1e-12 * math.sqrt(nlpDot(s, s) * nlpDot(y, y))) then base
                else
                  // The first update rescales the identity by sᵀy / yᵀy, the curvature seen.
                  val h0 = if fresh || !descent then Array.tabulate(n, n)((i, j) => if i == j then sy / nlpDot(y, y) else 0.0) else base
                  val r  = 1.0 / sy
                  val hy = Array.tabulate(n)(i => nlpDot(h0(i), y))
                  val yhy = nlpDot(y, hy)
                  Array.tabulate(n, n)((i, j) =>
                    h0(i)(j) - r * (s(i) * hy(j) + hy(i) * s(j)) + (r * r * yhy + r) * s(i) * s(j))
              loop(xn, v.getOrElse(fx), gn, next, false, it + 1)

  val start = clamp(x0)
  for fx <- value(start); gx <- gradient(start); x <- loop(start, fx, gx, unit, true, 0) yield x

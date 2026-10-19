package it.grypho.scala.leonardo
package optimize

import core.*
import scalar.*
import matrix.{_Matrix, numericalRank, rankTolerance}
import vector._Jacobian


/** Why [[lsqfit]] or [[curveFit]] declined (F_0052 Decision A: a reason at the Scala level,
 *  which the REPL appends as a note).  The node itself stays symbolic. */
enum LsqFailure:
  /** The problem as written is malformed; `detail` says how. */
  case InvalidInput(detail: String)
  /** The residual or its Jacobian is not real at the starting point. */
  case NotEvaluable
  /** The Jacobian has numerical rank `rank` of `params` at the solution (Decision B): the
   *  minimiser is a continuum, any point returned would be arbitrary. */
  case NotIdentifiable(rank: Int, params: Int)
  /** [[MaxLsqIterations]] was reached; `cosine` is the certificate's measure that was left. */
  case MaxIterations(limit: Int, cosine: Double)
  /** Progress stopped short of the certificate; `cosine` is what was left. */
  case Stalled(cosine: Double)
  /** `rows` residuals in `params` parameters exceed the work cap. */
  case TooLarge(rows: Int, params: Int)

/** A certified least-squares fit.
 *
 *  @param p            the parameters
 *  @param residualNorm `‖r(p)‖`
 *  @param iterations   how many iterations the solver took
 *  @param covariance   `σ²·(RᵀR)⁻¹`, `σ² = ‖r‖²/(m − n)`; absent when `m = n` (no residual degrees of
 *                      freedom) or when a bound is active (the linearised Gaussian approximation
 *                      does not hold at a boundary)
 *  @param active       the indices of the parameters held at a bound, ascending
 */
case class LsqResult(p: Vector[Double], residualNorm: Double, iterations: Int,
                     covariance: Option[_MatrixValue], active: Vector[Int])


/** The iteration cap of [[lsqfit]] and [[curveFit]] (F_0063), rejected steps included.
 *
 *  **Measured**: `a·e^(b·t)` on exact data converges in 8 iterations from `(1, 0)` and in 22–36
 *  from `(0.1, −3)`, `(0.01, 0.01)` and `(10, 1)`; Rosenbrock's residual `[[10(b − a²)], [1 − a]]`
 *  in 21 from `(−1.2, 1)`.  100 is about three times the slowest converging case.
 */
val MaxLsqIterations: Int = Limit.MaxLsqIterations.default.toInt

/** The cap on one iteration's work, `m·n²` for `m` residuals in `n` parameters (F_0063).
 *
 *  **Measured on Node, the slowest platform** (the F_0077 rule): the curve fit at `m = 10⁵`,
 *  `n = 2` (`4·10⁵`) takes 2.2 s for its 8 iterations there against 0.68 s on the JVM — the
 *  F_0060 figure for a fit at its cap; `m = 10⁶` took 29 s on Node.  An iteration costs about
 *  0.28 s at the cap on Node, so a fit that runs to [[MaxLsqIterations]] there is the worst case,
 *  ~28 s.
 */
private[leonardo] val MaxLsqWork: Long = Limit.MaxLsqWork.default


/** Minimises `‖r(p)‖²` for an `m×1` residual column `r`, optionally inside `lb ≤ p ≤ ub`
 *  (F_0063, MPC T2-05).
 *
 *  **Levenberg–Marquardt with Moré's scaling**, the step solved by QR of the augmented
 *  `[J·D⁻¹; √λ·I]` — never `JᵀJ`, which squares the condition number (the conditioning rule,
 *  and F_0062's ridge shape).  `D = diag(‖Jⱼ‖)` (a running maximum) makes the step invariant to
 *  the units of each parameter, where plain damping `λI` is a claim about them.  The damping
 *  follows Nielsen's gain-ratio update.  Under bounds the method is projected with an active
 *  set: a parameter on a bound with the gradient pushing outward is held there.
 *
 *  **The certificate always decides** (Decision A): small steps or a stalled damping only *stop*
 *  the iteration, and the point is answered only if every free Jacobian column is orthogonal to
 *  the residual within the precision's tolerance — `|Jⱼᵀr| ≤ 0.5·10⁻ᵖ·‖Jⱼ‖·‖r‖`, a cosine,
 *  invariant to rescaling `r` and any parameter — or the residual is at the rounding level of
 *  the terms it cancels (an exact fit, where the cosine is noise).  **A rank-deficient Jacobian
 *  at the answer declines** (Decision B).
 *
 *  `r` is evaluated with the parameters renamed to fresh names, so a session binding of a
 *  parameter's name cannot fold a cell to a constant.
 *
 *  @param r      the residual: an `m×1` column of expressions, or one expression
 *  @param params the parameters, in the order of `p0`
 *  @param p0     the starting point
 *  @param bounds lower and upper bounds, entries may be infinite; `None` when unbounded
 *  @param env    bindings and precision
 *  @return the certified fit, or why there is none
 */
def lsqfit(r: _Expression, params: Vector[_Variable], p0: Vector[Double],
           bounds: Option[(Vector[Double], Vector[Double])], env: Environment): Either[LsqFailure, LsqResult] =
  for
    box <- checked(params, p0, bounds)
    res <- residualSystem(r, params, env)
    fit <- levenbergMarquardt(res, p0, box, env)
  yield fit

/** Fits `model` to data `(tₖ, yₖ)` by minimising `Σ (yₖ − model(tₖ, p))²` — the curve-fit form
 *  of [[lsqfit]] (F_0063 Decision D), with the same solver and certificate.
 *
 *  The model is compiled once over `t` and the parameters, so the residual and its Jacobian are
 *  evaluated sample by sample rather than built as `m` trees.
 *
 *  @param model  the model, an expression in `t` and the parameters
 *  @param t      the independent variable
 *  @param tdata  the values of `t`
 *  @param ydata  the observations, one per value of `t`
 *  @param params the parameters, in the order of `p0`
 *  @param p0     the starting point
 *  @param bounds lower and upper bounds, entries may be infinite; `None` when unbounded
 *  @param env    bindings and precision
 *  @return the certified fit, or why there is none
 */
def curveFit(model: _Expression, t: _Variable, tdata: Vector[Double], ydata: Vector[Double],
             params: Vector[_Variable], p0: Vector[Double],
             bounds: Option[(Vector[Double], Vector[Double])], env: Environment): Either[LsqFailure, LsqResult] =
  for
    // Before `checked`, which sees `t` prepended and so never finds the list empty (F_0110).
    _   <- Either.cond(params.nonEmpty, (), LsqFailure.InvalidInput("there must be at least one parameter"))
    box <- checked(t +: params, 0.0 +: p0, bounds.map((l, h) => (Double.NegativeInfinity +: l, Double.PositiveInfinity +: h)))
             .map((l, h) => (l.tail, h.tail))
    _   <- Either.cond(tdata.size == ydata.size && tdata.nonEmpty, (),
                       LsqFailure.InvalidInput(s"t has ${tdata.size} values and y ${ydata.size}; they must agree"))
    _   <- Either.cond((tdata ++ ydata).forall(finite), (), LsqFailure.InvalidInput("the data must be finite"))
    res <- curveSystem(model, t, tdata, ydata, params, env)
    fit <- levenbergMarquardt(res, p0, box, env)
  yield fit


// ── input ──

private def finite(d: Double): Boolean = d.isFinite

/** The bounds as arrays, infinite where absent, or why the input is malformed. */
private def checked(vars: Vector[_Variable], p0: Vector[Double], bounds: Option[(Vector[Double], Vector[Double])])
    : Either[LsqFailure, (Vector[Double], Vector[Double])] =
  val n = vars.size
  val (lo, hi) = bounds.getOrElse((Vector.fill(n)(Double.NegativeInfinity), Vector.fill(n)(Double.PositiveInfinity)))
  val bad =
    if n == 0 then Some("there must be at least one parameter")
    else if !distinctVariables(vars) then Some("the names must be distinct")
    else if p0.size != n then Some(s"the starting point has ${p0.size} entries for $n names")
    else if !p0.forall(finite) then Some("the starting point must be finite")
    else if lo.size != n || hi.size != n then Some(s"the bounds must have $n entries each")
    else if lo.zip(hi).exists((a, b) => a.isNaN || b.isNaN || a > b) then Some("a lower bound exceeds its upper bound")
    else None
  bad.fold(Right((lo, hi)))(d => Left(LsqFailure.InvalidInput(d)))

/** The free names of `cells` that are neither in `vars` nor bound in `env`. */
private[optimize] def unbound(cells: Seq[_Expression], vars: Vector[_Variable], env: Environment): Set[String] =
  (cells.flatMap(_.freeVars).toSet -- vars.map(_.variable)).filter(env.get(_).isEmpty)


// ── the residual system ──

/** A residual `r : ℝⁿ → ℝᵐ` with its Jacobian (row-major `m×n`) and the size of the terms each
 *  residual cancels, which is what an exact fit's rounding is measured against. */
private final class Residual(val m: Int, val n: Int,
                             val values: Array[Double] => Option[Array[Double]],
                             val jacobian: Array[Double] => Option[Array[Double]],
                             val scale: Array[Double] => Option[Array[Double]])

/** Evaluates `cells` at a point of `vars`: compiled when the table covers them (F_0058), by
 *  tree evaluation otherwise; `None` when a cell is not a finite real. */
private[optimize] def evaluator(cells: Vector[_Expression], vars: Vector[_Variable],
                      env: Environment): Array[Double] => Option[Array[Double]] =
  def ok(a: Array[Double]) = Option.when(a.forall(finite))(a)
  compileMatrix(_Matrix(cells.size, 1, cells), vars, env) match
    case Some(f) => p => ok(f(p))
    case None    => p =>
      val at = vars.indices.foldLeft(env)((e, i) => e.withBinding(vars(i).variable, _Number(p(i))))
      sequence(cells.toList.map(_.eval(at) match
        case Right(_Number(d)) => Some(d)
        case _                 => None)).map(_.toArray).flatMap(ok)

/** The cells of an evaluated column (or of one expression, read as a single residual). */
private def columnCells(e: _Expression): Option[Vector[_Expression]] = e match
  case m: _MatrixShaped => Option.when(m.cols == 1)(m.children.toVector)
  case d: _MatrixValue  => Option.when(d.cols == 1)(d.toVector.map(_Number(_)))
  case other            => Some(Vector(other))

/** The cells of an evaluated `rows×cols` matrix, row-major. */
private[optimize] def gridCells(e: _Expression, rows: Int, cols: Int): Option[Vector[_Expression]] = e match
  case m: _MatrixShaped if m.rows == rows && m.cols == cols => Some(m.children.toVector)
  case d: _MatrixValue if d.rows == rows && d.cols == cols  => Some(d.toVector.map(_Number(_)))
  case _                                                     => None

/** The additive terms of `e`. */
private[optimize] def summands(e: _Expression): List[_Expression] = e match
  case Sum(a, b) => summands(a) ++ summands(b)
  case other     => List(other)

/** For each cell, `Σ|t|` over its terms: the size of what cancels when the cell is zero, which a
 *  residual's rounding is measured against — `lsqfit`'s exact fit and `nlp`'s KKT certificate.
 *  One definition for both (F_0108), which had a `while` loop and a boxed `map(…).sum` apiece.
 *
 *  `split` is how a cell is cut into terms, and the two callers differ with no recorded reason:
 *  `nlp` expands first (`summands(expand(c))`), `lsqfit` reads the terms as written. Unifying them
 *  would move `lsqfit`'s exact-fit threshold, so the difference is kept as found and only the
 *  kernel is shared.
 *
 *  @param cells the cells, possibly none
 *  @param vars  the variables they are evaluated over
 *  @param env   bindings and precision
 *  @param split a cell's terms
 *  @return the per-cell sums at a point, or `None` where a term is not a finite real */
private[optimize] def termSums(cells: Vector[_Expression], vars: Vector[_Variable], env: Environment,
                               split: _Expression => List[_Expression]): Array[Double] => Option[Array[Double]] =
  if cells.isEmpty then _ => Some(Array.empty[Double])
  else
    val terms  = cells.map(split)
    val starts = terms.scanLeft(0)(_ + _.size).toArray
    val at     = evaluator(terms.flatten, vars, env)
    x => at(x).map { v =>
      Array.tabulate(cells.size) { k =>
        var s = 0.0
        var i = starts(k)
        while i < starts(k + 1) do
          s += math.abs(v(i))
          i += 1
        s
      }
    }

/** The residual form: `r` renamed to fresh parameter names, evaluated, and differentiated by the
 *  library's one Jacobian ([[vector._Jacobian]]). */
private def residualSystem(r: _Expression, params: Vector[_Variable], env: Environment): Either[LsqFailure, Residual] =
  val fresh   = freshNames(params.size, "__p", r.freeVars ++ params.map(_.variable)).toVector
  val renamed = substitute(r, params.map(_.variable).zip(fresh).toMap)
  columnCells(renamed.eval(env).toExpression) match
    case None => Left(LsqFailure.InvalidInput("the residual must be a column, m×1"))
    case Some(cells) =>
      val names = unbound(cells, fresh, env)
      if names.nonEmpty then Left(LsqFailure.InvalidInput(s"the residual has free names that are not parameters: ${names.toList.sorted.mkString(", ")}"))
      else
        val (m, n) = (cells.size, params.size)
        gridCells(_Jacobian(_Matrix(m, 1, cells), fresh).eval(env).toExpression, m, n) match
          case None => Left(LsqFailure.NotEvaluable)
          case Some(jac) =>
            Right(Residual(m, n, evaluator(cells, fresh, env), evaluator(jac, fresh, env),
                           termSums(cells, fresh, env, summands)))

/** The curve-fit form: `rₖ = yₖ − model(tₖ, p)`, the model and its gradient compiled once. */
private def curveSystem(model: _Expression, t: _Variable, tdata: Vector[Double], ydata: Vector[Double],
                        params: Vector[_Variable], env: Environment): Either[LsqFailure, Residual] =
  val all     = t +: params
  val fresh   = freshNames(all.size, "__p", model.freeVars ++ all.map(_.variable)).toVector
  val renamed = substitute(model, all.map(_.variable).zip(fresh).toMap)
  val names   = unbound(Vector(renamed), fresh, env)
  if names.nonEmpty then Left(LsqFailure.InvalidInput(s"the model has free names that are neither t nor a parameter: ${names.toList.sorted.mkString(", ")}"))
  else gridCells(_Jacobian(_Matrix(1, 1, Vector(renamed)), fresh.tail).eval(env).toExpression, 1, params.size) match
    case None => Left(LsqFailure.NotEvaluable)
    case Some(grad) =>
      val (m, n) = (tdata.size, params.size)
      val f = evaluator(Vector(renamed), fresh, env)
      val g = evaluator(grad, fresh, env)
      def each[A](p: Array[Double], h: Array[Double] => Option[A]): Option[Vector[A]] =
        sequence(tdata.toList.map(tk => h(tk +: p))).map(_.toVector)
      Right(Residual(m, n,
        p => each(p, f).map(fs => Array.tabulate(m)(k => ydata(k) - fs(k)(0))),
        p => each(p, g).map(gs => Array.tabulate(m * n)(i => -gs(i / n)(i % n))),
        p => each(p, f).map(fs => Array.tabulate(m)(k => math.abs(ydata(k)) + math.abs(fs(k)(0))))))

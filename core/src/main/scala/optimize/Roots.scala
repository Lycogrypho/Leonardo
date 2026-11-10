package it.grypho.scala.leonardo
package optimize

import core.*
import scalar.expand
import matrix.{_Matrix, numericalRank, rankTolerance}


/** Why [[nsolve]] declined (F_0319, APAC L_0011).  The node stays symbolic; the REPL appends why. */
enum RootFailure:
  /** The problem as written is malformed; `detail` says how. */
  case InvalidInput(detail: String)
  /** `F`, its Jacobian or the equations' scales are not real at the starting point. */
  case NotEvaluable
  /** The Jacobian at the point has numerical rank `rank` of `n`: the root is not isolated, or the point
   *  is not one.  Declined rather than answered, since any point of a continuum would be arbitrary (the
   *  `lsqfit` NotIdentifiable rule). */
  case SingularJacobian(rank: Int, n: Int)
  /** The iteration stopped at a stationary point of `|F|^2` that is not a root; `residual` is `|F|`
   *  there.  Exactly the point a least-squares fit would have certified. */
  case NotARoot(residual: Double)
  /** [[MaxNsolveIterations]] was reached; `residual` is `|F|` at the last point. */
  case MaxIterations(limit: Int, residual: Double)
  /** `n` unknowns exceed the work cap. */
  case TooLarge(n: Int)

/** A certified root of a square nonlinear system (F_0319).
 *
 *  @param x          the root
 *  @param residual   `|F(x)|`, the Euclidean norm
 *  @param iterations how many dogleg steps the solver took, rejected ones included
 *  @param active     the indices of the unknowns held at a bound, ascending
 */
final case class RootResult(x: Vector[Double], residual: Double, iterations: Int, active: Vector[Int])

/** The iteration cap of [[nsolve]] (F_0319); the measurement is on [[core.Limit.MaxNsolveIterations]]. */
val MaxNsolveIterations: Int = Limit.MaxNsolveIterations.default.toInt

/** The tight stopping target on each equation relative to its terms, far below any precision's (the
 *  `LsqTarget` arrangement): the iteration aims well past the contract, and the certificate then
 *  decides -- so a root is answered to the digits Newton's quadratic convergence gives for free, not to
 *  the displayed ones.  At the precision's tolerance alone `nsolve([[x^2 + y^2 - 4], [x - y]], ...)`
 *  printed `1.41422`, a certified root whose fifth decimal was off. */
private val RootTarget: Double = 1e-10


/** Solves the square nonlinear system `F(x) = 0`, `F` an `n x 1` column of expressions in the `n`
 *  unknowns, from `x0`, optionally inside `lb <= x <= ub` (F_0319, APAC L_0011).
 *
 *  **Powell's dogleg trust region with More's column scaling** (Decision A, MINPACK's `hybrj`): the
 *  Newton step `J p = -F` when it fits the region, the scaled steepest-descent step when the region is
 *  smaller than that, and the dogleg between the two otherwise, the region grown or shrunk by the
 *  ratio of the actual to the predicted decrease of `|F|^2`.  The Jacobian is the symbolic one
 *  (`vector._Jacobian`, one definition), compiled once with `F` -- `lsqfit`'s `Residual`.  Under
 *  bounds the method is projected with an active set: an unknown on a bound with the gradient of
 *  `|F|^2` pushing outward is held there.
 *
 *  **The certificate is a root, not a minimum** (the units rule): every equation must vanish against
 *  the size of its own terms, `|F_i(x)| <= 0.5 * 10^-p * sum_j |t_ij(x)|`, so the verdict does not move
 *  when an equation or an unknown is rescaled; and the Jacobian at the root must have full numerical
 *  rank (the column-normalised one, at `rankTolerance`), so an isolated root is answered and a
 *  continuum declined.  A stationary point of `|F|^2` that is not a root -- what a least-squares fit
 *  certifies -- is declined as [[RootFailure.NotARoot]].  A start already at the root answers in no
 *  iterations, which is what a warm start from the previous step's solution wants.
 *
 *  @param f      the system: an `n x 1` column of expressions, each meaning `F_i = 0`, or one expression
 *  @param vars   the unknowns, in the order of `x0`
 *  @param x0     the starting point
 *  @param bounds lower and upper bounds, entries may be infinite; `None` when unbounded
 *  @param env    bindings and precision
 *  @return the certified root, or why there is none
 */
def nsolve(f: _Expression, vars: Vector[_Variable], x0: Vector[Double],
           bounds: Option[(Vector[Double], Vector[Double])], env: Environment): Either[RootFailure, RootResult] =
  for
    box  <- checked(vars, x0, bounds).left.map(fromLsq)
    // the terms of each equation after expansion (nlp's reading): a product's one term is the
    // residual itself, and a root could never certify against it
    res  <- residualSystem(f, vars, env, c => summands(expand(c))).left.map(fromLsq)
    _    <- Either.cond(res.m == res.n, (),
                        RootFailure.InvalidInput(s"a square system is needed: ${res.m} equations for ${res.n} unknowns"))
    root <- dogleg(res, x0, box, env)
  yield root

/** [[nsolve]] over numeric callbacks (Decision B): `f` the system, `jacobian` its `n x n` Jacobian
 *  row-major, and `scale` **each equation's term size** at a point -- what the expression form
 *  computes for itself, and what the root certificate measures each `F_i` against, since a callback
 *  carries no terms of its own.  A non-finite or wrongly sized array from any of the three is read as
 *  "not evaluable" there.
 *
 *  @param f        the system, `n` values
 *  @param jacobian the Jacobian, `n * n` values row-major
 *  @param scale    the size of each equation's terms, `n` values, each `>= |f_i|` where the equation
 *                  is a sum of terms
 *  @param x0       the starting point
 *  @param lo       lower bounds, `-inf` for none
 *  @param hi       upper bounds, `+inf` for none
 *  @param env      precision and budgets
 *  @return the certified root, or why there is none
 */
def nsolve(f: Array[Double] => Array[Double], jacobian: Array[Double] => Array[Double],
           scale: Array[Double] => Array[Double], x0: Vector[Double], lo: Vector[Double], hi: Vector[Double],
           env: Environment): Either[RootFailure, RootResult] =
  val n = x0.size
  def finiteOf(a: Array[Double], len: Int): Option[Array[Double]] =
    Option(a).filter(xs => xs.length == len && xs.forall(_.isFinite))
  val res = Residual(n, n, p => finiteOf(f(p), n), p => finiteOf(jacobian(p), n * n), p => finiteOf(scale(p), n))
  val bad =
    if n == 0 then Some("there must be at least one unknown")
    else if lo.size != n || hi.size != n then Some(s"the bounds must have $n entries each")
    else if !x0.forall(_.isFinite) then Some("the starting point must be finite")
    else if lo.zip(hi).exists((a, b) => a.isNaN || b.isNaN || a > b) then Some("a lower bound exceeds its upper bound")
    else None
  bad.fold(dogleg(res, x0, (lo, hi), env))(d => Left(RootFailure.InvalidInput(d)))

/** An input failure of the shared residual machinery, in this solver's terms. */
private def fromLsq(why: LsqFailure): RootFailure = why match
  case LsqFailure.InvalidInput(d) => RootFailure.InvalidInput(d)
  case LsqFailure.NotEvaluable    => RootFailure.NotEvaluable
  case other                      => RootFailure.InvalidInput(other.toString)


/** The dogleg iteration over a square [[Residual]], then the root certificate (see [[nsolve]]). */
private def dogleg(res: Residual, x0: Vector[Double], box: (Vector[Double], Vector[Double]),
                   env: Environment): Either[RootFailure, RootResult] =
  import RootFailure.*
  val n        = res.n
  val (lo, hi) = (box._1.toArray, box._2.toArray)
  val tol      = 0.5 * math.pow(10, -env.precision)
  val maxIt    = env.limits.count(Limit.MaxNsolveIterations)

  def clamp(p: Array[Double]): Array[Double] = Array.tabulate(n)(i => math.min(hi(i), math.max(lo(i), p(i))))
  def isActive(x: Array[Double], g: Array[Double], i: Int): Boolean =
    (x(i) <= lo(i) && g(i) > 0) || (x(i) >= hi(i) && g(i) < 0)
  def freeOf(x: Array[Double], g: Array[Double]): Array[Int] = (0 until n).filterNot(isActive(x, g, _)).toArray
  def norm(a: Array[Double]): Double = math.sqrt(sumSq(a))
  def atBound(x: Array[Double]): Vector[Int] = (0 until n).filter(i => x(i) <= lo(i) || x(i) >= hi(i)).toVector

  /** Every equation within `rel` of its own terms: the certificate at `tol`, the stopping target at
   *  the tighter of `tol` and [[RootTarget]]. */
  def within(fv: Array[Double], s: Array[Double], rel: Double): Boolean =
    fv.indices.forall(i => math.abs(fv(i)) <= rel * s(i))
  def certified(fv: Array[Double], s: Array[Double]): Boolean = within(fv, s, tol)
  val target = math.min(tol, RootTarget)

  /** The certificate, then the rank of the column-normalised Jacobian (the `lsqfit` reading: a
   *  parameter's units must not decide). */
  def finish(x: Array[Double], fv: Array[Double], j: Array[Double], k: Int, atCap: Boolean): Either[RootFailure, RootResult] =
    res.scale(x) match
      case None                         => Left(NotEvaluable)
      case Some(s) if !certified(fv, s) => Left(if atCap then MaxIterations(maxIt, norm(fv)) else NotARoot(norm(fv)))
      case Some(_) =>
        val cn = columnNorms(j, n, n)
        val normalised = _MatrixValue(n, n, Array.tabulate(n * n)(i => if cn(i % n) == 0.0 then 0.0 else j(i) / cn(i % n)))
        normalised.svd(env.limits).map((_, sigma, _) => numericalRank(sigma, rankTolerance(env))) match
          case Some(r) if r == n => Right(RootResult(x.toVector, norm(fv), k, atBound(x)))
          case Some(r)           => Left(SingularJacobian(r, n))
          case None              => Left(NotEvaluable)

  /** The Newton step over the free unknowns, `J_F p = -F`, by QR; `None` when `J_F` is rank-deficient. */
  def newton(j: Array[Double], fv: Array[Double], free: Array[Int]): Option[Array[Double]] =
    val nf = free.length
    val a  = Array.tabulate(n * nf)(idx => j((idx / nf) * n + free(idx % nf)))
    _MatrixValue(n, nf, a).qrDecompose.map { (q, rr) =>
      val (qv, rv) = (q.toVector, rr.toVector)
      val qtb = Array.tabulate(nf)(c => (0 until n).map(k => -qv(k * nf + c) * fv(k)).sum)
      val e   = new Array[Double](nf)
      for i <- (0 until nf).reverse do
        e(i) = (qtb(i) - (i + 1 until nf).map(c => rv(i * nf + c) * e(c)).sum) / rv(i * nf + i)
      e
    }

  /** One Newton step past the stopping target, kept only if `|F|` falls (F_0338, the `rootsAtScale` rule):
   *  the target is `10⁻¹⁰` of the terms, one quadratic step short of the rounding level, so `√2` was answered
   *  as Newton's fourth iterate `665857/470832` (`|F| ≈ 4.5·10⁻¹²`).  Not counted as an iteration. */
  def polished(x: Array[Double], fv: Array[Double], j: Array[Double]): (Array[Double], Array[Double], Array[Double]) =
    val step = for
      p  <- newton(j, fv, (0 until n).toArray)
      xn  = clamp(Array.tabulate(n)(i => x(i) + p(i)))
      fn <- res.values(xn)
      if sumSq(fn) < sumSq(fv)
      jn <- res.jacobian(xn)
    yield (xn, fn, jn)
    step.getOrElse((x, fv, j))

  @annotation.tailrec
  def loop(x: Array[Double], fv: Array[Double], j: Array[Double], d: Array[Double],
           delta: Double, k: Int): Either[RootFailure, RootResult] =
    val g    = jtr(j, fv, n, n)
    val free = freeOf(x, g)
    val fNow = 0.5 * sumSq(fv)
    if res.scale(x).exists(within(fv, _, target)) then
      val (xp, fp, jp) = polished(x, fv, j)
      finish(xp, fp, jp, k, atCap = false)
    else if free.isEmpty then finish(x, fv, j, k, atCap = false)
    else if k >= maxIt then finish(x, fv, j, k, atCap = true)
    else
      val nf     = free.length
      // the gradient of |F|^2/2 over the free unknowns in the scaled variables y = D x, and its image
      val gs     = Array.tabulate(nf)(c => g(free(c)) / d(free(c)))
      val gsNorm = norm(gs)
      if gsNorm == 0.0 then finish(x, fv, j, k, atCap = false)
      else
        val jg = Array.tabulate(n) { r =>
          var acc = 0.0
          var c   = 0
          while c < nf do
            acc += j(r * n + free(c)) * gs(c) / d(free(c))
            c += 1
          acc
        }
        val alpha = sumSq(gs) / sumSq(jg)                 // the Cauchy point's step length
        val yc    = gs.map(v => -alpha * v)
        val yn    = newton(j, fv, free).map(p => Array.tabulate(nf)(c => p(c) * d(free(c))))
        // With no Newton step (a singular J), the Cauchy point if it fits the region (F_0333: the guard
        // sent this case to the full radius, an overshoot whose predicted decrease was negative and read
        // as a stall, so a singular start declined without moving), else steepest descent to the edge.
        val y: Array[Double] = yn match
          case Some(yN) if norm(yN) <= delta => yN
          case _ if !alpha.isFinite || norm(yc) >= delta => gs.map(v => -(delta / gsNorm) * v)
          case None => yc
          case Some(yN) =>
            // the dogleg: from the Cauchy point towards the Newton point, as far as the region allows
            val diff = Array.tabulate(nf)(c => yN(c) - yc(c))
            val a    = sumSq(diff)
            val b    = 2 * (0 until nf).map(c => yc(c) * diff(c)).sum
            val cc   = sumSq(yc) - delta * delta
            val tau  = (-b + math.sqrt(math.max(0.0, b * b - 4 * a * cc))) / (2 * a)
            Array.tabulate(nf)(c => yc(c) + tau * diff(c))
        val p  = new Array[Double](n)
        for c <- 0 until nf do p(free(c)) = y(c) / d(free(c))
        val xn   = clamp(Array.tabulate(n)(i => x(i) + p(i)))
        val s    = Array.tabulate(n)(i => xn(i) - x(i))
        val ds   = norm(Array.tabulate(n)(i => d(i) * s(i)))
        val pred = -(0 until n).map(i => g(i) * s(i)).sum - 0.5 * sumSq(jtimes(j, s, n, n))
        // a step that no longer moves the point, or predicts a decrease at rounding level: stalled
        if ds == 0.0 || pred <= 4 * Eps * fNow then finish(x, fv, j, k, atCap = false)
        else
          val trial = for
            fn <- res.values(xn)
            actual = fNow - 0.5 * sumSq(fn)
            if actual / pred >= 1e-4
            jn <- res.jacobian(xn)
          yield (fn, jn, actual / pred)
          trial match
            case None => loop(x, fv, j, d, 0.5 * ds, k + 1)
            case Some((fn, jn, rho)) =>
              val cn   = columnNorms(jn, n, n)
              val dn   = Array.tabulate(n)(i => math.max(d(i), cn(i)))
              val next = if rho > 0.75 then math.max(delta, 2 * ds) else if rho < 0.25 then 0.5 * ds else delta
              loop(xn, fn, jn, dn, next, k + 1)

  if n.toLong * n * n > env.limits(Limit.MaxLsqWork) then Left(TooLarge(n))
  else
    val x = clamp(x0.toArray)
    (for fv <- res.values(x); j <- res.jacobian(x) yield (fv, j)) match
      case None => Left(NotEvaluable)
      case Some((fv, j)) =>
        // More: a column that starts at zero norm is scaled by 1 until it grows
        val d  = columnNorms(j, n, n).map(c => if c > 0 then c else 1.0)
        val dx = norm(Array.tabulate(n)(i => d(i) * x(i)))
        loop(x, fv, j, d, if dx > 0 then 100 * dx else 100.0, 0)


/** `nsolve(F, [[x], [y]], x0[, lb, ub])` -- the root of a square nonlinear system as an `n x 1`
 *  column (F_0319, Decision C).  A cell of `F` is an expression meaning `F_i = 0` or an equation
 *  `l = r`; the unknowns are a column of names, `minimize`'s shape, and binders excluded from
 *  `children`.  Stays symbolic whenever [[nsolve]] declines, and the REPL appends why.
 *
 *  @param f      the system
 *  @param vars   the unknowns, in the order of `x0`
 *  @param x0     the starting point
 *  @param bounds the lower and upper bounds
 */
case class _NSolve(f: _Expression, vars: Vector[_Variable], x0: _Expression,
                   bounds: Option[(_Expression, _Expression)]) extends _Expression:
  override def toString: String =
    s"nsolve($f, ${nameColumn(vars)}, $x0${bounds.fold("")((lo, hi) => s", $lo, $hi")})"
  override def children: List[_Expression] = f :: x0 :: bounds.toList.flatMap((lo, hi) => List(lo, hi))
  override def rebuild(c: List[_Expression]): _Expression =
    _NSolve(c.head, vars, c(1), bounds.map(_ => (c(2), c(3))))

  override def eval(env: Environment): Either[_Expression, _Value] =
    detailed(env).fold(_ => Left(this), r => Right(_MatrixValue(r.x.size, 1, r.x.toArray)))

  private val answers = LastAnswer[Either[RootFailure, RootResult]]()

  /** The solver's full answer, or why there is none; what the REPL queries -- computed once per
   *  environment, so its note reads the solve `eval` ran (F_0212). */
  def detailed(env: Environment): Either[RootFailure, RootResult] = answers(env) {
    Constraints.equalities(f) match
      case None => Left(RootFailure.InvalidInput("every equation must be an expression meaning F = 0, or an equation l = r"))
      case Some(cells) if cells.isEmpty => Left(RootFailure.InvalidInput("there is no equation to solve"))
      case Some(cells) =>
        startAndBounds(x0, bounds, vars.size, env).left.map(fromLsq)
          // F_0339: each equation evaluated bottom-up first, the unknowns unbound
          .flatMap((s, b) => nsolve(_Matrix(cells.size, 1, cells.map(reduced(_, vars, env))), vars, s, b, env))
  }

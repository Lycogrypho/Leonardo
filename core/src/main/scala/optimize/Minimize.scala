package it.grypho.scala.leonardo
package optimize

import core.*
import scalar.*


/** The numeric minimisation methods, always named by the caller (F_0010 slice 5).
 *
 *  **Never a hidden default**, the `c2d` rule: two methods can stop at different points of a
 *  non-convex function, so a silent choice would make two correct-looking answers disagree.
 */
enum MinimizeMethod:
  /** Steepest descent with a backtracking (Armijo) line search; projected under bounds. */
  case GradientDescent
  /** Newton's method on the symbolic Hessian.  Declines where the Hessian is not positive
   *  definite rather than modifying it — a silently regularised Newton step is a different
   *  method wearing Newton's name. */
  case Newton
  /** BFGS with a strong-Wolfe line search. */
  case BFGS
  /** **Projected** BFGS with an active set, for box constraints (F_0010 Decision C).
   *
   *  Named for what it is: at this library's scale — tens of variables — L-BFGS-B's limited
   *  memory buys nothing, and calling this method `lbfgsb` would be the silent-convention
   *  failure the `feedback` sign rule exists to prevent.  Whether the true L-BFGS-B is worth
   *  adding is re-checked after every release (recurrent task R_0069). */
  case ProjectedBFGS

object MinimizeMethod:
  /** The keyword a method is written with. */
  def keyword(m: MinimizeMethod): String = m match
    case GradientDescent => "gd"
    case Newton          => "newton"
    case BFGS            => "bfgs"
    case ProjectedBFGS   => "pbfgs"

  /** Looks a method up by its keyword. */
  def fromKeyword(s: String): Option[MinimizeMethod] = values.find(m => keyword(m) == s)


/** Why [[minimize]] declined (F_0052 Decision A: a reason at the Scala level, which the REPL
 *  appends as a note).  The node itself stays symbolic; this is what explains it. */
enum MinimizeFailure:
  /** The problem as written is malformed; `detail` says how. */
  case InvalidInput(detail: String)
  /** The objective or its gradient does not evaluate to a real number at an iterate. */
  case NotEvaluable
  /** The line search found no acceptable step at this iteration. */
  case LineSearchFailed(iteration: Int)
  /** [[MaxMinimizeIterations]] was reached; the gradient norm is what was left. */
  case MaxIterations(limit: Int, gradientNorm: Double)
  /** Newton's Hessian is not positive definite at this iteration. */
  case HessianNotPositiveDefinite(iteration: Int)
  /** The objective kept decreasing without bound along a search direction. */
  case Unbounded
  /** Progress stopped short of the certificate; `gradientNorm` is what was left. */
  case Stalled(gradientNorm: Double)
  /** A method that takes no bounds was given bounds. */
  case MethodTakesNoBounds(method: MinimizeMethod)

/** A certified minimiser.
 *
 *  @param x            the point
 *  @param value        `f` there
 *  @param iterations   how many iterations the method took
 *  @param gradientNorm `‖∇f‖∞` there, projected under bounds — reported, not the certificate,
 *                      which is the scale-free `stationarity` (F_0095)
 */
case class MinimizeResult(x: Vector[Double], value: Double, iterations: Int, gradientNorm: Double)


/** The iteration cap, shared by every method.
 *
 *  **Measured** (F_0010, JVM), Rosenbrock from `(−1.2, 1)`: BFGS converges in 35 iterations
 *  (25 ms), Newton in 22, projected BFGS in 39 unbounded and 21 with `x ≤ 0.5`; gradient
 *  descent needs 94 on a condition-10 quadratic.  On Rosenbrock gradient descent reaches the
 *  cap after 79 ms with the gradient still at 0.014 and declines — the honest answer for that
 *  method there, so the cap is not raised to flatter it.  2000 is more than fifty times every
 *  converging case.
 */
val MaxMinimizeIterations: Int = 2000


/** Minimises `f` over `vars` from `x0`, optionally inside the box `lb ≤ x ≤ ub`
 *  (F_0010 slice 5).
 *
 *  The gradient is the symbolic derivative, evaluated at each iterate; Newton also uses the
 *  symbolic Hessian.  **Two tolerances on one measure, deliberately**: the iteration runs until
 *  `stationarity` is below `1e-10` or progress stops, and the answer is then **certified**
 *  against the precision's tolerance `0.5·10^-precision` — the number `=` uses (X-02).  So a
 *  well-conditioned problem is answered far more accurately than the certificate demands, and
 *  an answer is never returned that fails it.  The measure is a pure number, so the verdict
 *  does not depend on the units of `f` or of any variable (F_0095).  Under bounds it reads the
 *  *projected* gradient, which is zero at a constrained minimum where the gradient is not.
 *
 *  @param f      the objective
 *  @param vars   the variables, in the order of `x0`
 *  @param x0     the starting point
 *  @param bounds lower and upper bounds, entries may be infinite; `None` when unbounded
 *  @param method the method
 *  @param env    bindings and precision
 *  @return the certified minimiser, or why it was not found
 */
def minimize(f: _Expression, vars: Vector[_Variable], x0: Vector[Double],
             bounds: Option[(Vector[Double], Vector[Double])], method: MinimizeMethod,
             env: Environment): Either[MinimizeFailure, MinimizeResult] =
  val n = vars.size
  val invalid: Option[String] =
    if !distinctVariables(vars) then Some("the variables must be distinct names")
    else if x0.size != n then Some(s"the starting point has ${x0.size} entries for $n variables")
    else if x0.exists(d => d.isNaN || d.isInfinite) then Some("the starting point must be finite")
    else bounds.flatMap { (lo, hi) =>
      if lo.size != n || hi.size != n then Some(s"the bounds must have $n entries each")
      else if lo.zip(hi).exists((a, b) => a.isNaN || b.isNaN || a > b) then
        Some("a lower bound exceeds its upper bound")
      else None
    }
  invalid match
    case Some(detail) => Left(MinimizeFailure.InvalidInput(detail))
    case None =>
      val problem = Problem(f, vars, env)
      (method, bounds) match
        case (MinimizeMethod.Newton | MinimizeMethod.BFGS, Some(_)) =>
          Left(MinimizeFailure.MethodTakesNoBounds(method))
        case (MinimizeMethod.Newton, None) => newton(problem, x0)
        case (MinimizeMethod.BFGS, None)   => bfgs(problem, x0)
        case (m, b) =>
          val (lo, hi) = b.getOrElse((Vector.fill(n)(Double.NegativeInfinity), Vector.fill(n)(Double.PositiveInfinity)))
          boxed(problem, x0, lo, hi, quasiNewton = m == MinimizeMethod.ProjectedBFGS)


/** The objective with its symbolic gradient and Hessian, evaluated on demand. */
private final class Problem(f: _Expression, vars: Vector[_Variable], env: Environment):
  val n: Int = vars.size
  private val gradient: Vector[_Expression] = vars.map(v => simplifyFully(derive(f, v)))
  private lazy val hessian: Option[Vector[_Expression]] = hessianCells(f, vars, env)

  /** The certificate's bound on `stationarity`: the number `=` uses. */
  val tolerance: Double = 0.5 * math.pow(10, -env.precision)

  private def bound(x: Vector[Double]): Environment =
    vars.zip(x).foldLeft(env) { case (acc, (v, d)) => acc.withBinding(v.variable, _Number(d)) }

  private def real(e: _Expression, at: Environment): Option[Double] = e.eval(at) match
    case Right(_Number(d)) if !d.isNaN && !d.isInfinite => Some(d)
    case _                                               => None

  def value(x: Vector[Double]): Option[Double] = real(f, bound(x))

  def grad(x: Vector[Double]): Option[Vector[Double]] =
    val at = bound(x)
    sequence(gradient.toList.map(real(_, at))).map(_.toVector)

  def hess(x: Vector[Double]): Option[Array[Double]] =
    val at = bound(x)
    hessian.flatMap(cells => sequence(cells.toList.map(real(_, at)))).map(_.toArray)


// ── vector helpers ──

private def dot(a: Vector[Double], b: Vector[Double]): Double = a.lazyZip(b).map(_ * _).sum
private def axpy(a: Double, x: Vector[Double], y: Vector[Double]): Vector[Double] = x.lazyZip(y).map(a * _ + _)
private def normInf(a: Vector[Double]): Double = a.map(math.abs).maxOption.getOrElse(0.0)

/** How far `x` is from stationary, as a pure number (F_0095): the first-order change the
 *  gradient still predicts over the displacement from the start, relative to the decrease
 *  achieved, `Σ |gᵢ|·|xᵢ − x0ᵢ| / (f(x0) − f(x))`.
 *
 *  It is about 1 while `f` still behaves linearly along the path and falls to 0 at a stationary
 *  point; on a quadratic reached from a distance `D` it is `2ε/D` for an error `ε`, so the
 *  certificate asks for the minimiser to a relative accuracy of the distance travelled.  **It
 *  is invariant under `f → k·f + c` and under rescaling or translating any variable**, which an
 *  absolute bound on `‖∇f‖` is not: that one certified the *starting point* of
 *  `1e-10·(x − 3)²`, whose gradient there is already below `5·10⁻⁶`, and could refuse a
 *  function in large units at its minimum (the units bug of 2.11, F_0075 and F_0091).  The
 *  relative-gradient test of the references (Dennis–Schnabel) needs a typical size of `x` and of
 *  `f` from the caller, and `max(1, |x|)` in place of one is itself a claim about the units;
 *  here both scales are *measured* — a displacement is in the units of `x` and a decrease in
 *  those of `f`, whatever they are.
 *
 *  A direction the gradient still points along with no displacement, or a point with no
 *  decrease, has no scale to be judged on and is not stationary (`∞`), unless the gradient
 *  vanishes exactly.  The price, accepted: a start already at the minimiser to rounding, whose
 *  gradient is noise rather than zero, may decline instead of being answered.
 */
private def stationarity(g: Vector[Double], x: Vector[Double], x0: Vector[Double],
                         f0: Double, fx: Double): Double =
  val decrease = f0 - fx
  val moved    = g.indices.map(i => math.abs(x(i) - x0(i)))
  if g.forall(_ == 0.0) then 0.0
  else if !(decrease > 0.0) || g.indices.exists(i => g(i) != 0.0 && moved(i) == 0.0) then
    Double.PositiveInfinity
  else g.indices.map(i => math.abs(g(i)) * moved(i)).sum / decrease

/** The tight stopping target on `stationarity`: far below any precision's certificate. */
private val Target: Double = 1e-10

/** The value below which the objective is taken to be unbounded — far beyond any value a
 *  bounded problem posed in this language reaches, and still well inside `Double`'s range. */
private val UnboundedBelow: Double = -1e100

/** Answers `x` if its `stationarity` `s` passes the certificate, else the stall it is.
 *  `g` is the (projected) gradient, reported beside the answer. */
private def certify(p: Problem, s: Double, x: Vector[Double], fx: Double, g: Vector[Double],
                    k: Int): Either[MinimizeFailure, MinimizeResult] =
  if s <= p.tolerance then Right(MinimizeResult(x, fx, k, normInf(g)))
  else Left(MinimizeFailure.Stalled(normInf(g)))

/** At the iteration cap: the certificate still decides.  The tight test is a target, not the
 *  contract — a point that passes the precision's certificate is answered however it was
 *  reached, and one that does not is the iteration limit. */
private def atCap(p: Problem, s: Double, x: Vector[Double], fx: Double,
                  g: Vector[Double]): Either[MinimizeFailure, MinimizeResult] =
  if s <= p.tolerance then Right(MinimizeResult(x, fx, MaxMinimizeIterations, normInf(g)))
  else Left(MinimizeFailure.MaxIterations(MaxMinimizeIterations, normInf(g)))

/** Whether the step made no measurable progress, in the point and in the value. */
private def stagnant(x: Vector[Double], xn: Vector[Double], fx: Double, fn: Double): Boolean =
  normInf(x.lazyZip(xn).map(_ - _)) <= 1e-15 * math.max(1.0, normInf(x)) &&
  math.abs(fx - fn) <= 1e-15 * math.max(1.0, math.abs(fx))


/** Newton's method with a backtracking line search from the full step. */
private def newton(p: Problem, x0: Vector[Double]): Either[MinimizeFailure, MinimizeResult] =
  def run(f0: Double): Either[MinimizeFailure, MinimizeResult] =
    @annotation.tailrec
    def loop(x: Vector[Double], fx: Double, k: Int): Either[MinimizeFailure, MinimizeResult] =
      p.grad(x) match
        case None => Left(MinimizeFailure.NotEvaluable)
        case Some(g) =>
          val s = stationarity(g, x, x0, f0, fx)
          if s <= Target then certify(p, s, x, fx, g, k)
          else if k >= MaxMinimizeIterations then atCap(p, s, x, fx, g)
          else p.hess(x).flatMap(h => choleskySolve(h, p.n, g)) match
            case None => Left(MinimizeFailure.HessianNotPositiveDefinite(k))
            case Some(step) =>
              val d = step.map(-_)
              armijo(p, x, fx, g, d, x => x) match
                case Left(fail)      => Left(fail(k))
                case Right((xn, fn)) =>
                  if !stagnant(x, xn, fx, fn) then loop(xn, fn, k + 1)
                  else p.grad(xn) match
                    case None     => Left(MinimizeFailure.NotEvaluable)
                    case Some(gn) => certify(p, stationarity(gn, xn, x0, f0, fn), xn, fn, gn, k + 1)
    loop(x0, f0, 0)
  p.value(x0).fold(Left(MinimizeFailure.NotEvaluable))(run)

/** BFGS on the inverse Hessian, with the strong-Wolfe line search that keeps it positive
 *  definite (`sᵀy > 0` is exactly the curvature condition). */
private def bfgs(p: Problem, x0: Vector[Double]): Either[MinimizeFailure, MinimizeResult] =
  val n = p.n
  def identity: Vector[Vector[Double]] = Vector.tabulate(n, n)((i, j) => if i == j then 1.0 else 0.0)

  @annotation.tailrec
  def loop(x: Vector[Double], fx: Double, g: Vector[Double], h: Vector[Vector[Double]],
           k: Int, f0: Double): Either[MinimizeFailure, MinimizeResult] =
    val st = stationarity(g, x, x0, f0, fx)
    if st <= Target then certify(p, st, x, fx, g, k)
    else if k >= MaxMinimizeIterations then atCap(p, st, x, fx, g)
    else
      val d0 = h.map(row => -dot(row, g))
      // A direction that does not descend means the approximation has gone bad: restart it.
      val (d, hk) = if dot(d0, g) < 0 then (d0, h) else (g.map(-_), identity)
      wolfe(p, x, fx, g, d) match
        case Left(fail) => Left(fail(k))
        case Right((alpha, xn, fn, gnew)) =>
          if stagnant(x, xn, fx, fn) then certify(p, stationarity(gnew, xn, x0, f0, fn), xn, fn, gnew, k + 1)
          else
            val s  = d.map(_ * alpha)
            val y  = gnew.lazyZip(g).map(_ - _)
            val sy = dot(s, y)
            val hn =
              if sy <= 1e-12 * math.sqrt(dot(s, s) * dot(y, y)) then hk
              else
                // The first step rescales the identity to the curvature just measured
                // (Nocedal & Wright 6.20), which spares a poorly scaled start most of its cost.
                val h0 = if k == 0 then hk.map(_.map(_ * sy / dot(y, y))) else hk
                bfgsUpdate(h0, s, y, sy)
            loop(xn, fn, gnew, hn, k + 1, f0)

  (for f0 <- p.value(x0); g0 <- p.grad(x0) yield (f0, g0)) match
    case None           => Left(MinimizeFailure.NotEvaluable)
    case Some((f0, g0)) => loop(x0, f0, g0, identity, 0, f0)

/** `H' = (I − ρsyᵀ) H (I − ρysᵀ) + ρssᵀ`, `ρ = 1/sᵀy`. */
private def bfgsUpdate(h: Vector[Vector[Double]], s: Vector[Double], y: Vector[Double],
                       sy: Double): Vector[Vector[Double]] =
  val rho = 1.0 / sy
  val hy  = h.map(dot(_, y))                       // H y
  val yhy = dot(y, hy)
  Vector.tabulate(s.size, s.size) { (i, j) =>
    h(i)(j) - rho * (s(i) * hy(j) + hy(i) * s(j)) + (rho * rho * yhy + rho) * s(i) * s(j)
  }

/** Projected gradient descent, or projected BFGS with an active set (F_0010 Decision C).
 *
 *  A coordinate is **active** when it sits on a bound and the gradient pushes it outward; the
 *  direction is zero there and the quasi-Newton step is taken in the free coordinates.  The
 *  step is projected back into the box and accepted by a projected Armijo test.  With
 *  infinite bounds this is plain BFGS (or descent) with a backtracking line search. */
private def boxed(p: Problem, x0: Vector[Double], lo: Vector[Double], hi: Vector[Double],
                  quasiNewton: Boolean): Either[MinimizeFailure, MinimizeResult] =
  val n = p.n
  def clamp(x: Vector[Double]): Vector[Double] =
    Vector.tabulate(n)(i => math.min(hi(i), math.max(lo(i), x(i))))
  /** Whether coordinate `i` sits on a bound with the gradient pushing it outward. */
  def isActive(x: Vector[Double], g: Vector[Double], i: Int): Boolean =
    (x(i) <= lo(i) && g(i) > 0) || (x(i) >= hi(i) && g(i) < 0)
  /** The projected gradient, component by component: zero where a coordinate is active.  It
   *  was `‖P(x − ∇f) − x‖∞`, which subtracts a gradient from a point and so mixes their units
   *  (F_0095): whether `x − ∇f` crosses a bound depends on the scale of `f`. */
  def projected(x: Vector[Double], g: Vector[Double]): Vector[Double] =
    Vector.tabulate(n)(i => if isActive(x, g, i) then 0.0 else g(i))
  def identity: Vector[Vector[Double]] = Vector.tabulate(n, n)((i, j) => if i == j then 1.0 else 0.0)

  @annotation.tailrec
  def loop(x: Vector[Double], fx: Double, g: Vector[Double], h: Vector[Vector[Double]],
           k: Int, x0: Vector[Double], f0: Double): Either[MinimizeFailure, MinimizeResult] =
    val pg = projected(x, g)
    val st = stationarity(pg, x, x0, f0, fx)
    if st <= Target then certify(p, st, x, fx, pg, k)
    else if k >= MaxMinimizeIterations then atCap(p, st, x, fx, pg)
    else
      val active = Vector.tabulate(n)(i => isActive(x, g, i))
      val raw =
        if quasiNewton then Vector.tabulate(n)(i =>
          if active(i) then 0.0 else -(0 until n).filterNot(active).map(j => h(i)(j) * g(j)).sum)
        else Vector.tabulate(n)(i => if active(i) then 0.0 else -g(i))
      val freeG = Vector.tabulate(n)(i => if active(i) then 0.0 else g(i))
      val (d, hk) =
        if dot(raw, freeG) < 0 then (raw, h) else (freeG.map(-_), identity)
      armijo(p, x, fx, g, d, clamp) match
        case Left(fail) => Left(fail(k))
        case Right((xn, fn)) =>
          p.grad(xn) match
            case None => Left(MinimizeFailure.NotEvaluable)
            case Some(gnew) =>
              if stagnant(x, xn, fx, fn) then
                val pgn = projected(xn, gnew)
                certify(p, stationarity(pgn, xn, x0, f0, fn), xn, fn, pgn, k + 1)
              else
                // The update sees the FREE coordinates only.  An active coordinate does not move
                // (s = 0 there) while its gradient still changes, and feeding that y into the
                // full-space update corrupts the free block: the secant condition then holds
                // for the whole vector but not for the subspace the step is taken in, and
                // convergence on the bounded Rosenbrock problem collapsed from superlinear to
                // thousands of iterations.
                val s  = Vector.tabulate(n)(i => if active(i) then 0.0 else xn(i) - x(i))
                val y  = Vector.tabulate(n)(i => if active(i) then 0.0 else gnew(i) - g(i))
                val sy = dot(s, y)
                val hn =
                  if !quasiNewton || sy <= 1e-12 * math.sqrt(dot(s, s) * dot(y, y)) then hk
                  else bfgsUpdate(if k == 0 then hk.map(_.map(_ * sy / dot(y, y))) else hk, s, y, sy)
                loop(xn, fn, gnew, hn, k + 1, x0, f0)

  // The certificate measures from the clamped start, the first point the iteration is at.
  val start = clamp(x0)
  (for f0 <- p.value(start); g0 <- p.grad(start) yield (f0, g0)) match
    case None           => Left(MinimizeFailure.NotEvaluable)
    case Some((f0, g0)) => loop(start, f0, g0, identity, 0, start, f0)


/** The line-search constants: sufficient decrease and (for Wolfe) curvature. */
private val C1 = 1e-4
private val C2 = 0.9

/** Backtracking from the full step until `f(P(x + αd)) ≤ f(x) + c₁·gᵀ(P(x + αd) − x)`.
 *
 *  `project` is the identity without bounds.  Fails once the step shrinks below any
 *  representable change of `x`.  A failure is reported as a function of the iteration
 *  number, which only the caller knows. */
private def armijo(p: Problem, x: Vector[Double], fx: Double, g: Vector[Double], d: Vector[Double],
                   project: Vector[Double] => Vector[Double])
    : Either[Int => MinimizeFailure, (Vector[Double], Double)] =
  @annotation.tailrec
  def search(alpha: Double): Either[Int => MinimizeFailure, (Vector[Double], Double)] =
    if alpha < 1e-20 then Left(k => MinimizeFailure.LineSearchFailed(k))
    else
      val xn = project(axpy(alpha, d, x))
      p.value(xn) match
        case Some(fn) if fn < UnboundedBelow => Left(_ => MinimizeFailure.Unbounded)
        case Some(fn) if fn <= fx + C1 * dot(g, xn.lazyZip(x).map(_ - _)) => Right((xn, fn))
        case _ => search(alpha / 2)
  search(1.0)

/** A strong-Wolfe line search (Nocedal & Wright, algorithms 3.5 and 3.6, bisection zoom).
 *
 *  @return the step length, the new point, its value and its gradient */
private def wolfe(p: Problem, x: Vector[Double], fx: Double, g: Vector[Double], d: Vector[Double])
    : Either[Int => MinimizeFailure, (Double, Vector[Double], Double, Vector[Double])] =
  val dphi0 = dot(g, d)
  type Hit = (Double, Vector[Double], Double, Vector[Double])
  def at(alpha: Double): Option[(Vector[Double], Double, Vector[Double], Double)] =
    val xa = axpy(alpha, d, x)
    for fa <- p.value(xa); ga <- p.grad(xa) yield (xa, fa, ga, dot(ga, d))

  @annotation.tailrec
  def zoom(lo: Double, flo: Double, hi: Double, k: Int): Either[Int => MinimizeFailure, Hit] =
    if k > 60 || math.abs(hi - lo) <= 1e-16 * math.max(1.0, math.abs(lo)) then
      Left(i => MinimizeFailure.LineSearchFailed(i))
    else
      val mid = (lo + hi) / 2
      at(mid) match
        case None => zoom(lo, flo, mid, k + 1)
        case Some((xm, fm, gm, dm)) =>
          if fm > fx + C1 * mid * dphi0 || fm >= flo then zoom(lo, flo, mid, k + 1)
          else if math.abs(dm) <= -C2 * dphi0 then Right((mid, xm, fm, gm))
          else if dm * (hi - lo) >= 0 then zoom(mid, fm, lo, k + 1)
          else zoom(mid, fm, hi, k + 1)

  @annotation.tailrec
  def bracket(prev: Double, fprev: Double, alpha: Double, k: Int): Either[Int => MinimizeFailure, Hit] =
    if k > 60 then Left(_ => MinimizeFailure.Unbounded)
    else at(alpha) match
      case None => zoom(prev, fprev, alpha, 0)
      case Some((xa, fa, ga, da)) =>
        if fa < UnboundedBelow then Left(_ => MinimizeFailure.Unbounded)
        else if fa > fx + C1 * alpha * dphi0 || (k > 0 && fa >= fprev) then zoom(prev, fprev, alpha, 0)
        else if math.abs(da) <= -C2 * dphi0 then Right((alpha, xa, fa, ga))
        else if da >= 0 then zoom(alpha, fa, prev, 0)
        else bracket(alpha, fa, alpha * 2, k + 1)

  bracket(0.0, fx, 1.0, 0)

/** Solves `H·s = g` by Cholesky, or `None` when `H` is not positive definite — which is the
 *  cheapest test there is, and the reason Newton uses it rather than an inverse. */
private def choleskySolve(h: Array[Double], n: Int, g: Vector[Double]): Option[Vector[Double]] =
  val l = Array.fill(n * n)(0.0)
  val ok = (0 until n).forall { j =>
    val diag = h(j * n + j) - (0 until j).map(k => l(j * n + k) * l(j * n + k)).sum
    if !(diag > 0.0) then false
    else
      l(j * n + j) = math.sqrt(diag)
      for i <- j + 1 until n do
        l(i * n + j) = (h(i * n + j) - (0 until j).map(k => l(i * n + k) * l(j * n + k)).sum) / l(j * n + j)
      true
  }
  Option.when(ok) {
    val y = Array.fill(n)(0.0)
    for i <- 0 until n do y(i) = (g(i) - (0 until i).map(k => l(i * n + k) * y(k)).sum) / l(i * n + i)
    val s = Array.fill(n)(0.0)
    for i <- (0 until n).reverse do
      s(i) = (y(i) - (i + 1 until n).map(k => l(k * n + i) * s(k)).sum) / l(i * n + i)
    s.toVector
  }

package it.grypho.scala.leonardo
package optimize

import core.*


// An uncertified derivative-free minimiser over a numeric callback (F_0302, AIACE L_0004, the
// owner's decision of 2026-11-05).  Library-only.

/** The derivative-free methods, always named by the caller (F_0302; the `c2d` rule). */
enum DerivativeFreeMethod:
  /** Compass search, the simplest generalised pattern search: poll `x ± Δᵢ·eᵢ`, move to the first
   *  point that decreases `f`, halve every step when none does.  It carries a **partial** convergence
   *  result — for a continuously differentiable `f` on a bounded set the steps go to zero at a
   *  stationary point (Kolda, Lewis and Torczon 2003) — and none for a non-smooth one, where it can
   *  stop at a point that is not a minimiser.  Supports integer coordinates. */
  case PatternSearch
  /** Nelder–Mead with every trial point projected into the bounds.  **No convergence result**: it
   *  can converge to a non-stationary point even of a smooth strictly convex function (McKinnon
   *  1998).  No integer coordinates. */
  case NelderMead

/** Why the search stopped with an answer (F_0302). */
enum DerivativeFreeStop:
  /** Pattern search: no poll point decreased `f` with every step at its floor — a real coordinate's
   *  below `0.5·10⁻ᵖ` of its scale, an integer coordinate's at 1. */
  case MeshConverged
  /** Nelder–Mead: the simplex's diameter, each coordinate in units of its scale, fell below
   *  `0.5·10⁻ᵖ`. */
  case SimplexConverged

/** A point the derivative-free search stopped at (F_0302).  **Uncertified by its type**: nothing
 *  proves it a minimiser, local or global, and it is never a [[MinimizeResult]] (the `odeStep`
 *  precedent).
 *
 *  @param x           the best point found
 *  @param value       `f` there
 *  @param evaluations how many times `f` was called
 *  @param step        the final step per coordinate (pattern search), or the final simplex's extent
 *                     per coordinate (Nelder–Mead), in the coordinate's own units
 *  @param stop        why the search stopped
 */
final case class UncertifiedMinimum(x: Vector[Double], value: Double, evaluations: Int, step: Vector[Double],
                                    stop: DerivativeFreeStop)

/** Why [[minimizeDerivativeFree]] declined (F_0302). */
enum DerivativeFreeFailure:
  /** The problem as written is malformed; `detail` says how. */
  case InvalidInput(detail: String)
  /** `f` is not a finite number at the start. */
  case NotEvaluable
  /** [[core.Limit.MaxDerivativeFreeEvaluations]] was reached before the search stopped; the best
   *  point so far is carried, uncertified like any answer of this search.
   *  @param limit the budget
   *  @param x     the best point so far
   *  @param value `f` there */
  case MaxEvaluations(limit: Long, x: Vector[Double], value: Double)

/** Minimises a numeric callback without derivatives (F_0302), **uncertified**: the answer is an
 *  [[UncertifiedMinimum]], never a certified [[MinimizeResult]].
 *
 *  For an objective with no formula, no gradient, possibly non-smooth and expensive — a closed-loop
 *  simulation scoring a controller tuning, say.  The method is always named.  **Steps and tolerances
 *  are relative to each variable's own scale** (the units rule): the bound width when both bounds are
 *  finite, else `|x0ᵢ|`; a real variable with neither declines, an integer one takes unit scale, and a
 *  variable with equal bounds is held fixed.  The initial step is a quarter of the scale (pattern
 *  search) or a twentieth (Nelder–Mead's simplex), the stopping step `0.5·10⁻ᵖ` of it, so rescaling a
 *  variable by a power of two rescales the iterates exactly.  A non-finite `f` at a trial point counts
 *  as `+∞`; at the start it declines.  Deterministic: no random numbers, so a deterministic `f` gives
 *  the same path on every platform.  **Not addressed**: a noisy objective, on which both methods may
 *  stop early or wander.
 *
 *  @param f      the objective, called with a fresh array it may not keep
 *  @param x0     the start, within the bounds
 *  @param lo     lower bounds, `-∞` for none
 *  @param hi     upper bounds, `+∞` for none
 *  @param method the method, named
 *  @param env    the precision that sets the stopping step, and the evaluation budget
 *  @return the point the search stopped at, or why it did not
 */
def minimizeDerivativeFree(f: Array[Double] => Double, x0: Vector[Double], lo: Vector[Double], hi: Vector[Double],
                           method: DerivativeFreeMethod, env: Environment): Either[DerivativeFreeFailure, UncertifiedMinimum] =
  minimizeDerivativeFree(f, x0, lo, hi, Set.empty, method, env)

/** [[minimizeDerivativeFree]] with **integer coordinates** (F_0302): each index in `integer` takes only
 *  whole values, its step never falling below 1 and its bounds rounded inwards.  Pattern search only;
 *  Nelder–Mead declines an integer coordinate.
 *
 *  @param f       the objective
 *  @param x0      the start; whole at every integer coordinate
 *  @param lo      lower bounds
 *  @param hi      upper bounds
 *  @param integer the integer coordinates, 0-based
 *  @param method  the method
 *  @param env     the precision and the budget
 *  @return the point the search stopped at, or why it did not
 */
def minimizeDerivativeFree(f: Array[Double] => Double, x0: Vector[Double], lo: Vector[Double], hi: Vector[Double],
                           integer: Set[Int], method: DerivativeFreeMethod,
                           env: Environment): Either[DerivativeFreeFailure, UncertifiedMinimum] =
  import DerivativeFreeFailure.*
  val n = x0.size
  def bad(d: String) = Left(InvalidInput(d))
  if n == 0 then bad("there are no variables")
  else if lo.size != n || hi.size != n then bad(s"the bounds need $n entries each")
  else if integer.exists(i => i < 0 || i >= n) then bad(s"an integer coordinate is not an index in 0 .. ${n - 1}")
  else if method == DerivativeFreeMethod.NelderMead && integer.nonEmpty then bad("Nelder-Mead takes no integer coordinates")
  else
    val lo1 = Vector.tabulate(n)(i => if integer(i) then math.ceil(lo(i)) else lo(i))
    val hi1 = Vector.tabulate(n)(i => if integer(i) then math.floor(hi(i)) else hi(i))
    val problem = (0 until n).collectFirst {
      case i if !x0(i).isFinite                     => s"x0($i) is not a finite number"
      case i if lo1(i).isNaN || hi1(i).isNaN || !(lo1(i) <= hi1(i)) => s"the bounds of variable $i are empty"
      case i if x0(i) < lo1(i) || x0(i) > hi1(i)   => s"x0($i) is outside its bounds"
      case i if integer(i) && x0(i) != math.rint(x0(i)) => s"x0($i) must be whole: it is an integer coordinate"
      case i if !integer(i) && lo1(i) != hi1(i) && !(hi1(i) - lo1(i)).isFinite && x0(i) == 0.0 =>
        s"variable $i has neither a finite range nor a non-zero start to measure its steps by"
    }
    problem match
      case Some(d) => bad(d)
      case None =>
        val scale = Vector.tabulate(n) { i =>
          val w = hi1(i) - lo1(i)
          if w == 0.0 then 0.0 else if w.isFinite then w else if x0(i) != 0.0 then math.abs(x0(i)) else 1.0
        }
        val eval = Evaluator(f, env.limits(Limit.MaxDerivativeFreeEvaluations))
        val tol  = 0.5 * math.pow(10, -env.precision)
        try
          val f0 = eval(x0)
          if !f0.isFinite then Left(NotEvaluable)
          else Right(method match
            case DerivativeFreeMethod.PatternSearch => patternSearch(eval, x0, f0, lo1, hi1, scale, integer, tol)
            case DerivativeFreeMethod.NelderMead    => nelderMead(eval, x0, f0, lo1, hi1, scale, tol))
        catch case _: EvaluationsSpent => Left(MaxEvaluations(eval.budget, eval.bestX, eval.bestF))

/** The budget ran out (a control throwable, caught where the search was entered). */
private final class EvaluationsSpent extends scala.util.control.ControlThrowable

/** `f` counted against the budget, a non-finite value read as `+∞`, the best point remembered. */
private final class Evaluator(f: Array[Double] => Double, val budget: Long):
  var count: Int = 0
  var bestX: Vector[Double] = Vector.empty
  var bestF: Double = Double.PositiveInfinity
  def apply(x: Vector[Double]): Double =
    if count >= budget then throw EvaluationsSpent()
    count += 1
    val raw = f(x.toArray)
    val v = if raw.isNaN then Double.PositiveInfinity else raw
    if bestX.isEmpty || v < bestF then { bestX = x; bestF = v }
    v

/** Compass search; see [[DerivativeFreeMethod.PatternSearch]]. */
private def patternSearch(eval: Evaluator, x0: Vector[Double], f0: Double, lo: Vector[Double], hi: Vector[Double],
                          scale: Vector[Double], integer: Set[Int], tol: Double): UncertifiedMinimum =
  val n = x0.size
  val step  = Array.tabulate(n)(i =>
    if scale(i) == 0.0 then 0.0 else if integer(i) then math.max(1.0, math.rint(0.25 * scale(i))) else 0.25 * scale(i))
  val floor = Array.tabulate(n)(i => if integer(i) then 1.0 else tol * scale(i))
  var x  = x0
  var fx = f0
  var done = false
  while !done do
    // poll x ± stepᵢ·eᵢ in order, moving to the first point that decreases f
    val moved = (0 until n).iterator.filter(step(_) > 0.0).flatMap(i => Iterator(1.0, -1.0).map((i, _))).exists { (i, sign) =>
      val xi = math.min(hi(i), math.max(lo(i), x(i) + sign * step(i)))
      xi != x(i) && {
        val trial = x.updated(i, xi)
        val ft = eval(trial)
        if ft < fx then { x = trial; fx = ft; true } else false
      }
    }
    if !moved then
      if (0 until n).forall(i => step(i) <= floor(i)) then done = true
      else for i <- 0 until n if step(i) > floor(i) do
        step(i) = if integer(i) then math.max(1.0, math.floor(step(i) / 2)) else step(i) / 2
  UncertifiedMinimum(x, fx, eval.count, step.toVector, DerivativeFreeStop.MeshConverged)

/** Nelder–Mead with projected trial points; see [[DerivativeFreeMethod.NelderMead]]. */
private def nelderMead(eval: Evaluator, x0: Vector[Double], f0: Double, lo: Vector[Double], hi: Vector[Double],
                       scale: Vector[Double], tol: Double): UncertifiedMinimum =
  val free = x0.indices.filter(scale(_) > 0.0).toVector
  def clamp(x: Vector[Double]) = Vector.tabulate(x.size)(i => math.min(hi(i), math.max(lo(i), x(i))))
  def affine(a: Vector[Double], b: Vector[Double], t: Double) =  // a + t·(b − a), projected
    clamp(Vector.tabulate(a.size)(i => a(i) + t * (b(i) - a(i))))
  def extent(vs: Vector[(Vector[Double], Double)]) =
    Vector.tabulate(x0.size)(i => vs.map(v => math.abs(v._1(i) - vs.head._1(i))).max)
  // the starting simplex: a twentieth of each free variable's scale, inwards where a bound is near
  var simplex: Vector[(Vector[Double], Double)] = (x0, f0) +: free.map { i =>
    val up = x0(i) + 0.05 * scale(i)
    val v  = x0.updated(i, if up <= hi(i) then up else math.max(lo(i), x0(i) - 0.05 * scale(i)))
    (v, eval(v))
  }
  // a stable sort by value keeps ties in vertex order, so the path is deterministic
  def sorted(vs: Vector[(Vector[Double], Double)]) = vs.sortBy(_._2)(using Ordering.Double.TotalOrdering)
  simplex = sorted(simplex)
  def converged = free.isEmpty || free.forall(i => extent(simplex)(i) <= tol * scale(i))
  while !converged do
    val m        = simplex.size - 1
    val (best, worst, second) = (simplex.head, simplex(m), simplex(m - 1))
    val centroid = Vector.tabulate(x0.size)(i => simplex.take(m).map(_._1(i)).sum / m)
    val xr = affine(centroid, worst._1, -1.0)
    val fr = eval(xr)
    val next: Option[(Vector[Double], Double)] =
      if fr < best._2 then
        val xe = affine(centroid, worst._1, -2.0)
        val fe = eval(xe)
        Some(if fe < fr then (xe, fe) else (xr, fr))
      else if fr < second._2 then Some((xr, fr))
      else
        val xc = if fr < worst._2 then affine(centroid, xr, 0.5) else affine(centroid, worst._1, 0.5)
        val fc = eval(xc)
        Option.when(fc < math.min(fr, worst._2))((xc, fc))
    simplex = next match
      case Some(v) => sorted(simplex.take(m) :+ v)
      case None    => sorted(best +: simplex.tail.map { (v, _) => val s = affine(best._1, v, 0.5); (s, eval(s)) })
  UncertifiedMinimum(simplex.head._1, simplex.head._2, eval.count, extent(simplex), DerivativeFreeStop.SimplexConverged)

package it.grypho.scala.leonardo
package optimize

import core.*

// The semidefinite path of `qpSolve` (F_0074): the primal active-set method and its feasible start by an exact LP.
// Moved from QP.scala by F_0391; both are private, so the move changes no published class.

/** A point satisfying the constraints, from [[lpSolve]] with zero cost (F_0074): exact, so an
 *  infeasible set is proved so.  Any other decline is the LP's, and is reported as
 *  [[QpFailure.FeasibleStart]] carrying it (F_0106): the LP checks its own dimension caps before
 *  any work, so they are not restated here, where a second copy could drift from them.  With
 *  `weights`, the smallest such point (F_0362): `min Σ wⱼ·tⱼ` under `tⱼ ≥ |zⱼ|`, `wⱼ` the reciprocal of each
 *  variable's own size, the zero-cost vertex when the doubled problem would pass the LP's caps. */
private def feasiblePoint(n: Int, ineq: Option[(_MatrixValue, Vector[Double])], eq: Option[(_MatrixValue, Vector[Double])],
                          lb: Vector[Double], ub: Vector[Double], limits: Limits,
                          weights: Option[Vector[Double]] = None): Either[QpFailure, Array[Double]] =
  def num(d: Double): _Value = _Number(d)
  def group(g: Option[(_MatrixValue, Vector[Double])], extra: Int) =
    g.map((a, b) => (Vector.tabulate(a.rows)(i => Vector.tabulate(a.cols + extra)(j => if j < a.cols then num(a(i, j)) else num(0.0))), b.map(num)))
  val rowCount = ineq.fold(0)(_._1.rows) + eq.fold(0)(_._1.rows)
  // The smallest feasible point (F_0362): min Σ wⱼ·tⱼ with tⱼ ≥ |zⱼ|, so the path does not start at a far corner
  // of wide bounds; the zero-cost LP when the doubled problem would pass the LP's caps.
  val smallest = weights.filter(_ => 2 * n <= limits.count(Limit.MaxLpVariables) &&
                                     rowCount + 2 * n <= limits.count(Limit.MaxLpConstraints))
  val answer = smallest match
    case Some(w) =>
      val abs = Vector.tabulate(2 * n) { r =>
        val (j, sign) = (r / 2, if r % 2 == 0 then 1.0 else -1.0)
        Vector.tabulate(2 * n)(c => if c == j then num(sign) else if c == n + j then num(-1.0) else num(0.0))
      }
      val rows = group(ineq, n).fold((abs, Vector.fill(2 * n)(num(0.0))))((a, b) => (a ++ abs, b ++ Vector.fill(2 * n)(num(0.0))))
      lpSolve(Vector.fill(n)(num(0.0)) ++ w.map(num), Some(rows), group(eq, n),
              lb.map(_Number(_)) ++ Vector.fill(n)(_Number(0.0)), ub.map(_Number(_)) ++ Vector.fill(n)(_Number(Double.PositiveInfinity)),
              limits).map(r => r.copy(x = r.x.take(n)))
    case None =>
      lpSolve(Vector.fill(n)(num(0.0)), group(ineq, 0), group(eq, 0), lb.map(_Number(_)), ub.map(_Number(_)), limits)
  answer match
    case Right(r) => Right(r.x.map { case _Number(d) => d; case _ => Double.NaN }.toArray)
    case Left(LpFailure.Infeasible) => Left(QpFailure.Infeasible)
    case Left(why) => Left(QpFailure.FeasibleStart(why))


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

  /** Whether a start lies farther than the step test can see past (F_0362): a step of the data's own size
   *  `max sⱼ` is negligible beside `iterTol·‖x‖∞` once `‖x‖∞` passes `(certTol/iterTol)·max sⱼ`; a tenth of it. */
  def far(z: Array[Double]): Boolean = !prob.near(normInfA(z))

  /** Whether `z` may start the iteration (F_0359): finite, and every row met to this method's own
   *  tolerance of its terms — an equality within it either side, an inequality short by at most it. */
  def feasible(z: Array[Double]): Boolean =
    z.length == n && z.forall(_.isFinite) && rows.forall { r =>
      val sl = r.slack(z)
      val tol = iterTol * prob.rowScale(r, z)
      if r.isEquality then math.abs(sl) <= tol else sl >= -tol
    }

  // `H`'s rows as arrays, read once: `hTimes` runs once per face direction per move, and a boxed
  // fold over `prob.h(i, j)` there broke the file's own rule for inner loops (F_0108).
  private val hRows: Array[Array[Double]] = Array.tabulate(n)(i => Array.tabulate(n)(j => prob.h(i, j)))
  private def hTimes(v: Array[Double]): Array[Double] = Array.tabulate(n)(i => dotA(hRows(i), v))
  /** `‖H‖∞`, the largest a reduced Hessian `ZᵀHZ` on an orthonormal basis can be. */
  private val hNorm: Double = hRows.map(row => row.map(math.abs).sum).maxOption.getOrElse(0.0)
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
        // The gradient's scale at the point (F_0359), not its own norm: at an optimum the gradient is
        // rounding, and against its own norm a null direction's share of that rounding read as a ray, so
        // an unconstrained semidefinite QP (`min (Gu − r)²`) was declined `Unbounded`.
        val gNorm = prob.iterationGradientScale(x)
        val cols = sigma.indices.map(i => Array.tabulate(k)(row => v(row, i)))
        val coef = cols.map(c => dotA(c, r))
        // A curvature is null below 1e-12 of `‖H‖`, not of the reduced Hessian's own top (F_0359): with
        // orthonormal `Z`, `ZᵀHZ` is never larger than `H`, and where every face direction is flat the
        // reduced Hessian is rounding (`10⁻³²` from a basis vector's `10⁻¹⁷` share of a curved variable),
        // which read against itself as curvature: the step `coef/10⁻³²` crossed a working-set bound by
        // `10¹⁶`, and the certificate's slack, scaled by that `‖z‖∞`, passed it (since F_0361 it reads each row
        // at its own variables' sizes, and refuses that point).
        val nul  = sigma.indices.filter(i => sigma(i) <= 1e-12 * math.max(top, hNorm) || top == 0.0)
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
        case Some(Move.Step(p)) if normInfA(p) <= iterTol * math.max(normInfA(x), prob.ownScaleMax) =>
          multipliers(x, w) match
            case None => Left(QpFailure.Uncertified(Double.PositiveInfinity))
            case Some(u) =>
              val scale = prob.iterationGradientScale(x)
              // At `iterTol`, not at the rounding the certificate's sign test reads (F_0362, the owner's choice):
              // at rounding the primal method dropped multipliers negative only by their own rounding, 3x slower
              // on an 8-state target layer for no changed answer, the smallest start having fixed the far vertex.
              val drop  = w.indices.filter(a => !rows(w(a)).isEquality && u(a) * normInfA(rows(w(a)).normal) < -iterTol * scale)
              // A multiplier negative by less than `iterTol` but more than the certificate's rounding is dropped
              // here, at the last step, so the two tests cannot leave a point neither drops nor certifies: a
              // weakly active constraint's multiplier at `-3e-11` was declined `Uncertified(4e-4)`, and which side of
              // zero such a multiplier lands on moved with the variables' units (F_0378).
              drop.minByOption(w(_)).orElse(prob.wrongSigns(x, w, u).headOption) match
                case None    => qpCertificate(prob, x, w, u, it, certTol)
                case Some(a) => loop(x, w.patch(a, Nil, 1), it + 1)
        case Some(Move.Step(p)) =>
          blocking(x, p, w) match
            case Some((t, j)) if t < 1.0 => loop(combine(x, Array(t), Vector(p), 1.0), w :+ j, it + 1)
            case _                       => loop(combine(x, Array(1.0), Vector(p), 1.0), w, it + 1)

    loop(x0, w0, 0)

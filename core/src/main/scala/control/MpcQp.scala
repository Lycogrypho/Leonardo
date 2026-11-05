package it.grypho.scala.leonardo
package control

import core.*
import optimize.{qpSolve, QpConstraint, QpResult}


/** Soft output bounds (F_0299, the owner's decision): one non-negative slack per output, shared over
 *  the horizon, relaxing both of that output's bounds and costing `ρ₁·s + ρ₂·s²`.  `ρ₂ > 0` keeps the
 *  Hessian positive definite; once `ρ₁` exceeds the bound's multiplier the penalty is exact — no slack
 *  is used while the hard problem is feasible.
 *
 *  @param linear    `ρ₁`, one per output, `≥ 0`
 *  @param quadratic `ρ₂`, one per output, `> 0`
 */
final case class MpcSoftOutputs(linear: Vector[Double], quadratic: Vector[Double])

/** The bounds of an MPC problem (F_0299): each pair is `(lower, upper)`, one entry per input or output,
 *  `±∞` for none; `None` for no bound of that kind at all.
 *
 *  @param u    the inputs `u(k+j)`
 *  @param du   the increments `u(k+j) − u(k+j−1)`, the first against `u(k−1)`
 *  @param y    the predicted outputs `y(k+1) … y(k+Np)`
 *  @param soft relaxes the output bounds through slacks; `None` keeps them hard
 */
final case class MpcBounds(u: Option[(Vector[Double], Vector[Double])] = None,
                           du: Option[(Vector[Double], Vector[Double])] = None,
                           y: Option[(Vector[Double], Vector[Double])] = None,
                           soft: Option[MpcSoftOutputs] = None)

/** A solved MPC step (F_0299).
 *
 *  @param moves  the decided sequence, `Nc` vectors of `m`: the inputs for an absolute prediction, the
 *                increments for an incremental one
 *  @param input  `u(k)`, the input to apply now
 *  @param slacks the output slacks, empty when the outputs are not softened
 *  @param qp     the certified QP optimum, whose `active` set warm-starts the next step
 */
final case class MpcSolution(moves: Vector[Vector[Double]], input: Vector[Double], slacks: Vector[Double],
                             qp: QpResult)

/** The condensed MPC quadratic programme `min ½ zᵀHz + fᵀz` under `A z ≤ b` and `lb ≤ z ≤ ub`, ready
 *  for [[optimize.qpSolve]] (F_0299), with `z` the `Nc·m` moves followed by the slacks.
 *
 *  @param h           the Hessian, `2(GᵀQ̄G + R̄)` with the slacks' `2ρ₂` on the diagonal
 *  @param f           the linear term
 *  @param ineq        the inequality rows, or `None`
 *  @param lb          lower bounds of `z`
 *  @param ub          upper bounds of `z`
 *  @param m           inputs per move
 *  @param nc          the control horizon
 *  @param incremental whether `z` holds increments
 *  @param uPrev       `u(k−1)`
 */
final case class MpcQp(h: _MatrixValue, f: Vector[Double], ineq: Option[(_MatrixValue, Vector[Double])],
                       lb: Vector[Double], ub: Vector[Double], m: Int, nc: Int, incremental: Boolean,
                       uPrev: Vector[Double]):

  /** Solves the programme and maps the optimum back to the moves (F_0299).
   *  @param warm the previous step's active set, or empty
   *  @param env  the precision the certificate holds and the QP budgets
   *  @return the step, or why the QP declined
   */
  def solve(warm: Set[QpConstraint], env: Environment): Either[MpcFailure, MpcSolution] =
    qpSolve(h, f, ineq, None, lb, ub, warm, env).left.map(MpcFailure.Qp(_)).map { r =>
      val moves = r.z.take(nc * m).grouped(m).toVector
      val now   = if incremental then uPrev.zip(moves.head).map(_ + _) else moves.head
      MpcSolution(moves, now, r.z.drop(nc * m), r)
    }

/** Builds the condensed MPC quadratic programme (F_0299, APAC L_0002).
 *
 *  The cost is `Σ ‖y(k+i) − r(k+i)‖²_Q + Σ ‖z_j‖²_R` over the horizon — `z_j` the move `u(k+j)` for an
 *  absolute prediction and the increment `Δu(k+j)` for an incremental one — plus the slack penalties;
 *  with `Y = c₀ + G·Z`, `c₀ = Φx + Ψu(k−1)`, it is `½ Zᵀ·2(GᵀQ̄G + R̄)Z + (2GᵀQ̄(c₀ − r))ᵀZ`.
 *  A bound on the decision variable itself is a bound of `z`; every other bound is a row, and an
 *  infinite bound is no row at all.
 *
 *  @param prediction the prediction matrices, absolute or incremental (F_0298)
 *  @param q          the output weight `Q`, `p×p`, the same at every step
 *  @param r          the move weight `R`, `m×m`, the same at every step
 *  @param reference  the reference: `p` entries held over the horizon, or `Np·p` stacked
 *  @param x          the current state, `n` entries
 *  @param uPrev      `u(k−1)`, `m` entries
 *  @param bounds     the bounds and the soft outputs
 *  @param limits     the budgets (the QP's own are checked when it is solved)
 *  @return the programme, or why it cannot be built
 */
def mpcQp(prediction: MpcPredictionMatrices, q: _MatrixValue, r: _MatrixValue, reference: Vector[Double],
          x: Vector[Double], uPrev: Vector[Double], bounds: MpcBounds, limits: Limits): Either[MpcFailure, MpcQp] =
  val (g, psi) = prediction match
    case MpcPrediction(_, gamma)                => (gamma, None)
    case MpcIncrementalPrediction(_, ps, theta) => (theta, Some(ps))
  val phi = prediction.phi
  val (n, rowsY) = (phi.cols, phi.rows)
  val m  = r.rows
  val p  = q.rows
  val incremental = psi.isDefined
  def bad(d: String) = Left(MpcFailure.InvalidInput(d))
  def pairOk(b: Option[(Vector[Double], Vector[Double])], k: Int) =
    b.forall((lo, hi) => lo.size == k && hi.size == k && lo.zip(hi).forall((l, h) => !l.isNaN && !h.isNaN && l <= h))
  if q.cols != p || p == 0 || rowsY % p != 0 then bad(s"Q must be square with as many rows as outputs")
  else if r.cols != m || g.cols % m != 0 then bad(s"R must be square with as many rows as inputs")
  else if x.size != n then bad(s"the state has ${x.size} entries for $n states")
  else if uPrev.size != m then bad(s"u(k-1) has ${uPrev.size} entries for $m inputs")
  else if reference.size != p && reference.size != rowsY then bad(s"the reference must have $p or $rowsY entries")
  else if !pairOk(bounds.u, m) || !pairOk(bounds.du, m) || !pairOk(bounds.y, p) then
    bad("a bound has the wrong length, a NaN, or a lower above its upper")
  else if bounds.soft.exists(s => s.linear.size != p || s.quadratic.size != p ||
                                  s.linear.exists(w => !(w >= 0)) || s.quadratic.exists(w => !(w > 0))) then
    bad("soft outputs need one linear weight >= 0 and one quadratic weight > 0 per output")
  else if !(q.isFinite && r.isFinite) || (x ++ uPrev ++ reference).exists(!_.isFinite) then bad("every entry must be finite")
  else
    val np  = rowsY / p
    val nc  = g.cols / m
    val nz  = nc * m
    val ns  = if bounds.soft.isDefined then p else 0
    val ref = if reference.size == rowsY then reference else Vector.fill(np)(reference).flatten
    // c0 = Φx + Ψu(k−1), the free response
    val c0 = Vector.tabulate(rowsY)(i =>
      (0 until n).map(j => phi(i, j) * x(j)).sum + psi.fold(0.0)(ps => (0 until m).map(j => ps(i, j) * uPrev(j)).sum))
    val qBar = _MatrixValue.identity(np).kronecker(q)
    val rBar = _MatrixValue.identity(nc).kronecker(r)
    val gtq  = g.transpose.multiply(qBar)
    val hz   = gtq.multiply(g).add(rBar).scale(2.0)
    val fz   = gtq.multiply(_MatrixValue(rowsY, 1, c0.zip(ref).map(_ - _).toArray)).scale(2.0).toVector
    val size = nz + ns
    val hData = Array.tabulate(size * size) { idx =>
      val (i, j) = (idx / size, idx % size)
      if i < nz && j < nz then hz(i, j)
      else if i == j then 2.0 * bounds.soft.fold(0.0)(_.quadratic(i - nz))
      else 0.0
    }
    val f = fz ++ bounds.soft.fold(Vector.empty[Double])(_.linear)

    val rows = Vector.newBuilder[(Array[Double], Double)]
    def row(coeffs: Array[Double], rhs: Double): Unit = if rhs.isFinite then rows += ((coeffs, rhs))
    // the outputs: G·Z − s ≤ yₘₐₓ − c₀ and −G·Z − s ≤ −(yₘᵢₙ − c₀)
    bounds.y.foreach { (lo, hi) =>
      for i <- 0 until rowsY do
        val o = i % p
        def coeffs(sign: Double) = Array.tabulate(size)(j =>
          if j < nz then sign * g(i, j) else if j - nz == o then -1.0 else 0.0)
        row(coeffs(1.0), hi(o) - c0(i))
        row(coeffs(-1.0), -(lo(o) - c0(i)))
    }
    // the bound on the quantity that is not the decision variable, as rows over the moves
    def cumulativeRows(lo: Vector[Double], hi: Vector[Double], offset: Vector[Double], coefficient: (Int, Int) => Double): Unit =
      for step <- 0 until nc; k <- 0 until m do
        def coeffs(sign: Double) = Array.tabulate(size)(j =>
          if j < nz && j % m == k then sign * coefficient(step, j / m) else 0.0)
        row(coeffs(1.0), hi(k) - offset(k) * (if step == 0 || incremental then 1.0 else 0.0))
        row(coeffs(-1.0), -(lo(k) - offset(k) * (if step == 0 || incremental then 1.0 else 0.0)))
    if incremental then
      // u(k+j) = u(k−1) + Σ_{l ≤ j} Δu_l
      bounds.u.foreach((lo, hi) => cumulativeRows(lo, hi, uPrev, (j, l) => if l <= j then 1.0 else 0.0))
    else
      // Δu(k+j) = u_j − u_{j−1}, the first against u(k−1)
      bounds.du.foreach((lo, hi) => cumulativeRows(lo, hi, uPrev.map(-_),
        (j, l) => if l == j then 1.0 else if l == j - 1 then -1.0 else 0.0))
    val direct = if incremental then bounds.du else bounds.u
    val lb = Vector.tabulate(nz)(j => direct.fold(Double.NegativeInfinity)(_._1(j % m))) ++ Vector.fill(ns)(0.0)
    val ub = Vector.tabulate(nz)(j => direct.fold(Double.PositiveInfinity)(_._2(j % m))) ++ Vector.fill(ns)(Double.PositiveInfinity)
    val built = rows.result()
    val ineq = Option.when(built.nonEmpty)(
      (_MatrixValue(built.size, size, built.flatMap(_._1).toArray), built.map(_._2)))
    val cells = size.toLong * size + built.size.toLong * size
    if cells > limits(Limit.MaxMatrixCells) then Left(MpcFailure.TooLarge(cells))
    else Right(MpcQp(_MatrixValue(size, size, hData), f, ineq, lb, ub, m, nc, incremental, uPrev))

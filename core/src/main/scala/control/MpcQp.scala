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

/** The terminal cost of an MPC programme (F_0304, APAC L_0007): `‖x(k+Np) − x_s‖²_P`.
 *
 *  **The stage cost already weighs `y(k+Np)`**, so the weight that completes the infinite-horizon
 *  (LQR) cost-to-go is `P − CᵀQC` with `P` from [[dare]] / [[dlqr]], not `P` itself; with it an
 *  unconstrained MPC reproduces the LQR law `u = −K·x` exactly.  `P` alone also stabilises, more
 *  conservatively.
 *
 *  @param weight     `P`, `n×n`
 *  @param prediction the state prediction at the horizon's end, from [[mpcTerminalPrediction]], in the
 *                    same form (absolute or incremental) as the output prediction
 *  @param target     `x_s`, `n` entries; `None` for the origin
 */
final case class MpcTerminal(weight: _MatrixValue, prediction: MpcPredictionMatrices,
                             target: Option[Vector[Double]] = None)

/** The weights of an MPC programme (F_0304, F_0305; APAC L_0007, L_0008).
 *
 *  @param q           the output weights: one `p×p` for every step, or `Np` of them, step by step
 *  @param r           the move weights: one `m×m` for every move, or `Nc` of them
 *  @param terminal    the terminal cost, or `None`
 *  @param inputTarget the absolute form's input target `u_s` (F_0305): `m` entries held, or `Nc·m`
 *                     stacked, the move cost becoming `‖u − u_s‖²_R`; `None` for `u_s = 0`.  Without
 *                     it the absolute form's `R` pulls the inputs towards zero, which offsets tracking
 *                     whenever the steady input is not zero.  Not accepted with an incremental
 *                     prediction, whose moves are increments
 */
final case class MpcWeights(q: Vector[_MatrixValue], r: Vector[_MatrixValue], terminal: Option[MpcTerminal] = None,
                            inputTarget: Option[Vector[Double]] = None)

/** The state prediction at the end of the horizon (F_0304): `x(k+Np) = Φₓ·x + Γₓ·U` (absolute) or
 *  `Φₓ·x + Ψₓ·u(k−1) + Θₓ·ΔU` (incremental), the last move held past `Nc` as everywhere.  It is the
 *  last block row of [[mpcPrediction]] / [[mpcIncrementalPrediction]] for `C = I`, so the two cannot
 *  disagree.
 *
 *  @param a           the state matrix, `n×n`
 *  @param b           the input matrix, `n×m`
 *  @param np          the prediction horizon
 *  @param nc          the control horizon
 *  @param incremental the incremental form
 *  @param limits      the budgets
 *  @return the prediction, `n` rows, or why not
 */
def mpcTerminalPrediction(a: _MatrixValue, b: _MatrixValue, np: Int, nc: Int, incremental: Boolean,
                          limits: Limits): Either[MpcFailure, MpcPredictionMatrices] =
  val eye = _MatrixValue.identity(a.rows)
  def last(mat: _MatrixValue) = _MatrixValue(a.rows, mat.cols,
    Array.tabulate(a.rows * mat.cols)(idx => mat(mat.rows - a.rows + idx / mat.cols, idx % mat.cols)))
  if incremental then
    mpcIncrementalPrediction(a, b, eye, np, nc, limits).map(p => MpcIncrementalPrediction(last(p.phi), last(p.psi), last(p.theta)))
  else mpcPrediction(a, b, eye, np, nc, limits).map(p => MpcPrediction(last(p.phi), last(p.gamma)))

/** Builds the condensed MPC quadratic programme (F_0299, APAC L_0002) with one weight for every step:
 *  [[mpcQp]] with `MpcWeights(Vector(q), Vector(r))`.
 *
 *  @param prediction the prediction matrices, absolute or incremental (F_0298)
 *  @param q          the output weight `Q`, `p×p`, the same at every step
 *  @param r          the move weight `R`, `m×m`, the same at every step; in the absolute form it
 *                    pulls the inputs towards zero (see [[MpcWeights.inputTarget]])
 *  @param reference  the reference: `p` entries held over the horizon, or `Np·p` stacked
 *  @param x          the current state, `n` entries
 *  @param uPrev      `u(k−1)`, `m` entries
 *  @param bounds     the bounds and the soft outputs
 *  @param limits     the budgets (the QP's own are checked when it is solved)
 *  @return the programme, or why it cannot be built
 */
def mpcQp(prediction: MpcPredictionMatrices, q: _MatrixValue, r: _MatrixValue, reference: Vector[Double],
          x: Vector[Double], uPrev: Vector[Double], bounds: MpcBounds, limits: Limits): Either[MpcFailure, MpcQp] =
  mpcQp(prediction, MpcWeights(Vector(q), Vector(r)), reference, x, uPrev, bounds, limits)

/** Builds the condensed MPC quadratic programme (F_0299, F_0304, F_0305).
 *
 *  The cost is `Σᵢ ‖y(k+i) − r(k+i)‖²_Qᵢ + Σⱼ ‖z_j − u_s,j‖²_Rⱼ + ‖x(k+Np) − x_s‖²_P` — `z_j` the move
 *  `u(k+j)` for an absolute prediction and the increment `Δu(k+j)` for an incremental one — plus the
 *  slack penalties; with `Y = c₀ + G·Z`, `c₀ = Φx + Ψu(k−1)`, and `x(k+Np) = d₀ + Gₓ·Z`, it is
 *  `½ Zᵀ·2(GᵀQ̄G + R̄ + GₓᵀPGₓ)Z + (2GᵀQ̄(c₀ − r) − 2R̄u_s + 2GₓᵀP(d₀ − x_s))ᵀZ`.
 *  A bound on the decision variable itself is a bound of `z`; every other bound is a row, and an
 *  infinite bound is no row at all.
 *
 *  @param prediction the prediction matrices, absolute or incremental (F_0298)
 *  @param weights    the step weights, the terminal cost and the input target
 *  @param reference  the reference: `p` entries held over the horizon, or `Np·p` stacked
 *  @param x          the current state, `n` entries
 *  @param uPrev      `u(k−1)`, `m` entries
 *  @param bounds     the bounds and the soft outputs
 *  @param limits     the budgets (the QP's own are checked when it is solved)
 *  @return the programme, or why it cannot be built
 */
def mpcQp(prediction: MpcPredictionMatrices, weights: MpcWeights, reference: Vector[Double],
          x: Vector[Double], uPrev: Vector[Double], bounds: MpcBounds, limits: Limits): Either[MpcFailure, MpcQp] =
  val (g, psi) = forms(prediction)
  val phi = prediction.phi
  val (n, rowsY) = (phi.cols, phi.rows)
  val q0 = weights.q.headOption
  val r0 = weights.r.headOption
  val m  = r0.fold(0)(_.rows)
  val p  = q0.fold(0)(_.rows)
  val incremental = psi.isDefined
  def bad(d: String) = Left(MpcFailure.InvalidInput(d))
  def pairOk(b: Option[(Vector[Double], Vector[Double])], k: Int) =
    b.forall((lo, hi) => lo.size == k && hi.size == k && lo.zip(hi).forall((l, h) => !l.isNaN && !h.isNaN && l <= h))
  lazy val np = rowsY / p
  lazy val nc = g.cols / m
  lazy val terminalError: Option[String] = weights.terminal.flatMap { t =>
    val (gt, psiT) = forms(t.prediction)
    if psiT.isDefined != incremental then Some("the terminal prediction must be in the same form as the output prediction")
    else if t.prediction.phi.rows != n || t.prediction.phi.cols != n || gt.cols != g.cols then
      Some(s"the terminal prediction must have $n rows over the same moves")
    else if t.weight.rows != n || t.weight.cols != n || !t.weight.isFinite then Some(s"the terminal weight must be a finite ${n}x$n")
    else if t.target.exists(v => v.size != n || v.exists(!_.isFinite)) then Some(s"the terminal target needs $n finite entries")
    else None
  }
  if p == 0 || weights.q.exists(w => w.rows != p || w.cols != p) || rowsY % p != 0 then
    bad(s"Q must be square with as many rows as outputs")
  else if m == 0 || weights.r.exists(w => w.rows != m || w.cols != m) || g.cols % m != 0 then
    bad(s"R must be square with as many rows as inputs")
  else if weights.q.size != 1 && weights.q.size != np then bad(s"there must be one output weight or $np")
  else if weights.r.size != 1 && weights.r.size != nc then bad(s"there must be one move weight or $nc")
  else if weights.inputTarget.isDefined && incremental then bad("an input target has no meaning for increments")
  else if weights.inputTarget.exists(v => (v.size != m && v.size != nc * m) || v.exists(!_.isFinite)) then
    bad(s"the input target needs $m or ${nc * m} finite entries")
  else if terminalError.isDefined then bad(terminalError.getOrElse(""))
  else if x.size != n then bad(s"the state has ${x.size} entries for $n states")
  else if uPrev.size != m then bad(s"u(k-1) has ${uPrev.size} entries for $m inputs")
  else if reference.size != p && reference.size != rowsY then bad(s"the reference must have $p or $rowsY entries")
  else if !pairOk(bounds.u, m) || !pairOk(bounds.du, m) || !pairOk(bounds.y, p) then
    bad("a bound has the wrong length, a NaN, or a lower above its upper")
  else if bounds.soft.exists(s => s.linear.size != p || s.quadratic.size != p ||
                                  s.linear.exists(w => !(w >= 0)) || s.quadratic.exists(w => !(w > 0))) then
    bad("soft outputs need one linear weight >= 0 and one quadratic weight > 0 per output")
  else if !(weights.q ++ weights.r).forall(_.isFinite) || (x ++ uPrev ++ reference).exists(!_.isFinite) then
    bad("every entry must be finite")
  else
    val nz  = nc * m
    val ns  = if bounds.soft.isDefined then p else 0
    val ref = if reference.size == rowsY then reference else Vector.fill(np)(reference).flatten
    // c0 = Φx + Ψu(k−1), the free response
    val c0 = Vector.tabulate(rowsY)(i =>
      (0 until n).map(j => phi(i, j) * x(j)).sum + psi.fold(0.0)(ps => (0 until m).map(j => ps(i, j) * uPrev(j)).sum))
    val qBar = blockDiagonal(Vector.tabulate(np)(i => weights.q(if weights.q.size == 1 then 0 else i)))
    val rBar = blockDiagonal(Vector.tabulate(nc)(j => weights.r(if weights.r.size == 1 then 0 else j)))
    val gtq  = g.transpose.multiply(qBar)
    val target = weights.inputTarget.map(v => if v.size == nz then v else Vector.fill(nc)(v).flatten)
    // the terminal cost: x(k+Np) = d0 + Gx·Z
    val term = weights.terminal.map { t =>
      val (gx, psiX) = forms(t.prediction)
      val d0 = Vector.tabulate(n)(i => (0 until n).map(j => t.prediction.phi(i, j) * x(j)).sum +
        psiX.fold(0.0)(ps => (0 until m).map(j => ps(i, j) * uPrev(j)).sum))
      val dev = t.target.fold(d0)(xs => d0.zip(xs).map(_ - _))
      val gtp = gx.transpose.multiply(t.weight)
      (gtp.multiply(gx), gtp.multiply(_MatrixValue(n, 1, dev.toArray)).toVector)
    }
    val hz0  = gtq.multiply(g).add(rBar)
    val hz   = term.fold(hz0)((ht, _) => hz0.add(ht)).scale(2.0)
    val fz0  = gtq.multiply(_MatrixValue(rowsY, 1, c0.zip(ref).map(_ - _).toArray)).toVector
    val fz1  = target.fold(fz0)(us => fz0.zip(mulVec(rBar, us)).map(_ - _))
    val fz   = term.fold(fz1)((_, ft) => fz1.zip(ft).map(_ + _)).map(2.0 * _)
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

/** The moves' matrix of a prediction and, for the incremental form, `Ψ`. */
private def forms(prediction: MpcPredictionMatrices): (_MatrixValue, Option[_MatrixValue]) = prediction match
  case MpcPrediction(_, gamma)                => (gamma, None)
  case MpcIncrementalPrediction(_, ps, theta) => (theta, Some(ps))

/** The block diagonal of square blocks. */
private def blockDiagonal(blocks: Vector[_MatrixValue]): _MatrixValue =
  val size = blocks.map(_.rows).sum
  val out  = new Array[Double](size * size)
  var off  = 0
  for blk <- blocks do
    for i <- 0 until blk.rows; j <- 0 until blk.cols do out((off + i) * size + off + j) = blk(i, j)
    off += blk.rows
  _MatrixValue(size, size, out)

private def mulVec(mv: _MatrixValue, v: Vector[Double]): Vector[Double] =
  Vector.tabulate(mv.rows)(i => (0 until mv.cols).map(j => mv(i, j) * v(j)).sum)

package it.grypho.scala.leonardo
package control

import core.*
import optimize.{qpPrepare, QpConstraint, QpPrepared, QpResult}


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
 *                increments for an incremental or an accumulated one
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
 *  @param prepared    the QP with `H` and the constraint matrix fixed, shared by every programme of one
 *                     [[MpcQpPrepared]] (F_0357), so a solve factors nothing again
 *
 *  Built only by [[mpcQp]] and [[MpcQpPrepared.at]]; equality reads the programme's data alone.
 */
final case class MpcQp private[leonardo] (h: _MatrixValue, f: Vector[Double], ineq: Option[(_MatrixValue, Vector[Double])],
                       lb: Vector[Double], ub: Vector[Double], m: Int, nc: Int, incremental: Boolean,
                       uPrev: Vector[Double])(private[leonardo] val prepared: QpPrepared):

  /** Solves the programme and maps the optimum back to the moves (F_0299).
   *  @param warm the previous step's active set, or empty
   *  @param env  the precision the certificate holds and the QP budgets
   *  @return the step, or why the QP declined
   */
  def solve(warm: Set[QpConstraint], env: Environment): Either[MpcFailure, MpcSolution] =
    prepared.solve(f, ineq.map(_._2), None, lb, ub, warm, env).left.map(MpcFailure.Qp(_)).map { r =>
      val moves = r.z.take(nc * m).grouped(m).toVector
      val now   = if incremental then uPrev.zip(moves.head).map(_ + _) else moves.head
      MpcSolution(moves, now, r.z.drop(nc * m), r)
    }

/** The terminal cost of an MPC programme (F_0304, AIACE L_0007): `‖x(k+Np) − x_s‖²_P`.
 *
 *  **The stage cost already weighs `y(k+Np)`**, so the weight that completes the infinite-horizon
 *  (LQR) cost-to-go is `P − CᵀQC` with `P` from [[dare]] / [[dlqr]], not `P` itself; with it an
 *  unconstrained MPC reproduces the LQR law `u = −K·x` exactly.  `P` alone also stabilises, more
 *  conservatively.
 *
 *  @param weight     `P`, `n×n`
 *  @param prediction the state prediction at the horizon's end, from [[mpcTerminalPrediction]] or
 *                    [[mpcAccumulatedTerminalPrediction]], in the same form (absolute, incremental or
 *                    accumulated) as the output prediction
 *  @param target     `x_s`, `n` entries; `None` for the origin
 */
final case class MpcTerminal(weight: _MatrixValue, prediction: MpcPredictionMatrices,
                             target: Option[Vector[Double]] = None)

/** The weights of an MPC programme (F_0304, F_0305; AIACE L_0007, L_0008).
 *
 *  @param q           the output weights: one `p×p` for every step, or `Np` of them, step by step
 *  @param r           the move weights: one `m×m` for every move, or `Nc` of them
 *  @param terminal    the terminal cost, or `None`
 *  @param inputTarget the absolute form's input target `u_s` (F_0305): `m` entries held, or `Nc·m`
 *                     stacked, the move cost becoming `‖u − u_s‖²_R`; `None` for `u_s = 0`.  Without
 *                     it the absolute form's `R` pulls the inputs towards zero, which offsets tracking
 *                     whenever the steady input is not zero.  Not accepted with an incremental or an
 *                     accumulated prediction, whose moves are increments: see `positionTarget`
 *  @param positionTarget the increment forms' cost on the input itself (F_0329), or `None`.  Not
 *                     accepted with an absolute prediction, where `inputTarget` with `r` is that cost
 */
final case class MpcWeights(q: Vector[_MatrixValue], r: Vector[_MatrixValue], terminal: Option[MpcTerminal] = None,
                            inputTarget: Option[Vector[Double]] = None,
                            positionTarget: Option[MpcPositionTarget] = None)

/** A cost on the **position** of the inputs when the moves are increments (F_0329, AIACE L_0013):
 *  `Σⱼ ‖u(k+j) − u_s,j‖²_Sⱼ` over the `Nc` decided steps, with `u(k+j) = u(k−1) + Δu(k) + … + Δu(k+j)`.
 *  In those forms `R` weighs the moves and nothing else weighs where the actuator sits, so a plant with
 *  more inputs than setpoints keeps whatever split it started from; this cost settles it (a pump as low
 *  as possible, a valve as open as possible).  With `L` the cumulative-sum matrix it adds `2·LᵀS̄L` to
 *  the Hessian and `2·LᵀS̄(1⊗u(k−1) − u_s)` to the linear term.  `S` need only be positive
 *  semidefinite — a zero row leaves that input free — as long as the whole Hessian stays positive
 *  definite, which `R > 0` ensures.
 *
 *  @param weight `S`: one `m×m` for every step, or `Nc` of them
 *  @param target `u_s`: `m` entries held, or `Nc·m` stacked
 */
final case class MpcPositionTarget(weight: Vector[_MatrixValue], target: Vector[Double])

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

/** The state prediction at the end of the horizon for a model whose input is an increment (F_0328):
 *  `x(k+Np) = Φₓ·x + Γₓ·ΔU`, the increments zero past `Nc` — the last block row of
 *  [[mpcAccumulatedPrediction]] for `C = I`, the terminal prediction an [[MpcAccumulatedPrediction]]
 *  goes with.
 *
 *  @param a      the state matrix, `n×n`
 *  @param b      the matrix of the increments, `n×m`
 *  @param np     the prediction horizon
 *  @param nc     the control horizon
 *  @param limits the budgets
 *  @return the prediction, `n` rows, or why not
 */
def mpcAccumulatedTerminalPrediction(a: _MatrixValue, b: _MatrixValue, np: Int, nc: Int,
                                     limits: Limits): Either[MpcFailure, MpcAccumulatedPrediction] =
  def last(mat: _MatrixValue) = _MatrixValue(a.rows, mat.cols,
    Array.tabulate(a.rows * mat.cols)(idx => mat(mat.rows - a.rows + idx / mat.cols, idx % mat.cols)))
  mpcAccumulatedPrediction(a, b, _MatrixValue.identity(a.rows), np, nc, limits)
    .map(p => MpcAccumulatedPrediction(last(p.phi), last(p.gamma)))

/** Builds the condensed MPC quadratic programme (F_0299, AIACE L_0002) with one weight for every step:
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
 *  `u(k+j)` for an absolute prediction and the increment `Δu(k+j)` for an incremental or an accumulated
 *  one — plus the slack penalties and, in the increment forms, the position cost of
 *  [[MpcPositionTarget]]; with `Y = c₀ + G·Z`, `c₀ = Φx + Ψu(k−1)` (`Φx` alone for an accumulated
 *  prediction, whose state holds the past input), and `x(k+Np) = d₀ + Gₓ·Z`, it is
 *  `½ Zᵀ·2(GᵀQ̄G + R̄ + GₓᵀPGₓ)Z + (2GᵀQ̄(c₀ − r) − 2R̄u_s + 2GₓᵀP(d₀ − x_s))ᵀZ`.
 *  A bound on the decision variable itself is a bound of `z`; every other bound is a row, and an
 *  infinite bound is no row at all: in the increment forms `MpcBounds.du` bounds `z` and `MpcBounds.u`
 *  is rows over the cumulative sums `u(k−1) + Δu(k) + … + Δu(k+j)`.
 *
 *  @param prediction the prediction matrices: absolute, incremental (F_0298) or accumulated (F_0328)
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
  mpcQpPrepared(prediction, weights, bounds, limits).flatMap(_.at(reference, x, uPrev, bounds))

/** Builds the part of the MPC programme that does not change between cycles (F_0357, AIACE L_0076):
 *  everything [[mpcQp]] builds except what depends on the state, `u(k−1)`, the reference and the
 *  bounds' values — the Hessian, the constraint matrix, and the QP's factor of `H` and its constraint
 *  normals.  [[MpcQpPrepared.at]] then forms only the linear term and the right-hand sides, so a
 *  controller prepares once per model and tuning and calls `at` each cycle.  [[mpcQp]] is a prepare
 *  followed by one `at`, so the programmes are the same, and their solutions too, to the last bit.
 *
 *  **The bounds given here fix the shape** (the owner's Decision B): which of `u`, `du` and `y` are
 *  bounded, which entries of each are finite, and the soft outputs.  An infinite bound is no row, so
 *  a bound turning finite would be another programme; `at` refuses bounds of another shape, and their
 *  finite values may change freely.
 *
 *  @param prediction the prediction matrices: absolute, incremental (F_0298) or accumulated (F_0328)
 *  @param weights    the step weights, the terminal cost and the input or position target
 *  @param bounds     the bounds whose shape is fixed, and the soft outputs
 *  @param limits     the budgets (the QP's own are checked when it is solved)
 *  @return the prepared programme, or why it cannot be built
 */
def mpcQpPrepared(prediction: MpcPredictionMatrices, weights: MpcWeights, bounds: MpcBounds,
                  limits: Limits): Either[MpcFailure, MpcQpPrepared] =
  val (g, psi, form) = forms(prediction)
  val phi = prediction.phi
  val (n, rowsY) = (phi.cols, phi.rows)
  val q0 = weights.q.headOption
  val r0 = weights.r.headOption
  val m  = r0.fold(0)(_.rows)
  val p  = q0.fold(0)(_.rows)
  val incremental = form != MoveForm.Absolute      // the moves are increments
  def bad(d: String) = Left(MpcFailure.InvalidInput(d))
  lazy val np = rowsY / p
  lazy val nc = g.cols / m
  lazy val terminalError: Option[String] = weights.terminal.flatMap { t =>
    val (gt, _, formT) = forms(t.prediction)
    if formT != form then Some("the terminal prediction must be in the same form as the output prediction")
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
  else if weights.positionTarget.isDefined && !incremental then
    bad("a position target is for moves that are increments: in the absolute form it is the input target with R")
  else if weights.positionTarget.exists(t => (t.weight.size != 1 && t.weight.size != nc) ||
                                             t.weight.exists(w => w.rows != m || w.cols != m || !w.isFinite)) then
    bad(s"the position weight must be one finite ${m}x$m matrix or $nc of them")
  else if weights.positionTarget.exists(t => (t.target.size != m && t.target.size != nc * m) || t.target.exists(!_.isFinite)) then
    bad(s"the position target needs $m or ${nc * m} finite entries")
  else if terminalError.isDefined then bad(terminalError.getOrElse(""))
  else if !boundsOk(bounds, m, p) then
    bad("a bound has the wrong length, a NaN, or a lower above its upper")
  else if bounds.soft.exists(s => s.linear.size != p || s.quadratic.size != p ||
                                  s.linear.exists(w => !(w >= 0)) || s.quadratic.exists(w => !(w > 0))) then
    bad("soft outputs need one linear weight >= 0 and one quadratic weight > 0 per output")
  else if !(weights.q ++ weights.r).forall(_.isFinite) then bad("every entry must be finite")
  else
    val nz  = nc * m
    val ns  = if bounds.soft.isDefined then p else 0
    val qBar = Dense.blockDiagonal(Vector.tabulate(np)(i => weights.q(if weights.q.size == 1 then 0 else i)))
    val rBar = Dense.blockDiagonal(Vector.tabulate(nc)(j => weights.r(if weights.r.size == 1 then 0 else j)))
    val gtq  = g.transpose.multiply(qBar)
    // the input target's part of the linear term, R̄·u_s; `at` forms it again for a cycle's own target
    val rTarget = weights.inputTarget.map(v => Dense.times(rBar, if v.size == nz then v else Vector.fill(nc)(v).flatten))
    // the terminal cost: x(k+Np) = d0 + Gx·Z, its d0 formed each cycle by `at`
    val term = weights.terminal.map { t =>
      val (gx, psiX, _) = forms(t.prediction)
      val gtp = gx.transpose.multiply(t.weight)
      (gtp.multiply(gx), MpcTerminalPart(t.prediction.phi, psiX, t.target, gtp))
    }
    // the position cost: u = 1⊗u(k−1) + L·Z, L the cumulative-sum matrix the u-bound rows also are
    val position = weights.positionTarget.map { t =>
      val sBar = Dense.blockDiagonal(Vector.tabulate(nc)(j => t.weight(if t.weight.size == 1 then 0 else j)))
      val l    = _MatrixValue(nz, nz, Array.tabulate(nz * nz) { idx =>
        val (i, j) = (idx / nz, idx % nz)
        if i % m == j % m && j / m <= i / m then 1.0 else 0.0
      })
      val us   = if t.target.size == nz then t.target else Vector.fill(nc)(t.target).flatten
      val lts  = l.transpose.multiply(sBar)
      (lts.multiply(l), (lts, us))
    }
    val hz0  = gtq.multiply(g).add(rBar)
    val hz1  = position.fold(hz0)((hp, _) => hz0.add(hp))
    val hz   = term.fold(hz1)((ht, _) => hz1.add(ht)).scale(2.0)
    val size = nz + ns
    val hData = Array.tabulate(size * size) { idx =>
      val (i, j) = (idx / size, idx % size)
      if i < nz && j < nz then hz(i, j)
      else if i == j then 2.0 * bounds.soft.fold(0.0)(_.quadratic(i - nz))
      else 0.0
    }

    // The rows' coefficients; `at` supplies their right-hand sides in the same order.  An infinite
    // bound is no row, read here from the bounds that fix the shape.
    val rows = Vector.newBuilder[Array[Double]]
    def row(coeffs: => Array[Double], bound: Double): Unit = if bound.isFinite then rows += coeffs
    // the outputs: G·Z − s ≤ yₘₐₓ − c₀ and −G·Z − s ≤ −(yₘᵢₙ − c₀)
    bounds.y.foreach { (lo, hi) =>
      for i <- 0 until rowsY do
        val o = i % p
        def coeffs(sign: Double) = Array.tabulate(size)(j =>
          if j < nz then sign * g(i, j) else if j - nz == o then -1.0 else 0.0)
        row(coeffs(1.0), hi(o))
        row(coeffs(-1.0), lo(o))
    }
    // the bound on the quantity that is not the decision variable, as rows over the moves
    def cumulativeRows(lo: Vector[Double], hi: Vector[Double], coefficient: (Int, Int) => Double): Unit =
      for step <- 0 until nc; k <- 0 until m do
        def coeffs(sign: Double) = Array.tabulate(size)(j =>
          if j < nz && j % m == k then sign * coefficient(step, j / m) else 0.0)
        row(coeffs(1.0), hi(k))
        row(coeffs(-1.0), lo(k))
    if incremental then
      // u(k+j) = u(k−1) + Σ_{l ≤ j} Δu_l
      bounds.u.foreach((lo, hi) => cumulativeRows(lo, hi, (j, l) => if l <= j then 1.0 else 0.0))
    else
      // Δu(k+j) = u_j − u_{j−1}, the first against u(k−1)
      bounds.du.foreach((lo, hi) => cumulativeRows(lo, hi,
        (j, l) => if l == j then 1.0 else if l == j - 1 then -1.0 else 0.0))
    val built = rows.result()
    val cells = size.toLong * size + built.size.toLong * size
    if cells > limits(Limit.MaxMatrixCells) then Left(MpcFailure.TooLarge(cells))
    else
      val h    = _MatrixValue(size, size, hData)
      val ineq = Option.when(built.nonEmpty)(_MatrixValue(built.size, size, built.flatten.toArray))
      val (lb, ub) = zBounds(bounds, incremental, nz, ns, m)
      Right(new MpcQpPrepared(phi, psi, gtq, Option.when(!incremental)(rBar), rTarget, term.map(_._2), position.map(_._2),
                              h, ineq, bounds, n, m, p, np, nc, incremental, qpPrepare(h, ineq, None, lb, ub)))

/** The bounds of `z`: the decision variable's own (`u` for an absolute prediction, `du` for an
 *  increment form), then the slacks', `[0, ∞)`. */
private def zBounds(bounds: MpcBounds, incremental: Boolean, nz: Int, ns: Int, m: Int): (Vector[Double], Vector[Double]) =
  val direct = if incremental then bounds.du else bounds.u
  (Vector.tabulate(nz)(j => direct.fold(Double.NegativeInfinity)(_._1(j % m))) ++ Vector.fill(ns)(0.0),
   Vector.tabulate(nz)(j => direct.fold(Double.PositiveInfinity)(_._2(j % m))) ++ Vector.fill(ns)(Double.PositiveInfinity))

/** Every bound pair the right length, with no NaN and no lower above its upper. */
private def boundsOk(b: MpcBounds, m: Int, p: Int): Boolean =
  def pairOk(x: Option[(Vector[Double], Vector[Double])], k: Int) =
    x.forall((lo, hi) => lo.size == k && hi.size == k && lo.zip(hi).forall((l, h) => !l.isNaN && !h.isNaN && l <= h))
  pairOk(b.u, m) && pairOk(b.du, m) && pairOk(b.y, p)

/** `Σⱼ mᵢⱼ·vⱼ` over `j < k`, summed from the first term the way `Seq.sum` reduces a sequence of known
 *  size (0 for `k = 0`): the free response and `Dense.times` were written so, and a prepared
 *  programme must give the same bits. */
private def rowDot(mv: _MatrixValue, i: Int, v: Array[Double], k: Int): Double =
  if k == 0 then 0.0
  else
    var s = mv(i, 0) * v(0)
    var j = 1
    while j < k do
      s += mv(i, j) * v(j)
      j += 1
    s

/** The terminal cost's per-cycle data: `x(k+Np) = d0 + Gₓ·Z` with `d0 = Φₓx + Ψₓu(k−1)`. */
private[control] final case class MpcTerminalPart(phi: _MatrixValue, psi: Option[_MatrixValue],
                                                  target: Option[Vector[Double]], gtp: _MatrixValue)

/** The part of an MPC programme fixed for one model, tuning and bound shape (F_0357, AIACE L_0076),
 *  built by [[mpcQpPrepared]]: the Hessian, the constraint matrix and the QP's factor of `H`.  Each
 *  cycle [[at]] forms only what the state, `u(k−1)`, the reference and the bounds' values change —
 *  the linear term and the right-hand sides — and every programme it builds solves through the one
 *  prepared QP.  Immutable, so one may serve several threads.
 */
final class MpcQpPrepared private[control] (phi: _MatrixValue, psi: Option[_MatrixValue], gtq: _MatrixValue,
                                            rBar: Option[_MatrixValue], rTarget: Option[Vector[Double]],
                                            term: Option[MpcTerminalPart],
                                            position: Option[(_MatrixValue, Vector[Double])], h: _MatrixValue,
                                            ineq: Option[_MatrixValue], shape: MpcBounds,
                                            n: Int, m: Int, p: Int, np: Int, nc: Int, incremental: Boolean,
                                            qp: QpPrepared):

  /** The programme for one cycle: the same as [[mpcQp]] with these arguments and the prepared
   *  prediction and weights, each target given here replacing the prepared one.
   *
   *  **The targets enter only the linear term** (F_0360, AIACE L_0080), never the Hessian, the rows or the
   *  prepared QP, so a controller whose steady state moves every cycle (a target layer's `x_s` and `u_s`)
   *  still prepares once.  A target is accepted only where the prepared weights carry its cost: the input
   *  target in the absolute form (whose `R` always weighs `u − u_s`), the position target where
   *  [[MpcWeights.positionTarget]] was given, the terminal target where [[MpcWeights.terminal]] was.
   *
   *  @param reference      the reference: `p` entries held over the horizon, or `Np·p` stacked
   *  @param x              the current state, `n` entries
   *  @param uPrev          `u(k−1)`, `m` entries
   *  @param bounds         the bounds, in the prepared shape: the same groups, finite at the same entries,
   *                        the same soft outputs; their finite values may differ
   *  @param inputTarget    this cycle's `u_s` for the absolute form, `m` or `Nc·m` entries; `None` for the
   *                        prepared one
   *  @param positionTarget this cycle's position target `u_s`, `m` or `Nc·m` entries; `None` for the
   *                        prepared one
   *  @param terminalTarget this cycle's `x_s`, `n` entries; `None` for the prepared one
   *  @return the programme, or why it cannot be built
   */
  def at(reference: Vector[Double], x: Vector[Double], uPrev: Vector[Double], bounds: MpcBounds,
         inputTarget: Option[Vector[Double]] = None, positionTarget: Option[Vector[Double]] = None,
         terminalTarget: Option[Vector[Double]] = None): Either[MpcFailure, MpcQp] =
    val rowsY = phi.rows
    val nz    = nc * m
    def bad(d: String) = Left(MpcFailure.InvalidInput(d))
    def stacked(v: Vector[Double]) = v.size == m || v.size == nz
    if inputTarget.isDefined && rBar.isEmpty then bad("an input target has no meaning for increments")
    else if inputTarget.exists(v => !stacked(v) || v.exists(!_.isFinite)) then
      bad(s"the input target needs $m or $nz finite entries")
    else if positionTarget.isDefined && position.isEmpty then bad("the prepared weights have no position cost")
    else if positionTarget.exists(v => !stacked(v) || v.exists(!_.isFinite)) then
      bad(s"the position target needs $m or $nz finite entries")
    else if terminalTarget.isDefined && term.isEmpty then bad("the prepared weights have no terminal cost")
    else if terminalTarget.exists(v => v.size != n || v.exists(!_.isFinite)) then
      bad(s"the terminal target needs $n finite entries")
    else if x.size != n then bad(s"the state has ${x.size} entries for $n states")
    else if uPrev.size != m then bad(s"u(k-1) has ${uPrev.size} entries for $m inputs")
    else if reference.size != p && reference.size != rowsY then bad(s"the reference must have $p or $rowsY entries")
    else if !boundsOk(bounds, m, p) then bad("a bound has the wrong length, a NaN, or a lower above its upper")
    else if !sameShape(bounds) then
      bad("the bounds must have the prepared shape: the same groups, finite at the same entries, the same soft outputs")
    else if (x ++ uPrev ++ reference).exists(!_.isFinite) then bad("every entry must be finite")
    else
      val xa  = x.toArray
      val ua  = uPrev.toArray
      val ref = if reference.size == rowsY then reference else Vector.fill(np)(reference).flatten
      // c0 = Φx + Ψu(k−1), the free response
      val c0 = new Array[Double](rowsY)
      var i = 0
      while i < rowsY do
        c0(i) = rowDot(phi, i, xa, n) + (psi match { case Some(ps) => rowDot(ps, i, ua, m); case None => 0.0 })
        i += 1
      val dev = new Array[Double](rowsY)
      i = 0
      while i < rowsY do
        dev(i) = c0(i) - ref(i)
        i += 1
      val fq = gtq.multiply(_MatrixValue(rowsY, 1, dev))
      def spread(v: Vector[Double]) = if v.size == nz then v else Vector.fill(nc)(v).flatten
      val rt = inputTarget.fold(rTarget)(v => rBar.map(rb => Dense.times(rb, spread(v))))
      val fp = position.map { (lts, prepared) =>
        val us = positionTarget.fold(prepared)(spread)
        val v = new Array[Double](nz)
        for k <- 0 until nz do v(k) = ua(k % m) - us(k)
        Vector.tabulate(nz)(k => rowDot(lts, k, v, nz))
      }
      val ft = term.map { t =>
        val d = new Array[Double](n)
        for k <- 0 until n do
          val d0 = rowDot(t.phi, k, xa, n) + (t.psi match { case Some(ps) => rowDot(ps, k, ua, m); case None => 0.0 })
          d(k) = terminalTarget.orElse(t.target).fold(d0)(xs => d0 - xs(k))
        t.gtp.multiply(_MatrixValue(n, 1, d))
      }
      // the same element-wise steps as the one-shot assembly: −R̄u_s, + the position, + the terminal, ×2
      val fz = Vector.tabulate(nz) { j =>
        val f0 = fq(j, 0)
        val f1 = rt.fold(f0)(v => f0 - v(j))
        val f2 = fp.fold(f1)(v => f1 + v(j))
        val f3 = ft.fold(f2)(v => f2 + v(j, 0))
        2.0 * f3
      }
      val f = fz ++ bounds.soft.fold(Vector.empty[Double])(_.linear)
      // the right-hand sides, in the order mpcQpPrepared laid the rows out
      val rhs = Vector.newBuilder[Double]
      bounds.y.foreach { (lo, hi) =>
        for i <- 0 until rowsY do
          val o = i % p
          if hi(o).isFinite then rhs += hi(o) - c0(i)
          if lo(o).isFinite then rhs += -(lo(o) - c0(i))
      }
      def cumulative(lo: Vector[Double], hi: Vector[Double], offset: Int => Double): Unit =
        for step <- 0 until nc; k <- 0 until m do
          if hi(k).isFinite then rhs += hi(k) - offset(k) * (if step == 0 || incremental then 1.0 else 0.0)
          if lo(k).isFinite then rhs += -(lo(k) - offset(k) * (if step == 0 || incremental then 1.0 else 0.0))
      if incremental then bounds.u.foreach((lo, hi) => cumulative(lo, hi, k => ua(k)))
      else bounds.du.foreach((lo, hi) => cumulative(lo, hi, k => -ua(k)))
      val (lb, ub) = zBounds(bounds, incremental, nz, if bounds.soft.isDefined then p else 0, m)
      Right(MpcQp(h, f, ineq.map(a => (a, rhs.result())), lb, ub, m, nc, incremental, uPrev)(qp))

  /** The same bound groups, finite at the same entries, and the same soft outputs. */
  private def sameShape(b: MpcBounds): Boolean =
    def pattern(x: Option[(Vector[Double], Vector[Double])]) = x.map((lo, hi) => (lo.map(_.isFinite), hi.map(_.isFinite)))
    pattern(b.u) == pattern(shape.u) && pattern(b.du) == pattern(shape.du) && pattern(b.y) == pattern(shape.y) &&
      b.soft == shape.soft

/** What a prediction's moves are: the inputs, or increments — with `u(k−1)` outside the state
 *  (incremental, a `Ψ` term) or inside it (accumulated, none). */
private enum MoveForm:
  case Absolute, Incremental, Accumulated

/** The moves' matrix of a prediction, `Ψ` for the incremental form, and the form. */
private def forms(prediction: MpcPredictionMatrices): (_MatrixValue, Option[_MatrixValue], MoveForm) = prediction match
  case MpcPrediction(_, gamma)                => (gamma, None, MoveForm.Absolute)
  case MpcIncrementalPrediction(_, ps, theta) => (theta, Some(ps), MoveForm.Incremental)
  case MpcAccumulatedPrediction(_, gamma)     => (gamma, None, MoveForm.Accumulated)

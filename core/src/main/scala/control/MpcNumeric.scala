package it.grypho.scala.leonardo
package control

import core.*


/** Why an MPC construction gave no answer (F_0298–F_0300): a reason at the Scala level, the F_0052
 *  Decision A arrangement every numeric tier of the library follows. */
enum MpcFailure:
  /** An operand has the wrong shape, a horizon is out of range, or an entry is not finite. */
  case InvalidInput(detail: String)
  /** A result would exceed the environment's `Limit.MaxMatrixCells`. */
  case TooLarge(cells: Long)
  /** A disturbance model the plant cannot estimate (F_0300); the detail says which condition failed. */
  case NotDetectable(detail: String)
  /** The quadratic programme declined (F_0299, F_0300). */
  case Qp(why: optimize.QpFailure)
  /** The Kalman gain of the augmented model could not be computed (F_0300). */
  case Riccati(why: RiccatiFailure)

/** The absolute-input prediction matrices (F_0298): `Y = Φ·x(k) + Γ·U` with `Y` the stacked outputs
 *  `y(k+1) … y(k+Np)` and `U` the `Nc` moves `u(k) … u(k+Nc−1)`, the last held over the rest of the
 *  horizon — the convention of [[mpcMatrices]] exactly.
 *
 *  @param phi   `Φ`, `(Np·p)×n`, stacking `C·Aⁱ` for `i = 1 … Np`
 *  @param gamma `Γ`, `(Np·p)×(Nc·m)`, its last column accumulating the held move
 */
final case class MpcPrediction(phi: _MatrixValue, gamma: _MatrixValue) extends MpcPredictionMatrices

/** Any form of the numeric prediction matrices (F_0298), which [[mpcQp]] takes (F_0299): the
 *  absolute form decides the moves `U`, the incremental and the accumulated ones (F_0328) the
 *  increments `ΔU`. */
sealed trait MpcPredictionMatrices:
  /** `Φ`, the free response's dependence on the state. */
  def phi: _MatrixValue

/** The incremental (delta-u) prediction matrices (F_0298): `Y = Φ·x(k) + Ψ·u(k−1) + Θ·ΔU`, with
 *  `u(k+j) = u(k−1) + Δu(k) + … + Δu(k+j)` and `Δu` zero past `Nc` — the form offset-free tracking
 *  needs, since a constant input costs nothing in `ΔU`.
 *
 *  @param phi   `Φ`, as in [[MpcPrediction]]
 *  @param psi   `Ψ`, `(Np·p)×m`, its block `i` the accumulated Markov sum `Σ_{j ≤ i} C·Aʲ·B`
 *  @param theta `Θ`, `(Np·p)×(Nc·m)`, its block `(i, l)` the sum `Σ_{j ≤ i−l} C·Aʲ·B` below the
 *               diagonal and zero above
 */
final case class MpcIncrementalPrediction(phi: _MatrixValue, psi: _MatrixValue, theta: _MatrixValue)
    extends MpcPredictionMatrices

/** The prediction matrices of a model whose **input is already an increment** `Δu` of an accumulated
 *  input (F_0328, APAC L_0012) — [[arx2nmssIncremental]]'s, or any model with `u(k−1)` in its state:
 *  `Y = Φ·x(k) + Γ·ΔU`.  The moves are increments, as in [[MpcIncrementalPrediction]], so [[mpcQp]]
 *  reads `MpcBounds.u` as bounds on the position `u(k−1) + Δu(k) + … + Δu(k+j)` and `MpcBounds.du` as
 *  bounds on the moves; but the past input is in the state, so there is no `Ψ·u(k−1)` term.
 *
 *  **Not an [[MpcPrediction]] of the same model**: that one holds its last move over the rest of the
 *  horizon, which for an increment is a ramp of the input.  Here the increments are **zero past `Nc`**,
 *  so the input is held — the two agree only when `Nc = Np`.  Build it with [[mpcAccumulatedPrediction]].
 *
 *  @param phi   `Φ`, as in [[MpcPrediction]]
 *  @param gamma `Γ`, `(Np·p)×(Nc·m)`, its block `(i, l)` the Markov parameter `C·Aⁱ⁻ˡ·B` on and below
 *               the diagonal and zero above
 */
final case class MpcAccumulatedPrediction(phi: _MatrixValue, gamma: _MatrixValue) extends MpcPredictionMatrices

/** The numeric prediction matrices of `(A, B, C)` in absolute inputs (F_0298, APAC L_0001): the dense
 *  counterpart of [[mpcMatrices]], computed over arrays for a controller that rebuilds them each cycle.
 *
 *  @param a      the state matrix, `n×n`
 *  @param b      the input matrix, `n×m`
 *  @param c      the output matrix, `p×n`
 *  @param np     the prediction horizon, `1 ≤ Np ≤ Limit.MaxMpcHorizon`
 *  @param nc     the control horizon, `1 ≤ Nc ≤ Np`
 *  @param limits the budgets: [[core.Limit.MaxMpcHorizon]] and [[core.Limit.MaxMatrixCells]]
 *  @return `(Φ, Γ)`, or why not
 */
def mpcPrediction(a: _MatrixValue, b: _MatrixValue, c: _MatrixValue, np: Int, nc: Int,
                  limits: Limits): Either[MpcFailure, MpcPrediction] =
  markovTerms(a, b, c, np, nc, limits).map { t =>
    MpcPrediction(t.phi, blocks(t, np, nc, t.p, t.m, (i, j) =>
      if i < j then None else if j < nc - 1 then Some(t.markov(i - j)) else Some(t.held(i - j))))
  }

/** The numeric prediction matrices of `(A, B, C)` in increments (F_0298, APAC L_0001).
 *
 *  @param a      the state matrix, `n×n`
 *  @param b      the input matrix, `n×m`
 *  @param c      the output matrix, `p×n`
 *  @param np     the prediction horizon, `1 ≤ Np ≤ Limit.MaxMpcHorizon`
 *  @param nc     the control horizon, `1 ≤ Nc ≤ Np`
 *  @param limits the budgets: [[core.Limit.MaxMpcHorizon]] and [[core.Limit.MaxMatrixCells]]
 *  @return `(Φ, Ψ, Θ)`, or why not
 */
def mpcIncrementalPrediction(a: _MatrixValue, b: _MatrixValue, c: _MatrixValue, np: Int, nc: Int,
                             limits: Limits): Either[MpcFailure, MpcIncrementalPrediction] =
  markovTerms(a, b, c, np, nc, limits).map { t =>
    MpcIncrementalPrediction(t.phi,
      blocks(t, np, 1, t.p, t.m, (i, _) => Some(t.held(i))),
      blocks(t, np, nc, t.p, t.m, (i, l) => Option.when(l <= i)(t.held(i - l))))
  }

/** The numeric prediction matrices of a model `(A, B, C)` whose input is an increment of an
 *  accumulated input (F_0328, APAC L_0012): the increments are zero past `Nc`, see
 *  [[MpcAccumulatedPrediction]].  For a plant `(A, B, C)` in absolute inputs, the model
 *  `[[A, B], [0, I]]`, `[[B], [I]]`, `[C, 0]` with the state `[x, u(k−1)]` gives the same programme as
 *  [[mpcIncrementalPrediction]] of the plant.
 *
 *  @param a      the state matrix, `n×n`
 *  @param b      the matrix of the increments, `n×m`
 *  @param c      the output matrix, `p×n`
 *  @param np     the prediction horizon, `1 ≤ Np ≤ Limit.MaxMpcHorizon`
 *  @param nc     the control horizon, `1 ≤ Nc ≤ Np`
 *  @param limits the budgets: [[core.Limit.MaxMpcHorizon]] and [[core.Limit.MaxMatrixCells]]
 *  @return `(Φ, Γ)`, or why not
 */
def mpcAccumulatedPrediction(a: _MatrixValue, b: _MatrixValue, c: _MatrixValue, np: Int, nc: Int,
                             limits: Limits): Either[MpcFailure, MpcAccumulatedPrediction] =
  markovTerms(a, b, c, np, nc, limits).map { t =>
    MpcAccumulatedPrediction(t.phi, blocks(t, np, nc, t.p, t.m, (i, l) => Option.when(l <= i)(t.markov(i - l))))
  }

/** `C·Aⁱ` stacked, the Markov parameters `C·Aʲ·B` and their running sums, each block row-major. */
private final case class MarkovTerms(phi: _MatrixValue, markov: Vector[Array[Double]], held: Vector[Array[Double]],
                                     p: Int, m: Int)

private def markovTerms(a: _MatrixValue, b: _MatrixValue, c: _MatrixValue, np: Int, nc: Int,
                        limits: Limits): Either[MpcFailure, MarkovTerms] =
  val (n, m, p) = (a.rows, b.cols, c.rows)
  val shapeError =
    if a.cols != n then Some(s"A must be square, not ${a.rows}x${a.cols}")
    else if b.rows != n then Some(s"B must have $n rows, not ${b.rows}")
    else if c.cols != n then Some(s"C must have $n columns, not ${c.cols}")
    else if !(a.isFinite && b.isFinite && c.isFinite) then Some("every entry must be finite")
    else if nc < 1 || nc > np then Some(s"the horizons must satisfy 1 <= Nc <= Np, not Nc = $nc, Np = $np")
    else if np > limits(Limit.MaxMpcHorizon) then Some(s"Np = $np exceeds the limit of ${limits(Limit.MaxMpcHorizon)}")
    else None
  val cells = np.toLong * p * math.max(n, nc * m)
  shapeError match
    case Some(d) => Left(MpcFailure.InvalidInput(d))
    case None if cells > limits(Limit.MaxMatrixCells) => Left(MpcFailure.TooLarge(cells))
    case None =>
      // C·A^k for k = 0 … Np, each p×n
      val caPow = (1 to np).scanLeft(c)((ck, _) => ck.multiply(a)).toVector
      val markov = caPow.take(np).map(_.multiply(b).toVector.toArray)       // C·A^k·B, p×m
      val held = markov.tail.scanLeft(markov.head)((acc, x) => acc.zip(x).map(_ + _))
      val phiData = caPow.tail.flatMap(_.toVector).toArray
      Right(MarkovTerms(_MatrixValue(np * p, n, phiData), markov, held, p, m))

/** A `(rows·p)×(cols·m)` matrix of `p×m` blocks, `block(i, j)` or zero. */
private def blocks(t: MarkovTerms, rows: Int, cols: Int, p: Int, m: Int,
                   block: (Int, Int) => Option[Array[Double]]): _MatrixValue =
  val width = cols * m
  val out   = new Array[Double](rows * p * width)
  var i = 0
  while i < rows do
    var j = 0
    while j < cols do
      block(i, j).foreach { blk =>
        var r = 0
        while r < p do
          System.arraycopy(blk, r * m, out, (i * p + r) * width + j * m, m)
          r += 1
      }
      j += 1
    i += 1
  _MatrixValue(rows * p, width, out)

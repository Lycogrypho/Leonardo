package it.grypho.scala.leonardo
package control

import core.*
import matrix.{rankTolerance, subspaceBasis, Subspace}


// A realisation reduced to its McMillan degree (F_0307, AIACE L_0009).

/** Why [[minreal]] declined (F_0307). */
enum MinrealFailure:
  /** The operands do not form a realisation; `detail` says how. */
  case InvalidInput(detail: String)
  /** No state is both controllable and observable: the transfer function is `D` alone, which a
   *  state-space matrix with no rows cannot carry. */
  case NoState
  /** A controllability or observability matrix was past the library's decomposition budgets. */
  case TooLarge
  /** The reduced realisation's Markov parameters did not agree with the original's to the
   *  precision; `worst` is the largest disagreement relative to the terms forming it. */
  case Uncertified(worst: Double)

/** A minimal realisation (F_0307): `(A, B, C, D)` and its order, the McMillan degree.
 *  @param a     the state matrix, `order×order`
 *  @param b     the input matrix
 *  @param c     the output matrix
 *  @param d     the feedthrough, unchanged
 *  @param order the number of states */
final case class MinimalRealisation(a: _MatrixValue, b: _MatrixValue, c: _MatrixValue, d: _MatrixValue, order: Int)

/** Reduces a discrete- or continuous-time realisation to its controllable and observable part
 *  (F_0307, AIACE L_0009): the Kalman decomposition, which removes exactly the states that do not
 *  reach the input-output map.
 *
 *  First an orthonormal basis `U` of the controllable subspace (the range of `[B, AB, …, Aⁿ⁻¹B]`),
 *  which is `A`-invariant, so `(UᵀAU, UᵀB, CU)` has the same transfer function; then an orthonormal
 *  basis `V` of the orthogonal complement of its unobservable subspace (the range of the observability
 *  matrix's transpose), so `(VᵀA₁V, VᵀB₁, C₁V)` keeps it too.  Each rank is read by singular values at
 *  the library's rank tolerance, relative to the largest (the 2.11 rule).  **Certified**: the Markov
 *  parameters `C·Aᵏ·B`, `k = 0 … n + r − 1` (enough to fix both transfer functions), must agree, each
 *  to `0.5·10⁻ᵖ` of `‖C‖‖Aᵏ‖‖B‖`.  **Not** balanced truncation, which approximates and needs a
 *  stable plant.
 *
 *  @param a   the state matrix, `n×n`
 *  @param b   the input matrix, `n×m`
 *  @param c   the output matrix, `p×n`
 *  @param d   the feedthrough, `p×m`
 *  @param env the precision of the rank tolerance and the certificate, and the budgets
 *  @return the minimal realisation, or why not
 */
def minreal(a: _MatrixValue, b: _MatrixValue, c: _MatrixValue, d: _MatrixValue,
            env: Environment): Either[MinrealFailure, MinimalRealisation] =
  import MinrealFailure.*
  val (n, m, p) = (a.rows, b.cols, c.rows)
  if a.cols != n || b.rows != n || c.cols != n then Left(InvalidInput("A must be n x n, B n x m and C p x n"))
  else if d.rows != p || d.cols != m then Left(InvalidInput(s"D must be ${p}x$m"))
  else if !(a.isFinite && b.isFinite && c.isFinite && d.isFinite) then Left(InvalidInput("every entry must be finite"))
  else if n.toLong * n * math.max(m, p) > env.limits(Limit.MaxMatrixCells) then Left(TooLarge)
  else
    val tol = rankTolerance(env)
    def basis(mat: _MatrixValue): Either[MinrealFailure, Option[_MatrixValue]] =
      subspaceBasis(mat, Subspace.Range, tol, env.limits).toRight(TooLarge)
    for
      uc <- basis(Dense.krylov(a, b)).flatMap(_.toRight(NoState))
      a1  = uc.transpose.multiply(a).multiply(uc)
      b1  = uc.transpose.multiply(b)
      c1  = c.multiply(uc)
      // the observable part: the range of O₁ᵀ = [C₁ᵀ, A₁ᵀC₁ᵀ, …]
      vo <- basis(Dense.krylov(a1.transpose, c1.transpose)).flatMap(_.toRight(NoState))
      a2  = vo.transpose.multiply(a1).multiply(vo)
      b2  = vo.transpose.multiply(b1)
      c2  = c1.multiply(vo)
      _  <- markovAgree(a, b, c, a2, b2, c2, env)
    yield MinimalRealisation(a2, b2, c2, d, a2.rows)

/** The Markov parameters of the two realisations agree, against the largest term of either sequence.
 *
 *  **One scale for the whole sequence, not one per term** (F_0311): a term's own scale
 *  `‖C‖‖Aᵏ‖‖B‖` is exactly 0 once `Aᵏ = 0` (a nilpotent plant — a deadbeat one, the NMSS of an FIR
 *  model), while the reduced realisation, rotated by its orthonormal basis, leaves a rounding-level
 *  term there, and the per-term ratio was `∞`: a correct answer declined.  The largest term of either
 *  sequence is units-safe and is zero only for a system with no response, which [[minreal]] has already
 *  answered `NoState`. */
private def markovAgree(a: _MatrixValue, b: _MatrixValue, c: _MatrixValue, ar: _MatrixValue, br: _MatrixValue,
                      cr: _MatrixValue, env: Environment): Either[MinrealFailure, Unit] =
  def fro(x: _MatrixValue) = math.sqrt(x.toVector.map(v => v * v).sum)
  val count = a.rows + ar.rows
  val tol   = 0.5 * math.pow(10, -env.precision)
  val powers  = Iterator.iterate(_MatrixValue.identity(a.rows))(_.multiply(a)).take(count).toVector
  val powersR = Iterator.iterate(_MatrixValue.identity(ar.rows))(_.multiply(ar)).take(count).toVector
  val scale = math.max(powers.map(fro(c) * fro(_) * fro(b)).max, powersR.map(fro(cr) * fro(_) * fro(br)).max)
  val worstDiff = powers.zip(powersR).map { (ak, akr) =>
    val (mk, mr) = (c.multiply(ak).multiply(b), cr.multiply(akr).multiply(br))
    math.sqrt(mk.toVector.zip(mr.toVector).map((x, y) => (x - y) * (x - y)).sum)
  }.max
  val worst = if scale > 0 then worstDiff / scale else if worstDiff > 0 then Double.PositiveInfinity else 0.0
  Either.cond(worst <= tol, (), MinrealFailure.Uncertified(worst))

package it.grypho.scala.leonardo
package control

import core.*
import matrix.{rankTolerance, subspaceBasis, Subspace}


// A realisation reduced to its McMillan degree (F_0307, APAC L_0009).

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
 *  (F_0307, APAC L_0009): the Kalman decomposition, which removes exactly the states that do not
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
    // [B, AB, …, A^(n-1)B], n x (n·m)
    def krylov(aa: _MatrixValue, bb: _MatrixValue): _MatrixValue =
      val cols = (0 until aa.rows).scanLeft(bb)((x, _) => aa.multiply(x)).take(aa.rows)
      _MatrixValue(aa.rows, aa.rows * bb.cols,
        Array.tabulate(aa.rows * aa.rows * bb.cols)(idx =>
          val (r, col) = (idx / (aa.rows * bb.cols), idx % (aa.rows * bb.cols))
          cols(col / bb.cols)(r, col % bb.cols)))
    for
      uc <- basis(krylov(a, b)).flatMap(_.toRight(NoState))
      a1  = uc.transpose.multiply(a).multiply(uc)
      b1  = uc.transpose.multiply(b)
      c1  = c.multiply(uc)
      // the observable part: the range of O₁ᵀ = [C₁ᵀ, A₁ᵀC₁ᵀ, …]
      vo <- basis(krylov(a1.transpose, c1.transpose)).flatMap(_.toRight(NoState))
      a2  = vo.transpose.multiply(a1).multiply(vo)
      b2  = vo.transpose.multiply(b1)
      c2  = c1.multiply(vo)
      _  <- markovAgree(a, b, c, a2, b2, c2, env)
    yield MinimalRealisation(a2, b2, c2, d, a2.rows)

/** The Markov parameters of the two realisations agree, each against the size of its own terms. */
private def markovAgree(a: _MatrixValue, b: _MatrixValue, c: _MatrixValue, ar: _MatrixValue, br: _MatrixValue,
                      cr: _MatrixValue, env: Environment): Either[MinrealFailure, Unit] =
  def fro(x: _MatrixValue) = math.sqrt(x.toVector.map(v => v * v).sum)
  val count = a.rows + ar.rows
  val tol   = 0.5 * math.pow(10, -env.precision)
  val (cn, bn) = (fro(c), fro(b))
  var (ak, akr) = (_MatrixValue.identity(a.rows), _MatrixValue.identity(ar.rows))
  var worst = 0.0
  for _ <- 0 until count do
    val (mk, mr) = (c.multiply(ak).multiply(b), cr.multiply(akr).multiply(br))
    val scale = cn * fro(ak) * bn
    val diff  = math.sqrt(mk.toVector.zip(mr.toVector).map((x, y) => (x - y) * (x - y)).sum)
    if scale > 0 then worst = math.max(worst, diff / scale)
    else if diff > 0 then worst = Double.PositiveInfinity
    ak = ak.multiply(a)
    akr = akr.multiply(ar)
  Either.cond(worst <= tol, (), MinrealFailure.Uncertified(worst))

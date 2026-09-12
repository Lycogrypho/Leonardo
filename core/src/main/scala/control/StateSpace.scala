package it.grypho.scala.leonardo
package control

import core.*
import scalar.*
import matrix.*


/** State-space models and the discretisation that needs them (issue 6.29 slices 5 and 6).
 *
 *  **A model is a 1x4 `matrix._Matrix` of matrices**, exactly the shape `lu`/`qr`/`eig` return,
 *  so `at(model, 1, k)` indexes it and `:save` round-trips it with no new machinery
 *  (6.29 Decision A).
 */

/** Bundles `(A, B, C, D)` into the 1x4 row that represents a state-space model.
 *
 *  @param a state matrix
 *  @param b input matrix
 *  @param c output matrix
 *  @param d feedthrough matrix
 *  @return the model as a `_Matrix` node
 */
def stateSpace(a: _Expression, b: _Expression, c: _Expression, d: _Expression): _Expression =
  _Matrix(1, 4, Vector(a, b, c, d))

/** Reduces an expression to a dense matrix value, or `None`. */
private def denseOf(e: _Expression): Option[_MatrixValue] =
  e.eval(new Environment()) match
    case Right(m: _MatrixValue) => Some(m)
    case _                      => None

/** Controllability matrix `[B, A*B, A^2*B, …, A^(n-1)*B]`. */
private def ctrbOf(a: _MatrixValue, b: _MatrixValue): _MatrixValue =
  (1 until a.rows).foldLeft(Vector(b))((acc, _) => acc :+ a.multiply(acc.last))
    .reduce((l, r) => hcat(l, r))

/** Horizontal concatenation of two matrices with the same row count. */
private def hcat(l: _MatrixValue, r: _MatrixValue): _MatrixValue =
  val cols = l.cols + r.cols
  val out  = new Array[Double](l.rows * cols)
  for i <- 0 until l.rows do
    for j <- 0 until l.cols do out(i * cols + j) = l(i, j)
    for j <- 0 until r.cols do out(i * cols + l.cols + j) = r(i, j)
  _MatrixValue(l.rows, cols, out)

/** Full row rank test, by QR of the transpose.
 *
 *  `qrDecompose` returns `None` exactly when its input is rank-deficient (a column
 *  orthogonalises to zero norm), so `rank(M) = rows` is precisely `Mᵀ` decomposing.  The
 *  transpose is what makes it applicable: a controllability matrix is `n x (n*m)` and so is
 *  wide for every multi-input plant, while QR requires rows >= cols.
 *
 *  **Deliberately not `det(M * Mᵀ)`, which is the trap this replaced** (issue 2.11).  That
 *  form is wrong twice over.  Forming the Gram matrix **squares the condition number** — the
 *  very reason `statistics.leastSquares` solves by QR rather than by the normal equations, so
 *  using it here contradicted a rule the library had already settled.  And a determinant
 *  scales like `‖M‖^(2n)`, so any fixed threshold is really a statement about the *units* of
 *  the model: scaling `B` by `1e-3` — millivolts instead of volts — drove a perfectly
 *  controllable two-state plant to `1e-12` and reported it uncontrollable.  Controllability
 *  is invariant under that scaling, which is what the tests pin.
 */
private def fullRowRank(m: _MatrixValue): Boolean =
  m.transpose.qrDecompose.isDefined

/** Is the pair `(A, B)` controllable — can the input steer every state?
 *
 *  @param a the state matrix
 *  @param b the input matrix
 *  @return `Some(true)`/`Some(false)`, or `None` when either operand is not a dense matrix
 */
def controllable(a: _Expression, b: _Expression): Option[Boolean] =
  for
    am <- denseOf(a); bm <- denseOf(b)
    if am.rows == am.cols && bm.rows == am.rows
  yield fullRowRank(ctrbOf(am, bm))

/** Is the pair `(A, C)` observable — can the output distinguish every state?
 *
 *  Observability of `(A, C)` is controllability of `(Aᵀ, Cᵀ)` — the duality, used here rather
 *  than restated, so the two can never disagree.
 *
 *  @param a the state matrix
 *  @param c the output matrix
 *  @return `Some(true)`/`Some(false)`, or `None` when either operand is not a dense matrix
 */
def observable(a: _Expression, c: _Expression): Option[Boolean] =
  for
    am <- denseOf(a); cm <- denseOf(c)
    if am.rows == am.cols && cm.cols == am.rows
  yield fullRowRank(ctrbOf(am.transpose, cm.transpose))

/** **Exact** state-space discretisation over one sample period: `A_d = e^(A*Ts)`.
 *
 *  **Uses the block-matrix form, not `B_d = A⁻¹(A_d − I)B`, and that is a correctness matter
 *  rather than tidiness.**  Exponentiating `[[A, B], [0, 0]]*Ts` yields `[[A_d, B_d], [0, I]]`
 *  in one step, and it is defined for a **singular** `A` — where the inverse form is not, even
 *  though `B_d` exists.  A singular `A` is entirely ordinary: any system with an integrator
 *  has one, so the naive form would fail on a common case rather than an exotic one.
 *
 *  This is what issue 6.39 was carved out of 6.29 to supply, and what a Kalman filter needs.
 *
 *  @param a  the continuous state matrix
 *  @param b  the continuous input matrix
 *  @param ts the sample period
 *  @return `(A_d, B_d)`, or `None` when the operands are not conforming dense matrices
 */
def c2dExact(a: _Expression, b: _Expression, ts: Double): Option[(_MatrixValue, _MatrixValue)] =
  for
    am <- denseOf(a); bm <- denseOf(b)
    if am.rows == am.cols && bm.rows == am.rows
    n    = am.rows
    m    = bm.cols
    size = n + m
    block = _MatrixValue(size, size, Array.tabulate(size * size) { idx =>
      val (i, j) = (idx / size, idx % size)
      if i < n && j < n then am(i, j) * ts
      else if i < n then bm(i, j - n) * ts
      else 0.0
    })
    e <- block.expm
  yield
    (_MatrixValue(n, n, Array.tabulate(n * n)(idx => e(idx / n, idx % n))),
     _MatrixValue(n, m, Array.tabulate(n * m)(idx => e(idx / m, n + idx % m))))

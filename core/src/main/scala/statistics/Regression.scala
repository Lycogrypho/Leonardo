package it.grypho.scala.leonardo
package statistics

import core.*


/** Ordinary least squares — issue 4.Q slice B.
 *
 *  **Solved by QR, not by the normal equations**, and that is the whole design decision.
 *  `β = (XᵀX)⁻¹Xᵀy` is the textbook form and every piece of it already exists
 *  (`transpose`, `multiply`, `inverse`), but forming `XᵀX` **squares the condition number**:
 *  a design matrix that merely looks poorly scaled loses about half the available digits
 *  before the solve even starts.  It is the same class of trap as the one-pass variance next
 *  door and as the quadratic formula in issue 1.1 — an algebraically equivalent rearrangement
 *  that is numerically far worse.
 *
 *  `qrDecompose` gives `X = QR` with `R` upper triangular, so `Rβ = Qᵀy` is solved directly
 *  and the condition number is never squared.  It costs nothing extra: the decomposition was
 *  already there.
 *
 *  @see [[https://en.wikipedia.org/wiki/Ordinary_least_squares Ordinary least squares]]
 *  @see [[https://en.wikipedia.org/wiki/QR_decomposition#Using_for_solution_to_linear_inverse_problems QR for least squares]]
 */

/** Fits `y ≈ X·β` by ordinary least squares.
 *
 *  **No intercept is added.**  A constant term is a column of ones in `X`, which the caller
 *  supplies — silently inserting one would make `regress` mean something different from what
 *  was written, and there is no way to opt out of a hidden column.
 *
 *  @param x the design matrix, `n × p`, with `n ≥ p`
 *  @param y the response, `n × 1` or `1 × n`
 *  @return the `p × 1` coefficient vector, or `None` when the shapes do not conform or the
 *          design is rank-deficient (`qrDecompose` refuses it, so nonsense is not produced)
 */
def leastSquares(x: _MatrixValue, y: _MatrixValue): Option[_MatrixValue] =
  val n = x.rows
  // Accept the response either way up: a sample is a vector, and which way a user writes it
  // is not a meaningful distinction.
  val yCol =
    if y.rows == n && y.cols == 1 then Some(y)
    else if y.rows == 1 && y.cols == n then Some(y.transpose)
    else None

  for
    yv       <- yCol
    if x.rows >= x.cols
    // `qrDecompose` returns None for a rank-deficient design, which is exactly the case
    // where a normal-equations solve would have returned confident nonsense.
    (q, r)   <- x.qrDecompose
    qty      <- Option.when(q.cols == q.cols)(q.transpose.multiply(yv))
    // R is p x p and upper triangular; inverting it is well conditioned precisely because
    // the condition number was never squared.
    rInv     <- r.inverse
    beta     <- Option.when(rInv.cols == qty.rows)(rInv.multiply(qty))
  yield beta

/** The fitted values `X·β`.
 *  @param x    the design matrix
 *  @param beta the coefficients
 */
def fitted(x: _MatrixValue, beta: _MatrixValue): Option[_MatrixValue] =
  Option.when(x.cols == beta.rows)(x.multiply(beta))

/** The residual sum of squares of a fit, which the inference tier needs.
 *  @param x    the design matrix
 *  @param y    the response, `n × 1`
 *  @param beta the coefficients
 */
def residualSumOfSquares(x: _MatrixValue, y: _MatrixValue, beta: _MatrixValue): Option[Double] =
  fitted(x, beta).map { f =>
    f.toVector.zip(y.toVector).map((a, b) => (b - a) * (b - a)).sum
  }

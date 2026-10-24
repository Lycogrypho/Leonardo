package it.grypho.scala.leonardo
package statistics

import core.*


/** [[https://en.wikipedia.org/wiki/Ordinary_least_squares Ordinary least squares]].
 *
 *  **Solved by QR, not by the normal equations**, and that is the whole design decision.
 *  `β = (XᵀX)⁻¹Xᵀy` is the textbook form and every piece of it already exists
 *  (`transpose`, `multiply`, `inverse`), but forming `XᵀX` **squares the [[https://en.wikipedia.org/wiki/Condition_number condition number]]**:
 *  a design matrix that merely looks poorly scaled loses about half the available digits
 *  before the solve even starts.  It is the same class of trap as the one-pass variance next
 *  door and as the textbook quadratic formula — an algebraically equivalent rearrangement
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
  for
    yv       <- columnOf(y, x.rows)
    if x.rows >= x.cols
    // `qrDecompose` returns None for a rank-deficient design, which is exactly the case
    // where a normal-equations solve would have returned confident nonsense.
    (q, r)   <- x.qrDecompose
    // Q is m×n beside an m-row y by construction, so the product always conforms (the guard
    // here compared `q.cols` with itself, F_0213).
    qty       = q.transpose.multiply(yv)
    // R is p x p and upper triangular; inverting it is well conditioned precisely because
    // the condition number was never squared.
    rInv     <- r.inverse
    beta     <- Option.when(rInv.cols == qty.rows)(rInv.multiply(qty))
  yield beta

/** A vector as an `n × 1` column, from either orientation: a sample is a vector, and which way
 *  a user writes it is not a meaningful distinction. `None` when it does not have `n` entries. */
private def columnOf(v: _MatrixValue, n: Int): Option[_MatrixValue] =
  if v.rows == n && v.cols == 1 then Some(v)
  else if v.rows == 1 && v.cols == n then Some(v.transpose)
  else None

/** Fits `y ≈ X·β` by [[https://en.wikipedia.org/wiki/Ridge_regression ridge regression]], minimising
 *  `‖Xβ − y‖² + λ‖β‖²` (F_0062).
 *
 *  **Solved as the ordinary problem on the augmented design `[X; √λ·I]` against `[y; 0]`**, by
 *  the same QR — never as `(XᵀX + λI)⁻¹Xᵀy`, which squares the condition number exactly as the
 *  unregularised normal equations do, and for the same small `λ` that is meant to rescue a
 *  nearly collinear design. `λ = 0` *is* [[leastSquares]], called directly so the two agree bit
 *  for bit. For `λ > 0` the augmented design has full column rank whatever `X` is, so a
 *  rank-deficient or wide `X` that ordinary regression declines is solved — which is what ridge
 *  is for. No intercept is added, and the intercept column is penalised like any other.
 *
 *  @param x      the design matrix, `n × p`
 *  @param y      the response, `n` entries either way up
 *  @param lambda the penalty, `≥ 0`
 *  @return the `p × 1` coefficients, or `None` for a negative or non-finite `λ`, a response of
 *          the wrong length, or (at `λ = 0`) a rank-deficient design
 */
def ridgeLeastSquares(x: _MatrixValue, y: _MatrixValue, lambda: Double): Option[_MatrixValue] =
  if !(lambda >= 0) || lambda.isInfinite then None
  else if lambda == 0 then leastSquares(x, y)
  else
    val (n, p) = (x.rows, x.cols)
    val s = math.sqrt(lambda)
    columnOf(y, n).flatMap { yv =>
      val xa = _MatrixValue(n + p, p, Array.tabulate((n + p) * p) { idx =>
        val (i, j) = (idx / p, idx % p)
        if i < n then x(i, j) else if i - n == j then s else 0.0
      })
      val ya = _MatrixValue(n + p, 1, Array.tabulate(n + p)(i => if i < n then yv(i, 0) else 0.0))
      leastSquares(xa, ya)
    }

/** Fits `y ≈ X·β` by [[https://en.wikipedia.org/wiki/Weighted_least_squares weighted least squares]],
 *  minimising `Σ wᵢ(xᵢβ − yᵢ)²` (F_0062).
 *
 *  **Row scaling by `√wᵢ`, then the same QR** — never `(XᵀWX)⁻¹XᵀWy`, for the condition-number
 *  reason [[leastSquares]] states. Equal weights give the ordinary fit; a zero weight removes its
 *  row, so a design left rank-deficient by zeros is declined as an ordinary one would be.
 *
 *  @param x the design matrix, `n × p`
 *  @param y the response, `n` entries either way up
 *  @param w the weights, `n` entries either way up, each `≥ 0` and finite
 *  @return the `p × 1` coefficients, or `None` for a negative or non-finite weight, a length
 *          mismatch, or a rank-deficient weighted design
 */
def weightedLeastSquares(x: _MatrixValue, y: _MatrixValue, w: _MatrixValue): Option[_MatrixValue] =
  val (n, p) = (x.rows, x.cols)
  for
    yv <- columnOf(y, n)
    wv <- columnOf(w, n)
    if wv.toVector.forall(d => d >= 0 && !d.isInfinite)
    root = wv.toVector.map(math.sqrt)
    beta <- leastSquares(
              _MatrixValue(n, p, Array.tabulate(n * p)(idx => root(idx / p) * x(idx / p, idx % p))),
              _MatrixValue(n, 1, Array.tabulate(n)(i => root(i) * yv(i, 0))))
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

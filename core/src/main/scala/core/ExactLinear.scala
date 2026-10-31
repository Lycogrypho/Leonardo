package it.grypho.scala.leonardo
package core


/** Square rational matrix kernels that `scalar` needs as well as `matrix` (F_0227).
 *
 *  `A^n` is a `scalar.Power`, and `scalar` cannot import `matrix`, where the exact kernels were
 *  written; `core` is the layer both share, the arrangement `_MatrixValue.augmentedExp` set for
 *  `ode` and `control`.  `matrix.exactInverse` forwards here, so the inverse has one definition.
 *  Cells are row-major `n×n`.
 *
 *  **Since F_0231 each is the rational instance of a kernel over an [[ExactField]]**, so the same
 *  code eliminates over `Q(√d)` when a matrix holds surds of one radical (Decision B).
 */

/** The exact product of two square rational matrices.
 *  @param a      the left factor's cells
 *  @param b      the right factor's cells
 *  @param n      the dimension
 *  @param policy the reduction policy
 *  @return the cells of `a·b` */
private[leonardo] def rationalProduct(a: Vector[_Rational], b: Vector[_Rational], n: Int,
                                      policy: GcdPolicy): Vector[_Rational] =
  fieldProduct(RationalField(policy), a, b, n)

/** The exact inverse of a square rational matrix, by Gauss–Jordan elimination: the pivot only has
 *  to be non-zero, since there is no rounding for partial pivoting to contain.
 *  @param cells  the row-major exact cells
 *  @param n      the dimension
 *  @param policy the reduction policy
 *  @return the cells of the inverse, or `None` when the matrix is singular */
private[leonardo] def rationalInverse(cells: Vector[_Rational], n: Int,
                                      policy: GcdPolicy): Option[Vector[_Rational]] =
  fieldInverse(RationalField(policy), cells, n)

/** The `k`-th power of a square rational matrix, by binary exponentiation; a negative power is
 *  the inverse's.  **Held to `maxBits`** before any product is formed, the `_Rational.pow` rule:
 *  each product adds at most `b + ⌈log₂ n⌉` bits to an entry of `b` bits, so `|k|·(b + ⌈log₂ n⌉)`
 *  bounds the result's entries, and a power past it is declined rather than attempted.
 *  @param cells   the row-major exact cells
 *  @param n       the dimension
 *  @param k       the exponent
 *  @param policy  the reduction policy
 *  @param maxBits the most bits an entry may reach
 *  @param maxWork the reduction work the last product may cost (F_0249)
 *  @return the cells of the power, or `None` for a singular base to a negative power or past a
 *          budget */
private[leonardo] def rationalMatrixPower(cells: Vector[_Rational], n: Int, k: Long, policy: GcdPolicy,
                                          maxBits: Long, maxWork: Long): Option[Vector[_Rational]] =
  fieldMatrixPower(RationalField(policy), cells, n, k, maxBits, maxWork)

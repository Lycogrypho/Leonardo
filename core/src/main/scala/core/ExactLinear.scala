package it.grypho.scala.leonardo
package core

import scala.annotation.tailrec


/** Square rational matrix kernels that `scalar` needs as well as `matrix` (F_0227).
 *
 *  `A^n` is a `scalar.Power`, and `scalar` cannot import `matrix`, where the exact kernels were
 *  written; `core` is the layer both share, the arrangement `_MatrixValue.augmentedExp` set for
 *  `ode` and `control`.  `matrix.exactInverse` forwards here, so the inverse has one definition.
 *  Cells are row-major `n×n`.
 */

/** The exact product of two square rational matrices.
 *  @param a      the left factor's cells
 *  @param b      the right factor's cells
 *  @param n      the dimension
 *  @param policy the reduction policy
 *  @return the cells of `a·b` */
private[leonardo] def rationalProduct(a: Vector[_Rational], b: Vector[_Rational], n: Int,
                                      policy: GcdPolicy): Vector[_Rational] =
  Vector.tabulate(n, n)((i, j) =>
    (0 until n).foldLeft(_Rational.Zero)((s, k) => s.add(a(i * n + k).multiply(b(k * n + j), policy), policy))
  ).flatten

/** The exact inverse of a square rational matrix, by Gauss–Jordan elimination: the pivot only has
 *  to be non-zero, since there is no rounding for partial pivoting to contain.
 *  @param cells  the row-major exact cells
 *  @param n      the dimension
 *  @param policy the reduction policy
 *  @return the cells of the inverse, or `None` when the matrix is singular */
private[leonardo] def rationalInverse(cells: Vector[_Rational], n: Int,
                                      policy: GcdPolicy): Option[Vector[_Rational]] =
  // Augment with the identity, reduce the left half to it, and read off the right half.
  val augmented = Vector.tabulate(n, 2 * n) { (i, j) =>
    if j < n then cells(i * n + j)
    else if j - n == i then _Rational.One
    else _Rational.Zero
  }

  @tailrec
  def go(m: Vector[Vector[_Rational]], k: Int): Option[Vector[Vector[_Rational]]] =
    if k >= n then Some(m)
    else (k until n).find(i => !m(i)(k).isZero) match
      case None => None   // singular
      case Some(p) =>
        val swapped = if p == k then m else m.updated(k, m(p)).updated(p, m(k))
        swapped(k)(k).reciprocal(policy) match
          case None => None
          case Some(inv) =>
            // Normalise the pivot row, then clear the column in every other row.
            val norm  = swapped.updated(k, swapped(k).map(_.multiply(inv, policy)))
            val pivot = norm(k)
            val cleared = norm.zipWithIndex.map { (row, i) =>
              if i == k || row(k).isZero then row
              else
                val f = row(k)
                row.zipWithIndex.map((cell, j) => cell.subtract(f.multiply(pivot(j), policy), policy))
            }
            go(cleared, k + 1)

  go(augmented, 0).map(m => Vector.tabulate(n, n)((i, j) => m(i)(j + n)).flatten)

/** The `k`-th power of a square rational matrix, by binary exponentiation; a negative power is
 *  the inverse's.  **Held to `maxBits`** before any product is formed, the `_Rational.pow` rule:
 *  each product adds at most `b + ⌈log₂ n⌉` bits to an entry of `b` bits, so `|k|·(b + ⌈log₂ n⌉)`
 *  bounds the result's entries, and a power past it is declined rather than attempted.
 *  @param cells   the row-major exact cells
 *  @param n       the dimension
 *  @param k       the exponent
 *  @param policy  the reduction policy
 *  @param maxBits the most bits an entry may reach
 *  @return the cells of the power, or `None` for a singular base to a negative power or past
 *          `maxBits` */
private[leonardo] def rationalMatrixPower(cells: Vector[_Rational], n: Int, k: Long, policy: GcdPolicy,
                                          maxBits: Long): Option[Vector[_Rational]] =
  val base = if k >= 0 then Some(cells) else rationalInverse(cells, n, policy)
  base.filter { b =>
    val bits = b.map(_.maxBitLength).maxOption.getOrElse(0) + (32 - Integer.numberOfLeadingZeros(n))
    BigInt(math.abs(k)) * bits <= maxBits
  }.map { b =>
    val identity = Vector.tabulate(n * n)(i => if i / n == i % n then _Rational.One else _Rational.Zero)
    @tailrec
    def go(result: Vector[_Rational], factor: Vector[_Rational], e: Long): Vector[_Rational] =
      if e == 0 then result
      else
        val next = if (e & 1L) == 1L then rationalProduct(result, factor, n, policy) else result
        if e == 1 then next else go(next, rationalProduct(factor, factor, n, policy), e >> 1)
    go(identity, b, math.abs(k))
  }

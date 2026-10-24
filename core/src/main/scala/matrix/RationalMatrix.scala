package it.grypho.scala.leonardo
package matrix

import spire.math.Rational as Q


/** Dense matrix kernels over spire's `Rational`, defined once (F_0213).
 *
 *  The exact Jordan form (`ExactJordan`) and the exact vectorised matrix-equation tier
 *  (`equation/Solve`) each carried a private product, identity and elimination; two copies of
 *  one kernel are a way for the two to disagree.  `matrix/Exact.scala`'s kernels stay apart:
 *  they work on the `_Rational` carrier cells, not on spire values.
 */

/** A matrix of rationals, row-major. */
private[leonardo] type QMat = Vector[Vector[Q]]

/** The `n×n` identity. */
private[leonardo] def qIdentity(n: Int): QMat = Vector.tabulate(n, n)((i, j) => if i == j then Q.one else Q.zero)

/** `a·b` for conforming operands (the caller's guarantee). */
private[leonardo] def qMatMul(a: QMat, b: QMat): QMat =
  Vector.tabulate(a.size, b.head.size)((i, j) => a(i).indices.foldLeft(Q.zero)((s, k) => s + a(i)(k) * b(k)(j)))

/** `a·b`, or `None` when the operands do not conform. */
private[leonardo] def qMul(a: QMat, b: QMat): Option[QMat] = Option.when(a.head.size == b.size)(qMatMul(a, b))

/** The unique solution of `m·x = c` by Gauss–Jordan, or `None` when `m` is singular.  No
 *  tolerance decides a pivot: over the rationals a non-zero pivot is any one that is not zero.
 *  `m` and `c` are overwritten.
 *  @param m the square system
 *  @param c the right-hand side
 *  @return the solution */
private[leonardo] def qSolve(m: Array[Array[Q]], c: Array[Q]): Option[Vector[Q]] =
  val n = c.length
  var ok = true
  var col = 0
  while ok && col < n do
    (col until n).find(r => !m(r)(col).isZero) match
      case None => ok = false
      case Some(p) =>
        val (tr, tc) = (m(col), c(col)); m(col) = m(p); c(col) = c(p); m(p) = tr; c(p) = tc
        val inv = Q.one / m(col)(col)
        for k <- 0 until n do m(col)(k) = m(col)(k) * inv
        c(col) = c(col) * inv
        for r <- 0 until n if r != col && !m(r)(col).isZero do
          val f = m(r)(col)
          for k <- 0 until n do m(r)(k) = m(r)(k) - f * m(col)(k)
          c(r) = c(r) - f * c(col)
        col += 1
  Option.when(ok)(c.toVector)

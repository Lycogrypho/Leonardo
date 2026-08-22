package it.grypho.scala.leonardo
package matrix

import core.*
import scala.annotation.tailrec


/** Exact linear algebra over rational matrix entries (issue 4.L slice B).
 *
 *  **Why the carrier is the symbolic [[_Matrix]] and not a second dense type.**
 *  `core._MatrixValue` holds a private `Array[Double]` and has to keep it: the block-tiled
 *  multiply, the QR iteration and Gram–Schmidt are all `Double` algorithms, and making them
 *  arbitrary-precision would be 50–200× slower for no gain, since none of them is exact
 *  anyway.  So rather than maintain a parallel dense carrier forever, an exactly-written
 *  matrix simply *does not collapse*: `_Matrix.eval` keeps it symbolic when any cell is a
 *  `_Rational`, and these kernels operate on that.  The same instinct that put `_Rational`
 *  beside `_Number` instead of widening it.
 *
 *  The cost is that element-wise operations build AST nodes rather than running a dense
 *  kernel.  That is the right trade here — exact work is small-matrix work, and the
 *  alternative was duplicating most of `_MatrixValue`'s surface.
 *
 *  These are **Gaussian elimination**, not the cofactor expansion in `_MatrixOperation`:
 *  that one is `O(n!)` and capped at `MaxSymbolicDim = 6`, which would have made "exact
 *  matrices" mean "exact matrices up to 6×6, slowly".  Elimination over an exact field
 *  needs no pivoting-for-stability at all — there is no rounding to be unstable about — so
 *  the pivot search only has to find a non-zero.
 *
 *  @see [[https://en.wikipedia.org/wiki/Gaussian_elimination Gaussian elimination]]
 */

/** The cells of `m` as exact rationals, when **every** cell is one.
 *
 *  All-or-nothing on purpose: a matrix with one inexact entry is an inexact matrix, and
 *  running an exact kernel over it would dress float contagion up as an exact answer.
 *
 *  @param m the symbolic matrix
 *  @return the row-major exact cells, or `None` if any cell is not exact
 */
def exactCells(m: _Matrix): Option[Vector[_Rational]] =
  val cells = m.elems.collect { case r: _Rational => r }
  if cells.size == m.elems.size then Some(cells) else None

/** Reads a symbolic matrix as a dense `Double` one.
 *
 *  The demotion the iterative decompositions (`lu`, `qr`, `eigen`, `eig`, `jordan`) need:
 *  they cannot be exact, so an exact operand must degrade rather than stay symbolic and
 *  lose the feature.  Reads through the widening `_Number` extractor, so exact and inexact
 *  cells alike are accepted.
 *
 *  @param m the symbolic matrix
 *  @return the dense matrix, or `None` if any cell is not a real number
 */
def denseOf(m: _Matrix): Option[_MatrixValue] =
  val ds = m.elems.collect { case _Number(d) => d }
  if ds.size == m.elems.size then Some(_MatrixValue(m.rows, m.cols, ds.toArray)) else None

/** Row-major cells as rows, for the elimination kernels. */
private def rowsOf(cells: Vector[_Rational], rows: Int, cols: Int): Vector[Vector[_Rational]] =
  Vector.tabulate(rows, cols)((i, j) => cells(i * cols + j))

/** Index of the first row at or below `k` whose column-`k` entry is non-zero.
 *
 *  Exact arithmetic does not round, so unlike the `Double` kernels this looks for a
 *  *usable* pivot rather than the largest one — partial pivoting exists to limit growth of
 *  rounding error, and there is none here.
 */
private def pivotRow(m: Vector[Vector[_Rational]], k: Int): Option[Int] =
  (k until m.length).find(i => !m(i)(k).isZero)

/** The exact determinant of a square rational matrix, by Gaussian elimination.
 *
 *  Elimination is `O(n³)` against the cofactor expansion's `O(n!)`, which is what lets this
 *  be uncapped.  A row swap flips the sign; a column with no usable pivot means a singular
 *  matrix, whose determinant is exactly zero — not a failure.
 *
 *  @param cells  the row-major exact cells
 *  @param n      the dimension
 *  @param policy the reduction policy for the intermediate arithmetic
 *  @return the exact determinant
 */
def exactDeterminant(cells: Vector[_Rational], n: Int, policy: GcdPolicy): _Rational =
  @tailrec
  def go(m: Vector[Vector[_Rational]], k: Int, acc: _Rational): _Rational =
    if acc.isZero || k >= n then acc
    else pivotRow(m, k) match
      case None => _Rational.Zero   // a zero column: singular, determinant exactly zero
      case Some(p) =>
        val swapped = if p == k then m else m.updated(k, m(p)).updated(p, m(k))
        val sign    = if p == k then acc else acc.negate
        val pivot   = swapped(k)
        val reduced = swapped.zipWithIndex.map { (row, i) =>
          if i <= k then row
          else row(k).divide(pivot(k), policy) match
            case None    => row   // unreachable: the pivot is non-zero by construction
            case Some(f) => row.zipWithIndex.map { (cell, j) =>
              if j < k then cell else cell.subtract(f.multiply(pivot(j), policy), policy)
            }
        }
        go(reduced, k + 1, sign.multiply(pivot(k), policy))

  if n == 0 then _Rational.One else go(rowsOf(cells, n, n), 0, _Rational.One)

/** The exact inverse of a square rational matrix, by Gauss–Jordan elimination.
 *
 *  @param cells  the row-major exact cells
 *  @param n      the dimension
 *  @param policy the reduction policy for the intermediate arithmetic
 *  @return the row-major cells of the inverse, or `None` when the matrix is singular
 */
def exactInverse(cells: Vector[_Rational], n: Int, policy: GcdPolicy): Option[Vector[_Rational]] =
  // Augment with the identity, reduce the left half to it, and read off the right half.
  val augmented = Vector.tabulate(n, 2 * n) { (i, j) =>
    if j < n then cells(i * n + j)
    else if j - n == i then _Rational.One
    else _Rational.Zero
  }

  @tailrec
  def go(m: Vector[Vector[_Rational]], k: Int): Option[Vector[Vector[_Rational]]] =
    if k >= n then Some(m)
    else pivotRow(m, k) match
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

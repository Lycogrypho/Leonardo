package it.grypho.scala.leonardo
package matrix

import core.*
import scala.annotation.tailrec


/** Exact linear algebra over rational matrix entries.
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
 *  The demotion the iterative decompositions (`qr`, `eigen`, `eig`, a numeric `jordan`) need:
 *  they cannot be exact, so an exact operand must degrade rather than stay symbolic and
 *  lose the feature.  Reads through the widening `_Number` extractor, so exact and inexact
 *  cells alike are accepted.
 *
 *  @param m the symbolic matrix
 *  @return the dense matrix, or `None` if any cell is not a real number
 */
def denseOf(m: _Matrix): Option[_MatrixValue] = _MatrixValue.ofEvaluated(Left(m))

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
  // The kernel lives in `core` since F_0227, so `scalar`'s exact `A^(-n)` shares it.
  rationalInverse(cells, n, policy)

/** The exact LU factorisation `P·A = L·U` of a square rational matrix (F_0226).
 *
 *  Elimination is finite, so over the rationals it is exact.  **The pivot is the largest in
 *  magnitude**, not merely the first non-zero [[pivotRow]] takes: exactness does not need it,
 *  but it makes `P` the one the dense kernel chooses, so the same matrix written exactly and
 *  inexactly factors the same way.  **A column with no non-zero pivot is skipped, not refused**:
 *  a singular matrix has an LU factorisation too, with a zero on `U`'s diagonal, where the dense
 *  kernel declines because in floating point an exact zero pivot cannot be told from a tiny one.
 *
 *  @param cells  the row-major exact cells
 *  @param n      the dimension
 *  @param policy the reduction policy for the intermediate arithmetic
 *  @return the row-major cells of the unit lower-triangular `L`, the upper-triangular `U` and
 *          the permutation `P`
 */
def exactLu(cells: Vector[_Rational], n: Int,
            policy: GcdPolicy): (Vector[_Rational], Vector[_Rational], Vector[_Rational]) =
  /** One elimination step at column `k`: the multipliers stored in place, as the dense kernel does. */
  def step(state: (Vector[Vector[_Rational]], Vector[Int]), k: Int): (Vector[Vector[_Rational]], Vector[Int]) =
    val (m, perm) = state
    val p = (k until n).maxBy(i => m(i)(k).abs)   // the first of equal magnitudes, as the dense kernel
    if m(p)(k).isZero then state
    else
      val swapped = if p == k then m else m.updated(k, m(p)).updated(p, m(k))
      val order   = if p == k then perm else perm.updated(k, perm(p)).updated(p, perm(k))
      val pivot   = swapped(k)
      val reduced = swapped.zipWithIndex.map { (row, i) =>
        if i <= k then row
        else row(k).divide(pivot(k), policy) match
          case None    => row   // unreachable: the pivot is non-zero by construction
          case Some(f) => row.zipWithIndex.map { (cell, j) =>
            if j < k then cell
            else if j == k then f
            else cell.subtract(f.multiply(pivot(j), policy), policy)
          }
      }
      (reduced, order)

  val (a, perm) = (0 until n).foldLeft((rowsOf(cells, n, n), Vector.range(0, n)))(step)
  val l = Vector.tabulate(n, n)((i, j) => if i == j then _Rational.One else if j < i then a(i)(j) else _Rational.Zero)
  val u = Vector.tabulate(n, n)((i, j) => if j >= i then a(i)(j) else _Rational.Zero)
  val p = Vector.tabulate(n, n)((i, j) => if perm(i) == j then _Rational.One else _Rational.Zero)
  (l.flatten, u.flatten, p.flatten)

/** The Cholesky factor of a symmetric positive-definite rational matrix (F_0054).
 *
 *  Symmetry and the sign of every pivot are decided **exactly**, so no tolerance enters a
 *  refusal.  Each diagonal entry is the square root of an exact pivot: rational when the pivot
 *  is a rational square, otherwise `core.exactSqrt` at the working precision — the entries
 *  below it are then exact quotients by that value, so `L·Lᵀ` reproduces `A` to the working
 *  precision.
 *
 *  @param cells  the row-major exact cells
 *  @param n      the dimension
 *  @param policy the reduction policy for the intermediate arithmetic
 *  @param digits the working precision for an irrational pivot root
 *  @return the row-major cells of the lower-triangular `L`, or `None` when the matrix is not
 *          symmetric or not positive definite
 */
def exactCholesky(cells: Vector[_Rational], n: Int, policy: GcdPolicy,
                  digits: Int): Option[Vector[_Rational]] =
  val a = rowsOf(cells, n, n)
  def dot(x: Vector[_Rational], y: Vector[_Rational], upTo: Int): _Rational =
    (0 until upTo).foldLeft(_Rational.Zero)((s, k) => s.add(x(k).multiply(y(k), policy), policy))

  /** Fills column `j` of `l`, or `None` at a non-positive pivot. */
  def column(l: Vector[Vector[_Rational]], j: Int): Option[Vector[Vector[_Rational]]] =
    val pivot = a(j)(j).subtract(dot(l(j), l(j), j), policy)
    if pivot.signum <= 0 then None
    else exactSqrt(pivot, digits).flatMap { ljj =>
      (j + 1 until n).foldLeft(Option(l.updated(j, l(j).updated(j, ljj)))) { (acc, i) =>
        acc.flatMap { m =>
          a(i)(j).subtract(dot(m(i), m(j), j), policy).divide(ljj, policy).map(v => m.updated(i, m(i).updated(j, v)))
        }
      }
    }

  val symmetric = (0 until n).forall(i => (0 until i).forall(j => a(i)(j) == a(j)(i)))
  if !symmetric then None
  else (0 until n).foldLeft(Option(Vector.fill(n, n)(_Rational.Zero)))((acc, j) => acc.flatMap(column(_, j)))
    .map(_.flatten)

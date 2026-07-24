package it.grypho.scala.leonardo
package matrix

import core.*


/** Factory and companion for [[_Matrix]]. */
object _Matrix:
  /** Constructs a [[_Matrix]] from a sequence of row vectors.
   *
   *  All rows must be non-empty and of equal length.
   *
   *  @param rows one or more row vectors; each element is an arbitrary `_Expression`
   *  @return a new [[_Matrix]] with the rows stacked in order
   */
  def ofRows(rows: Vector[_Expression]*): _Matrix =
    require(rows.nonEmpty && rows.forall(_.size == rows.head.size),
      "rows must be non-empty and all of the same length")
    _Matrix(rows.size, rows.head.size, rows.toVector.flatten)

  /** Re-inflates a dense `_MatrixValue` into a [[_Matrix]] of `_Number` elements.
   *
   *  Used by the matrix operations to combine a concrete operand with a symbolic
   *  one element-wise: once all elements are `_Number` literals the resulting
   *  [[_Matrix]] collapses back to a `_MatrixValue` on the next `eval` pass.
   *
   *  @param v the dense concrete matrix to re-inflate
   *  @return a [[_Matrix]] with one `_Number` element per cell of `v`
   */
  def fromValue(v: _MatrixValue): _Matrix =
    _Matrix(v.rows, v.cols, v.toVector.map(_Number(_)))


/** A symbolic matrix whose elements are arbitrary expressions.
 *
 *  This is the AST-side matrix; the fully-reduced counterpart is `_MatrixValue`
 *  (a dense `Array[Double]` in `core`).  Elements may be numbers, variables,
 *  functions, functionals, or any other `_Expression`.
 *
 *  `eval` reduces every element: when all of them fold to `_Number` values the
 *  matrix collapses to a single `_MatrixValue`; otherwise it stays symbolic with
 *  each element as far reduced as it goes (the dual-evaluation contract,
 *  applied element-wise).
 *
 *  Marked `_ElementWise`: `derive`/`simplify`/`expand`/`integrate` distribute
 *  over the elements (e.g. `d/dx [a_ij] = [d(a_ij)/dx]`), which is valid because
 *  the matrix is a plain container with no coupling between cells.
 *
 *  `children` / `rebuild` expose the elements to generic traversals, so
 *  `substitute` and `dependsOn` work through matrix elements without
 *  matrix-specific cases.
 *
 *  @param rows  number of rows (must be positive)
 *  @param cols  number of columns (must be positive)
 *  @param elems row-major element vector; must have exactly `rows * cols` entries
 */
case class _Matrix(rows: Int, cols: Int, elems: Vector[_Expression]) extends _ElementWise, _MatrixShaped:
  require(rows > 0 && cols > 0, s"matrix dimensions must be positive: ${rows}x$cols")
  require(elems.size == rows * cols, s"expected ${rows * cols} elements, got ${elems.size}")

  /** Returns the element at zero-based row `i` and column `j`.
   *
   *  @param i zero-based row index
   *  @param j zero-based column index
   *  @return the expression stored at position `(i, j)`
   */
  def apply(i: Int, j: Int): _Expression = elems(i * cols + j)

  override def toString: String =
    (0 until rows).map(i => (0 until cols).map(this(i, _)).mkString("[", ", ", "]"))
      .mkString("[", ", ", "]")

  override def children: List[_Expression] = elems.toList
  override def rebuild(c: List[_Expression]): _Expression = _Matrix(rows, cols, c.toVector)

  override def eval(env: Environment): Either[_Expression, _Value] =
    val reduced = elems.map(_.eval(env))
    val numbers = reduced.collect { case Right(_Number(d)) => d }
    if numbers.size == elems.size then Right(_MatrixValue(rows, cols, numbers.toArray))
    else Left(_Matrix(rows, cols, reduced.map(_.toExpression)))

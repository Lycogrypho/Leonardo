package it.grypho.scala.leonardo
package matrix

import core.*


// Symbolic matrix node: a rows×cols grid of arbitrary expressions — numbers,
// variables, functions, functionals, anything an _Expression can be — stored
// row-major. This is the AST-side matrix; the fully-reduced counterpart is
// core._MatrixValue (dense Array[Double]).
//
// eval reduces every element: when all of them fold to numbers the matrix collapses
// to a _MatrixValue; otherwise it stays symbolic with each element as far reduced
// as it goes (the usual Left/Right dual-evaluation contract, element-wise).
//
// children/rebuild expose the elements to the generic traversals, so Substitute and
// Analysis (dependsOn) work through matrix elements without matrix-specific cases.

object _Matrix:
  // Row-of-rows convenience constructor: _Matrix.ofRows(Vector(a, b), Vector(c, d)).
  def ofRows(rows: Vector[_Expression]*): _Matrix =
    require(rows.nonEmpty && rows.forall(_.size == rows.head.size),
      "rows must be non-empty and all of the same length")
    _Matrix(rows.size, rows.head.size, rows.toVector.flatten)

  // Re-inflate a dense value into a literal of _Number elements — used by the matrix
  // operations to combine a concrete operand with a symbolic one element-wise.
  def fromValue(v: _MatrixValue): _Matrix =
    _Matrix(v.rows, v.cols, v.toVector.map(_Number(_)))


// _ElementWise: derive/simplify/expand/integrate distribute over the elements
// (d/dx [aᵢⱼ] = [daᵢⱼ/dx] — valid because the matrix is a plain container).
case class _Matrix(rows: Int, cols: Int, elems: Vector[_Expression]) extends _ElementWise:
  require(rows > 0 && cols > 0, s"matrix dimensions must be positive: ${rows}x$cols")
  require(elems.size == rows * cols, s"expected ${rows * cols} elements, got ${elems.size}")

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

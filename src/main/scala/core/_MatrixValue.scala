package it.grypho.scala.leonardo
package core

import java.util.stream.IntStream


// Dense, fully-reduced matrix value: a row-major Array[Double], not n² _Number nodes.
// This is the concrete counterpart of the symbolic matrix node (matrix._Matrix),
// exactly as _Number is the concrete counterpart of a scalar expression. It lives in
// core so Environment can bind matrix values without core depending on any domain.
//
// The O(n²)/O(n³) kernels (add, multiply, transpose, scale) operate directly on the
// dense array. multiply parallelizes over rows — each row of the output depends only
// on one row of the left operand, so the writes are disjoint and need no locking —
// but only above a work-volume threshold, where the fork/join overhead is amortized.

object _MatrixValue:
  // rows × inner × cols below this stays sequential: fork/join costs more than it saves.
  private val ParallelThreshold = 1L << 16


case class _MatrixValue(rows: Int, cols: Int, data: Array[Double]) extends _Value:
  require(rows > 0 && cols > 0, s"matrix dimensions must be positive: ${rows}x$cols")
  require(data.length == rows * cols, s"expected ${rows * cols} elements, got ${data.length}")

  def apply(i: Int, j: Int): Double = data(i * cols + j)

  override def eval(env: Environment): Either[_Expression, _Value] = Right(this)
  override def children: List[_Expression] = List.empty
  override def rebuild(c: List[_Expression]): _Expression = this

  // The synthesized case-class equals compares the Array field by reference;
  // matrices must compare by dimensions and contents.
  override def equals(other: Any): Boolean = other match
    case that: _MatrixValue =>
      rows == that.rows && cols == that.cols && java.util.Arrays.equals(data, that.data)
    case _ => false

  override def hashCode: Int =
    31 * (31 * rows + cols) + java.util.Arrays.hashCode(data)

  // toString rounds for display only (DefaultPrecision), mirroring _Number.
  override def toString: String =
    (0 until rows).map(i =>
      (0 until cols).map(j => _Number.round(this(i, j), Environment.DefaultPrecision))
        .mkString("[", ", ", "]")
    ).mkString("[", ", ", "]")

  def isFinite: Boolean = data.forall(d => !d.isNaN && !d.isInfinite)

  def add(that: _MatrixValue): _MatrixValue =
    require(rows == that.rows && cols == that.cols, s"dimension mismatch: ${rows}x$cols + ${that.rows}x${that.cols}")
    val out = new Array[Double](data.length)
    var i = 0
    while i < data.length do
      out(i) = data(i) + that.data(i)
      i += 1
    _MatrixValue(rows, cols, out)

  def scale(k: Double): _MatrixValue =
    _MatrixValue(rows, cols, data.map(_ * k))

  def transpose: _MatrixValue =
    val out = new Array[Double](data.length)
    var i = 0
    while i < rows do
      var j = 0
      while j < cols do
        out(j * rows + i) = data(i * cols + j)
        j += 1
      i += 1
    _MatrixValue(cols, rows, out)

  // (this: rows×n) * (that: n×that.cols). The i-k-j loop order streams both arrays
  // sequentially (cache-friendly) instead of striding down columns of `that`.
  def multiply(that: _MatrixValue): _MatrixValue =
    require(cols == that.rows, s"dimension mismatch: ${rows}x$cols * ${that.rows}x${that.cols}")
    val n   = cols
    val out = new Array[Double](rows * that.cols)

    def rowKernel(i: Int): Unit =
      var k = 0
      while k < n do
        val a = data(i * n + k)
        if a != 0.0 then
          var j = 0
          while j < that.cols do
            out(i * that.cols + j) += a * that.data(k * that.cols + j)
            j += 1
        k += 1

    if rows.toLong * n * that.cols >= _MatrixValue.ParallelThreshold then
      IntStream.range(0, rows).parallel().forEach(i => rowKernel(i))
    else
      var i = 0
      while i < rows do
        rowKernel(i)
        i += 1
    _MatrixValue(rows, that.cols, out)

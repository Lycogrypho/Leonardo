package it.grypho.scala.leonardo
package core

import java.util.stream.IntStream


// Dense, fully-reduced matrix value: a row-major Array[Double], not n² _Number nodes.
// This is the concrete counterpart of the symbolic matrix node (matrix._Matrix),
// exactly as _Number is the concrete counterpart of a scalar expression. It lives in
// core so Environment can bind matrix values without core depending on any domain.
//
// Immutability: the public factory takes a defensive copy of the caller's array and
// the storage is private, so no live reference into a value's data can exist outside
// it — a _MatrixValue is as immutable as every other _Value. Kernels build results
// with `new` on arrays they own, skipping the copy.
//
// The O(n²)/O(n³) kernels (add, multiply, transpose, scale) operate directly on the
// dense array. multiply parallelizes over rows — each row of the output depends only
// on one row of the left operand, so the writes are disjoint and need no locking —
// but only above a work-volume threshold, where the fork/join overhead is amortized.

object _MatrixValue:
  // rows × inner × cols below this stays sequential: fork/join costs more than it saves.
  private val ParallelThreshold = 1L << 16
  // Multiply block edge: 64×64 doubles = 32 KB, an L1-sized tile of each operand.
  private val Tile = 64

  def apply(rows: Int, cols: Int, data: Array[Double]): _MatrixValue =
    new _MatrixValue(rows, cols, data.clone)


final class _MatrixValue private (val rows: Int, val cols: Int, private val data: Array[Double]) extends _Value:
  require(rows > 0 && cols > 0, s"matrix dimensions must be positive: ${rows}x$cols")
  require(data.length == rows * cols, s"expected ${rows * cols} elements, got ${data.length}")

  def apply(i: Int, j: Int): Double = data(i * cols + j)

  // Read-only view of the dense storage (row-major).
  def toVector: Vector[Double] = data.toVector

  override def eval(env: Environment): Either[_Expression, _Value] = Right(this)
  override def children: List[_Expression] = List.empty
  override def rebuild(c: List[_Expression]): _Expression = this

  override def equals(other: Any): Boolean = other match
    case that: _MatrixValue =>
      rows == that.rows && cols == that.cols && java.util.Arrays.equals(data, that.data)
    case _ => false

  // Cached: the storage is immutable, and recomputing would rescan the whole array.
  override lazy val hashCode: Int =
    31 * (31 * rows + cols) + java.util.Arrays.hashCode(data)

  // display(p) rounds to p decimal places — mirrors _Number.display(p) for REPL precision.
  def display(precision: Int): String =
    (0 until rows).map(i =>
      (0 until cols).map(j => _Number.round(this(i, j), precision))
        .mkString("[", ", ", "]")
    ).mkString("[", ", ", "]")

  // toString rounds for display only (DefaultPrecision), mirroring _Number.
  override def toString: String = display(Environment.DefaultPrecision)

  def isFinite: Boolean = data.forall(d => !d.isNaN && !d.isInfinite)

  // Lift this matrix into an Either for eval: Right when all elements are finite,
  // Left(orElse) otherwise (non-finite = domain error; stays symbolic like x/0).
  // Shared by scalar._Operation and matrix._MatrixOperation; living here avoids
  // duplicating the guard in two packages that cannot import each other.
  def guarded(orElse: _Expression): Either[_Expression, _Value] =
    if isFinite then Right(this) else Left(orElse)

  def add(that: _MatrixValue): _MatrixValue =
    require(rows == that.rows && cols == that.cols, s"dimension mismatch: ${rows}x$cols + ${that.rows}x${that.cols}")
    val out = new Array[Double](data.length)
    var i = 0
    while i < data.length do
      out(i) = data(i) + that.data(i)
      i += 1
    new _MatrixValue(rows, cols, out)

  def scale(k: Double): _MatrixValue =
    new _MatrixValue(rows, cols, data.map(_ * k))

  def transpose: _MatrixValue =
    val out = new Array[Double](data.length)
    var i = 0
    while i < rows do
      var j = 0
      while j < cols do
        out(j * rows + i) = data(i * cols + j)
        j += 1
      i += 1
    new _MatrixValue(cols, rows, out)

  // Determinant via LU decomposition with partial pivoting, O(n³) — the numeric
  // counterpart of the symbolic cofactor expansion in matrix.Determinant. None when
  // the matrix is non-square (undefined); a zero pivot means a singular matrix and
  // yields Some(0.0). Works on a defensive clone, so the value's storage is untouched.
  def determinant: Option[Double] =
    if rows != cols then None
    else
      val n   = rows
      val a   = data.clone
      var det = 1.0
      var col = 0
      while col < n do
        var pivot  = col
        var maxAbs = math.abs(a(col * n + col))
        var r      = col + 1
        while r < n do
          val v = math.abs(a(r * n + col))
          if v > maxAbs then { maxAbs = v; pivot = r }
          r += 1
        if a(pivot * n + col) == 0.0 then return Some(0.0)   // singular
        if pivot != col then
          var k = 0
          while k < n do
            val tmp = a(col * n + k); a(col * n + k) = a(pivot * n + k); a(pivot * n + k) = tmp
            k += 1
          det = -det
        val diag = a(col * n + col)
        det *= diag
        var rr = col + 1
        while rr < n do
          val f = a(rr * n + col) / diag
          var k = col
          while k < n do
            a(rr * n + k) -= f * a(col * n + k)
            k += 1
          rr += 1
        col += 1
      Some(det)

  // Inverse via Gauss–Jordan elimination with partial pivoting, O(n³). None when the
  // matrix is non-square or singular (a zero pivot) — the caller stays symbolic, the
  // same contract as x/0 in scalar.Ratio. The augmented identity is reduced in lockstep
  // with a clone of the data, so no live reference into the value's storage escapes.
  def inverse: Option[_MatrixValue] =
    if rows != cols then None
    else
      val n   = rows
      val a   = data.clone
      val inv = new Array[Double](n * n)
      var d   = 0
      while d < n do { inv(d * n + d) = 1.0; d += 1 }   // identity
      var col = 0
      while col < n do
        var pivot  = col
        var maxAbs = math.abs(a(col * n + col))
        var r      = col + 1
        while r < n do
          val v = math.abs(a(r * n + col))
          if v > maxAbs then { maxAbs = v; pivot = r }
          r += 1
        if a(pivot * n + col) == 0.0 then return None    // singular
        if pivot != col then
          var k = 0
          while k < n do
            val ta = a(col * n + k); a(col * n + k) = a(pivot * n + k); a(pivot * n + k) = ta
            val ti = inv(col * n + k); inv(col * n + k) = inv(pivot * n + k); inv(pivot * n + k) = ti
            k += 1
        val diag = a(col * n + col)
        var k = 0
        while k < n do
          a(col * n + k) /= diag
          inv(col * n + k) /= diag
          k += 1
        var rr = 0
        while rr < n do
          if rr != col then
            val f = a(rr * n + col)
            var kk = 0
            while kk < n do
              a(rr * n + kk)   -= f * a(col * n + kk)
              inv(rr * n + kk) -= f * inv(col * n + kk)
              kk += 1
          rr += 1
        col += 1
      Some(new _MatrixValue(n, n, inv))

  // (this: rows×n) * (that: n×that.cols). Block-tiled i-k-j: Tile×Tile blocks keep
  // the hot block of `that` (and of `out`) cache-resident across a whole row block,
  // instead of re-streaming all of `that` from memory for every output row. Within
  // each output cell the k-accumulation order is still ascending, so results are
  // bit-identical to the untiled kernel. Row blocks write disjoint output slices,
  // so they parallelize with no locking, above the work-volume threshold.
  def multiply(that: _MatrixValue): _MatrixValue =
    require(cols == that.rows, s"dimension mismatch: ${rows}x$cols * ${that.rows}x${that.cols}")
    val n    = cols
    val w    = that.cols
    val tile = _MatrixValue.Tile
    val out  = new Array[Double](rows * w)

    // One row block: output rows [ii, min(ii+Tile, rows)).
    def blockKernel(ii: Int): Unit =
      val iEnd = math.min(ii + tile, rows)
      var kk = 0
      while kk < n do
        val kEnd = math.min(kk + tile, n)
        var jj = 0
        while jj < w do
          val jEnd = math.min(jj + tile, w)
          var i = ii
          while i < iEnd do
            var k = kk
            while k < kEnd do
              val a = data(i * n + k)
              if a != 0.0 then
                val offO = i * w
                val offB = k * w
                var j = jj
                while j < jEnd do
                  out(offO + j) += a * that.data(offB + j)
                  j += 1
              k += 1
            i += 1
          jj += tile
        kk += tile

    val blocks = (rows + tile - 1) / tile
    if rows.toLong * n * w >= _MatrixValue.ParallelThreshold && blocks > 1 then
      IntStream.range(0, blocks).parallel().forEach(b => blockKernel(b * tile))
    else
      var b = 0
      while b < blocks do
        blockKernel(b * tile)
        b += 1
    new _MatrixValue(rows, that.cols, out)

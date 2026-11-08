package it.grypho.scala.leonardo
package control

import core.*


/** The small dense-matrix kernels `control`'s numeric files share (F_0314): one definition of each,
 *  called qualified (`Dense.blocks(…)`).  They were private top-level definitions in several files,
 *  and a top-level `private def` is visible across the package's files, so a private name could shadow
 *  a public one — `zeros` shadowed `TransferFunction`'s, a compile error in another file — while the
 *  same kernel existed twice. */
private[control] object Dense:

  /** The `rows×cols` zero matrix. */
  def zeros(rows: Int, cols: Int): _MatrixValue = _MatrixValue(rows, cols, new Array[Double](rows * cols))

  /** `mv·v`. */
  def times(mv: _MatrixValue, v: Vector[Double]): Vector[Double] =
    Vector.tabulate(mv.rows)(i => (0 until mv.cols).map(j => mv(i, j) * v(j)).sum)

  /** A block matrix from rows of blocks with conforming shapes; a zero-width block is not allowed. */
  def blocks(rows: Vector[Vector[_MatrixValue]]): _MatrixValue =
    val width  = rows.head.map(_.cols).sum
    val height = rows.map(_.head.rows).sum
    val out    = new Array[Double](height * width)
    var r0 = 0
    for blockRow <- rows do
      var c0 = 0
      for blk <- blockRow do
        for i <- 0 until blk.rows; j <- 0 until blk.cols do out((r0 + i) * width + c0 + j) = blk(i, j)
        c0 += blk.cols
      r0 += blockRow.head.rows
    _MatrixValue(height, width, out)

  /** The block diagonal of square blocks. */
  def blockDiagonal(diagonal: Vector[_MatrixValue]): _MatrixValue =
    val size = diagonal.map(_.rows).sum
    val out  = new Array[Double](size * size)
    var off  = 0
    for blk <- diagonal do
      for i <- 0 until blk.rows; j <- 0 until blk.cols do out((off + i) * size + off + j) = blk(i, j)
      off += blk.rows
    _MatrixValue(size, size, out)

  /** `[B, AB, …, Aⁿ⁻¹B]`, `n×(n·m)`: the controllability matrix, or the observability matrix's transpose
   *  for `(Aᵀ, Cᵀ)`. */
  def krylov(a: _MatrixValue, b: _MatrixValue): _MatrixValue =
    val cols = (0 until a.rows).scanLeft(b)((x, _) => a.multiply(x)).take(a.rows)
    _MatrixValue(a.rows, a.rows * b.cols,
      Array.tabulate(a.rows * a.rows * b.cols)(idx =>
        val (r, col) = (idx / (a.rows * b.cols), idx % (a.rows * b.cols))
        cols(col / b.cols)(r, col % b.cols)))

  /** The `c×c` upper triangular `R` of `X = Q·R` by Householder reflections, for `X` with at least
   *  as many rows as columns; it never declines (a zero column leaves a zero row). */
  def householderR(x: _MatrixValue): _MatrixValue =
    val (m, n) = (x.rows, x.cols)
    val a = Array.tabulate(m * n)(idx => x(idx / n, idx % n))
    val v = new Array[Double](m)
    var k = 0
    while k < n do
      var big = 0.0
      var i = k
      while i < m do { big = math.max(big, math.abs(a(i * n + k))); i += 1 }
      if big > 0.0 then
        var ss = 0.0
        i = k
        // v is the column scaled by its largest entry, so no square overflows or underflows
        while i < m do { val t = a(i * n + k) / big; v(i) = t; ss += t * t; i += 1 }
        val norm  = math.sqrt(ss)
        val alpha = if v(k) > 0.0 then -norm else norm
        v(k) -= alpha
        var vv = 0.0
        i = k
        while i < m do { vv += v(i) * v(i); i += 1 }
        if vv > 0.0 then
          var j = k
          while j < n do
            var s = 0.0
            i = k
            while i < m do { s += v(i) * a(i * n + j); i += 1 }
            val f = 2.0 * s / vv
            i = k
            while i < m do { a(i * n + j) -= f * v(i); i += 1 }
            j += 1
      k += 1
    _MatrixValue(n, n, Array.tabulate(n * n)(idx => if idx % n >= idx / n then a(idx) else 0.0))

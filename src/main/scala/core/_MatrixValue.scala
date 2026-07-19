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

  // n×n identity — the dense counterpart of the matrix.IdentityMatrix node. Used by the
  // vectorized (Sylvester) solver tier for absent left/right coefficients (issue 4.5).
  def identity(n: Int): _MatrixValue =
    val out = new Array[Double](n * n)
    var i = 0
    while i < n do
      out(i * n + i) = 1.0
      i += 1
    new _MatrixValue(n, n, out)

  // Inverse of `vec`: reshape a (rows·cols)×1 column vector back into a rows×cols
  // matrix, column-major (matching vec's stacking). None when v does not conform.
  def unvec(v: _MatrixValue, rows: Int, cols: Int): Option[_MatrixValue] =
    if v.cols != 1 || v.rows != rows * cols then None
    else
      val out = new Array[Double](rows * cols)
      var j = 0
      while j < cols do
        var i = 0
        while i < rows do
          out(i * cols + j) = v(j * rows + i, 0)
          i += 1
        j += 1
      Some(new _MatrixValue(rows, cols, out))


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

  // Kronecker product: (this ⊗ that) — the (rows·that.rows)×(cols·that.cols) block
  // matrix whose (i, j) block is this(i, j) · that. The kernel behind the vectorized
  // matrix-equation tier (issue 4.5): vec(A·X·B) = (Bᵀ ⊗ A) · vec(X).
  def kronecker(that: _MatrixValue): _MatrixValue =
    val kc  = cols * that.cols
    val out = new Array[Double](rows * that.rows * kc)
    var i = 0
    while i < rows do
      var j = 0
      while j < cols do
        val f = data(i * cols + j)
        if f != 0.0 then
          var k = 0
          while k < that.rows do
            var l = 0
            while l < that.cols do
              out((i * that.rows + k) * kc + (j * that.cols + l)) = f * that(k, l)
              l += 1
            k += 1
        j += 1
      i += 1
    new _MatrixValue(rows * that.rows, kc, out)

  // Column-stacking vectorization: the (rows·cols)×1 column vector with
  // vec[j·rows + i] = this(i, j) — the vec(·) of the Kronecker identity above.
  def vec: _MatrixValue =
    val out = new Array[Double](rows * cols)
    var j = 0
    while j < cols do
      var i = 0
      while i < rows do
        out(j * rows + i) = data(i * cols + j)
        i += 1
      j += 1
    new _MatrixValue(rows * cols, 1, out)

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

  // LU decomposition with partial pivoting: P·A = L·U.
  // L is unit lower triangular, U is upper triangular, P is the permutation matrix.
  // None when non-square or singular (zero pivot encountered).
  // The combined a array holds multipliers below the diagonal (L) and the elimination
  // result above and on the diagonal (U), matching the standard compact LU storage.
  def luDecompose: Option[(_MatrixValue, _MatrixValue, _MatrixValue)] =
    if rows != cols then None
    else
      val n    = rows
      val a    = data.clone
      val perm = Array.tabulate(n)(identity)   // perm(i) = original row at position i
      var col  = 0
      while col < n do
        var pivot  = col
        var maxAbs = math.abs(a(col * n + col))
        var r      = col + 1
        while r < n do
          val v = math.abs(a(r * n + col))
          if v > maxAbs then { maxAbs = v; pivot = r }
          r += 1
        if a(pivot * n + col) == 0.0 then return None
        if pivot != col then
          val tmp = perm(col); perm(col) = perm(pivot); perm(pivot) = tmp
          var k = 0
          while k < n do
            val tmp = a(col * n + k); a(col * n + k) = a(pivot * n + k); a(pivot * n + k) = tmp
            k += 1
        val diag = a(col * n + col)
        var rr = col + 1
        while rr < n do
          val f = a(rr * n + col) / diag
          a(rr * n + col) = f          // store L multiplier in place of the zeroed entry
          var k = col + 1
          while k < n do
            a(rr * n + k) -= f * a(col * n + k)
            k += 1
          rr += 1
        col += 1
      val lData = Array.fill(n * n)(0.0)
      val uData = Array.fill(n * n)(0.0)
      val pData = Array.fill(n * n)(0.0)
      for i <- 0 until n do
        lData(i * n + i) = 1.0          // unit diagonal of L
        pData(i * n + perm(i)) = 1.0   // P[i, perm(i)] = 1  →  (P·A)[i] = A[perm(i)]
        for j <- 0 until n do
          if j < i then lData(i * n + j) = a(i * n + j)
          else uData(i * n + j) = a(i * n + j)
      Some((_MatrixValue(n, n, lData), _MatrixValue(n, n, uData), _MatrixValue(n, n, pData)))

  // Eigenvalue decomposition via QR iteration with Wilkinson shifts.
  // Returns the n eigenvalues as a Vector[_Value] — each is a _Number (real) or
  // _Complex (conjugate pair from a 2×2 block). None for non-square matrices or when
  // the iteration does not converge within 300·n steps.
  //
  // Algorithm: maintain a shrinking active sub-matrix (sz×sz). Each step either
  // deflates the bottom eigenvalue (sub-diagonal < tolerance) or applies a QR step
  // with Wilkinson shift (eigenvalue of the bottom-right 2×2 closest to a[sz-1,sz-1]).
  // 2×2 blocks are solved analytically, catching complex conjugate pairs without
  // further iteration. When the Wilkinson shift causes rank deficiency (shift is an
  // exact eigenvalue), a tiny perturbation is added to let Gram-Schmidt proceed.
  def eigenDecompose: Option[Vector[_Value]] =
    if rows != cols then None
    else
      val n = rows
      if n == 1 then return Some(Vector(_Number(data(0))))
      val tol     = 1e-12
      val maxIter = 300 * n
      val eigs    = scala.collection.mutable.ArrayBuffer[_Value]()

      // Active sub-matrix: sz×sz, row-major in aArr(0 .. sz*sz-1).
      // Replaced entirely on every deflation.
      var sz   = n
      var aArr = data.clone

      @inline def aGet(r: Int, c: Int): Double = aArr(r * sz + c)

      def shiftedMatrix(shift: Double): _MatrixValue =
        val out = new Array[Double](sz * sz)
        var k = 0
        while k < sz * sz do
          out(k) = aArr(k) - (if k / sz == k % sz then shift else 0.0)
          k += 1
        new _MatrixValue(sz, sz, out)

      var iters = 0

      while sz > 0 && iters < maxIter do
        iters += 1
        sz match
          case 1 =>
            eigs += _Number(aGet(0, 0))
            sz = 0
          case 2 =>
            val tr   = aGet(0, 0) + aGet(1, 1)
            val det  = aGet(0, 0) * aGet(1, 1) - aGet(0, 1) * aGet(1, 0)
            val disc = tr * tr - 4 * det
            if disc >= 0 then
              eigs += _Number((tr + math.sqrt(disc)) / 2)
              eigs += _Number((tr - math.sqrt(disc)) / 2)
            else
              val re = tr / 2
              val im = math.sqrt(-disc) / 2
              eigs += _Complex.of(re, im)
              eigs += _Complex.of(re, -im)
            sz = 0
          case _ =>
            val offDiag = math.abs(aGet(sz - 1, sz - 2))
            val diagSum = math.abs(aGet(sz - 2, sz - 2)) + math.abs(aGet(sz - 1, sz - 1))
            if offDiag <= tol * diagSum then
              eigs += _Number(aGet(sz - 1, sz - 1))
              val szNew  = sz - 1
              val newArr = new Array[Double](szNew * szNew)
              var r = 0
              while r < szNew do
                var c = 0
                while c < szNew do
                  newArr(r * szNew + c) = aArr(r * sz + c)
                  c += 1
                r += 1
              sz   = szNew
              aArr = newArr
            else
              // Wilkinson shift: eigenvalue of bottom-right 2×2 closest to a[sz-1,sz-1].
              val b00   = aGet(sz - 2, sz - 2)
              val b01   = aGet(sz - 2, sz - 1)
              val b10   = aGet(sz - 1, sz - 2)
              val b11   = aGet(sz - 1, sz - 1)
              val tr2   = b00 + b11
              val det2  = b00 * b11 - b01 * b10
              val disc2 = tr2 * tr2 - 4 * det2
              val shift =
                if disc2 >= 0 then
                  val e1 = (tr2 + math.sqrt(disc2)) / 2
                  val e2 = (tr2 - math.sqrt(disc2)) / 2
                  if math.abs(e1 - b11) <= math.abs(e2 - b11) then e1 else e2
                else
                  tr2 / 2   // complex pair: shift to centre avoids rank deficiency

              val qrResult = shiftedMatrix(shift).qrDecompose.orElse {
                // Shift hit an eigenvalue exactly; tiny perturbation breaks rank deficiency.
                val eps = 1e-10 * (1.0 + math.abs(shift))
                shiftedMatrix(shift + eps).qrDecompose
              }
              qrResult match
                case None => return None
                case Some((q, r)) =>
                  val rq = r.multiply(q)
                  var row = 0
                  while row < sz do
                    var col = 0
                    while col < sz do
                      aArr(row * sz + col) = rq(row, col) + (if row == col then shift else 0.0)
                      col += 1
                    row += 1

      if sz == 0 && eigs.size == n then Some(eigs.toVector) else None

  // Null vector of a real n×n matrix b via RREF + back-substitution.
  // Assumes b has rank n-1; returns the normalised null vector, or None when the
  // matrix has full rank or the null vector turns out to be zero.
  private def realNullVec(b: Array[Double], n: Int): Option[Array[Double]] =
    val a   = b.clone
    val piv = Array.fill(n)(-1)
    var row = 0; var col = 0
    // Relative pivot threshold: ties the zero-detection to the matrix's own scale.
    // The eigenvalue returned by QR iteration can have an absolute error several
    // orders of magnitude larger than the deflation criterion (1e-12), so the
    // near-zero diagonal entries of (A - λI) can be ≈ 1e-10 even for simple
    // eigenvalues.  A relative tolerance of 1e-8 (of the largest entry in b)
    // absorbs this without misclassifying genuine small pivots.
    val scale = b.foldLeft(0.0)((m, v) => math.max(m, math.abs(v)))
    val tol   = math.max(1e-12, scale * 1e-8)
    while row < n && col < n do
      var maxAbs = 0.0; var maxRow = row; var r = row
      while r < n do
        val v = math.abs(a(r * n + col))
        if v > maxAbs then { maxAbs = v; maxRow = r }
        r += 1
      if maxAbs < tol then col += 1
      else
        if maxRow != row then
          var k = 0
          while k < n do
            val t = a(row*n+k); a(row*n+k) = a(maxRow*n+k); a(maxRow*n+k) = t; k += 1
        piv(row) = col
        val d = a(row * n + col)
        var k = 0; while k < n do { a(row*n+k) /= d; k += 1 }
        var rr = 0
        while rr < n do
          if rr != row then
            val f = a(rr * n + col)
            if math.abs(f) > 1e-15 then
              var k = 0; while k < n do { a(rr*n+k) -= f * a(row*n+k); k += 1 }
          rr += 1
        row += 1; col += 1
    val pivSet = piv.filter(_ >= 0).toSet
    (0 until n).find(!pivSet.contains(_)) match
      case None => None
      case Some(freeCol) =>
        val vec = Array.fill(n)(0.0)
        vec(freeCol) = 1.0
        var r2 = 0
        while r2 < row do
          if piv(r2) >= 0 then vec(piv(r2)) = -a(r2 * n + freeCol)
          r2 += 1
        var norm = 0.0; var k = 0
        while k < n do { norm += vec(k) * vec(k); k += 1 }
        norm = math.sqrt(norm)
        if norm < 1e-14 then None
        else
          k = 0; while k < n do { vec(k) /= norm; k += 1 }
          Some(vec)

  // Null vector of the complex n×n matrix (A - λI) where λ = (lRe + i·lIm),
  // via complex RREF + back-substitution.
  // Returns (re-part, im-part) of the normalised null vector, or None.
  private def complexNullVec(n: Int, lRe: Double, lIm: Double): Option[(Array[Double], Array[Double])] =
    val re = Array.tabulate(n * n)(k => data(k) - (if k/n == k%n then lRe else 0.0))
    val im = Array.tabulate(n * n)(k =>                             if k/n == k%n then -lIm else 0.0)
    val piv = Array.fill(n)(-1)
    var row = 0; var col = 0
    val scale = re.zip(im).foldLeft(0.0) { case (m, (r, i)) => math.max(m, math.hypot(r, i)) }
    val tol   = math.max(1e-12, scale * 1e-8)
    while row < n && col < n do
      var maxMag = 0.0; var maxRow = row; var r = row
      while r < n do
        val m = math.hypot(re(r*n+col), im(r*n+col))
        if m > maxMag then { maxMag = m; maxRow = r }
        r += 1
      if maxMag < tol then col += 1
      else
        if maxRow != row then
          var k = 0
          while k < n do
            val tr = re(row*n+k); re(row*n+k) = re(maxRow*n+k); re(maxRow*n+k) = tr
            val ti = im(row*n+k); im(row*n+k) = im(maxRow*n+k); im(maxRow*n+k) = ti
            k += 1
        piv(row) = col
        val pRe = re(row*n+col); val pIm = im(row*n+col)
        val den = pRe*pRe + pIm*pIm
        var k = 0
        while k < n do
          val oRe = re(row*n+k); val oIm = im(row*n+k)
          re(row*n+k) = (oRe*pRe + oIm*pIm) / den
          im(row*n+k) = (oIm*pRe - oRe*pIm) / den
          k += 1
        var rr = 0
        while rr < n do
          if rr != row then
            val fRe = re(rr*n+col); val fIm = im(rr*n+col)
            if math.hypot(fRe, fIm) > 1e-15 then
              var k = 0
              while k < n do
                re(rr*n+k) -= fRe*re(row*n+k) - fIm*im(row*n+k)
                im(rr*n+k) -= fRe*im(row*n+k) + fIm*re(row*n+k)
                k += 1
          rr += 1
        row += 1; col += 1
    val pivSet = piv.filter(_ >= 0).toSet
    (0 until n).find(!pivSet.contains(_)) match
      case None => None
      case Some(freeCol) =>
        val vRe = Array.fill(n)(0.0); val vIm = Array.fill(n)(0.0)
        vRe(freeCol) = 1.0
        var r2 = 0
        while r2 < row do
          if piv(r2) >= 0 then
            vRe(piv(r2)) = -re(r2*n+freeCol)
            vIm(piv(r2)) = -im(r2*n+freeCol)
          r2 += 1
        var norm = 0.0; var k = 0
        while k < n do { norm += vRe(k)*vRe(k) + vIm(k)*vIm(k); k += 1 }
        norm = math.sqrt(norm)
        if norm < 1e-14 then None
        else
          k = 0; while k < n do { vRe(k) /= norm; vIm(k) /= norm; k += 1 }
          Some((vRe, vIm))

  // Spectral decomposition: eigenvalues + eigenvectors.
  // Returns (columns of V as Vector[Vector[_Value]], eigenvalues as Vector[_Value]).
  // Eigenvector column j corresponds to eigenvalue j in the returned vector.
  // Complex conjugate pairs (α ± βi) always occupy consecutive positions.
  // None when non-square, QR iteration fails to converge, or an eigenvector
  // cannot be extracted (numerically degenerate/defective matrix).
  def spectralDecompose: Option[(Vector[Vector[_Value]], Vector[_Value])] =
    if rows != cols then None
    else eigenDecompose.flatMap { eigs =>
      val n = rows
      var i = 0
      val evOpts = scala.collection.mutable.ArrayBuffer[Option[Vector[_Value]]]()
      while i < n do
        eigs(i) match
          case _Number(lam) =>
            val b = Array.tabulate(n * n)(k => data(k) - (if k/n == k%n then lam else 0.0))
            evOpts += realNullVec(b, n).map(_.map(_Number(_)).toVector)
            i += 1
          case c: _Complex =>
            complexNullVec(n, c.re, c.im) match
              case None =>
                evOpts += None; evOpts += None
              case Some((vRe, vIm)) =>
                evOpts += Some(vRe.zip(vIm).map { case (r, m) => _Complex.of(r, m) }.toVector)
                evOpts += Some(vRe.zip(vIm).map { case (r, m) => _Complex.of(r, -m) }.toVector)
            i += 2
          case _ =>
            evOpts += None; i += 1
      val opts = evOpts.toVector
      if opts.exists(_.isEmpty) then None
      else Some((opts.map(_.get), eigs))
    }

  // QR decomposition via modified Gram-Schmidt: A = Q·R (for square matrices; m ≥ n).
  // Q is m×n with orthonormal columns, R is n×n upper triangular.
  // None when rows < cols or the matrix is rank-deficient (column collapses to zero norm).
  def qrDecompose: Option[(_MatrixValue, _MatrixValue)] =
    if rows < cols then None
    else
      val m = rows
      val n = cols
      val q = data.clone    // q[i*n+j] = row i, col j — will become Q
      val r = Array.fill(n * n)(0.0)
      var j = 0
      while j < n do
        // Modified Gram-Schmidt: orthogonalize column j against prior orthonormal columns.
        var i = 0
        while i < j do
          var dot = 0.0
          var k   = 0
          while k < m do { dot += q(k * n + i) * q(k * n + j); k += 1 }
          r(i * n + j) = dot
          k = 0
          while k < m do { q(k * n + j) -= dot * q(k * n + i); k += 1 }
          i += 1
        var norm = 0.0
        var k    = 0
        while k < m do { norm += q(k * n + j) * q(k * n + j); k += 1 }
        norm = math.sqrt(norm)
        if norm < 1e-14 then return None   // rank-deficient
        r(j * n + j) = norm
        k = 0
        while k < m do { q(k * n + j) /= norm; k += 1 }
        j += 1
      Some((_MatrixValue(m, n, q), _MatrixValue(n, n, r)))

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

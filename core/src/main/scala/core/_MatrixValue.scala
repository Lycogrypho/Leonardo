package it.grypho.scala.leonardo
package core



/** Companion for the dense concrete matrix value [[_MatrixValue]]. */
object _MatrixValue:
  // rows × inner × cols below this stays sequential: fork/join costs more than it saves.
  private val ParallelThreshold = 1L << 16
  // Multiply block edge: 64×64 doubles = 32 KB, an L1-sized tile of each operand.
  private val Tile = 64

  /** Creates a `_MatrixValue` from a caller-supplied row-major array (defensively copied).
   *  @param rows number of rows; must be positive
   *  @param cols number of columns; must be positive
   *  @param data row-major element array; must have length `rows * cols`
   */
  def apply(rows: Int, cols: Int, data: Array[Double]): _MatrixValue =
    new _MatrixValue(rows, cols, data.clone)

  /** Radius within which the degree-13 Padé approximant to `e^A` is accurate to `Double`
   *  precision (Higham 2005).  Beyond it [[_MatrixValue.expm]] scales by a power of two and
   *  squares the result back.
   */
  private[core] val PadeTheta13: Double = 5.371920351148152

  /** The relative tolerance of [[_MatrixValue.cholesky]]'s two refusals (F_0054), as a fraction
   *  of the matrix's largest entry: an asymmetry `|aᵢⱼ − aⱼᵢ|` above it is a non-symmetric
   *  matrix, and a pivot at or below it a matrix that is not (numerically) positive definite.
   *
   *  **Relative, never absolute** — both quantities scale with `A`, so an absolute threshold
   *  would be a claim about the matrix's units and `A → k·A` would move the verdict (the 2.11
   *  rule).  `1e-12` sits four orders above `Double`'s rounding of a computed `M·Mᵀ`, so such a
   *  matrix is accepted; a relative pivot below it is a matrix whose condition number exceeds
   *  about `1e12`, whose factor would carry no reliable digits — declined rather than returned.
   */
  private[core] val CholeskyTolerance: Double = 1e-12

  /** Iterations [[_MatrixValue.eigenDecompose]] allows per eigenvalue before declining, with an
   *  exceptional shift every tenth. The Francis iteration converges quadratically, so a typical
   *  eigenvalue takes two to four; the cap only stops a pathological cycle.
   */
  private val MaxEigenIterations = 60

  /** `|a|` carrying the sign of `b`, `+` for `b = 0` (Fortran's `SIGN`, which the EISPACK
   *  routines below are written in terms of — `math.copySign` differs at `b = -0.0`). */
  private def sign(a: Double, b: Double): Double = if b >= 0.0 then math.abs(a) else -math.abs(a)

  /** A 1-based `(n+1)×(n+1)` copy of a row-major `n×n` array: the EISPACK routines below are
   *  transcribed in their own indexing, since an off-by-one in a bulge chase is invisible until
   *  some matrix fails to converge. Row and column 0 are unused. */
  private def oneBased(n: Int, data: Array[Double]): Array[Array[Double]] =
    val a = Array.ofDim[Double](n + 1, n + 1)
    var i = 1
    while i <= n do
      var j = 1
      while j <= n do
        a(i)(j) = data((i - 1) * n + (j - 1))
        j += 1
      i += 1
    a

  /** Balancing (EISPACK `balanc`): a diagonal similarity by powers of two that brings each row
   *  and its column to comparable norms. Exact in binary, so the spectrum is untouched, while the
   *  rounding the QR iteration makes is relative to a better-scaled matrix. */
  private def balance(a: Array[Array[Double]], n: Int): Unit =
    val radix = 2.0
    val sqrdx = radix * radix
    var done  = false
    while !done do
      done = true
      var i = 1
      while i <= n do
        var r = 0.0
        var c = 0.0
        var j = 1
        while j <= n do
          if j != i then
            c += math.abs(a(j)(i))
            r += math.abs(a(i)(j))
          j += 1
        if c != 0.0 && r != 0.0 then
          val s = c + r
          var f = 1.0
          var g = r / radix
          while c < g do
            f *= radix
            c *= sqrdx
          g = r * radix
          while c > g do
            f /= radix
            c /= sqrdx
          if (c + r) / f < 0.95 * s then
            done = false
            g = 1.0 / f
            j = 1
            while j <= n do
              a(i)(j) *= g
              a(j)(i) *= f
              j += 1
        i += 1

  /** Reduction to upper Hessenberg form by stabilised elimination (EISPACK `elmhes`), a
   *  similarity. Hessenberg form is what makes one sub-diagonal entry a valid deflation test,
   *  and the QR iteration preserves it. The multipliers the routine leaves below the
   *  sub-diagonal are cleared: they are not part of the reduced matrix. */
  private def hessenberg(a: Array[Array[Double]], n: Int): Unit =
    var m = 2
    while m < n do
      var x = 0.0
      var i = m
      var j = m
      while j <= n do
        if math.abs(a(j)(m - 1)) > math.abs(x) then
          x = a(j)(m - 1)
          i = j
        j += 1
      if i != m then
        j = m - 1
        while j <= n do
          val t = a(i)(j); a(i)(j) = a(m)(j); a(m)(j) = t
          j += 1
        j = 1
        while j <= n do
          val t = a(j)(i); a(j)(i) = a(j)(m); a(j)(m) = t
          j += 1
      if x != 0.0 then
        i = m + 1
        while i <= n do
          val y = a(i)(m - 1) / x
          if y != 0.0 then
            a(i)(m - 1) = y
            j = m
            while j <= n do
              a(i)(j) -= y * a(m)(j)
              j += 1
            j = 1
            while j <= n do
              a(j)(m) += y * a(j)(i)
              j += 1
          i += 1
      m += 1
    var i = 3
    while i <= n do
      var j = 1
      while j <= i - 2 do
        a(i)(j) = 0.0
        j += 1
      i += 1

  /** The eigenvalues of an upper Hessenberg matrix by the Francis double-shift QR iteration
   *  (EISPACK `hqr`): deflation wherever a sub-diagonal entry becomes negligible, a trailing
   *  1×1 or 2×2 block solved directly, and an implicit double shift that splits a complex pair
   *  in real arithmetic. Destroys `a`. Eigenvalues are returned in deflation order, bottom up,
   *  a pair larger-first (real) or `+im` first (complex); `None` past
   *  [[MaxEigenIterations]] for one eigenvalue. */
  private def hqr(a: Array[Array[Double]], n: Int): Option[Vector[_Value]] =
    val eps   = math.ulp(1.0)
    var anorm = 0.0
    var i0 = 1
    while i0 <= n do
      var j = math.max(i0 - 1, 1)
      while j <= n do
        anorm += math.abs(a(i0)(j))
        j += 1
      i0 += 1
    val eigs   = scala.collection.mutable.ArrayBuffer[_Value]()
    var nn     = n
    var t      = 0.0
    var failed = false
    while nn >= 1 && !failed do
      var its   = 0
      var l     = nn
      var again = true
      while again && !failed do
        // The lowest negligible sub-diagonal entry above row nn splits off the active block l..nn.
        l = nn
        var split = false
        while l >= 2 && !split do
          var s = math.abs(a(l - 1)(l - 1)) + math.abs(a(l)(l))
          if s == 0.0 then s = anorm
          if math.abs(a(l)(l - 1)) <= eps * s then
            a(l)(l - 1) = 0.0
            split = true
          else l -= 1
        var x = a(nn)(nn)
        if l == nn then
          eigs += _Number(x + t)
          nn -= 1
        else
          var y = a(nn - 1)(nn - 1)
          var w = a(nn)(nn - 1) * a(nn - 1)(nn)
          if l == nn - 1 then
            val p = 0.5 * (y - x)
            val q = p * p + w
            val r = math.sqrt(math.abs(q))
            x += t
            if q >= 0.0 then
              val z  = p + sign(r, p)
              val e1 = x + z
              val e2 = if z != 0.0 then x - w / z else e1
              eigs += _Number(math.max(e1, e2))
              eigs += _Number(math.min(e1, e2))
            else
              eigs += _Complex.of(x + p, r)
              eigs += _Complex.of(x + p, -r)
            nn -= 2
          else if its == MaxEigenIterations then failed = true
          else
            if its > 0 && its % 10 == 0 then
              // Exceptional shift: breaks a cycle the standard shifts can fall into.
              t += x
              var i = 1
              while i <= nn do
                a(i)(i) -= x
                i += 1
              val s = math.abs(a(nn)(nn - 1)) + math.abs(a(nn - 1)(nn - 2))
              x = 0.75 * s
              y = x
              w = -0.4375 * s * s
            its += 1
            // Look for two consecutive small sub-diagonal entries, where the bulge can start.
            var m = nn - 2
            var p = 0.0
            var q = 0.0
            var r = 0.0
            var z = 0.0
            var s = 0.0
            var found = false
            while !found do
              z = a(m)(m)
              r = x - z
              s = y - z
              p = (r * s - w) / a(m + 1)(m) + a(m)(m + 1)
              q = a(m + 1)(m + 1) - z - r - s
              r = a(m + 2)(m + 1)
              s = math.abs(p) + math.abs(q) + math.abs(r)
              p /= s
              q /= s
              r /= s
              if m == l then found = true
              else
                val u = math.abs(a(m)(m - 1)) * (math.abs(q) + math.abs(r))
                val v = math.abs(p) * (math.abs(a(m - 1)(m - 1)) + math.abs(z) + math.abs(a(m + 1)(m + 1)))
                if u <= eps * v then found = true else m -= 1
            var i = m + 2
            while i <= nn do
              a(i)(i - 2) = 0.0
              if i != m + 2 then a(i)(i - 3) = 0.0
              i += 1
            // The double-shift step: chase the bulge down with 3×3 Householder reflections.
            var k = m
            while k <= nn - 1 do
              if k != m then
                p = a(k)(k - 1)
                q = a(k + 1)(k - 1)
                r = if k != nn - 1 then a(k + 2)(k - 1) else 0.0
                x = math.abs(p) + math.abs(q) + math.abs(r)
                if x != 0.0 then
                  p /= x
                  q /= x
                  r /= x
              s = sign(math.sqrt(p * p + q * q + r * r), p)
              if s != 0.0 then
                if k == m then
                  if l != m then a(k)(k - 1) = -a(k)(k - 1)
                else a(k)(k - 1) = -s * x
                p += s
                x = p / s
                y = q / s
                z = r / s
                q /= p
                r /= p
                var j = k
                while j <= nn do
                  p = a(k)(j) + q * a(k + 1)(j)
                  if k != nn - 1 then
                    p += r * a(k + 2)(j)
                    a(k + 2)(j) -= p * z
                  a(k + 1)(j) -= p * y
                  a(k)(j) -= p * x
                  j += 1
                val mmin = math.min(nn, k + 3)
                var i2 = l
                while i2 <= mmin do
                  p = x * a(i2)(k) + y * a(i2)(k + 1)
                  if k != nn - 1 then
                    p += z * a(i2)(k + 2)
                    a(i2)(k + 2) -= p * r
                  a(i2)(k + 1) -= p * q
                  a(i2)(k) -= p
                  i2 += 1
              k += 1
        again = l < nn - 1
    Option.when(!failed && eigs.size == n)(eigs.toVector)

  /** The eigenvalues of a row-major `n×n` array: balance, reduce, iterate (F_0100). */
  private[core] def eigenvaluesOf(n: Int, data: Array[Double]): Option[Vector[_Value]] =
    val a = oneBased(n, data)
    balance(a, n)
    hessenberg(a, n)
    hqr(a, n)

  /** Coefficients of the degree-13 Padé approximant to `e^x`, indexed `b(0) … b(13)`.
   *
   *  Tabulated rather than derived: they are the numerators of a fixed rational approximant,
   *  the same constants every implementation of this algorithm carries.  Held as `Double`
   *  although every value is an integer, because they are only ever multiplied into `Double`
   *  matrices — `b(0)` exceeds `Int` and would silently truncate.
   */
  private[core] val PadeB13: Array[Double] = Array(
    64764752532480000.0, 32382376266240000.0, 7771770303897600.0, 1187353796428800.0,
    129060195264000.0, 10559470521600.0, 670442572800.0, 33522128640.0,
    1323241920.0, 40840800.0, 960960.0, 16380.0, 182.0, 1.0)

  /** The augmented exponential `exp([[A, B], [0, 0]]·τ) = [[Φ, Γ], [0, I]]`, returned as `(Φ, Γ)`.
   *
   *  `Φ = e^(Aτ)` and `Γ = ∫₀^τ e^(As) ds · B`.  The block form is defined for a **singular**
   *  `A`, where the inverse form `Γ = A⁻¹(Φ − I)B` is not — and a singular `A` is ordinary,
   *  since any system with an integrator has one.  One kernel serves both callers, so they
   *  agree by construction (F_0006): `control.c2dExact` reads `(A_d, B_d)` at `τ = Ts`, and the
   *  `ode` system tier reads `y(T) = Φ·y0 + Γ` at `τ = T − t0` for `y' = A·y + b`.
   *
   *  @param a   the square state matrix
   *  @param b   the input matrix, with as many rows as `a`
   *  @param tau the time step, of either sign
   *  @return `Some((Φ, Γ))`, or `None` when the operands do not conform or [[expm]] fails
   */
  private[leonardo] def augmentedExp(a: _MatrixValue, b: _MatrixValue,
                                     tau: Double): Option[(_MatrixValue, _MatrixValue)] =
    if a.rows != a.cols || b.rows != a.rows then None
    else
      val n    = a.rows
      val m    = b.cols
      val size = n + m
      val block = new _MatrixValue(size, size, Array.tabulate(size * size) { idx =>
        val (i, j) = (idx / size, idx % size)
        if i < n && j < n then a(i, j) * tau
        else if i < n then b(i, j - n) * tau
        else 0.0
      })
      block.expm.map { e =>
        (new _MatrixValue(n, n, Array.tabulate(n * n)(idx => e(idx / n, idx % n))),
         new _MatrixValue(n, m, Array.tabulate(n * m)(idx => e(idx / m, n + idx % m))))
      }

  /** Machine epsilon for `Double`. */
  private[leonardo] val Epsilon: Double = 2.220446049250313e-16

  /** How many `max(m, n)·ε` a column's orthogonalised norm, relative to the largest original
   *  column norm, may fall to before [[_MatrixValue.qrDecompose]] declares it dependent (F_0091).
   *
   *  **Relative, never absolute**: the test was `norm < 1e-14`, which refused a well-conditioned
   *  design written in units of `1e-15` and *answered* an exactly collinear one written in units
   *  of `1000` — least squares then returned `β ≈ 2e13` (the 2.11 / F_0075 rule, a third time).
   *  **Measured** (2026-10-09, 400 collinear-as-typed decimal matrices per size): the residue of a
   *  dependent column reached `0.89·m·ε` at `m = 3`, `0.27` at 10, `0.09` at 50, `0.04` at 200,
   *  while the smallest independent one was `8.7e-6`. One `m·ε` — the `cond` cut-off — would
   *  leave no margin at `m = 3`; ten leaves 11× there and 270× at `m = 200`, and is still nine
   *  orders below an independent column.
   */
  private[core] val QrDependenceFactor: Double = 10.0

  /** The most Jacobi sweeps [[_MatrixValue.svd]] runs before declining; convergence is
   *  quadratic once the columns are nearly orthogonal, so a handful usually suffice. */
  private[core] val SvdMaxSweeps: Int = 60

  /** The most work [[_MatrixValue.svd]] attempts, as `max(m, n)·min(m, n)²` — the order of one
   *  Jacobi sweep and of the certificate — checked before any is done (F_0061).
   *
   *  **Measured on Node, the slowest platform** (the F_0077 rule): a 200×200 matrix (`8·10⁶`)
   *  takes a median of ~0.6 s there against 67 ms on the JVM, and a 1000×100 one (`10⁷`) ~0.45 s
   *  against 53 ms. Uncapped, `svd(ones(2000))` — which a share link can carry — would hold a
   *  browser for minutes with no way to interrupt it. The MPC demo's sizes (≤ ~200 rows) sit
   *  inside the cap.
   *
   *  The **default** of [[Limit.MaxSvdWork]] (F_0124); [[_MatrixValue.svd]]'s `Limits` overload
   *  reads the caller's value.
   */
  private[leonardo] val MaxSvdWork: Long = Limit.MaxSvdWork.default

  /** `max(m, n)·min(m, n)²`, the work measure [[MaxSvdWork]] bounds. */
  private[leonardo] def svdWork(m: Int, n: Int): Long =
    val (big, small) = (math.max(m, n).toLong, math.min(m, n).toLong)
    big * small * small

  /** How far an SVD may miss its own reconstruction and orthonormality, relative to `‖A‖`,
   *  before it is declined rather than returned (X-02). */
  private[core] val SvdCertificate: Double = 1e-10

  /** Rotates two columns in place by the Jacobi rotation `(c, s)`. */
  private[core] def rotate(a: Array[Double], b: Array[Double], c: Double, s: Double): Unit =
    var i = 0
    while i < a.length do
      val (x, y) = (a(i), b(i))
      a(i) = c * x - s * y
      b(i) = s * x + c * y
      i += 1

  /** A matrix with the given columns, each of length `rows`. */
  private[core] def fromColumns(cols: Vector[Array[Double]], rows: Int): _MatrixValue =
    val k = cols.size
    new _MatrixValue(rows, k, Array.tabulate(rows * k)(idx => cols(idx % k)(idx / k)))

  /** `base` (orthonormal columns of length `dim`) extended to `want` orthonormal columns by
   *  **pivoted** Gram–Schmidt against the standard basis: each step takes the unit vector with the
   *  largest residual, orthogonalised twice so the result stays orthonormal to rounding.
   *
   *  The pivot is what makes completion certain. With `k` columns in hand the residuals' squared
   *  norms sum to `dim − k`, so the largest is at least `(dim − k)/dim`. The first version took
   *  the unit vectors in order and kept one only if its residual exceeded 0.5, claiming that never
   *  failed; with 20 directions spread over 40 dimensions the residuals average `√0.5`, the pass
   *  ran out of candidates, and `svd` of a rank-20 40×40 matrix declined (F_0117).
   *  `None` only when even the best residual is below half its guaranteed size: `base` was not
   *  orthonormal. */
  private[leonardo] def completeBasis(base: Vector[Array[Double]], dim: Int, want: Int): Option[Vector[Array[Double]]] =
    // `while` loops over arrays, not folds over ranges: a fold boxes per step, which Node pays
    // for heavily (the QP precedent); the residuals are updated in place, O(dim²) per column.
    def dot(a: Array[Double], b: Array[Double]): Double =
      var s = 0.0
      var i = 0
      while i < dim do { s += a(i) * b(i); i += 1 }
      s
    def subtract(x: Array[Double], q: Array[Double], c: Double): Unit =
      var i = 0
      while i < dim do { x(i) -= c * q(i); i += 1 }
    def orthogonalised(e: Array[Double], against: Vector[Array[Double]]): Array[Double] =
      val x = e.clone
      for _ <- 1 to 2; q <- against do subtract(x, q, dot(q, x))
      x
    val residual = Array.tabulate(dim)(i => orthogonalised(Array.tabulate(dim)(r => if r == i then 1.0 else 0.0), base))
    val used     = Array.fill(dim)(false)
    var acc      = base
    var stuck    = false
    while acc.size < want && !stuck do
      val candidates = (0 until dim).filterNot(used)
      if candidates.isEmpty then stuck = true
      else
        val best = candidates.maxBy(i => dot(residual(i), residual(i)))
        val need = 0.5 * math.sqrt((dim - acc.size).toDouble / dim)
        if math.sqrt(dot(residual(best), residual(best))) < need then stuck = true
        else
          val x    = orthogonalised(residual(best), acc)
          val norm = math.sqrt(dot(x, x))
          val q    = x.map(_ / norm)
          used(best) = true
          acc = acc :+ q
          candidates.foreach(i => if i != best then subtract(residual(i), q, dot(q, residual(i))))
    Option.when(acc.size >= want)(acc.take(want))

  /** Whether `U·diag(σ)·Vᵀ` reconstructs `a` and `U`, `V` are orthonormal, all relative to `‖a‖`. */
  private[core] def svdCertified(a: _MatrixValue, u: _MatrixValue, s: Vector[Double], v: _MatrixValue): Boolean =
    // Array `while` loops throughout: the checks are O(m·n·k), the same order as the
    // decomposition, and a boxed fold made them ten times its cost on Node (measured).
    val (m, n, k) = (a.rows, a.cols, s.size)
    val (ad, ud, vd) = (a.data, u.data, v.data)
    val sv = s.toArray
    var scale2 = 0.0
    var err2   = 0.0
    var i = 0
    while i < m do
      var j = 0
      while j < n do
        var r = -ad(i * n + j)
        var l = 0
        while l < k do { r += ud(i * k + l) * sv(l) * vd(j * k + l); l += 1 }
        err2 += r * r
        scale2 += ad(i * n + j) * ad(i * n + j)
        j += 1
      i += 1
    def offOrthonormal(q: Array[Double], rows: Int, cols: Int): Double =
      var worst = 0.0
      var p = 0
      while p < cols do
        var r = p
        while r < cols do
          var dot = 0.0
          var t   = 0
          while t < rows do { dot += q(t * cols + p) * q(t * cols + r); t += 1 }
          worst = math.max(worst, math.abs(dot - (if p == r then 1.0 else 0.0)))
          r += 1
        p += 1
      worst
    math.sqrt(err2) <= SvdCertificate * math.sqrt(scale2) &&
      offOrthonormal(ud, u.rows, u.cols) <= SvdCertificate && offOrthonormal(vd, v.rows, v.cols) <= SvdCertificate

  /** n×n identity matrix — dense counterpart of the `matrix.IdentityMatrix` node.
   *  Used by the vectorized (Sylvester) solver tier for absent left/right coefficients.
   *  @param n side length; must be positive
   */
  def identity(n: Int): _MatrixValue =
    val out = new Array[Double](n * n)
    var i = 0
    while i < n do
      out(i * n + i) = 1.0
      i += 1
    new _MatrixValue(n, n, out)

  /** Inverse of `vec`: reshapes a `(rows·cols)×1` column vector back into a `rows×cols`
   *  matrix using column-major order (matching `vec`'s stacking).
   *  `None` when `v` does not conform to the requested dimensions.
   *  @param v    the vectorised matrix; must be a column vector of length `rows * cols`
   *  @param rows target number of rows
   *  @param cols target number of columns
   */
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

  /** Reads an evaluated operand as a dense matrix: a `_MatrixValue` as it is, or a symbolic
   *  matrix whose every cell is a real number.
   *
   *  The one reader of that fact (F_0080); four private copies used to disagree about it.  An
   *  **exact** matrix stays symbolic — `_Matrix.eval` does not collapse rational cells — so it
   *  is accepted here and **demoted**, through the widening `_Number` extractor: every caller is
   *  a `Double` algorithm (`expm`, QR, the cdf kernels), and the alternative is losing the
   *  operation rather than its exactness, the precedent `lu`/`qr`/`eigen` set.  Needs only the
   *  `core` shape trait, so every layer can use it.
   *
   *  @param r the evaluated operand
   *  @return the dense matrix, or `None` for a scalar, a non-matrix, or a non-numeric cell
   */
  private[leonardo] def ofEvaluated(r: Either[_Expression, _Value]): Option[_MatrixValue] = r match
    case Right(m: _MatrixValue) => Some(m)
    case other =>
      cellsOf(other).flatMap { (rows, cols, cells) =>
        val ds = cells.collect { case _Number(d) => d }
        Option.when(ds.size == cells.size)(new _MatrixValue(rows, cols, ds.toArray))
      }

  /** Reads an evaluated operand as a matrix of values: a dense matrix's numbers, or a
   *  `_MatrixShaped` whose every cell is a `_Value` — **nothing demoted**, so an exact or complex
   *  cell arrives as itself (F_0147).
   *
   *  The one reader under [[ofEvaluated]], `statistics.sampleOf`, `transform`'s vector reader and
   *  `probability`'s, which each matched the two carriers by hand; they differ now only in the
   *  filter they apply afterwards (a vector shape, a real cell, a complex one).
   *
   *  @param r the evaluated operand
   *  @return `(rows, cols, cells)` row-major, or `None` for a scalar, a non-matrix or a cell that
   *          is not a value
   */
  private[leonardo] def cellsOf(r: Either[_Expression, _Value]): Option[(Int, Int, Vector[_Value])] = r match
    case Right(m: _MatrixValue) => Some((m.rows, m.cols, m.toVector.map(d => _Number(d))))
    case Left(m: _MatrixShaped) =>
      val cells = m.children.toVector.collect { case v: _Value => v }
      Option.when(cells.size == m.rows * m.cols)((m.rows, m.cols, cells))
    case _ => None


/** Dense, fully-reduced matrix value: a row-major `Array[Double]`, not `n²` [[_Number]] nodes.
 *
 *  The concrete counterpart of the symbolic matrix node (`matrix._Matrix`), exactly as
 *  [[_Number]] is the concrete counterpart of a scalar expression.  Lives in `core` so
 *  [[Environment]] can bind matrix values without `core` depending on any domain.
 *
 *  Immutability: the public factory ([[_MatrixValue$.apply]]) takes a defensive copy and the
 *  storage is private — kernels build results on arrays they own, skipping the copy.
 *
 *  `multiply` parallelises over independent row blocks above a work-volume threshold;
 *  writes are disjoint and require no locking.
 *
 *  @param rows number of rows; must be positive
 *  @param cols number of columns; must be positive
 */
final class _MatrixValue private (val rows: Int, val cols: Int, private val data: Array[Double]) extends _Value:
  require(rows > 0 && cols > 0, s"matrix dimensions must be positive: ${rows}x$cols")
  require(data.length == rows * cols, s"expected ${rows * cols} elements, got ${data.length}")

  /** Returns the element at row `i`, column `j` (zero-based). */
  def apply(i: Int, j: Int): Double = data(i * cols + j)

  /** Read-only view of the dense storage in row-major order. */
  def toVector: Vector[Double] = data.toVector

  /** Returns `Right(this)` — a concrete matrix needs no further reduction. */
  override def eval(env: Environment): Either[_Expression, _Value] = Right(this)
  override def children: List[_Expression] = List.empty
  override def rebuild(c: List[_Expression]): _Expression = this

  override def equals(other: Any): Boolean = other match
    case that: _MatrixValue =>
      rows == that.rows && cols == that.cols && java.util.Arrays.equals(data, that.data)
    case _ => false

  /** Cached hash code; safe because the storage is immutable. */
  override lazy val hashCode: Int =
    31 * (31 * rows + cols) + java.util.Arrays.hashCode(data)

  /** Renders this matrix at `precision` decimal places for REPL display; mirrors
   *  [[_Number$.round _Number.round]] behaviour.
   *  @param precision number of decimal places to show
   */
  def display(precision: Int): String =
    (0 until rows).map(i =>
      (0 until cols).map(j => _Number.render(_Number.round(this(i, j), precision)))
        .mkString("[", ", ", "]")
    ).mkString("[", ", ", "]")

  /** Renders this matrix at [[Environment.DefaultPrecision]] decimal places. */
  override def toString: String = display(Environment.DefaultPrecision)

  /** Returns `true` when every element is finite (not NaN, not infinite). */
  def isFinite: Boolean = data.forall(d => d.isFinite)

  /** Lifts this matrix into an `Either` for use in `eval`: `Right(this)` when all elements
   *  are finite; `Left(orElse)` otherwise (non-finite is a domain error — stays symbolic like
   *  `x / 0`).  Shared by `scalar` and `matrix` packages without requiring cross-imports.
   *  @param orElse the symbolic fallback expression
   */
  def guarded(orElse: _Expression): Either[_Expression, _Value] =
    if isFinite then Right(this) else Left(orElse)

  /** Element-wise matrix addition.
   *  @param that matrix to add; must have the same `rows` and `cols`
   */
  def add(that: _MatrixValue): _MatrixValue =
    require(rows == that.rows && cols == that.cols, s"dimension mismatch: ${rows}x$cols + ${that.rows}x${that.cols}")
    val out = new Array[Double](data.length)
    var i = 0
    while i < data.length do
      out(i) = data(i) + that.data(i)
      i += 1
    new _MatrixValue(rows, cols, out)

  /** Multiplies every element by scalar `k`.
   *  @param k the scalar factor
   */
  def scale(k: Double): _MatrixValue =
    new _MatrixValue(rows, cols, data.map(_ * k))

  /** Returns the transpose of this matrix (`cols × rows`). */
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

  /** [[https://en.wikipedia.org/wiki/Kronecker_product Kronecker product]] `this ⊗ that`: the `(rows·that.rows) × (cols·that.cols)` block
   *  matrix whose `(i, j)` block is `this(i, j) · that`.  The kernel behind the vectorized
   *  matrix-equation tier: `vec(A·X·B) = (Bᵀ ⊗ A) · vec(X)`.
   *  @param that the right factor
   */
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

  /** Column-stacking vectorisation: returns the `(rows·cols) × 1` column vector where
   *  `vec[j·rows + i] = this(i, j)`.  Paired with [[_MatrixValue$.unvec]].
   */
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

  /** Determinant via [[https://en.wikipedia.org/wiki/LU_decomposition LU decomposition]] with [[https://en.wikipedia.org/wiki/Pivot_element partial pivoting]], O(n³).
   *  `None` when the matrix is non-square; `Some(0.0)` when singular (zero pivot).
   *  Operates on a defensive clone — the value's storage is untouched.
   *  @return `Some(det)` for square matrices, `None` for non-square
   */
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

  /** [[https://en.wikipedia.org/wiki/Matrix_exponential Matrix exponential]] `e^A = sum(k >= 0) A^k / k!`, by [[https://en.wikipedia.org/wiki/Matrix_exponential#Computing_the_matrix_exponential scaling and squaring]] with a
   *  degree-13 Padé approximant.
   *
   *  **Not the eigen route.**  `V·diag(e^λ)·V⁻¹` is tempting because [[spectralDecompose]]
   *  already exists, but it needs an eigenbasis and a **defective** matrix has none — and a
   *  defective matrix is ordinary in the problems that want this, since a repeated pole
   *  produces one.  A Jordan block `(λ 1; 0 λ)` would come back wrong rather than refused.
   *  Scaling and squaring is uniform over both cases, which is why it is the standard method.
   *
   *  The identity `e^A = (e^(A/2^s))^(2^s)` is what makes it work: `A` is scaled until its
   *  norm is inside the Padé approximant's accurate radius, the approximant is evaluated
   *  there, and the result is squared back `s` times.
   *
   *  Note this is *matrix* Padé — a fixed rational approximant with tabulated coefficients —
   *  and is unrelated to the scalar `scalar.padeApproximant`, which fits a series.
   *
   *  Exactness follows the decomposition precedent rather than `matrix/Exact.scala`: the
   *  exponential is transcendental, so an exact operand demotes to `Double` exactly as
   *  `lu`/`qr`/`eigen` do — "demote an exact operand rather than lose the operation".
   *
   *  @return `Some(e^A)` for a square matrix, `None` when non-square or non-finite
   */
  def expm: Option[_MatrixValue] =
    if rows != cols || !isFinite then None
    else
      val n  = rows
      val id = _MatrixValue.identity(n)

      // Padé 13 is accurate for ||A||_1 <= theta; beyond it, scale by a power of two and
      // square back afterwards. The 1-norm (max absolute column sum) is the standard choice.
      val norm = (0 until n).map(j => (0 until n).map(i => math.abs(this(i, j))).sum).maxOption.getOrElse(0.0)
      val squarings = if norm > _MatrixValue.PadeTheta13 then
        math.max(0, math.ceil(math.log(norm / _MatrixValue.PadeTheta13) / math.log(2)).toInt)
      else 0
      val a = scale(1.0 / math.pow(2, squarings))

      // Higham's evaluation scheme: even powers only, so the degree-13 approximant costs six
      // multiplications rather than thirteen.
      val b  = _MatrixValue.PadeB13
      val a2 = a.multiply(a)
      val a4 = a2.multiply(a2)
      val a6 = a4.multiply(a2)

      val uInner = a6.scale(b(13)).add(a4.scale(b(11))).add(a2.scale(b(9)))
      val u = a.multiply(
        a6.multiply(uInner).add(a6.scale(b(7))).add(a4.scale(b(5))).add(a2.scale(b(3))).add(id.scale(b(1))))
      val vInner = a6.scale(b(12)).add(a4.scale(b(10))).add(a2.scale(b(8)))
      val v = a6.multiply(vInner).add(a6.scale(b(6))).add(a4.scale(b(4))).add(a2.scale(b(2))).add(id.scale(b(0)))

      // r = (V - U)^-1 (V + U); a singular (V - U) means the approximant is unusable here.
      v.add(u.scale(-1)).inverse
        .map(inv => (1 to squarings).foldLeft(inv.multiply(v.add(u)))((acc, _) => acc.multiply(acc)))
        .filter(_.isFinite)

  /** Inverse via [[https://en.wikipedia.org/wiki/Gaussian_elimination#Gauss%E2%80%93Jordan_elimination Gauss–Jordan elimination]] with partial pivoting, O(n³).
   *  `None` when the matrix is non-square or singular — the caller stays symbolic, the
   *  same "domain error stays symbolic" contract as `x / 0` in `scalar.Ratio`.
   *  @return `Some(A⁻¹)` for invertible square matrices, `None` otherwise
   */
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

  /** [[https://en.wikipedia.org/wiki/LU_decomposition LU decomposition]] with partial pivoting: `P·A = L·U`, where `L` is unit lower
   *  triangular, `U` is upper triangular, and `P` is the permutation matrix.
   *  `None` when the matrix is non-square or singular (zero pivot encountered).
   *  @return `Some((L, U, P))` for square non-singular matrices, `None` otherwise
   */
  def luDecompose: Option[(_MatrixValue, _MatrixValue, _MatrixValue)] =
    if rows != cols then None
    else
      val n    = rows
      val a    = data.clone
      val perm = Array.tabulate(n)(i => i)   // perm(i) = original row at position i
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

  /** [[https://en.wikipedia.org/wiki/Cholesky_decomposition Cholesky factorisation]] of a
   *  symmetric positive-definite matrix: the lower-triangular `L` with a positive diagonal and
   *  `A = L·Lᵀ` (F_0054).
   *
   *  **The refusals are the point**: its failure is the cheapest convexity test there is, which
   *  is what the quadratic-programming tier (F_0053) relies on.  A non-square matrix, an
   *  asymmetric one — never factored from one triangle, which would answer for a different
   *  matrix — and one with a non-positive pivot (indefinite, negative definite, or singular
   *  semidefinite) give `None`.  Both tests are relative to the largest entry, at
   *  `CholeskyTolerance`, so rescaling the matrix never moves the verdict.
   *
   *  @return `Some(L)` for a symmetric positive-definite matrix, `None` otherwise
   */
  def cholesky: Option[_MatrixValue] =
    val n     = rows
    val scale = data.foldLeft(0.0)((m, d) => math.max(m, math.abs(d)))
    val tol   = _MatrixValue.CholeskyTolerance * scale
    def symmetric: Boolean =
      (0 until n).forall(i => (0 until i).forall(j => math.abs(data(i * n + j) - data(j * n + i)) <= tol))
    if rows != cols || !isFinite || scale == 0.0 || !symmetric then None
    else
      val l = new Array[Double](n * n)
      // The O(n³) inner product, as a `while` loop like this file's other kernels: a `foldLeft`
      // over an index range boxes a `Double` per step, which F_0053 measured at ~8x and F_0081
      // measured here (see `Done.md`).
      def dot(i: Int, j: Int, upTo: Int): Double =
        var s = 0.0
        var k = 0
        val (ri, rj) = (i * n, j * n)
        while k < upTo do
          s += l(ri + k) * l(rj + k)
          k += 1
        s
      // Column by column: the pivot first, then the entries below it.  `forall` stops at the
      // first non-positive pivot, which is the whole refusal.
      val positive = (0 until n).forall { j =>
        val pivot = data(j * n + j) - dot(j, j, j)
        pivot > tol && {
          val ljj = math.sqrt(pivot)
          l(j * n + j) = ljj
          (j + 1 until n).foreach(i => l(i * n + j) = (data(i * n + j) - dot(i, j, j)) / ljj)
          true
        }
      }
      Option.when(positive)(new _MatrixValue(n, n, l))

  /** Eigenvalues by the [[https://en.wikipedia.org/wiki/QR_algorithm Francis double-shift QR iteration]]:
   *  the matrix is balanced, reduced to upper Hessenberg form, and iterated with implicit
   *  double shifts (EISPACK `balanc`, `elmhes`, `hqr`). Returns the `n` eigenvalues as
   *  [[_Number]] (real) or [[_Complex]] (a conjugate pair, `+im` first).
   *
   *  **It was a full-matrix QR iteration with a single real shift until F_0100**, and declined
   *  most random dense matrices from `n = 3`: it deflated only a trailing `1×1`, tested one
   *  sub-diagonal entry of a matrix it never reduced to Hessenberg form (so the test did not
   *  say the last row was split off), and shifted a trailing complex pair by its real part,
   *  which cannot split it. The double shift splits the pair in real arithmetic, and a
   *  deflation may happen at any sub-diagonal entry. Every test is relative to the entries it
   *  compares, so `k·A` gives `k` times the spectrum (the F_0091 rule).
   *  @return `Some(eigenvalues)` in deflation order, bottom up; `None` for a non-square or
   *          non-finite matrix, or no convergence within 60 iterations for one eigenvalue
   */
  def eigenDecompose: Option[Vector[_Value]] =
    if rows != cols || !isFinite then None
    else if rows == 1 then Some(Vector(_Number(data(0))))
    else _MatrixValue.eigenvaluesOf(rows, data)

  // Null vector of a real n×n matrix b via RREF + back-substitution: the one belonging to the
  // `which`-th free column, so a repeated eigenvalue asking for its 0th, 1st, ... vector gets a
  // basis of its eigenspace rather than the same direction each time (found by LA 06: eig(-I)
  // returned two equal columns).  Returns the normalised vector, or None when b has fewer than
  // `which + 1` free columns (full rank, or a defective eigenvalue) or the vector is zero.
  private def realNullVec(b: Array[Double], n: Int, which: Int): Option[Array[Double]] =
    val a   = b.clone
    val piv = Array.fill(n)(-1)
    var row = 0; var col = 0
    // Relative pivot threshold: ties the zero-detection to the matrix's own scale.
    // The eigenvalue returned by QR iteration can have an absolute error several
    // orders of magnitude larger than the deflation criterion (1e-12), so the
    // near-zero diagonal entries of (A - λI) can be ≈ 1e-10 even for simple
    // eigenvalues.  A relative tolerance of 1e-8 (of the largest entry in b)
    // absorbs this without misclassifying genuine small pivots.
    // Purely relative (F_0125): the 1e-12 floor sat above every entry of a matrix in units of
    // 1e-14, and eig(1e-14·[[1, 1], [0, 2]]) returned (1, 0) for both eigenvectors. `<=` keeps an
    // all-zero `b` (scale 0) a skipped column rather than a division by zero.
    val scale = b.foldLeft(0.0)((m, v) => math.max(m, math.abs(v)))
    val tol   = scale * 1e-8
    while row < n && col < n do
      var maxAbs = 0.0; var maxRow = row; var r = row
      while r < n do
        val v = math.abs(a(r * n + col))
        if v > maxAbs then { maxAbs = v; maxRow = r }
        r += 1
      if maxAbs <= tol then col += 1
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
            // An exact test: an absolute 1e-15 left a row of a 1e-14 matrix uneliminated.
            if f != 0.0 then
              var k = 0; while k < n do { a(rr*n+k) -= f * a(row*n+k); k += 1 }
          rr += 1
        row += 1; col += 1
    val pivSet = piv.filter(_ >= 0).toSet
    (0 until n).filterNot(pivSet.contains).lift(which) match
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
  // via complex RREF + back-substitution: the one belonging to the `which`-th free column, as in
  // realNullVec, so a repeated pair gets a basis of its eigenspace (F_0175).
  // Returns (re-part, im-part) of the normalised null vector, or None.
  private def complexNullVec(n: Int, lRe: Double, lIm: Double, which: Int): Option[(Array[Double], Array[Double])] =
    val re = Array.tabulate(n * n)(k => data(k) - (if k/n == k%n then lRe else 0.0))
    val im = Array.tabulate(n * n)(k =>                             if k/n == k%n then -lIm else 0.0)
    val piv = Array.fill(n)(-1)
    var row = 0; var col = 0
    // Purely relative, as in realNullVec (F_0125).
    val scale = re.zip(im).foldLeft(0.0) { case (m, (r, i)) => math.max(m, math.hypot(r, i)) }
    val tol   = scale * 1e-8
    while row < n && col < n do
      var maxMag = 0.0; var maxRow = row; var r = row
      while r < n do
        val m = math.hypot(re(r*n+col), im(r*n+col))
        if m > maxMag then { maxMag = m; maxRow = r }
        r += 1
      if maxMag <= tol then col += 1
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
            if fRe != 0.0 || fIm != 0.0 then
              var k = 0
              while k < n do
                re(rr*n+k) -= fRe*re(row*n+k) - fIm*im(row*n+k)
                im(rr*n+k) -= fRe*im(row*n+k) + fIm*re(row*n+k)
                k += 1
          rr += 1
        row += 1; col += 1
    val pivSet = piv.filter(_ >= 0).toSet
    (0 until n).filterNot(pivSet.contains).lift(which) match
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

  /** Spectral decomposition: eigenvalues and right eigenvectors.
   *  Returns `(columns of V, eigenvalues)` where eigenvector column `j` corresponds to
   *  eigenvalue `j`.  Complex conjugate pairs `(α ± βi)` occupy consecutive positions.
   *  An eigenvalue met for the `k`-th time (within `1e-8` of the largest entry) takes the null
   *  vector of the `k`-th free column, so a repeated eigenvalue with a full eigenspace gets
   *  independent eigenvectors; a defective one runs out of free columns.  Real eigenvalues since
   *  F_0174, complex pairs since F_0175 (counted by their `+im` member).
   *  `None` when non-square, QR iteration fails, or an eigenvector cannot be extracted
   *  (numerically degenerate or defective matrix).
   *  @return `Some((V, eigenvalues))` or `None`
   */
  def spectralDecompose: Option[(Vector[Vector[_Value]], Vector[_Value])] =
    if rows != cols then None
    else eigenDecompose.flatMap { eigs =>
      val n = rows
      var i = 0
      val evOpts = scala.collection.mutable.ArrayBuffer[Option[Vector[_Value]]]()
      val same   = 1e-8 * data.foldLeft(0.0)((m, v) => math.max(m, math.abs(v)))
      val seen   = scala.collection.mutable.ArrayBuffer[Double]()
      val seenC  = scala.collection.mutable.ArrayBuffer[(Double, Double)]()
      while i < n do
        eigs(i) match
          case _Number(lam) =>
            val b     = Array.tabulate(n * n)(k => data(k) - (if k/n == k%n then lam else 0.0))
            val which = seen.count(s => math.abs(s - lam) <= same)
            seen += lam
            evOpts += realNullVec(b, n, which).map(_.map(_Number(_)).toVector)
            i += 1
          case c: _Complex =>
            val which = seenC.count((r, m) => math.hypot(r - c.re, m - c.im) <= same)
            seenC += ((c.re, c.im))
            complexNullVec(n, c.re, c.im, which) match
              case None =>
                evOpts += None; evOpts += None
              case Some((vRe, vIm)) =>
                evOpts += Some(vRe.zip(vIm).map { case (r, m) => _Complex.of(r, m) }.toVector)
                evOpts += Some(vRe.zip(vIm).map { case (r, m) => _Complex.of(r, -m) }.toVector)
            i += 2
          case _ =>
            evOpts += None; i += 1
      // Sequence the per-eigenvalue options: all defined → Some(vectors), else None
      // (flatten drops the Nones, so equal sizes means none were dropped).
      val opts = evOpts.toVector
      val cols = opts.flatten
      if cols.size == opts.size then Some((cols, eigs)) else None
    }

  /** [[https://en.wikipedia.org/wiki/QR_decomposition QR decomposition]] via modified [[https://en.wikipedia.org/wiki/Gram%E2%80%93Schmidt_process Gram-Schmidt]]: `A = Q·R` (`m ≥ n`).
   *  `Q` is `m × n` with orthonormal columns; `R` is `n × n` upper triangular.
   *  `None` when `rows < cols` or the matrix is rank-deficient: a column whose orthogonalised
   *  norm is at most `QrDependenceFactor·max(m, n)·ε` times the largest original column norm —
   *  **relative**, so the verdict for `k·A` is the verdict for `A` (F_0091).
   *  @return `Some((Q, R))` or `None`
   */
  def qrDecompose: Option[(_MatrixValue, _MatrixValue)] =
    if rows < cols then None
    else
      val m = rows
      val n = cols
      val q = data.clone    // q[i*n+j] = row i, col j — will become Q
      val r = Array.fill(n * n)(0.0)
      // The largest original column norm sets the scale of "numerically zero"; a zero matrix
      // gives a zero threshold, and its first column's zero norm still declines.
      var maxColNorm = 0.0
      var c = 0
      while c < n do
        var s = 0.0
        var k = 0
        while k < m do { s += data(k * n + c) * data(k * n + c); k += 1 }
        maxColNorm = math.max(maxColNorm, math.sqrt(s))
        c += 1
      val dependent = _MatrixValue.QrDependenceFactor * math.max(m, n) * _MatrixValue.Epsilon * maxColNorm
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
        if norm <= dependent then return None   // rank-deficient
        r(j * n + j) = norm
        k = 0
        while k < m do { q(k * n + j) /= norm; k += 1 }
        j += 1
      Some((_MatrixValue(m, n, q), _MatrixValue(n, n, r)))

  /** The [[https://en.wikipedia.org/wiki/Singular_value_decomposition singular value decomposition]] `A = U·diag(σ)·Vᵀ`, economy size (F_0061).
   *
   *  `U` is `m×k` and `V` is `n×k` with orthonormal columns, `k = min(m, n)`, and `σ` holds the
   *  `k` singular values in descending order. **One-sided Jacobi** (Hestenes): column pairs are
   *  rotated until every pair is orthogonal, so the singular values come from column norms and
   *  small ones keep their relative accuracy — and `AᵀA` is never formed, which would square the
   *  condition number (the 2.11 rule). A wide matrix is decomposed through its transpose. A
   *  column whose singular value is numerically zero has no direction of its own, so `U` is
   *  completed there by Gram–Schmidt against the standard basis.
   *
   *  **Certified** (X-02): the answer is returned only if `‖U·Σ·Vᵀ − A‖` and the departure of
   *  `U` and `V` from orthonormality are below [[_MatrixValue.SvdCertificate]], relative to `‖A‖`.
   *
   *  @return `Some((U, σ, V))`, or `None` for a non-finite entry, non-convergence within
   *          [[_MatrixValue.SvdMaxSweeps]] sweeps, a failed certificate, or work past the
   *          default [[Limit.MaxSvdWork]]
   */
  def svd: Option[(_MatrixValue, Vector[Double], _MatrixValue)] = svd(Limits.Default)

  /** [[svd]] under `limits` (F_0124): the work `max(m, n)·min(m, n)²` may be at most
   *  [[Limit.MaxSvdWork]]. An overload rather than a defaulted parameter, which would be a
   *  binary break.
   *
   *  @param limits the budgets
   *  @return `Some((U, σ, V))`, or `None` as for [[svd]]
   */
  def svd(limits: Limits): Option[(_MatrixValue, Vector[Double], _MatrixValue)] =
    if !isFinite || _MatrixValue.svdWork(rows, cols) > limits(Limit.MaxSvdWork) then None
    else if rows < cols then transpose.svd(limits).map((u, s, v) => (v, s, u))
    else
      val (m, n) = (rows, cols)
      val w = Array.tabulate(n)(j => Array.tabulate(m)(i => data(i * n + j)))
      val v = Array.tabulate(n)(j => Array.tabulate(n)(i => if i == j then 1.0 else 0.0))
      // The orthogonality test is on the cosine of each pair's angle; rounding alone leaves a
      // dot product of order m·ε between orthogonal columns, so the tolerance scales with m.
      val tol = math.max(m, 1) * _MatrixValue.Epsilon
      var sweep   = 0
      var rotated = true
      while rotated && sweep < _MatrixValue.SvdMaxSweeps do
        rotated = false
        var p = 0
        while p < n - 1 do
          var q = p + 1
          while q < n do
            val (wp, wq) = (w(p), w(q))
            var alpha = 0.0
            var beta  = 0.0
            var gamma = 0.0
            var i = 0
            while i < m do
              alpha += wp(i) * wp(i)
              beta  += wq(i) * wq(i)
              gamma += wp(i) * wq(i)
              i += 1
            // Two floating-point floors, both reached by a rank-deficient matrix, whose dead column
            // the sweep drives towards zero geometrically (F_0099):
            //  - ζ grows past 1e154 and ζ² overflows, so √(1 + ζ²) made the tangent 1/∞ = 0 and
            //    the rotation the identity -- hence hypot(1, ζ);
            //  - the column's squared norm underflows to exactly 0 while γ is still a non-zero
            //    subnormal, so the cosine test read |γ| > 0 forever. A column whose squared norm
            //    is zero has no direction left to rotate: the pair counts as converged.
            // Either way the sweep repeated a no-op until SvdMaxSweeps and the SVD declined.
            if alpha > 0 && beta > 0 && math.abs(gamma) > tol * math.sqrt(alpha) * math.sqrt(beta) then
              rotated = true
              val zeta = (beta - alpha) / (2 * gamma)
              val t    = (if zeta >= 0 then 1.0 else -1.0) / (math.abs(zeta) + math.hypot(1.0, zeta))
              val c    = 1 / math.sqrt(1 + t * t)
              _MatrixValue.rotate(wp, wq, c, c * t)
              _MatrixValue.rotate(v(p), v(q), c, c * t)
            q += 1
          p += 1
        sweep += 1
      if rotated then None
      else
        val sigma  = w.map(col => math.sqrt(col.map(x => x * x).sum))
        val order  = (0 until n).sortBy(j => -sigma(j)).toVector
        val sMax   = sigma(order.head)
        val live   = order.filter(j => sigma(j) > 0 && sigma(j) > _MatrixValue.Epsilon * sMax)
        val uCols  = live.map(j => w(j).map(_ / sigma(j)))
        _MatrixValue.completeBasis(uCols, m, n).flatMap { us =>
          val u     = _MatrixValue.fromColumns(us, m)
          val vv    = _MatrixValue.fromColumns(order.map(j => v(j)), n)
          val s     = order.map(j => sigma(j))
          Option.when(_MatrixValue.svdCertified(this, u, s, vv))((u, s, vv))
        }

  /** Matrix multiplication `this × that` (`this.cols` must equal `that.rows`).
   *
   *  Block-tiled `i-k-j` loop: `Tile × Tile` blocks keep the hot tile of `that` (and `out`)
   *  cache-resident across a whole row block.  Within each output cell, `k`-accumulation is
   *  ascending, so results are bit-identical to the untiled kernel.  Row blocks write disjoint
   *  slices of `out` and parallelise with no locking above the work-volume threshold.
   *  @param that right factor; `that.rows` must equal `this.cols`
   */
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

    // Dispatch is platform-specific and lives in BlockDispatch: `java.util.stream` is the one
    // JDK API this library uses that Scala.js does not provide (issue F_0003). The JS side is
    // sequential, which costs nothing in correctness -- blocks write disjoint slices of `out`,
    // so parallel and sequential were always required to agree.
    val blocks   = (rows + tile - 1) / tile
    val parallel = rows.toLong * n * w >= _MatrixValue.ParallelThreshold && blocks > 1
    BlockDispatch.foreachBlock(blocks, parallel)(b => blockKernel(b * tile))
    new _MatrixValue(rows, that.cols, out)

package it.grypho.scala.leonardo
package matrix

import core.*
import scalar.{Sum, Product, Ratio}


/** Marker trait for matrix-valued AST nodes.
 *
 *  Evaluation is two-path: when both operands reduce to dense `_MatrixValue`s the
 *  dense kernels in `_MatrixValue` are used (fast path); otherwise, matrix literals
 *  combine element-wise via the `sumOf`/`productOf` helpers, and the resulting
 *  [[_Matrix]] collapses to a dense value when every element folded, or stays
 *  symbolic otherwise.
 *
 *  Dimension mismatches and non-finite results are domain errors and stay symbolic,
 *  exactly like `x/0` in `scalar.Ratio`.
 *
 *  Not every matrix-related node extends `_MatrixOperation`: [[Determinant]] and
 *  [[_MatrixIndex]] produce scalar results, so they extend `_Expression` directly
 *  to prevent the parser from treating `2 * det(A)` as a matrix scale.
 */
trait _MatrixOperation extends _Expression


/** Returns the [[_Matrix]] view of an already-evaluated operand, or `None`. */
private def asLiteral(r: Either[_Expression, _Value]): Option[_Matrix] = r match
  case Left(m: _Matrix)       => Some(m)
  case Right(v: _MatrixValue) => Some(_Matrix.fromValue(v))
  case _                      => None

/** Folds two already-reduced operands with `+`, avoiding a redundant eval pass.
 *
 *  The exact case comes first because `_Number` is a widening extractor; without it every
 *  element-wise matrix sum would drop out of the exact tier (issue 4.L slice B).  The
 *  reduction policy is the benchmarked default rather than the environment's, since these
 *  helpers take no `Environment` — the policy is a cost knob, never a semantic one.
 */
private def sumOf(a: _Expression, b: _Expression): _Expression = (a, b) match
  case (x: _Rational, y: _Rational) => x.add(y, _Rational.DefaultPolicy)
  case (_Number(x), _Number(y))     => _Number(x + y)
  case _                            => Sum(a, b)

/** Folds two already-reduced operands with `*`, applying the zero short-circuit. */
private def productOf(a: _Expression, b: _Expression): _Expression = (a, b) match
  // An exact zero stays exact: it is the additive identity of the sums this feeds, so
  // demoting it here would make one zero entry infect a whole exact row.
  case (z: _Rational, _) if z.isZero          => z
  case (_, z: _Rational) if z.isZero          => z
  case (x: _Rational, y: _Rational)           => x.multiply(y, _Rational.DefaultPolicy)
  case (_Number(0.0), _) | (_, _Number(0.0))  => _Number(0)
  case (_Number(x), _Number(y))               => _Number(x * y)
  case _                                      => Product(a, b)

/** Collapses a combined [[_Matrix]] literal to a dense value if all elements are `_Number`s.
 *
 *  Exact elements block the collapse, for the reason given in `matrix.Exact`: the dense
 *  carrier is an `Array[Double]`, so collapsing an exact matrix would silently throw the
 *  exactness away.  Same rule as `_Matrix.eval`.
 */
private def collapse(m: _Matrix, orElse: _Expression): Either[_Expression, _Value] =
  val numbers = m.elems.collect { case _Number(d) => d }
  if m.elems.exists(_.isInstanceOf[_Rational]) then Left(m)
  else if numbers.size == m.elems.size then _MatrixValue(m.rows, m.cols, numbers.toArray).guarded(orElse)
  else Left(m)

/** Whether an evaluated operand is a real scalar — a `_Number` **or** an exact `_Rational`.
 *
 *  A type test on `_Number` alone would miss the exact case and leave `2 * A` symbolic in
 *  exact mode; the widening extractor covers both.
 */
private def isScalarValue(r: Either[_Expression, _Value]): Boolean =
  r.exists { case _Number(_) => true; case _ => false }


/** Maximum matrix dimension for symbolic cofactor expansion (n! grows fast above this). */
private val MaxSymbolicDim = 6

/** Maximum single dimension for dense constructor results (`eye`/`zeros`).
 *  A dimension >= 46341 overflows `rows * cols` as an `Int`; 4096^2 doubles is ~128 MB.
 *  Above this bound a constructor stays symbolic.
 */
private val MaxDenseDim = 4096

/** Returns `true` when `d` is a valid dense dimension: a positive integer at most `MaxDenseDim`. */
private def validDenseDim(d: Double): Boolean =
  d >= 1.0 && d == d.toLong && d <= MaxDenseDim

/** Returns the `(rows-1) x (cols-1)` minor of `m` with row `si` and column `sj` removed. */
private def minorOf(m: _Matrix, si: Int, sj: Int): _Matrix =
  val elems =
    for i <- 0 until m.rows if i != si; j <- 0 until m.cols if j != sj yield m(i, j)
  _Matrix(m.rows - 1, m.cols - 1, elems.toVector)

/** Returns the determinant of a symbolic square matrix by Laplace expansion along the first row.
 *  Concrete element pairs are folded immediately via `sumOf`/`productOf`.
 *  The 1x1 base case returns the single element directly.
 */
private def symbolicDet(m: _Matrix): _Expression =
  if m.rows == 1 then m(0, 0)
  else
    (0 until m.cols).map { j =>
      val term = productOf(m(0, j), symbolicDet(minorOf(m, 0, j)))
      if j % 2 == 0 then term else productOf(_Number(-1), term)
    }.reduce(sumOf)

/** Returns the inverse of a symbolic square matrix as `adjugate / determinant`, or `None`.
 *  The 1x1 case is special-cased (a 0x0 minor is not a valid [[_Matrix]]).
 *  Returns `None` when the matrix is non-square or above `MaxSymbolicDim`.
 */
private def symbolicInverse(m: _Matrix): Option[_Matrix] =
  if m.rows != m.cols || m.rows > MaxSymbolicDim then None
  else if m.rows == 1 then Some(_Matrix(1, 1, Vector(Ratio(_Number(1), m(0, 0)))))
  else
    val d = symbolicDet(m)
    val elems =
      for i <- 0 until m.rows; j <- 0 until m.cols yield
        val cof  = symbolicDet(minorOf(m, j, i))
        val signed = if (i + j) % 2 == 0 then cof else productOf(_Number(-1), cof)
        Ratio(signed, d)
    Some(_Matrix(m.rows, m.cols, elems.toVector))


/** Shared product reduction used by both [[MatProduct]] and [[MatScale]].
 *
 *  Parse-time dispatch cannot always distinguish scalars from matrices (a bare
 *  variable may be bound to a `_MatrixValue` only at eval time), so both nodes
 *  route through here: matrix-matrix multiplies, a `_Number` on either side
 *  scales, and anything else stays symbolic as a [[MatProduct]].
 *
 *  @param ra     the already-evaluated left operand
 *  @param rb     the already-evaluated right operand
 *  @param orElse the fallback symbolic node when no rule fires
 *  @return the reduced result, or `Left(orElse)` on dimension mismatch / unknown types
 */
private def reduceProduct(ra: Either[_Expression, _Value], rb: Either[_Expression, _Value],
                          orElse: _Expression): Either[_Expression, _Value] =
  (ra, rb) match
    case (Right(x: _MatrixValue), Right(y: _MatrixValue)) =>
      if x.cols == y.rows then x.multiply(y).guarded(orElse) else Left(orElse)
    case (Right(_Number(s)), Right(mv: _MatrixValue)) => mv.scale(s).guarded(orElse)
    case (Right(mv: _MatrixValue), Right(_Number(s))) => mv.scale(s).guarded(orElse)
    case _ => (asLiteral(ra), asLiteral(rb)) match
      case (Some(x), Some(y)) if x.cols == y.rows =>
        val elems =
          for i <- 0 until x.rows; j <- 0 until y.cols yield
            (0 until x.cols).map(k => productOf(x(i, k), y(k, j))).reduce(sumOf)
        collapse(_Matrix(x.rows, y.cols, elems.toVector), orElse)
      case (Some(_), Some(_)) => Left(orElse)   // dimension mismatch
      case (Some(lit), None) if isScalarValue(rb) =>
        collapse(_Matrix(lit.rows, lit.cols, lit.elems.map(productOf(_, rb.toExpression))), orElse)
      case (None, Some(lit)) if isScalarValue(ra) =>
        collapse(_Matrix(lit.rows, lit.cols, lit.elems.map(productOf(ra.toExpression, _))), orElse)
      case _ => Left(MatProduct(ra.toExpression, rb.toExpression))


/** Scale-relative tolerance for the Jordan invertibility test. */
private val JordanRelTol = 1e-8

/** Returns the product of the Euclidean norms of the columns of `m` (Hadamard bound factor).
 *
 *  Used as the scale reference in [[isJordanInvertible]]: dividing `|det(m)|` by this
 *  value gives a scale-independent ratio in `[0, 1]`.
 *
 *  @param m the matrix whose column norm product to compute
 *  @return product of `||col_j||_2` over all columns `j`
 */
private def columnNormProduct(m: _MatrixValue): Double =
  (0 until m.cols).foldLeft(1.0) { (prod, j) =>
    prod * math.sqrt((0 until m.rows).foldLeft(0.0)((s, i) => s + m(i, j) * m(i, j)))
  }

/** Scale-relative invertibility test for the Jordan eigenvector matrix.
 *
 *  Tests `|det(v)| > JordanRelTol * product-of-column-norms` rather than an absolute
 *  threshold, making the test independent of the eigenvectors' overall magnitude.
 *  An absolute `|det| < tol` test would wrongly flag a well-conditioned but
 *  small-magnitude `V` as singular (and pass a badly scaled one).
 *
 *  @param v the candidate eigenvector matrix (must be square)
 *  @return `true` when `v` passes the scale-relative invertibility test
 */
private[matrix] def isJordanInvertible(v: _MatrixValue): Boolean =
  v.determinant match
    case Some(det) => math.abs(det) > JordanRelTol * columnNormProduct(v)
    case None      => false   // non-square -- cannot be an invertible P

/** Builds the `(V, D)` pair from `_MatrixValue.spectralDecompose` output.
 *
 *  `V` is `n x n` with column `j` = eigenvector `j` (column-major layout stored
 *  row-major).  `D` is `n x n` diagonal with eigenvalue `j` on the diagonal.
 *  Both use [[_Matrix]] so they can hold `_Complex` elements.
 *
 *  @param mv the square dense matrix to decompose
 *  @return `Some((V, D))` on success; `None` when `spectralDecompose` fails
 */
private def buildVD(mv: _MatrixValue): Option[(_Matrix, _Matrix)] =
  mv.spectralDecompose.map { case (cols, eigs) =>
    val n = mv.rows
    // V[i,j] = cols(j)(i)
    val vElems = for i <- 0 until n; j <- 0 until n yield cols(j)(i)
    val vMat   = _Matrix(n, n, vElems.toVector)
    // D[i,j] = eigs(i) when i==j else _Number(0)
    val dElems = for i <- 0 until n; j <- 0 until n yield
      if i == j then eigs(i): _Expression else _Number(0.0)
    val dMat   = _Matrix(n, n, dElems.toVector)
    (vMat, dMat)
  }


/** Element-wise matrix addition: `(A + B)_ij = A_ij + B_ij`.
 *
 *  Marked `_ElementWise` because addition is linear: `d/dx (A + B) = dA/dx + dB/dx`.
 *  [[MatProduct]] and [[MatScale]] are NOT marked -- they need product rules.
 *
 *  @param a left matrix operand
 *  @param b right matrix operand
 */
case class MatSum(a: _Expression, b: _Expression) extends _MatrixOperation, _ElementWise:
  override def toString: String = s"($a + $b)"
  override def children: List[_Expression] = List(a, b)
  override def rebuild(c: List[_Expression]): _Expression = MatSum(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    (a.eval(env), b.eval(env)) match
      case (Right(x: _MatrixValue), Right(y: _MatrixValue)) =>
        if x.rows == y.rows && x.cols == y.cols then x.add(y).guarded(this) else Left(this)
      case (ra, rb) => (asLiteral(ra), asLiteral(rb)) match
        case (Some(x), Some(y)) if x.rows == y.rows && x.cols == y.cols =>
          collapse(_Matrix(x.rows, x.cols, x.elems.zip(y.elems).map((p, q) => sumOf(p, q))), this)
        case (Some(_), Some(_)) => Left(this)   // dimension mismatch
        case _                  => Left(MatSum(ra.toExpression, rb.toExpression))


/** Matrix product: `(A * B)_ij = sum_k A_ik * B_kj`, for `A: r x n` and `B: n x c`.
 *
 *  A `_Number` operand scales instead of multiplying -- the parser builds
 *  `MatProduct(M, y)` for `M * y` with a non-literal `y`, because `y` may be
 *  either a scalar or a matrix at eval time (variable bound to `_MatrixValue`).
 *  Both cases are resolved by [[reduceProduct]] at eval time.
 *
 *  @param a left operand (matrix or scalar)
 *  @param b right operand (matrix or scalar)
 */
case class MatProduct(a: _Expression, b: _Expression) extends _MatrixOperation:
  override def toString: String = s"($a * $b)"
  override def children: List[_Expression] = List(a, b)
  override def rebuild(c: List[_Expression]): _Expression = MatProduct(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    reduceProduct(a.eval(env), b.eval(env), this)


/** Scalar multiple: `(k * A)_ij = k * A_ij`, where `k` is any scalar expression.
 *
 *  When `k` turns out to be matrix-valued at eval time (a variable bound to a
 *  `_MatrixValue`, or a scalar `Product` that reduced to one), the node is really
 *  a matrix product `k * m` and is reduced as such via [[reduceProduct]], preserving
 *  operand order (matrix products are not commutative in general).
 *
 *  @param k the scalar multiplier
 *  @param m the matrix operand
 */
case class MatScale(k: _Expression, m: _Expression) extends _MatrixOperation:
  override def toString: String = s"($k * $m)"
  override def children: List[_Expression] = List(k, m)
  override def rebuild(c: List[_Expression]): _Expression = MatScale(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    (k.eval(env), m.eval(env)) match
      case (Right(_Number(s)), Right(mv: _MatrixValue)) => mv.scale(s).guarded(this)
      case (rk, rm) if asLiteral(rk).isDefined          => reduceProduct(rk, rm, this)
      case (rk, rm) => asLiteral(rm) match
        case Some(lit) =>
          collapse(_Matrix(lit.rows, lit.cols, lit.elems.map(productOf(rk.toExpression, _))), this)
        case None => Left(MatScale(rk.toExpression, rm.toExpression))


/** Matrix transpose: `(A^T)_ij = A_ji`.
 *
 *  Marked `_ElementWise` because transposition is linear and element-independent:
 *  `d/dx (A^T) = (dA/dx)^T`.
 *
 *  @param m the matrix to transpose
 */
case class Transpose(m: _Expression) extends _MatrixOperation, _ElementWise:
  override def toString: String = s"transpose($m)"
  override def children: List[_Expression] = List(m)
  override def rebuild(c: List[_Expression]): _Expression = Transpose(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    m.eval(env) match
      case Right(mv: _MatrixValue) => Right(mv.transpose)
      case r => asLiteral(r) match
        case Some(lit) =>
          val elems = for j <- 0 until lit.cols; i <- 0 until lit.rows yield lit(i, j)
          collapse(_Matrix(lit.cols, lit.rows, elems.toVector), this)
        case None => Left(Transpose(r.toExpression))


/** Determinant of a matrix: `det(A)` -- a SCALAR result.
 *
 *  Deliberately does NOT extend `_MatrixOperation` (and `isMatrixShaped` must not
 *  match it), so `2 * det(A)` builds a scalar `Product`, not a matrix scale --
 *  the same design as [[_MatrixIndex]].
 *
 *  `eval` reduces the operand: a dense value uses the O(n^3) `_MatrixValue.determinant`
 *  kernel; a symbolic square matrix expands by cofactors (capped at `MaxSymbolicDim`);
 *  non-square or over-cap operands stay symbolic.
 *
 *  @param m the matrix expression whose determinant to compute
 */
case class Determinant(m: _Expression) extends _Expression:
  override def toString: String = s"det($m)"
  override def children: List[_Expression] = List(m)
  override def rebuild(c: List[_Expression]): _Expression = Determinant(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    m.eval(env) match
      case Right(mv: _MatrixValue) =>
        mv.determinant.map(d => Right(_Number(d))).getOrElse(Left(this))
      case r => asLiteral(r) match
        // Exact entries go to Gaussian elimination, not the cofactor expansion: O(n^3)
        // rather than O(n!), so this path carries no dimension cap (issue 4.L slice B).
        case Some(lit) if lit.rows == lit.cols && exactCells(lit).isDefined =>
          exactCells(lit).map(c => Right(exactDeterminant(c, lit.rows, env.rationalPolicy)))
            .getOrElse(Left(this))
        case Some(lit) if lit.rows == lit.cols && lit.rows <= MaxSymbolicDim =>
          symbolicDet(lit).eval(env)
        case Some(_) => Left(this)   // non-square or over the cofactor cap
        case None    => Left(Determinant(r.toExpression))


/** Matrix inverse: `inv(A)`.
 *
 *  Result is a matrix, so this extends `_MatrixOperation` and `isMatrixShaped`
 *  matches it.  `eval` reduces the operand: a dense value uses the Gauss-Jordan
 *  `_MatrixValue.inverse` kernel (`None` -> singular or non-square -> stays symbolic,
 *  like `x/0` in scalar division); a symbolic square matrix builds adjugate/det
 *  (capped at `MaxSymbolicDim`) and reduces element-wise.
 *
 *  @param m the matrix expression to invert
 */
case class Inverse(m: _Expression) extends _MatrixOperation:
  override def toString: String = s"inv($m)"
  override def children: List[_Expression] = List(m)
  override def rebuild(c: List[_Expression]): _Expression = Inverse(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    m.eval(env) match
      case Right(mv: _MatrixValue) =>
        mv.inverse.map(_.guarded(this)).getOrElse(Left(this))
      case r => asLiteral(r) match
        // Exact entries: Gauss-Jordan over the rationals, uncapped, singular -> symbolic
        // (the same rule the dense kernel follows).
        case Some(lit) if lit.rows == lit.cols && exactCells(lit).isDefined =>
          exactCells(lit).flatMap(c => exactInverse(c, lit.rows, env.rationalPolicy)) match
            case Some(inv) => Left(_Matrix(lit.rows, lit.cols, inv.map(x => x: _Expression)))
            case None      => Left(Inverse(r.toExpression))
        case Some(lit) => symbolicInverse(lit) match
          case Some(inv) => inv.eval(env)
          case None      => Left(Inverse(r.toExpression))
        case None => Left(Inverse(r.toExpression))


/** Identity matrix: `eye(n)` -- an `n x n` matrix with `1`s on the diagonal.
 *
 *  Extends `_MatrixOperation` so `isMatrixShaped` picks it up and the parser routes
 *  `eye(3) + M` to [[MatSum]] rather than `Sum`.  Non-integer, non-positive, or
 *  over-`MaxDenseDim` arguments stay symbolic.
 *
 *  @param n the dimension expression (must reduce to a positive integer at most `MaxDenseDim`)
 */
case class IdentityMatrix(n: _Expression) extends _MatrixOperation:
  override def toString: String = s"eye($n)"
  override def children: List[_Expression] = List(n)
  override def rebuild(c: List[_Expression]): _Expression = IdentityMatrix(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    n.eval(env) match
      case Right(_Number(d)) if validDenseDim(d) =>
        val sz   = d.toInt
        val data = Array.tabulate(sz * sz)(i => if i / sz == i % sz then 1.0 else 0.0)
        Right(_MatrixValue(sz, sz, data))
      case Left(expr) => Left(IdentityMatrix(expr))
      case _          => Left(this)   // non-integer, non-positive, or over MaxDenseDim


/** Zero matrix: `zeros(rows, cols)` -- a `rows x cols` matrix of zeros.
 *
 *  `zeros(n)` in the grammar is shorthand for `zeros(n, n)` (square zero matrix);
 *  the parser handles the one-argument form.  Non-integer, non-positive, or
 *  over-`MaxDenseDim` arguments stay symbolic.
 *
 *  @param nRows number-of-rows expression
 *  @param nCols number-of-columns expression
 */
case class ZeroMatrix(nRows: _Expression, nCols: _Expression) extends _MatrixOperation:
  override def toString: String = s"zeros($nRows, $nCols)"
  override def children: List[_Expression] = List(nRows, nCols)
  override def rebuild(c: List[_Expression]): _Expression = ZeroMatrix(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    (nRows.eval(env), nCols.eval(env)) match
      case (Right(_Number(r)), Right(_Number(c))) if validDenseDim(r) && validDenseDim(c) =>
        Right(_MatrixValue(r.toInt, c.toInt, Array.fill(r.toInt * c.toInt)(0.0)))
      case (rr, rc) => Left(ZeroMatrix(rr.toExpression, rc.toExpression))


/** LU decomposition: `lu(A)` -- returns `[[L, U, P]]` where `P * A = L * U`.
 *
 *  `L` is unit lower triangular, `U` is upper triangular, `P` is the permutation
 *  matrix.  Evaluates when `A` reduces to a dense `_MatrixValue`; stays symbolic
 *  otherwise.  Singular or non-square matrices give no result (node stays symbolic).
 *
 *  The result is `Left(_Matrix(1, 3, ...))` because a matrix of matrices is not a
 *  `_Value`; individual factors are accessible via `at(result, 1, k)`.
 *
 *  @param m the matrix expression to decompose
 */
/** The operand of a decomposition as a **dense** matrix, demoting exact entries.
 *
 *  `lu`, `qr`, `eigen`, `eig` and `jordan` are iterative `Double` algorithms — QR iteration,
 *  Gram–Schmidt — so they cannot be exact whatever their input.  Since issue 4.L slice B an
 *  exactly-written matrix stays a symbolic `_Matrix` rather than collapsing, which would
 *  otherwise make every one of them silently stop working in exact mode.  Demoting here
 *  keeps the feature and is honest about what these kernels can deliver.
 *
 *  @param r the evaluated operand
 *  @return the dense matrix, or `None` when the operand is not a matrix of real numbers
 */
private def denseOperand(r: Either[_Expression, _Value]): Option[_MatrixValue] = r match
  case Right(mv: _MatrixValue) => Some(mv)
  case Left(lit: _Matrix)      => denseOf(lit)
  case _                       => None


case class _LUDecomposition(m: _Expression) extends _Expression:
  override def toString: String = s"lu($m)"
  override def children: List[_Expression] = List(m)
  override def rebuild(c: List[_Expression]): _Expression = _LUDecomposition(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    m.eval(env) match
      case r0 if denseOperand(r0).isDefined =>
        denseOperand(r0).flatMap(_.luDecompose) match
          case Some((l, u, p)) => Left(_Matrix(1, 3, Vector(l, u, p)))
          case None            => Left(this)
      case Left(expr) => Left(_LUDecomposition(expr))
      case _          => Left(this)


/** QR decomposition: `qr(A)` -- returns `[[Q, R]]` where `A = Q * R`.
 *
 *  `Q` is orthogonal (`m x n`, orthonormal columns), `R` is upper triangular
 *  (`n x n`).  Requires `rows >= cols`; rank-deficient matrices give no result.
 *  Evaluates when `A` reduces to a dense `_MatrixValue`; stays symbolic otherwise.
 *
 *  The result is `Left(_Matrix(1, 2, ...))` because a matrix of matrices is not a
 *  `_Value`; individual factors are accessible via `at(result, 1, k)`.
 *
 *  @param m the matrix expression to decompose
 */
case class _QRDecomposition(m: _Expression) extends _Expression:
  override def toString: String = s"qr($m)"
  override def children: List[_Expression] = List(m)
  override def rebuild(c: List[_Expression]): _Expression = _QRDecomposition(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    m.eval(env) match
      case r0 if denseOperand(r0).isDefined =>
        denseOperand(r0).flatMap(_.qrDecompose) match
          case Some((q, r)) => Left(_Matrix(1, 2, Vector(q, r)))
          case None         => Left(this)
      case Left(expr) => Left(_QRDecomposition(expr))
      case _          => Left(this)


/** Eigenvalue decomposition: `eigen(A)` -- returns `[[lambda_1, lambda_2, ..., lambda_n]]`.
 *
 *  Eigenvalues are `_Number` for real results and `_Complex` for complex conjugate
 *  pairs.  Evaluates when `A` reduces to a square dense `_MatrixValue` via the QR
 *  iteration kernel; stays symbolic for non-square operands or when the iteration
 *  does not converge.
 *
 *  Does NOT extend `_MatrixOperation` because the result is not a matrix of `Double`s
 *  (eigenvalues can be complex): it stays as `Left(_Matrix(1, n, ...))` rather than
 *  collapsing to a single `_MatrixValue`.
 *
 *  @param m the square matrix expression whose eigenvalues to compute
 */
case class _EigenDecomposition(m: _Expression) extends _Expression:
  override def toString: String = s"eigen($m)"
  override def children: List[_Expression] = List(m)
  override def rebuild(c: List[_Expression]): _Expression = _EigenDecomposition(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    m.eval(env) match
      case r0 if denseOperand(r0).isDefined =>
        denseOperand(r0).flatMap(_.eigenDecompose) match
          case Some(eigs) => Left(_Matrix(1, eigs.size, eigs))
          case None       => Left(this)
      case Left(expr) => Left(_EigenDecomposition(expr))
      case _          => Left(this)


/** Eigenvalue/eigenvector decomposition: `eig(A)` -- returns `[[V, D]]` where `A * V = V * D`.
 *
 *  `V` columns are right eigenvectors; `D` is diagonal with the eigenvalues.
 *  Evaluates when `A` reduces to a dense square `_MatrixValue`; stays symbolic
 *  for non-convergent or defective-detected cases (when the eigenvector matrix `V`
 *  fails [[isJordanInvertible]]).
 *
 *  The result is `Left(_Matrix(1, 2, ...))` because a matrix of matrices is not a
 *  `_Value`; individual components are accessible via `at(result, 1, k)`.
 *
 *  @param m the square matrix expression to decompose
 */
case class _EigDecomposition(m: _Expression) extends _Expression:
  override def toString: String = s"eig($m)"
  override def children: List[_Expression] = List(m)
  override def rebuild(c: List[_Expression]): _Expression = _EigDecomposition(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    m.eval(env) match
      case r0 if denseOperand(r0).isDefined =>
        denseOperand(r0).flatMap(buildVD) match
          case Some((v, d)) => Left(_Matrix(1, 2, Vector(v, d)))
          case None         => Left(this)
      case Left(expr) => Left(_EigDecomposition(expr))
      case _          => Left(this)


/** Jordan decomposition: `jordan(A)` -- returns `[[P, J]]` where `A = P * J * P^(-1)`.
 *
 *  For diagonalizable matrices `J` is diagonal (identical to `D` in `eig(A)`); `P = V`.
 *  Non-diagonalizable matrices (defective / repeated eigenvalues where `V` is singular,
 *  detected via [[isJordanInvertible]]) stay symbolic -- the test is conservative but safe.
 *
 *  The result is `Left(_Matrix(1, 2, ...))` because a matrix of matrices is not a `_Value`.
 *
 *  @param m the square matrix expression to decompose
 */
case class _JordanDecomposition(m: _Expression) extends _Expression:
  override def toString: String = s"jordan($m)"
  override def children: List[_Expression] = List(m)
  override def rebuild(c: List[_Expression]): _Expression = _JordanDecomposition(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    m.eval(env) match
      case r0 if denseOperand(r0).isDefined =>
        denseOperand(r0).flatMap(mv => buildVD(mv).flatMap { case (v, d) =>
          // Verify that P is invertible (non-singular) so P*J*P^-1 = A is valid.
          // For a dense V (all-real eigenvectors) we test invertibility scale-relatively
          // (isJordanInvertible); for symbolic V (complex entries) we accept it -- the
          // caller can verify via at(...).
          val allReal = v.elems.forall(_.isInstanceOf[_Number])
          if allReal then
            val vData  = v.elems.collect { case _Number(d) => d }
            val vDense = _MatrixValue(mv.rows, mv.cols, vData.toArray)
            if isJordanInvertible(vDense) then Some((v, d)) else None   // defective: stay symbolic
          else Some((v, d))   // complex eigenvectors -- accept
        }) match
          case Some((p, j)) => Left(_Matrix(1, 2, Vector(p, j)))
          case None         => Left(this)
      case Left(expr) => Left(_JordanDecomposition(expr))
      case _          => Left(this)


/** Element access: `at(A, i, j)` returns the element at 1-based row `i`, column `j`.
 *
 *  Does NOT extend `_MatrixOperation` because the result is a scalar, not a matrix --
 *  `isMatrixShaped` must not match it or the REPL would dispatch it as a matrix op.
 *  When the matrix is still symbolic but the indices are concrete, the element is
 *  extracted directly from the [[_Matrix]] literal so that `at([[x, 2]], 1, 2) -> 2.0`
 *  without requiring the whole matrix to be dense.
 *
 *  @param matrix the matrix expression to index into
 *  @param row    the row index expression (1-based)
 *  @param col    the column index expression (1-based)
 */
case class _MatrixIndex(matrix: _Expression, row: _Expression, col: _Expression) extends _Expression:
  override def toString: String = s"at($matrix, $row, $col)"
  override def children: List[_Expression] = List(matrix, row, col)
  override def rebuild(c: List[_Expression]): _Expression = _MatrixIndex(c.head, c(1), c(2))

  override def eval(env: Environment): Either[_Expression, _Value] =
    val rm = matrix.eval(env)
    val rr = row.eval(env)
    val rc = col.eval(env)
    (rr, rc) match
      case (Right(_Number(r)), Right(_Number(c))) =>
        val i = r.round.toInt - 1   // 1-based -> 0-based
        val j = c.round.toInt - 1
        rm match
          case Right(mv: _MatrixValue) if i >= 0 && i < mv.rows && j >= 0 && j < mv.cols =>
            Right(_Number(mv(i, j)))
          case Left(m: _Matrix) if i >= 0 && i < m.rows && j >= 0 && j < m.cols =>
            m(i, j).eval(env)
          case _ => Left(this)
      case _ => Left(_MatrixIndex(rm.toExpression, rr.toExpression, rc.toExpression))

package it.grypho.scala.leonardo
package matrix

import core.*
import scalar.{Sum, Product, Ratio}


// Matrix-valued operations, mirroring scalar._Operation for the matrix domain.
// Evaluation is two-path:
//   - both operands reduce to dense _MatrixValues → dense kernels (fast path);
//   - otherwise, operands that are matrix literals combine element-wise: pairs of
//     already-reduced elements fold directly (sumOf/productOf below — no second
//     eval pass over the tree), and the combined matrix collapses to a dense value
//     when every element folded, or stays symbolic otherwise.
// Dimension mismatches and non-finite results are domain errors and stay symbolic,
// exactly like x/0 in scalar.Ratio.

trait _MatrixOperation extends _Expression


// A matrix-literal view of an eval result: a symbolic _Matrix as-is, a dense
// _MatrixValue re-inflated to _Number elements, anything else None.
private def asLiteral(r: Either[_Expression, _Value]): Option[_Matrix] = r match
  case Left(m: _Matrix)       => Some(m)
  case Right(v: _MatrixValue) => Some(_Matrix.fromValue(v))
  case _                      => None

// Element combiners over ALREADY-REDUCED operands: fold concrete pairs immediately
// instead of building a Sum/Product node and evaluating it again (issue 2.2).
// productOf keeps Product.eval's zero short-circuit so 0 * symbolic still folds.
private def sumOf(a: _Expression, b: _Expression): _Expression = (a, b) match
  case (_Number(x), _Number(y)) => _Number(x + y)
  case _                        => Sum(a, b)

private def productOf(a: _Expression, b: _Expression): _Expression = (a, b) match
  case (_Number(0.0), _) | (_, _Number(0.0)) => _Number(0)
  case (_Number(x), _Number(y))              => _Number(x * y)
  case _                                     => Product(a, b)

// Collapse a combined literal without re-evaluating: all elements folded to numbers
// → dense value (finite-guarded); otherwise stay symbolic as-is.
private def collapse(m: _Matrix, orElse: _Expression): Either[_Expression, _Value] =
  val numbers = m.elems.collect { case _Number(d) => d }
  if numbers.size == m.elems.size then _MatrixValue(m.rows, m.cols, numbers.toArray).guarded(orElse)
  else Left(m)


// Symbolic determinant/inverse support (the numeric counterparts are the dense
// _MatrixValue.determinant / .inverse kernels). Cofactor (Laplace) expansion grows
// as n!, so it is capped: above this dimension a symbolic matrix stays symbolic and
// only its concrete form (via the dense kernels) is reducible.
private val MaxSymbolicDim = 6

// The (rows-1)×(cols-1) matrix with row `si` and column `sj` removed.
private def minorOf(m: _Matrix, si: Int, sj: Int): _Matrix =
  val elems =
    for i <- 0 until m.rows if i != si; j <- 0 until m.cols if j != sj yield m(i, j)
  _Matrix(m.rows - 1, m.cols - 1, elems.toVector)

// Determinant of a symbolic matrix by Laplace expansion along the first row, folding
// concrete element pairs via sumOf/productOf (so a numeric literal still collapses to
// a single _Number). Assumes m is square; the 1×1 base returns its only element.
private def symbolicDet(m: _Matrix): _Expression =
  if m.rows == 1 then m(0, 0)
  else
    (0 until m.cols).map { j =>
      val term = productOf(m(0, j), symbolicDet(minorOf(m, 0, j)))
      if j % 2 == 0 then term else productOf(_Number(-1), term)
    }.reduce(sumOf)

// Inverse of a symbolic square matrix as adjugate / determinant: entry (i, j) is the
// (j, i) cofactor over the determinant. 1×1 is special-cased ([[1/a]]) because a 0×0
// minor is not a valid _Matrix. None when non-square or above the cofactor cap.
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


// Element-wise sum: (A + B)ᵢⱼ = Aᵢⱼ + Bᵢⱼ.
// _ElementWise: sum is linear, so derive/simplify/expand/integrate distribute over
// the operands (d/dx (A + B) = dA/dx + dB/dx). MatProduct and MatScale are NOT
// marked — they need product rules.
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


// Shared product reduction over two already-reduced operands. Parse-time dispatch
// cannot always tell scalars from matrices (a bare variable may be bound to a
// _MatrixValue only at eval time — issue 1.2), so both MatProduct and MatScale
// route through here: matrix·matrix multiplies (dense or element-wise), a _Number
// on either side scales, anything else stays symbolic as a MatProduct.
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
      case (Some(lit), None) if rb.exists(_.isInstanceOf[_Number]) =>
        collapse(_Matrix(lit.rows, lit.cols, lit.elems.map(productOf(_, rb.toExpression))), orElse)
      case (None, Some(lit)) if ra.exists(_.isInstanceOf[_Number]) =>
        collapse(_Matrix(lit.rows, lit.cols, lit.elems.map(productOf(ra.toExpression, _))), orElse)
      case _ => Left(MatProduct(ra.toExpression, rb.toExpression))


// Matrix product: (A · B)ᵢⱼ = Σₖ Aᵢₖ · Bₖⱼ, for A: r×n and B: n×c.
// A _Number operand scales instead — the parser builds MatProduct(M, y) for "M * y"
// with a non-literal y, because y may be either a scalar or a matrix at eval time.
case class MatProduct(a: _Expression, b: _Expression) extends _MatrixOperation:
  override def toString: String = s"($a * $b)"
  override def children: List[_Expression] = List(a, b)
  override def rebuild(c: List[_Expression]): _Expression = MatProduct(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    reduceProduct(a.eval(env), b.eval(env), this)


// Scalar multiple: (k · A)ᵢⱼ = k · Aᵢⱼ, where k is any scalar expression.
// When k turns out to be matrix-valued at eval time (a variable bound to a
// _MatrixValue, or a scalar Product that reduced to one — issue 1.2), the node is
// really a matrix product k · m and is reduced as such, preserving operand order.
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


// Transpose: (Aᵀ)ᵢⱼ = Aⱼᵢ.
// _ElementWise: transposition is linear and element-independent, so per-element
// algorithms commute with it (d/dx Aᵀ = (dA/dx)ᵀ).
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


// Determinant: det(A) — a SCALAR result, so it deliberately does NOT extend
// _MatrixOperation (and isMatrixShaped must not match it), exactly like _MatrixIndex:
// "2 * det(A)" must build a scalar Product, not a matrix scale. eval reduces the
// operand: a dense value uses the O(n³) kernel; a symbolic square matrix expands by
// cofactors (capped at MaxSymbolicDim) and evaluates the resulting scalar expression;
// non-square / non-matrix operands stay symbolic.
case class Determinant(m: _Expression) extends _Expression:
  override def toString: String = s"det($m)"
  override def children: List[_Expression] = List(m)
  override def rebuild(c: List[_Expression]): _Expression = Determinant(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    m.eval(env) match
      case Right(mv: _MatrixValue) =>
        mv.determinant.map(d => Right(_Number(d))).getOrElse(Left(this))
      case r => asLiteral(r) match
        case Some(lit) if lit.rows == lit.cols && lit.rows <= MaxSymbolicDim =>
          symbolicDet(lit).eval(env)
        case Some(_) => Left(this)   // non-square or over the cofactor cap
        case None    => Left(Determinant(r.toExpression))


// Matrix inverse: inv(A). Result IS a matrix, so it extends _MatrixOperation (and
// isMatrixShaped matches it). eval reduces the operand: a dense value uses the
// Gauss–Jordan kernel (None → singular/non-square → stays symbolic, like x/0); a
// symbolic square matrix builds adjugate/det (capped) and reduces element-wise.
case class Inverse(m: _Expression) extends _MatrixOperation:
  override def toString: String = s"inv($m)"
  override def children: List[_Expression] = List(m)
  override def rebuild(c: List[_Expression]): _Expression = Inverse(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    m.eval(env) match
      case Right(mv: _MatrixValue) =>
        mv.inverse.map(_.guarded(this)).getOrElse(Left(this))
      case r => asLiteral(r) match
        case Some(lit) => symbolicInverse(lit) match
          case Some(inv) => inv.eval(env)
          case None      => Left(Inverse(r.toExpression))
        case None => Left(Inverse(r.toExpression))


// Identity matrix: eye(n) → n×n matrix with 1s on the diagonal and 0s elsewhere.
// Extends _MatrixOperation so isMatrixShaped matches it and the parser routes
// "eye(3) + M" to MatSum rather than Sum.
case class IdentityMatrix(n: _Expression) extends _MatrixOperation:
  override def toString: String = s"eye($n)"
  override def children: List[_Expression] = List(n)
  override def rebuild(c: List[_Expression]): _Expression = IdentityMatrix(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    n.eval(env) match
      case Right(_Number(d)) if d > 0 && d == d.toLong =>
        val sz   = d.toInt
        val data = Array.tabulate(sz * sz)(i => if i / sz == i % sz then 1.0 else 0.0)
        Right(_MatrixValue(sz, sz, data))
      case Left(expr) => Left(IdentityMatrix(expr))
      case _          => Left(this)   // non-integer or non-positive dimension


// Zero matrix: zeros(rows, cols) → rows×cols matrix of 0.0.
// zeros(n) is shorthand for zeros(n, n) (square zero matrix).
case class ZeroMatrix(nRows: _Expression, nCols: _Expression) extends _MatrixOperation:
  override def toString: String = s"zeros($nRows, $nCols)"
  override def children: List[_Expression] = List(nRows, nCols)
  override def rebuild(c: List[_Expression]): _Expression = ZeroMatrix(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    (nRows.eval(env), nCols.eval(env)) match
      case (Right(_Number(r)), Right(_Number(c)))
          if r > 0 && c > 0 && r == r.toLong && c == c.toLong =>
        Right(_MatrixValue(r.toInt, c.toInt, Array.fill(r.toInt * c.toInt)(0.0)))
      case (rr, rc) => Left(ZeroMatrix(rr.toExpression, rc.toExpression))


// LU decomposition: lu(A) — returns [[L, U, P]] where P·A = L·U.
// L is unit lower triangular, U is upper triangular, P is the permutation matrix.
// Evaluates when A reduces to a dense _MatrixValue; stays symbolic otherwise.
// Singular or non-square matrices give no result (the node stays symbolic).
// Result is Left(_Matrix(1, 3, …)) because a matrix of matrices is not a _Value.
case class _LUDecomposition(m: _Expression) extends _Expression:
  override def toString: String = s"lu($m)"
  override def children: List[_Expression] = List(m)
  override def rebuild(c: List[_Expression]): _Expression = _LUDecomposition(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    m.eval(env) match
      case Right(mv: _MatrixValue) =>
        mv.luDecompose match
          case Some((l, u, p)) => Left(_Matrix(1, 3, Vector(l, u, p)))
          case None            => Left(this)
      case Left(expr) => Left(_LUDecomposition(expr))
      case _          => Left(this)


// QR decomposition: qr(A) — returns [[Q, R]] where A = Q·R.
// Q is orthogonal (m×n, orthonormal columns), R is upper triangular (n×n).
// Requires rows ≥ cols; stays symbolic when A is not yet a dense value.
// Rank-deficient matrices give no result (the node stays symbolic).
case class _QRDecomposition(m: _Expression) extends _Expression:
  override def toString: String = s"qr($m)"
  override def children: List[_Expression] = List(m)
  override def rebuild(c: List[_Expression]): _Expression = _QRDecomposition(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    m.eval(env) match
      case Right(mv: _MatrixValue) =>
        mv.qrDecompose match
          case Some((q, r)) => Left(_Matrix(1, 2, Vector(q, r)))
          case None         => Left(this)
      case Left(expr) => Left(_QRDecomposition(expr))
      case _          => Left(this)


// Eigenvalue decomposition: eigen(A) → [[λ₁, λ₂, …, λₙ]] (1×n row of eigenvalues).
// Eigenvalues are _Number for real results and _Complex for complex conjugate pairs.
// Evaluates when A reduces to a square dense _MatrixValue via the QR iteration kernel;
// stays symbolic for non-square operands or when the iteration doesn't converge.
// Does NOT extend _MatrixOperation because the result is not a matrix of Doubles
// (eigenvalues can be complex) — it stays as Left(_Matrix(1, n, …)) rather than
// collapsing to a single _MatrixValue.
case class _EigenDecomposition(m: _Expression) extends _Expression:
  override def toString: String = s"eigen($m)"
  override def children: List[_Expression] = List(m)
  override def rebuild(c: List[_Expression]): _Expression = _EigenDecomposition(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    m.eval(env) match
      case Right(mv: _MatrixValue) =>
        mv.eigenDecompose match
          case Some(eigs) => Left(_Matrix(1, eigs.size, eigs))
          case None       => Left(this)
      case Left(expr) => Left(_EigenDecomposition(expr))
      case _          => Left(this)


// Shared helper: build the symbolic V and D matrices from spectralDecompose output.
// V is n×n with column j = eigenvector j (in column-major layout stored row-major).
// D is n×n diagonal with eigenvalue j on the diagonal.
// Both use _Matrix so they can hold _Complex elements.
private def buildVD(mv: _MatrixValue): Option[(_Matrix, _Matrix)] =
  mv.spectralDecompose.map { case (cols, eigs) =>
    val n = mv.rows
    // V[i,j] = cols(j)(i)
    val vElems = for i <- 0 until n; j <- 0 until n yield cols(j)(i)
    val vMat   = _Matrix(n, n, vElems.toVector)
    // D[i,j] = eigs(i) when i==j else _Number(0)
    val dElems = for i <- 0 until n; j <- 0 until n yield
      if i == j then (eigs(i): _Expression) else _Number(0.0)
    val dMat   = _Matrix(n, n, dElems.toVector)
    (vMat, dMat)
  }


// Eigenvalue/eigenvector decomposition: eig(A) → [[V, D]] where A·V = V·D.
// V columns are right eigenvectors; D is diagonal with eigenvalues.
// Evaluates when A reduces to a dense square _MatrixValue; stays symbolic otherwise.
// Non-convergent or defective-detected cases also stay symbolic.
case class _EigDecomposition(m: _Expression) extends _Expression:
  override def toString: String = s"eig($m)"
  override def children: List[_Expression] = List(m)
  override def rebuild(c: List[_Expression]): _Expression = _EigDecomposition(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    m.eval(env) match
      case Right(mv: _MatrixValue) =>
        buildVD(mv) match
          case Some((v, d)) => Left(_Matrix(1, 2, Vector(v, d)))
          case None         => Left(this)
      case Left(expr) => Left(_EigDecomposition(expr))
      case _          => Left(this)


// Jordan decomposition: jordan(A) → [[P, J]] where A = P·J·P⁻¹.
// For diagonalizable matrices J is diagonal (identical to D in eig(A)); P = V.
// Non-diagonalizable matrices (defective / repeated eigenvalues where V is singular)
// stay symbolic — detecting that numerically is conservative but safe.
case class _JordanDecomposition(m: _Expression) extends _Expression:
  override def toString: String = s"jordan($m)"
  override def children: List[_Expression] = List(m)
  override def rebuild(c: List[_Expression]): _Expression = _JordanDecomposition(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    m.eval(env) match
      case Right(mv: _MatrixValue) =>
        buildVD(mv).flatMap { case (v, d) =>
          // Verify that P is invertible (non-singular) so P·J·P⁻¹ = A is valid.
          // For a dense V (all-real eigenvectors) we check det ≠ 0; for symbolic V
          // (complex entries) we accept it — the caller can verify via at(…).
          val allReal = v.elems.forall(_.isInstanceOf[_Number])
          if allReal then
            val vData = v.elems.collect { case _Number(d) => d }
            val vDense = _MatrixValue(mv.rows, mv.cols, vData.toArray)
            vDense.determinant match
              case Some(det) if math.abs(det) > 1e-10 => Some((v, d))
              case _                                    => None   // defective: stay symbolic
          else Some((v, d))   // complex eigenvectors — accept
        } match
          case Some((p, j)) => Left(_Matrix(1, 2, Vector(p, j)))
          case None         => Left(this)
      case Left(expr) => Left(_JordanDecomposition(expr))
      case _          => Left(this)


// Element access: at(A, i, j) returns the element at row i, column j (1-based).
// Does NOT extend _MatrixOperation because the result is a scalar, not a matrix —
// isMatrixShaped must not match it or the REPL would try to dispatch it as a matrix op.
// When the matrix is still symbolic but the indices are concrete we extract the
// element directly from the _Matrix literal so that at([[x, 2]], 1, 2) → 2.0
// without needing the whole matrix to be dense.
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
        val i = r.round.toInt - 1   // 1-based → 0-based
        val j = c.round.toInt - 1
        rm match
          case Right(mv: _MatrixValue) if i >= 0 && i < mv.rows && j >= 0 && j < mv.cols =>
            Right(_Number(mv(i, j)))
          case Left(m: _Matrix) if i >= 0 && i < m.rows && j >= 0 && j < m.cols =>
            m(i, j).eval(env)
          case _ => Left(this)
      case _ => Left(_MatrixIndex(rm.toExpression, rr.toExpression, rc.toExpression))

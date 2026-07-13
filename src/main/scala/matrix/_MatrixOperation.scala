package it.grypho.scala.leonardo
package matrix

import core.*
import scalar.{Sum, Product}


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


// Matrix product: (A · B)ᵢⱼ = Σₖ Aᵢₖ · Bₖⱼ, for A: r×n and B: n×c.
case class MatProduct(a: _Expression, b: _Expression) extends _MatrixOperation:
  override def toString: String = s"($a * $b)"
  override def children: List[_Expression] = List(a, b)
  override def rebuild(c: List[_Expression]): _Expression = MatProduct(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    (a.eval(env), b.eval(env)) match
      case (Right(x: _MatrixValue), Right(y: _MatrixValue)) =>
        if x.cols == y.rows then x.multiply(y).guarded(this) else Left(this)
      case (ra, rb) => (asLiteral(ra), asLiteral(rb)) match
        case (Some(x), Some(y)) if x.cols == y.rows =>
          val elems =
            for i <- 0 until x.rows; j <- 0 until y.cols yield
              (0 until x.cols).map(k => productOf(x(i, k), y(k, j))).reduce(sumOf)
          collapse(_Matrix(x.rows, y.cols, elems.toVector), this)
        case (Some(_), Some(_)) => Left(this)   // dimension mismatch
        case _                  => Left(MatProduct(ra.toExpression, rb.toExpression))


// Scalar multiple: (k · A)ᵢⱼ = k · Aᵢⱼ, where k is any scalar expression.
case class MatScale(k: _Expression, m: _Expression) extends _MatrixOperation:
  override def toString: String = s"($k * $m)"
  override def children: List[_Expression] = List(k, m)
  override def rebuild(c: List[_Expression]): _Expression = MatScale(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    (k.eval(env), m.eval(env)) match
      case (Right(_Number(s)), Right(mv: _MatrixValue)) => mv.scale(s).guarded(this)
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


// Element access: at(A, i, j) returns the element at row i, column j (1-based).
// Does NOT extend _MatrixOperation because the result is a scalar, not a matrix —
// isMatrixShaped must not match it or the REPL would try to dispatch it as a matrix op.
// When the matrix is still symbolic but the indices are concrete we extract the
// element directly from the _Matrix literal so that at([[x, 2]], 1, 2) → 2.0
// without needing the whole matrix to be dense.
case class _MatrixIndex(matrix: _Expression, row: _Expression, col: _Expression) extends _Expression:
  override def toString: String = s"at($matrix, $row, $col)"
  override def children: List[_Expression] = List(matrix, row, col)
  override def rebuild(c: List[_Expression]): _Expression = _MatrixIndex(c(0), c(1), c(2))

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

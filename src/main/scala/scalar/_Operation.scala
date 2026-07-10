package it.grypho.scala.leonardo
package scalar

import core.*
import scala.math.pow


trait _Operation extends _Expression


// The scalar operations also evaluate CONCRETE matrix operands: _MatrixValue and its
// dense kernels live in core, so no scalar → matrix dependency is created. This is
// what makes "M + N" / "2 * M" work through the ordinary + and * nodes when the
// operands reduce to matrix values (e.g. REPL bindings, matrix literals). Symbolic
// element-wise combination remains the job of the matrix package's own nodes
// (MatSum/MatProduct/MatScale) — a scalar op over a partially-symbolic matrix stays
// symbolic. Dimension mismatches and non-finite results stay symbolic, like x/0.
private def guardedMatrix(result: _MatrixValue, orElse: _Expression): Either[_Expression, _Value] =
  if result.isFinite then Right(result) else Left(orElse)


case class Sum(a: _Expression, b: _Expression) extends _Operation:
  override def toString: String = s"($a + $b)"
  override def children: List[_Expression] = List(a, b)
  override def rebuild(c: List[_Expression]): _Expression = Sum(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    (a.eval(env), b.eval(env)) match
      case (Right(_Number(x)), Right(_Number(y))) => Right(_Number(x + y))
      case (Right(x: _MatrixValue), Right(y: _MatrixValue)) =>
        if x.rows == y.rows && x.cols == y.cols then guardedMatrix(x.add(y), this) else Left(this)
      case (ra, rb)                               => Left(Sum(ra.toExpression, rb.toExpression))


case class Product(a: _Expression, b: _Expression) extends _Operation:
  override def toString: String = s"($a * $b)"
  override def children: List[_Expression] = List(a, b)
  override def rebuild(c: List[_Expression]): _Expression = Product(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    (a.eval(env), b.eval(env)) match
      // matrix cases precede the zero short-circuit: 0 * M is the zero MATRIX
      case (Right(_Number(k)), Right(m: _MatrixValue))      => guardedMatrix(m.scale(k), this)
      case (Right(m: _MatrixValue), Right(_Number(k)))      => guardedMatrix(m.scale(k), this)
      case (Right(x: _MatrixValue), Right(y: _MatrixValue)) =>
        if x.cols == y.rows then guardedMatrix(x.multiply(y), this) else Left(this)
      // zero short-circuit for everything scalar or unreducible: 0 * e folds to 0
      case (Right(_Number(0.0)), _) | (_, Right(_Number(0.0))) => Right(_Number(0.0))
      case (Right(_Number(x)), Right(_Number(y)))           => Right(_Number(x * y))
      case (ra, rb) => Left(Product(ra.toExpression, rb.toExpression))


case class Ratio(a: _Expression, b: _Expression) extends _Operation:
  override def toString: String = s"($a / $b)"
  override def children: List[_Expression] = List(a, b)
  override def rebuild(c: List[_Expression]): _Expression = Ratio(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    (a.eval(env), b.eval(env)) match
      case (Right(_Number(x)), Right(_Number(y))) =>
        val r = x / y
        // x/0 and 0/0 are domain errors: stay symbolic instead of propagating ±Infinity/NaN
        if r.isNaN || r.isInfinite then Left(this) else Right(_Number(r))
      // M / k = (1/k) · M; k = 0 yields non-finite elements → the guard stays symbolic
      case (Right(m: _MatrixValue), Right(_Number(k))) => guardedMatrix(m.scale(1.0 / k), this)
      case (ra, rb)                               => Left(Ratio(ra.toExpression, rb.toExpression))


// Exponentiation is a binary operation like the others; the parser keeps it
// right-associative (2 ^ 3 ^ 2 = 2 ^ (3 ^ 2)).
case class Power(base: _Expression, exp: _Expression) extends _Operation:
  override def toString: String = s"($base ^ $exp)"
  override def children: List[_Expression] = List(base, exp)
  override def rebuild(c: List[_Expression]): _Expression = Power(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    (base.eval(env), exp.eval(env)) match
      case (Right(_Number(b)), Right(_Number(e))) =>
        val r = pow(b, e)
        // 0^negative and (negative)^fractional are domain errors: stay symbolic
        // instead of propagating ±Infinity/NaN
        if r.isNaN || r.isInfinite then Left(this) else Right(_Number(r))
      case (rb, re)                               => Left(Power(rb.toExpression, re.toExpression))

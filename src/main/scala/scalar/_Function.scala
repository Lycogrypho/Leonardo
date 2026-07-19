package it.grypho.scala.leonardo
package scalar

import core.*
import scala.math.{exp, log, log10, sin, cos, tan, asin, acos, atan}


abstract class _Function extends _Expression:
  // Element-wise application of a real scalar function over a dense matrix argument
  // (issue 1.3: sin(A), exp(A), … distribute over the elements of A). _MatrixValue
  // is a core type, so this stays within the core → scalar layering — no matrix
  // package dependency. Reuses _MatrixValue.guarded so an out-of-domain element
  // (e.g. ln of a negative entry → NaN) leaves the whole node symbolic rather than
  // emitting a matrix with holes — the dense carrier cannot hold the complex value a
  // per-element scalar fallback would produce.
  protected def mapMatrix(mv: _MatrixValue, f: Double => Double): Either[_Expression, _Value] =
    _MatrixValue(mv.rows, mv.cols, mv.toVector.map(f).toArray).guarded(this)


case class Exp(e: _Expression) extends _Function:
  override def toString: String = s"exp($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Exp(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(_Number(x)) => Right(_Number(exp(x)))
      case Right(mv: _MatrixValue) => mapMatrix(mv, exp)
      case Right(c: _Complex) => _Complex.expc(c).map(Right(_)).getOrElse(Left(Exp(c)))
      case other             => Left(Exp(other.toExpression))


// Natural logarithm (base e). ln(x) in the grammar.
case class Ln(e: _Expression) extends _Function:
  override def toString: String = s"ln($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Ln(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(_Number(x)) =>
        val r = log(x)
        // ln of a negative number is now the principal complex value ln|x| + iπ;
        // ln(0) is still undefined (_Complex.logc returns None) → stays symbolic.
        if r.isNaN || r.isInfinite then
          _Complex.logc(_Number(x)).map(Right(_)).getOrElse(Left(this))
        else Right(_Number(r))
      case Right(mv: _MatrixValue) => mapMatrix(mv, log)
      case Right(c: _Complex) => _Complex.logc(c).map(Right(_)).getOrElse(Left(Ln(c)))
      case other             => Left(Ln(other.toExpression))


// General-base logarithm. log(x, b) in the grammar; log(x) redirects to log(x, 10).
// Evaluated via the change-of-base formula: log_b(x) = ln(x) / ln(b).
// Complex closure is inherited: both ln(x) and ln(b) use _Complex.logc, so
// log(-1, 10) = iπ / ln(10) and log(i, e) = iπ/2 are computed correctly.
// Undefined forms (log(0, b), log(x, 1), log(x, 0)) stay symbolic.
case class LogBase(e: _Expression, base: _Expression) extends _Function:
  override def toString: String = s"log($e, $base)"
  override def children: List[_Expression] = List(e, base)
  override def rebuild(c: List[_Expression]): _Expression = LogBase(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    (e.eval(env), base.eval(env)) match
      // Matrix argument with a real base: distribute log_b element-wise (issue 1.3).
      case (Right(mv: _MatrixValue), Right(_Number(b))) =>
        val lb = log(b)
        if lb.isNaN || lb.isInfinite then Left(this) else mapMatrix(mv, x => log(x) / lb)
      case (Right(ev: _Value), Right(bv: _Value)) =>
        (_Complex.logc(ev), _Complex.logc(bv)) match
          case (Some(le), Some(lb)) =>
            _Complex.div(le, lb).map(Right(_)).getOrElse(Left(this))
          case _ => Left(this)
      case (re, rb) => Left(LogBase(re.toExpression, rb.toExpression))


case class Sin(e: _Expression) extends _Function:
  override def toString: String = s"sin($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Sin(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(_Number(x)) => Right(_Number(sin(x)))
      case Right(mv: _MatrixValue) => mapMatrix(mv, sin)
      case Right(c: _Complex) => _Complex.sinc(c).map(Right(_)).getOrElse(Left(Sin(c)))
      case other             => Left(Sin(other.toExpression))


case class Cos(e: _Expression) extends _Function:
  override def toString: String = s"cos($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Cos(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(_Number(x)) => Right(_Number(cos(x)))
      case Right(mv: _MatrixValue) => mapMatrix(mv, cos)
      case Right(c: _Complex) => _Complex.cosc(c).map(Right(_)).getOrElse(Left(Cos(c)))
      case other             => Left(Cos(other.toExpression))


case class Tg(e: _Expression) extends _Function:
  override def toString: String = s"tan($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Tg(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(_Number(x)) =>
        val r = tan(x)
        if r.isNaN || r.isInfinite then Left(this) else Right(_Number(r))
      case Right(mv: _MatrixValue) => mapMatrix(mv, tan)
      case Right(c: _Complex) => _Complex.tanc(c).map(Right(_)).getOrElse(Left(Tg(c)))
      case other             => Left(Tg(other.toExpression))


case class Asin(e: _Expression) extends _Function:
  override def toString: String = s"asin($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Asin(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(_Number(x)) =>
        val r = asin(x)
        if r.isNaN then Left(this) else Right(_Number(r))
      case Right(mv: _MatrixValue) => mapMatrix(mv, asin)
      case other             => Left(Asin(other.toExpression))


case class Acos(e: _Expression) extends _Function:
  override def toString: String = s"acos($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Acos(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(_Number(x)) =>
        val r = acos(x)
        if r.isNaN then Left(this) else Right(_Number(r))
      case Right(mv: _MatrixValue) => mapMatrix(mv, acos)
      case other             => Left(Acos(other.toExpression))


case class Atan(e: _Expression) extends _Function:
  override def toString: String = s"atan($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Atan(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(_Number(x)) => Right(_Number(atan(x)))
      case Right(mv: _MatrixValue) => mapMatrix(mv, atan)
      case other             => Left(Atan(other.toExpression))

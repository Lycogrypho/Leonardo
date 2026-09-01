package it.grypho.scala.leonardo
package scalar

import core.*
import spire.math.Real
import spire.algebra.Trig
import scala.math.{exp, log, log10, sin, cos, tan, asin, acos, atan}


/** Base class for unary elementary functions (`exp`, `ln`, `sin`, `cos`, etc.).
 *
 *  Concrete subclasses share two helpers for distributing the function over a
 *  matrix argument without importing the `matrix` package: `_MatrixValue` is a
 *  `core` type, so these helpers stay within the `core -> scalar` layering.
 */
abstract class _Function extends _Expression:
  /** Applies a real scalar function element-wise over a dense matrix argument.
   *
   *  An out-of-domain element (e.g. `ln` of a negative entry yields NaN) leaves the
   *  whole node symbolic: the dense `_MatrixValue` carrier cannot hold the complex
   *  fallback value that a per-element scalar result would produce.
   *
   *  @param mv the dense matrix to map over
   *  @param f  the scalar function to apply to each element
   *  @return `Right(_MatrixValue)` when all elements are finite, `Left(this)` otherwise
   */
  protected def mapMatrix(mv: _MatrixValue, f: Double => Double): Either[_Expression, _Value] =
    _MatrixValue(mv.rows, mv.cols, mv.toVector.map(f).toArray).guarded(this)

  /** The exact non-negative integer an argument denotes, if it denotes one.
   *
   *  The gate on every exact path in the factorial family: those functions are only closed
   *  over the rationals at non-negative integers, and everywhere else (`Gamma(0.5)`,
   *  `fact(-1)`) the analytic kernels remain the right answer.
   *
   *  @param r the exact argument
   *  @return `Some(n)` when `r` is a non-negative integer, `None` otherwise
   */
  protected def exactIntArg(r: _Rational): Option[BigInt] =
    r.toBigIntExact.filter(_.signum >= 0)

  /** This function as an arbitrary-precision kernel, when it has one (issue 4.N).
   *
   *  Returning `None` means "not defined here", and covers both cases: a function with no
   *  `Real` counterpart at all, and an argument outside the real domain — `ln` of a negative,
   *  `asin` beyond `±1`.  The domain is checked **explicitly** per function rather than by
   *  computing in `Double` first and seeing whether it came back finite, because that oracle
   *  gets `exp(1000)` exactly wrong: `Double` overflows to an infinity there while the true
   *  value is a perfectly finite 435-digit number, which is the acceptance case.
   *
   *  Out-of-domain arguments fall through to [[viaDouble]], which owns the complex fallback.
   */
  protected def exactKernel: Option[_Rational => Option[Real]] = None

  /** Evaluates this function on an exact argument, at the working precision.
   *
   *  Uses the `Double` kernel when the working precision is inside `Double`'s reliable range:
   *  there is nothing to gain there and roughly 110× to lose, since a `Real` transcendental
   *  costs about 0.6 ms against 0.006 ms.  The two agree to within the precision either
   *  claims, so which one ran is not observable — only how long it took.
   *
   *  @param r   the exact argument
   *  @param env supplies the working precision
   *  @return the result at the working precision, or the `Double` path's answer
   */
  protected def viaExact(r: _Rational, env: Environment): Either[_Expression, _Value] =
    def fallback = viaDouble(List(_Number(r.toDouble)), env)
    if env.workingPrecision <= _Rational.DoubleReliableDigits then fallback
    else
      exactKernel.flatMap(_(r)).flatMap(fromReal(_, env.workingPrecision)) match
        case Some(v) => Right(v)
        case None    => fallback

  /** Evaluates this function on exact arguments by way of the `Double` kernel, lifting the
   *  result back into the exact tier.
   *
   *  No transcendental function is closed over the rationals — `exp(1)` and `sin(1/3)` are
   *  irrational — so the exact tier's contract for them is "compute, then re-approximate to
   *  the working precision", and this is that step.
   *
   *  Implemented by rebuilding the node with `_Number` arguments and re-evaluating, rather
   *  than by taking a `Double => Double` kernel: that reuses each node's *own* logic,
   *  including its domain handling and its complex fallback, instead of duplicating a
   *  function reference at fourteen call sites and getting one of them wrong.  The
   *  rebuilt node hits its `_Number` case, so there is no recursion.
   *
   *  A complex or symbolic result passes through untouched — a complex value is inexact, so
   *  float contagion is already the right answer for it.
   *
   *  Since issue 4.N this is the *fallback*: [[viaExact]] is what the transcendentals reach
   *  first, and it comes back here for an out-of-domain argument or a low working precision.
   *
   *  @param args the same children, with every exact argument replaced by its `Double`
   *  @param env  supplies the working precision
   *  @return the result lifted back to a `_Rational`, or whatever non-real result came out
   */
  protected def viaDouble(args: List[_Expression], env: Environment): Either[_Expression, _Value] =
    rebuild(args).eval(env) match
      case Right(_Number(d)) =>
        _Rational.fromApproximation(d, env.workingPrecision).map(Right(_)).getOrElse(Left(this))
      case other => other

  /** Applies this single-argument function element-wise over a symbolic matrix argument.
   *
   *  Rebuilds the matrix with this function wrapped around each cell and re-evaluates.
   *  Unlike [[mapMatrix]], each cell degrades independently -- an out-of-domain cell
   *  becomes its own complex or symbolic result rather than dropping the whole matrix.
   *
   *  @param m   the symbolic matrix (`_MatrixShaped`) whose cells are not yet concrete
   *  @param env the evaluation environment
   *  @return the element-wise result, with numeric cells folded and symbolic cells kept
   */
  protected def mapMatrixExpr(m: _MatrixShaped, env: Environment): Either[_Expression, _Value] =
    m.rebuild(m.children.map(el => rebuild(List(el)))).eval(env)


/** The natural exponential function `exp(e)`.
 *
 *  Accepts `_Number`, `_MatrixValue` (element-wise), and `_Complex` arguments.
 *  Delegates to `_Complex.expc` for complex inputs.
 *
 *  @param e the exponent expression
 */
case class Exp(e: _Expression) extends _Function:
  override def toString: String = s"exp($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Exp(c.head)

  /** exp is defined on the whole real line. */
  override protected def exactKernel: Option[_Rational => Option[Real]] =
    Some(r => Some(Real.exp(toReal(r))))

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x)) => Right(_Number(exp(x)))
      case Right(mv: _MatrixValue) => mapMatrix(mv, exp)
      case Right(c: _Complex) => _Complex.expc(c).map(Right(_)).getOrElse(Left(Exp(c)))
      case Left(m: _MatrixShaped) => mapMatrixExpr(m, env)
      case other             => Left(Exp(other.toExpression))


/** The natural logarithm `ln(e)`.
 *
 *  For a negative real argument returns the principal complex value `ln|x| + i*pi`
 *  via `_Complex.logc`; `ln(0)` stays symbolic.  Accepts `_MatrixValue` (element-wise)
 *  and `_Complex` arguments.
 *
 *  @param e the argument expression
 */
case class Ln(e: _Expression) extends _Function:
  override def toString: String = s"ln($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Ln(c.head)

  /** ln needs a strictly positive argument; a negative one is the complex case, which
   *  `viaDouble` already owns, and ln(0) is undefined either way.
   */
  override protected def exactKernel: Option[_Rational => Option[Real]] =
    Some(r => if r.signum > 0 then Some(Real.log(toReal(r))) else None)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x)) =>
        val r = log(x)
        // ln of a negative number is now the principal complex value ln|x| + i*pi;
        // ln(0) is still undefined (_Complex.logc returns None) -> stays symbolic.
        if r.isNaN || r.isInfinite then
          _Complex.logc(_Number(x)).map(Right(_)).getOrElse(Left(this))
        else Right(_Number(r))
      case Right(mv: _MatrixValue) => mapMatrix(mv, log)
      case Right(c: _Complex) => _Complex.logc(c).map(Right(_)).getOrElse(Left(Ln(c)))
      case Left(m: _MatrixShaped) => mapMatrixExpr(m, env)
      case other             => Left(Ln(other.toExpression))


/** The general-base logarithm `log(e, base)`.
 *
 *  `log(x)` in the grammar is syntactic sugar for `LogBase(x, 10)`; `log(x, b)` for
 *  `LogBase(x, b)`.  Evaluated via the change-of-base formula `ln(x) / ln(base)`.
 *  Complex closure: `log(-1, 10) = i*pi / ln(10)` and similar are computed correctly.
 *  Undefined forms (`log(0, b)`, `log(x, 1)`, `log(x, 0)`) stay symbolic.
 *
 *  @param e    the argument expression
 *  @param base the logarithm base
 */
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
      // Symbolic matrix argument (any base): distribute log_base element-wise.
      case (Left(m: _MatrixShaped), rb) =>
        m.rebuild(m.children.map(el => LogBase(el, rb.toExpression))).eval(env)
      // Exact tier: two cases, not a guard -- the first catches an exact argument under
      // any base, so by the second the argument is known inexact and only the base is exact.
      case (Right(re: _Rational), Right(bv: _Value)) => viaDouble(List(_Number(re.toDouble), bv), env)
      case (Right(ev: _Value), Right(rb: _Rational)) => viaDouble(List(ev, _Number(rb.toDouble)), env)
      case (Right(ev: _Value), Right(bv: _Value)) =>
        (_Complex.logc(ev), _Complex.logc(bv)) match
          case (Some(le), Some(lb)) =>
            _Complex.div(le, lb).map(Right(_)).getOrElse(Left(this))
          case _ => Left(this)
      case (re, rb) => Left(LogBase(re.toExpression, rb.toExpression))


/** The sine function `sin(e)`.
 *
 *  Accepts `_Number`, `_MatrixValue` (element-wise), and `_Complex` arguments.
 *
 *  @param e the argument expression
 */
case class Sin(e: _Expression) extends _Function:
  override def toString: String = s"sin($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Sin(c.head)

  /** sin is defined on the whole real line. */
  override protected def exactKernel: Option[_Rational => Option[Real]] =
    Some(r => Some(Real.sin(toReal(r))))

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x)) => Right(_Number(sin(x)))
      case Right(mv: _MatrixValue) => mapMatrix(mv, sin)
      case Right(c: _Complex) => _Complex.sinc(c).map(Right(_)).getOrElse(Left(Sin(c)))
      case Left(m: _MatrixShaped) => mapMatrixExpr(m, env)
      case other             => Left(Sin(other.toExpression))


/** The cosine function `cos(e)`.
 *
 *  Accepts `_Number`, `_MatrixValue` (element-wise), and `_Complex` arguments.
 *
 *  @param e the argument expression
 */
case class Cos(e: _Expression) extends _Function:
  override def toString: String = s"cos($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Cos(c.head)

  /** cos is defined on the whole real line. */
  override protected def exactKernel: Option[_Rational => Option[Real]] =
    Some(r => Some(Real.cos(toReal(r))))

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x)) => Right(_Number(cos(x)))
      case Right(mv: _MatrixValue) => mapMatrix(mv, cos)
      case Right(c: _Complex) => _Complex.cosc(c).map(Right(_)).getOrElse(Left(Cos(c)))
      case Left(m: _MatrixShaped) => mapMatrixExpr(m, env)
      case other             => Left(Cos(other.toExpression))


/** The tangent function `tan(e)` (also parsed as `tg(e)`).
 *
 *  A real result that is NaN or infinite (at multiples of `pi/2`) stays symbolic.
 *  Accepts `_MatrixValue` (element-wise) and `_Complex` arguments.
 *
 *  @param e the argument expression
 */
case class Tg(e: _Expression) extends _Function:
  override def toString: String = s"tan($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Tg(c.head)

  /** 	an has poles at odd multiples of pi/2, but those are irrational and the argument
   *  here is exactly rational, so it can never land on one.
   */
  override protected def exactKernel: Option[_Rational => Option[Real]] =
    Some(r => Some(Real.tan(toReal(r))))

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x)) =>
        val r = tan(x)
        if r.isNaN || r.isInfinite then Left(this) else Right(_Number(r))
      case Right(mv: _MatrixValue) => mapMatrix(mv, tan)
      case Right(c: _Complex) => _Complex.tanc(c).map(Right(_)).getOrElse(Left(Tg(c)))
      case Left(m: _MatrixShaped) => mapMatrixExpr(m, env)
      case other             => Left(Tg(other.toExpression))


/** The arcsine function `asin(e)`.
 *
 *  Out-of-domain real arguments (`|x| > 1`) stay symbolic.  Complex inputs stay symbolic
 *  (the "Asin convention" -- no complex extension is applied).
 *  Accepts `_MatrixValue` (element-wise).
 *
 *  @param e the argument expression
 */
case class Asin(e: _Expression) extends _Function:
  override def toString: String = s"asin($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Asin(c.head)

  /** `asin` is real only on `[-1, 1]`. */
  override protected def exactKernel: Option[_Rational => Option[Real]] =
    Some(r => if r.abs <= _Rational.One then Some(Real.asin(toReal(r))) else None)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x)) =>
        val r = asin(x)
        if r.isNaN then Left(this) else Right(_Number(r))
      case Right(mv: _MatrixValue) => mapMatrix(mv, asin)
      case Left(m: _MatrixShaped) => mapMatrixExpr(m, env)
      case other             => Left(Asin(other.toExpression))


/** The arccosine function `acos(e)`.
 *
 *  Out-of-domain real arguments (`|x| > 1`) stay symbolic.  Complex inputs stay symbolic.
 *  Accepts `_MatrixValue` (element-wise).
 *
 *  @param e the argument expression
 */
case class Acos(e: _Expression) extends _Function:
  override def toString: String = s"acos($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Acos(c.head)

  /** `acos` is real only on `[-1, 1]`. */
  override protected def exactKernel: Option[_Rational => Option[Real]] =
    Some(r => if r.abs <= _Rational.One then Some(Real.acos(toReal(r))) else None)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x)) =>
        val r = acos(x)
        if r.isNaN then Left(this) else Right(_Number(r))
      case Right(mv: _MatrixValue) => mapMatrix(mv, acos)
      case Left(m: _MatrixShaped) => mapMatrixExpr(m, env)
      case other             => Left(Acos(other.toExpression))


/** The arctangent function `atan(e)`.
 *
 *  Defined on all real inputs; accepts `_MatrixValue` (element-wise).
 *  Complex inputs stay symbolic.
 *
 *  @param e the argument expression
 */
case class Atan(e: _Expression) extends _Function:
  override def toString: String = s"atan($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Atan(c.head)

  /** `atan` is defined on the whole real line. */
  override protected def exactKernel: Option[_Rational => Option[Real]] =
    Some(r => Some(Real.atan(toReal(r))))

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x)) => Right(_Number(atan(x)))
      case Right(mv: _MatrixValue) => mapMatrix(mv, atan)
      case Left(m: _MatrixShaped) => mapMatrixExpr(m, env)
      case other             => Left(Atan(other.toExpression))


// ── hyperbolic and reciprocal-trigonometric functions (issue 3.9) ────────────
// The `Double` inverse-hyperbolic kernels, which `scala.math` does not provide, written
// through their logarithmic closed forms.  Each is real only on its own domain, so an
// out-of-domain argument returns NaN and the node stays symbolic.

/** `asinh(x) = ln(x + sqrt(x² + 1))`, defined on the whole real line. */
private def asinhD(x: Double): Double = math.log(x + math.sqrt(x * x + 1.0))

/** `acosh(x) = ln(x + sqrt(x² − 1))`, real only for `x ≥ 1` (NaN below). */
private def acoshD(x: Double): Double = math.log(x + math.sqrt(x * x - 1.0))

/** `atanh(x) = ½·ln((1 + x)/(1 − x))`, real only for `|x| < 1`. */
private def atanhD(x: Double): Double = 0.5 * math.log((1.0 + x) / (1.0 - x))


/** The hyperbolic sine `sinh(e)`.  Entire; accepts `_MatrixValue` and `_Complex` arguments. */
case class Sinh(e: _Expression) extends _Function:
  override def toString: String = s"sinh($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Sinh(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x))       => Right(_Number(math.sinh(x)))
      case Right(mv: _MatrixValue) => mapMatrix(mv, math.sinh)
      case Right(c: _Complex)      => _Complex.sinhc(c).map(Right(_)).getOrElse(Left(Sinh(c)))
      case Left(m: _MatrixShaped)  => mapMatrixExpr(m, env)
      case other                   => Left(Sinh(other.toExpression))


/** The hyperbolic cosine `cosh(e)`.  Entire; accepts `_MatrixValue` and `_Complex` arguments. */
case class Cosh(e: _Expression) extends _Function:
  override def toString: String = s"cosh($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Cosh(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x))       => Right(_Number(math.cosh(x)))
      case Right(mv: _MatrixValue) => mapMatrix(mv, math.cosh)
      case Right(c: _Complex)      => _Complex.coshc(c).map(Right(_)).getOrElse(Left(Cosh(c)))
      case Left(m: _MatrixShaped)  => mapMatrixExpr(m, env)
      case other                   => Left(Cosh(other.toExpression))


/** The hyperbolic tangent `tanh(e)`.  Entire; accepts `_MatrixValue` and `_Complex` arguments. */
case class Tanh(e: _Expression) extends _Function:
  override def toString: String = s"tanh($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Tanh(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x))       => Right(_Number(math.tanh(x)))
      case Right(mv: _MatrixValue) => mapMatrix(mv, math.tanh)
      case Right(c: _Complex)      => _Complex.tanhc(c).map(Right(_)).getOrElse(Left(Tanh(c)))
      case Left(m: _MatrixShaped)  => mapMatrixExpr(m, env)
      case other                   => Left(Tanh(other.toExpression))


/** The inverse hyperbolic sine `asinh(e)`.  Defined on all reals; complex inputs stay symbolic
 *  (the `Asin` convention).  Accepts `_MatrixValue` (element-wise). */
case class Asinh(e: _Expression) extends _Function:
  override def toString: String = s"asinh($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Asinh(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x))       => Right(_Number(asinhD(x)))
      case Right(mv: _MatrixValue) => mapMatrix(mv, asinhD)
      case Left(m: _MatrixShaped)  => mapMatrixExpr(m, env)
      case other                   => Left(Asinh(other.toExpression))


/** The inverse hyperbolic cosine `acosh(e)`.  Real only for `e ≥ 1`; out-of-domain and complex
 *  inputs stay symbolic.  Accepts `_MatrixValue` (element-wise). */
case class Acosh(e: _Expression) extends _Function:
  override def toString: String = s"acosh($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Acosh(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x))       => val r = acoshD(x); if r.isNaN then Left(this) else Right(_Number(r))
      case Right(mv: _MatrixValue) => mapMatrix(mv, acoshD)
      case Left(m: _MatrixShaped)  => mapMatrixExpr(m, env)
      case other                   => Left(Acosh(other.toExpression))


/** The inverse hyperbolic tangent `atanh(e)`.  Real only for `|e| < 1`; out-of-domain and
 *  complex inputs stay symbolic.  Accepts `_MatrixValue` (element-wise). */
case class Atanh(e: _Expression) extends _Function:
  override def toString: String = s"atanh($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Atanh(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x))       => val r = atanhD(x); if r.isNaN || r.isInfinite then Left(this) else Right(_Number(r))
      case Right(mv: _MatrixValue) => mapMatrix(mv, atanhD)
      case Left(m: _MatrixShaped)  => mapMatrixExpr(m, env)
      case other                   => Left(Atanh(other.toExpression))


/** The secant `sec(e) = 1/cos(e)`.  Symbolic at the poles (`cos = 0`); accepts `_MatrixValue`
 *  and `_Complex` arguments. */
case class Sec(e: _Expression) extends _Function:
  override def toString: String = s"sec($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Sec(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x))       => val r = 1.0 / cos(x); if r.isNaN || r.isInfinite then Left(this) else Right(_Number(r))
      case Right(mv: _MatrixValue) => mapMatrix(mv, x => 1.0 / cos(x))
      case Right(c: _Complex)      => _Complex.secc(c).map(Right(_)).getOrElse(Left(Sec(c)))
      case Left(m: _MatrixShaped)  => mapMatrixExpr(m, env)
      case other                   => Left(Sec(other.toExpression))


/** The cosecant `csc(e) = 1/sin(e)`.  Symbolic at the poles (`sin = 0`); accepts `_MatrixValue`
 *  and `_Complex` arguments. */
case class Csc(e: _Expression) extends _Function:
  override def toString: String = s"csc($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Csc(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x))       => val r = 1.0 / sin(x); if r.isNaN || r.isInfinite then Left(this) else Right(_Number(r))
      case Right(mv: _MatrixValue) => mapMatrix(mv, x => 1.0 / sin(x))
      case Right(c: _Complex)      => _Complex.cscc(c).map(Right(_)).getOrElse(Left(Csc(c)))
      case Left(m: _MatrixShaped)  => mapMatrixExpr(m, env)
      case other                   => Left(Csc(other.toExpression))


/** The cotangent `cot(e) = cos(e)/sin(e)`.  Symbolic at the poles (`sin = 0`); accepts
 *  `_MatrixValue` and `_Complex` arguments. */
case class Cot(e: _Expression) extends _Function:
  override def toString: String = s"cot($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Cot(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x))       => val r = cos(x) / sin(x); if r.isNaN || r.isInfinite then Left(this) else Right(_Number(r))
      case Right(mv: _MatrixValue) => mapMatrix(mv, x => cos(x) / sin(x))
      case Right(c: _Complex)      => _Complex.cotc(c).map(Right(_)).getOrElse(Left(Cot(c)))
      case Left(m: _MatrixShaped)  => mapMatrixExpr(m, env)
      case other                   => Left(Cot(other.toExpression))


/** The hyperbolic secant `sech(e) = 1/cosh(e)`.  Entire (`cosh ≥ 1`); accepts `_MatrixValue`
 *  and `_Complex` arguments. */
case class Sech(e: _Expression) extends _Function:
  override def toString: String = s"sech($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Sech(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x))       => Right(_Number(1.0 / math.cosh(x)))
      case Right(mv: _MatrixValue) => mapMatrix(mv, x => 1.0 / math.cosh(x))
      case Right(c: _Complex)      => _Complex.sechc(c).map(Right(_)).getOrElse(Left(Sech(c)))
      case Left(m: _MatrixShaped)  => mapMatrixExpr(m, env)
      case other                   => Left(Sech(other.toExpression))


/** The hyperbolic cosecant `csch(e) = 1/sinh(e)`.  Symbolic at `e = 0`; accepts `_MatrixValue`
 *  and `_Complex` arguments. */
case class Csch(e: _Expression) extends _Function:
  override def toString: String = s"csch($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Csch(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x))       => val r = 1.0 / math.sinh(x); if r.isNaN || r.isInfinite then Left(this) else Right(_Number(r))
      case Right(mv: _MatrixValue) => mapMatrix(mv, x => 1.0 / math.sinh(x))
      case Right(c: _Complex)      => _Complex.cschc(c).map(Right(_)).getOrElse(Left(Csch(c)))
      case Left(m: _MatrixShaped)  => mapMatrixExpr(m, env)
      case other                   => Left(Csch(other.toExpression))


/** The hyperbolic cotangent `coth(e) = cosh(e)/sinh(e)`.  Symbolic at `e = 0`; accepts
 *  `_MatrixValue` and `_Complex` arguments. */
case class Coth(e: _Expression) extends _Function:
  override def toString: String = s"coth($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Coth(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x))       => val r = math.cosh(x) / math.sinh(x); if r.isNaN || r.isInfinite then Left(this) else Right(_Number(r))
      case Right(mv: _MatrixValue) => mapMatrix(mv, x => math.cosh(x) / math.sinh(x))
      case Right(c: _Complex)      => _Complex.cothc(c).map(Right(_)).getOrElse(Left(Coth(c)))
      case Left(m: _MatrixShaped)  => mapMatrixExpr(m, env)
      case other                   => Left(Coth(other.toExpression))


/** The factorial `fact(e)` = `e!`.
 *
 *  A non-negative integer argument uses the exact table; a non-integer argument is the
 *  analytic continuation `Gamma(e + 1)`, so `fact(0.5)` is `sqrt(pi)/2`.  Stays symbolic
 *  at the poles (negative integers), past the `170!` overflow bound, and on a complex
 *  argument (the `Asin` convention).  Distributes element-wise over a matrix argument
 *  like every other `_Function`.
 *
 *  Differentiating it needs the digamma function, which is not implemented, so
 *  `derive(fact(x), x)` stays symbolic.
 *
 *  @param e the argument expression
 */
case class Factorial(e: _Expression) extends _Function:
  override def toString: String = s"fact($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Factorial(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      // An exact non-negative integer argument is computed exactly and without the 170!
      // ceiling, which exists only because a Double overflows there (issue 4.L slice B).
      case Right(r: _Rational) if exactIntArg(r).isDefined =>
        exactIntArg(r).flatMap(factorialExact).map(n => Right(_Rational(n))).getOrElse(Left(this))
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x))       => factorialOf(x).map(r => Right(_Number(r))).getOrElse(Left(this))
      case Right(mv: _MatrixValue) => mapMatrix(mv, d => factorialOf(d).getOrElse(Double.NaN))
      case Left(m: _MatrixShaped)  => mapMatrixExpr(m, env)
      case other                   => Left(Factorial(other.toExpression))


/** The multifactorial `mfact(e, k)` = `e * (e-k) * (e-2k) * ...`.
 *
 *  `mfact(n, 1)` is the ordinary factorial and `mfact(n, 2)` the double factorial, which
 *  the grammar spells `dfact(n)` and desugars to `mfact(n, 2)` -- the same sugar
 *  relationship `log(x)` has with `LogBase(x, 10)`.  Defined for a non-negative integer
 *  argument and a positive integer step; anything else stays symbolic.
 *
 *  @param e the argument expression
 *  @param k the step expression
 */
case class MultiFactorial(e: _Expression, k: _Expression) extends _Function:
  override def toString: String = s"mfact($e, $k)"
  override def children: List[_Expression] = List(e, k)
  override def rebuild(c: List[_Expression]): _Expression = MultiFactorial(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    (e.eval(env), k.eval(env)) match
      // Exact tier first -- `_Number` is a widening extractor, so anything below it never
      // sees a `_Rational`.  Two cases rather than a guard: by the second, the first
      // argument is known inexact and only the second is exact.
      // Both arguments exact integers: compute exactly, no 170! ceiling.
      case (Right(rx: _Rational), Right(rk: _Rational))
        if exactIntArg(rx).isDefined && exactIntArg(rk).isDefined =>
        (for
          n <- exactIntArg(rx)
          s <- exactIntArg(rk)
          v <- multiFactorialExact(n, s)
        yield Right(_Rational(v))).getOrElse(Left(this))
      case (Right(rx: _Rational), Right(sv: _Value)) => viaDouble(List(_Number(rx.toDouble), sv), env)
      case (Right(xv: _Value), Right(rs: _Rational)) => viaDouble(List(xv, _Number(rs.toDouble)), env)
      case (Right(_Number(x)), Right(_Number(s))) =>
        multiFactorialOf(x, s).map(r => Right(_Number(r))).getOrElse(Left(this))
      case (ra, rk) => Left(MultiFactorial(ra.toExpression, rk.toExpression))


/** The gamma function `Gamma(e)`, the analytic continuation of the factorial.
 *
 *  Spelled with a capital `G` in the grammar on purpose: it keeps the lowercase `gamma`
 *  free as an ordinary variable name, which it very commonly is (Lorentz factor,
 *  Euler-Mascheroni constant, regression coefficients).  `Beta` follows the same rule;
 *  `lgamma` does not need it, since it is not a name anyone binds.
 *
 *  Stays symbolic at the poles `0, -1, -2, ...`, on overflow, and on complex arguments.
 *
 *  @param e the argument expression
 */
case class Gamma(e: _Expression) extends _Function:
  override def toString: String = s"Gamma($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Gamma(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      // Gamma(n) = (n-1)! at a positive integer, so the exact factorial covers it; every
      // other argument is genuinely transcendental and goes through Lanczos as before.
      case Right(r: _Rational) if exactIntArg(r).exists(_.signum > 0) =>
        exactIntArg(r).flatMap(n => factorialExact(n - 1)).map(n => Right(_Rational(n))).getOrElse(Left(this))
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x))       => gammaOf(x).map(r => Right(_Number(r))).getOrElse(Left(this))
      case Right(mv: _MatrixValue) => mapMatrix(mv, d => gammaOf(d).getOrElse(Double.NaN))
      // 4.O slice 4: Lanczos generalises to the complex plane, so Gamma no longer has to
      // give up on a complex argument the way Asin and friends still do.
      case Right(c: _Complex)      =>
        gammaComplex(c.re, c.im).map((r, i) => Right(_Complex.of(r, i))).getOrElse(Left(this))
      case Left(m: _MatrixShaped)  => mapMatrixExpr(m, env)
      case other                   => Left(Gamma(other.toExpression))


/** The log-gamma function `lgamma(e)` = `ln|Gamma(e)|`.
 *
 *  Stays finite well past the point where `Gamma` itself overflows, which is what it is
 *  for.  Lowercase because, unlike `Gamma`/`Beta`, it collides with nothing.
 *
 *  @param e the argument expression
 */
case class LogGamma(e: _Expression) extends _Function:
  override def toString: String = s"lgamma($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = LogGamma(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x))       => lgammaOf(x).map(r => Right(_Number(r))).getOrElse(Left(this))
      case Right(mv: _MatrixValue) => mapMatrix(mv, d => lgammaOf(d).getOrElse(Double.NaN))
      case Left(m: _MatrixShaped)  => mapMatrixExpr(m, env)
      case other                   => Left(LogGamma(other.toExpression))


/** The beta function `Beta(a, b)` = `Gamma(a)Gamma(b) / Gamma(a+b)`.
 *
 *  Capitalised for the same reason as [[Gamma]]: lowercase `beta` stays available as a
 *  variable.  Computed through `lgamma` for positive arguments so it survives large
 *  inputs; stays symbolic when any factor is undefined.
 *
 *  @param a the first argument
 *  @param b the second argument
 */
case class Beta(a: _Expression, b: _Expression) extends _Function:
  override def toString: String = s"Beta($a, $b)"
  override def children: List[_Expression] = List(a, b)
  override def rebuild(c: List[_Expression]): _Expression = Beta(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    (a.eval(env), b.eval(env)) match
      // Exact tier first -- see MultiFactorial.
      case (Right(rx: _Rational), Right(yv: _Value)) => viaDouble(List(_Number(rx.toDouble), yv), env)
      case (Right(xv: _Value), Right(ry: _Rational)) => viaDouble(List(xv, _Number(ry.toDouble)), env)
      case (Right(_Number(x)), Right(_Number(y))) =>
        betaOf(x, y).map(r => Right(_Number(r))).getOrElse(Left(this))
      case (ra, rb) => Left(Beta(ra.toExpression, rb.toExpression))


/** The error function `erf(e)`.
 *
 *  Like the rest of the 4.O family this has no [[_Function.exactKernel]] — spire supplies no
 *  special functions, so the kernel is `Double`-based and an exact argument falls back to
 *  `viaDouble`, capped at `_Rational.DoubleReliableDigits`.
 *
 *  @param e the argument expression
 */
case class Erf(e: _Expression) extends _Function:
  override def toString: String = s"erf($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Erf(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x))       => erfOf(x).map(v => Right(_Number(v))).getOrElse(Left(this))
      case Right(mv: _MatrixValue) => mapMatrix(mv, d => erfOf(d).getOrElse(Double.NaN))
      case Left(m: _MatrixShaped)  => mapMatrixExpr(m, env)
      case other                   => Left(Erf(other.toExpression))


/** The complementary error function `erfc(e) = 1 − erf(e)`.
 *
 *  Its own node rather than sugar for `1 - erf(x)`, because the kernel computes it without
 *  that subtraction and so keeps its accuracy far out in the tail.
 *
 *  @param e the argument expression
 */
case class Erfc(e: _Expression) extends _Function:
  override def toString: String = s"erfc($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Erfc(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x))       => erfcOf(x).map(v => Right(_Number(v))).getOrElse(Left(this))
      case Right(mv: _MatrixValue) => mapMatrix(mv, d => erfcOf(d).getOrElse(Double.NaN))
      case Left(m: _MatrixShaped)  => mapMatrixExpr(m, env)
      case other                   => Left(Erfc(other.toExpression))


/** The digamma function `digamma(e) = Γ'(e)/Γ(e)`.
 *
 *  Spelled lowercase, unlike `Gamma`/`Beta`: those are capitalised because `gamma` and
 *  `beta` are extremely common variable names, and `digamma` is not.
 *
 *  @param e the argument expression
 */
case class Digamma(e: _Expression) extends _Function:
  override def toString: String = s"digamma($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Digamma(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x))       => digammaOf(x).map(v => Right(_Number(v))).getOrElse(Left(this))
      case Right(mv: _MatrixValue) => mapMatrix(mv, d => digammaOf(d).getOrElse(Double.NaN))
      case Left(m: _MatrixShaped)  => mapMatrixExpr(m, env)
      case other                   => Left(Digamma(other.toExpression))


// ── Special integral functions (issue 3.15) ──────────────────────────────────
// The named antiderivatives of the classic non-elementary integrals, on the Gamma/Erf
// template: a symbolic node with a numeric kernel returning Option[Double], so an
// undefined point stays symbolic. `Si`/`Ci`/`Ei` are capitalised and reserved (the
// Gamma/Beta collision reasoning — they are plausible variable names); `li` keeps the
// C spelling; Fresnel uses the spelled `fresnelS`/`fresnelC` (bare `S`/`C` would be
// homoglyph-prone variable collisions).

/** The sine integral `Si(e) = ∫₀ᵉ sin(t)/t dt` — the antiderivative of `sin(v)/v`.
 *
 *  @param e the argument expression
 */
case class Si(e: _Expression) extends _Function:
  override def toString: String = s"Si($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Si(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x))       => siOf(x).map(v => Right(_Number(v))).getOrElse(Left(this))
      case Right(mv: _MatrixValue) => mapMatrix(mv, d => siOf(d).getOrElse(Double.NaN))
      case Left(m: _MatrixShaped)  => mapMatrixExpr(m, env)
      case other                   => Left(Si(other.toExpression))


/** The cosine integral `Ci(e)` — the antiderivative of `cos(v)/v`; real only for `e > 0`.
 *
 *  @param e the argument expression
 */
case class Ci(e: _Expression) extends _Function:
  override def toString: String = s"Ci($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Ci(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x))       => ciOf(x).map(v => Right(_Number(v))).getOrElse(Left(this))
      case Right(mv: _MatrixValue) => mapMatrix(mv, d => ciOf(d).getOrElse(Double.NaN))
      case Left(m: _MatrixShaped)  => mapMatrixExpr(m, env)
      case other                   => Left(Ci(other.toExpression))


/** The exponential integral `Ei(e)` — the antiderivative of `e^v/v`; `e = 0` is the pole.
 *
 *  @param e the argument expression
 */
case class Ei(e: _Expression) extends _Function:
  override def toString: String = s"Ei($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Ei(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x))       => eiOf(x).map(v => Right(_Number(v))).getOrElse(Left(this))
      case Right(mv: _MatrixValue) => mapMatrix(mv, d => eiOf(d).getOrElse(Double.NaN))
      case Left(m: _MatrixShaped)  => mapMatrixExpr(m, env)
      case other                   => Left(Ei(other.toExpression))


/** The logarithmic integral `li(e) = Ei(ln e)` — the antiderivative of `1/ln(v)`;
 *  real for `e > 0`, `e ≠ 1`.
 *
 *  @param e the argument expression
 */
case class Li(e: _Expression) extends _Function:
  override def toString: String = s"li($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = Li(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x))       => liOf(x).map(v => Right(_Number(v))).getOrElse(Left(this))
      case Right(mv: _MatrixValue) => mapMatrix(mv, d => liOf(d).getOrElse(Double.NaN))
      case Left(m: _MatrixShaped)  => mapMatrixExpr(m, env)
      case other                   => Left(Li(other.toExpression))


/** The Fresnel sine integral `fresnelS(e) = ∫₀ᵉ sin(π t²/2) dt`.
 *
 *  @param e the argument expression
 */
case class FresnelS(e: _Expression) extends _Function:
  override def toString: String = s"fresnelS($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = FresnelS(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x))       => fresnelSOf(x).map(v => Right(_Number(v))).getOrElse(Left(this))
      case Right(mv: _MatrixValue) => mapMatrix(mv, d => fresnelSOf(d).getOrElse(Double.NaN))
      case Left(m: _MatrixShaped)  => mapMatrixExpr(m, env)
      case other                   => Left(FresnelS(other.toExpression))


/** The Fresnel cosine integral `fresnelC(e) = ∫₀ᵉ cos(π t²/2) dt`.
 *
 *  @param e the argument expression
 */
case class FresnelC(e: _Expression) extends _Function:
  override def toString: String = s"fresnelC($e)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = FresnelC(c.head)

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(r: _Rational)     => viaExact(r, env)
      case Right(_Number(x))       => fresnelCOf(x).map(v => Right(_Number(v))).getOrElse(Left(this))
      case Right(mv: _MatrixValue) => mapMatrix(mv, d => fresnelCOf(d).getOrElse(Double.NaN))
      case Left(m: _MatrixShaped)  => mapMatrixExpr(m, env)
      case other                   => Left(FresnelC(other.toExpression))


/** The regularised lower incomplete gamma `gammaP(a, x) = P(a, x)`.
 *
 *  Exposed directly because it is the chi-squared and gamma cdf, not only an internal step
 *  towards `erf`.
 *
 *  @param a the shape parameter
 *  @param x the argument
 */
case class GammaP(a: _Expression, x: _Expression) extends _Function:
  override def toString: String = s"gammaP($a, $x)"
  override def children: List[_Expression] = List(a, x)
  override def rebuild(c: List[_Expression]): _Expression = GammaP(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    (a.eval(env), x.eval(env)) match
      // Exact tier first -- `_Number` widens, so anything below it never sees a `_Rational`.
      case (Right(ra: _Rational), Right(xv: _Value)) => viaDouble(List(_Number(ra.toDouble), xv), env)
      case (Right(av: _Value), Right(rx: _Rational)) => viaDouble(List(av, _Number(rx.toDouble)), env)
      case (Right(_Number(av)), Right(_Number(xv)))  =>
        lowerGammaP(av, xv).map(v => Right(_Number(v))).getOrElse(Left(this))
      case (ra, rx) => Left(GammaP(ra.toExpression, rx.toExpression))


/** The regularised upper incomplete gamma `gammaQ(a, x) = Q(a, x) = 1 − P(a, x)`.
 *
 *  @param a the shape parameter
 *  @param x the argument
 */
case class GammaQ(a: _Expression, x: _Expression) extends _Function:
  override def toString: String = s"gammaQ($a, $x)"
  override def children: List[_Expression] = List(a, x)
  override def rebuild(c: List[_Expression]): _Expression = GammaQ(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    (a.eval(env), x.eval(env)) match
      case (Right(ra: _Rational), Right(xv: _Value)) => viaDouble(List(_Number(ra.toDouble), xv), env)
      case (Right(av: _Value), Right(rx: _Rational)) => viaDouble(List(av, _Number(rx.toDouble)), env)
      case (Right(_Number(av)), Right(_Number(xv)))  =>
        upperGammaQ(av, xv).map(v => Right(_Number(v))).getOrElse(Left(this))
      case (ra, rx) => Left(GammaQ(ra.toExpression, rx.toExpression))


/** The regularised incomplete beta `betaI(x, a, b) = I_x(a, b)`.
 *
 *  The Student-t and F cdfs.  Three arguments, which no other `_Function` has; `children`
 *  and `rebuild` carry all three, so the generic traversal handles it like any other node.
 *
 *  @param x the argument, in `[0, 1]`
 *  @param a the first shape parameter
 *  @param b the second shape parameter
 */
case class BetaI(x: _Expression, a: _Expression, b: _Expression) extends _Function:
  override def toString: String = s"betaI($x, $a, $b)"
  override def children: List[_Expression] = List(x, a, b)
  override def rebuild(c: List[_Expression]): _Expression = BetaI(c.head, c(1), c(2))

  override def eval(env: Environment): Either[_Expression, _Value] =
    (x.eval(env), a.eval(env), b.eval(env)) match
      case (Right(_Number(xv)), Right(_Number(av)), Right(_Number(bv))) =>
        // `_Number` widens over `_Rational`, so exact arguments are read as Doubles here --
        // correct for this tier, which has no arbitrary-precision kernel to offer.
        incompleteBetaOf(xv, av, bv).map(v => Right(_Number(v))).getOrElse(Left(this))
      case (rx, ra, rb) => Left(BetaI(rx.toExpression, ra.toExpression, rb.toExpression))

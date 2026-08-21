package it.grypho.scala.leonardo
package scalar

import core.*
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

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
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

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
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

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
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

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
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

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
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

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
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

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
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

  override def eval(env: Environment): Either[_Expression, _Value] =
    e.eval(env) match
      case Right(_Number(x)) => Right(_Number(atan(x)))
      case Right(mv: _MatrixValue) => mapMatrix(mv, atan)
      case Left(m: _MatrixShaped) => mapMatrixExpr(m, env)
      case other             => Left(Atan(other.toExpression))


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
      case Right(_Number(x))       => gammaOf(x).map(r => Right(_Number(r))).getOrElse(Left(this))
      case Right(mv: _MatrixValue) => mapMatrix(mv, d => gammaOf(d).getOrElse(Double.NaN))
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
      case (Right(_Number(x)), Right(_Number(y))) =>
        betaOf(x, y).map(r => Right(_Number(r))).getOrElse(Left(this))
      case (ra, rb) => Left(Beta(ra.toExpression, rb.toExpression))
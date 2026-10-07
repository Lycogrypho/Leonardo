package it.grypho.scala.leonardo
package scalar

import core.*
import scala.math.{pow, exp, log, sin, cos, tan, asin, acos, atan}


/** Attempts to compile expression `e` into a raw `Double => Double` closure over `v`,
 *  resolving any other free variables from `env`.
 *
 *  When `Some(f)` is returned, `f(x)` evaluates `e` with `v` bound to `x` at full
 *  `Double` precision with no per-sample allocation, no tree traversal, and no environment
 *  lookup — O(1) per sample after compilation.  This is the fast path in `_DefIntegral.eval`
 *  (Simpson's rule) and in [[sample]].
 *
 *  `None` is returned for expressions that contain unresolvable symbolic nodes
 *  (`_Derivative`, `_Integral`, unbound variables, complex values, etc.).
 *
 *  @param e   the expression to compile
 *  @param v   the free variable that maps to the closure argument `x`
 *  @param env environment for resolving any other free variables
 *  @return `Some(f)` when the expression is fully compilable, `None` otherwise
 */
def compile(e: _Expression, v: _Variable, env: Environment): Option[Double => Double] =
  kernel(e, Map(v.variable -> ((x, _) => x)), env).map(k => (x: Double) => k.at(x, NoArgs))

/** Compiles `e` into a closure over several variables, read positionally (F_0058).
 *
 *  `f(xs)` evaluates `e` with `vars(i)` bound to `xs(i)`; `xs` must hold one value per
 *  variable, in the order given.  Any other free name is resolved from `env` at compile time,
 *  and a name in `vars` shadows a binding of the same name in `env`.  The closure is pure — it
 *  reads `xs` and writes nothing — so one compiled function may be shared across threads, and
 *  the caller may reuse its argument array between calls.
 *
 *  It is the same table as the one-variable form, not a second one: a node the table does not
 *  cover declines both, and a function added to it reaches both.
 *
 *  @param e    the expression to compile
 *  @param vars the variables, in argument order; a repeated name declines
 *  @param env  environment for resolving every other free variable
 *  @return `Some(f)` when the expression is fully compilable, `None` for a node the table does
 *          not cover, a free name neither in `vars` nor bound to a number, or a repeated variable
 */
def compile(e: _Expression, vars: Seq[_Variable], env: Environment): Option[Array[Double] => Double] =
  positionalLeaves(vars).flatMap(kernel(e, _, env)).map(k => (xs: Array[Double]) => k.at(0.0, xs))

/** Compiles a symbolic matrix into a closure returning its cells, row-major (F_0058).
 *
 *  The cells are compiled exactly as [[compile]] compiles one expression, over the same
 *  positional variables, so a Jacobian built by `jacobian(F, x, y, …)` evaluates in one call
 *  without walking the tree.  The argument must already BE a matrix — a `_MatrixShaped` literal
 *  or a dense value; a matrix operation is not evaluated here, so evaluate it first.  Each call
 *  returns a fresh array of `rows · cols` values.
 *
 *  @param m    the matrix to compile
 *  @param vars the variables, in argument order; a repeated name declines
 *  @param env  environment for resolving every other free variable
 *  @return `Some(f)` when every cell compiles, `None` for a non-matrix or any cell that declines
 */
def compileMatrix(m: _Expression, vars: Seq[_Variable], env: Environment): Option[Array[Double] => Array[Double]] =
  val cells = m match
    case s: _MatrixShaped => Some(s.children)
    case d: _MatrixValue  => Some(d.toVector.toList.map(_Number(_)))
    case _                => None
  for
    leaves <- positionalLeaves(vars)
    cs     <- cells
    ks      = cs.map(kernel(_, leaves, env))
    if ks.forall(_.isDefined)
  yield
    val compiled = ks.flatten.toArray
    (xs: Array[Double]) => compiled.map(_.at(0.0, xs))


/** A compiled expression: the one shape every `compile` form is built from.
 *
 *  A leaf reads exactly one of its two arguments — `x` in the one-variable form, `xs` in the
 *  positional one — so the table below is written once and serves both, and the one-variable
 *  form keeps its promise of no allocation per sample (wrapping `x` in an array would break it).
 *  A trait rather than a function type so the call is a primitive `double` method, with no
 *  boxing at any node.
 */
private trait Kernel:
  def at(x: Double, xs: Array[Double]): Double

/** A constant leaf, recognised by the node above it so it is read without a call. */
private final class ConstK(val d: Double) extends Kernel:
  def at(x: Double, xs: Array[Double]): Double = d

/** A positional variable `xs(i)`, recognised by the node above it so it is read without a call. */
private final class ArgK(val i: Int) extends Kernel:
  def at(x: Double, xs: Array[Double]): Double = xs(i)

/** The empty positional argument the one-variable form passes. */
private val NoArgs: Array[Double] = Array.emptyDoubleArray

/** One leaf per variable reading `xs(i)`; `None` when a name repeats. */
private def positionalLeaves(vars: Seq[_Variable]): Option[Map[String, Kernel]] =
  val names = vars.map(_.variable)
  Option.when(names.distinct.size == names.size)(names.zipWithIndex.map((n, i) => n -> ArgK(i)).toMap)

// The combinators below do two things the measurement asked for (F_0058, Done.md): an operation
// whose operands are all constant is computed ONCE, here, rather than per call; and an operand
// that is a constant or a positional variable is read in place rather than through `at`. Both
// apply `f` to exactly the doubles the plain closure would, so the answers are bit-identical --
// only the number of interface calls per evaluation falls. The full constant/variable matrix is
// spelled out for the four arithmetic operators alone, where a call is a large share of the cost;
// a special function costs far more than the call that reaches it, so it only gets the folding.

private inline def lift1(ka: Kernel)(inline f: Double => Double): Kernel = ka match
  case c: ConstK => ConstK(f(c.d))
  case a: ArgK   => val i = a.i; (_, xs) => f(xs(i))
  case _         => (x, xs) => f(ka.at(x, xs))

private inline def fold2(ka: Kernel, kb: Kernel)(inline f: (Double, Double) => Double): Kernel = (ka, kb) match
  case (a: ConstK, b: ConstK) => ConstK(f(a.d, b.d))
  case _                      => (x, xs) => f(ka.at(x, xs), kb.at(x, xs))

private inline def fold3(ka: Kernel, kb: Kernel, kc: Kernel)
                        (inline f: (Double, Double, Double) => Double): Kernel = (ka, kb, kc) match
  case (p: ConstK, q: ConstK, r: ConstK) => ConstK(f(p.d, q.d, r.d))
  case _                                 => (x, xs) => f(ka.at(x, xs), kb.at(x, xs), kc.at(x, xs))

private inline def lift2(ka: Kernel, kb: Kernel)(inline f: (Double, Double) => Double): Kernel = (ka, kb) match
  case (a: ConstK, b: ConstK) => ConstK(f(a.d, b.d))
  case (a: ConstK, b: ArgK)   => val (c, j) = (a.d, b.i); (_, xs) => f(c, xs(j))
  case (a: ArgK, b: ConstK)   => val (i, c) = (a.i, b.d); (_, xs) => f(xs(i), c)
  case (a: ArgK, b: ArgK)     => val (i, j) = (a.i, b.i); (_, xs) => f(xs(i), xs(j))
  case (a: ConstK, _)         => val c = a.d; (x, xs) => f(c, kb.at(x, xs))
  case (_, b: ConstK)         => val c = b.d; (x, xs) => f(ka.at(x, xs), c)
  case (a: ArgK, _)           => val i = a.i; (x, xs) => f(xs(i), kb.at(x, xs))
  case (_, b: ArgK)           => val j = b.i; (x, xs) => f(ka.at(x, xs), xs(j))
  case _                      => (x, xs) => f(ka.at(x, xs), kb.at(x, xs))

private inline def unary(a: _Expression, leaves: Map[String, Kernel], env: Environment)
                        (inline f: Double => Double): Option[Kernel] =
  kernel(a, leaves, env).map(ka => lift1(ka)(f))

/** The four arithmetic operators: every constant/variable pairing specialised. */
private inline def arith(a: _Expression, b: _Expression, leaves: Map[String, Kernel], env: Environment)
                        (inline f: (Double, Double) => Double): Option[Kernel] =
  for ka <- kernel(a, leaves, env); kb <- kernel(b, leaves, env) yield lift2(ka, kb)(f)

private inline def binary(a: _Expression, b: _Expression, leaves: Map[String, Kernel], env: Environment)
                         (inline f: (Double, Double) => Double): Option[Kernel] =
  for ka <- kernel(a, leaves, env); kb <- kernel(b, leaves, env) yield fold2(ka, kb)(f)

private inline def ternary(a: _Expression, b: _Expression, c: _Expression,
                           leaves: Map[String, Kernel], env: Environment)
                          (inline f: (Double, Double, Double) => Double): Option[Kernel] =
  for ka <- kernel(a, leaves, env); kb <- kernel(b, leaves, env); kc <- kernel(c, leaves, env)
  yield fold3(ka, kb, kc)(f)

/** The compilation table: `leaves` says how each variable is read, `env` supplies the rest. */
private def kernel(e: _Expression, leaves: Map[String, Kernel], env: Environment): Option[Kernel] = e match
  case _Number(d)    => Some(ConstK(d))
  case _: _Complex   => None    // a complex value has no Double closure
  case _Variable(n)  => leaves.get(n).orElse(env.get(n).collect { case _Number(d) => ConstK(d) })
  case Sum(a, b)     => arith(a, b, leaves, env)(_ + _)
  case Product(a, b) => arith(a, b, leaves, env)(_ * _)
  case Ratio(a, b)   => arith(a, b, leaves, env)(_ / _)
  case Power(a, b)   => arith(a, b, leaves, env)(pow)
  case Exp(a)        => unary(a, leaves, env)(exp)
  case Ln(a)         => unary(a, leaves, env)(log)
  case LogBase(a, b) => binary(a, b, leaves, env)((p, q) => log(p) / log(q))
  case Abs(a)        => unary(a, leaves, env)(math.abs)
  // F_0056: the scalar order functions; NaN stands for "undefined here", as for Ln.
  case _Join(k, args) =>
    val ks = args.map(kernel(_, leaves, env))
    Option.when(ks.forall(_.isDefined)) {
      val gs = ks.flatten
      val op: (Double, Double) => Double = if k == Extreme.Max then math.max else math.min
      (x, xs) => gs.tail.foldLeft(gs.head.at(x, xs))((acc, g) => op(acc, g.at(x, xs)))
    }
  case _Clamp(a, lo, hi) =>
    ternary(a, lo, hi, leaves, env)((p, l, h) => if l > h then Double.NaN else math.min(math.max(p, l), h))
  case _Softplus(a, k) =>
    binary(a, k, leaves, env)((p, kk) => if kk > 0 then softplusOf(p, kk) else Double.NaN)
  case Sin(a)        => unary(a, leaves, env)(sin)
  case Cos(a)        => unary(a, leaves, env)(cos)
  case Tg(a)         => unary(a, leaves, env)(tan)
  case Asin(a)       => unary(a, leaves, env)(asin)
  case Acos(a)       => unary(a, leaves, env)(acos)
  case Atan(a)       => unary(a, leaves, env)(atan)
  // Special functions: an undefined point yields NaN, which `sample` and Simpson
  // already drop -- the same contract `Ln` has for a negative argument.
  case Factorial(a) => unary(a, leaves, env)(factorialOf(_).getOrElse(Double.NaN))
  case Gamma(a)     => unary(a, leaves, env)(gammaOf(_).getOrElse(Double.NaN))
  case LogGamma(a)  => unary(a, leaves, env)(lgammaOf(_).getOrElse(Double.NaN))
  case MultiFactorial(a, b) => binary(a, b, leaves, env)(multiFactorialOf(_, _).getOrElse(Double.NaN))
  case Beta(a, b)   => binary(a, b, leaves, env)(betaOf(_, _).getOrElse(Double.NaN))
  // The analytic special functions: same shape, so sample and Simpson get them on the fast path too.
  case Erf(a)       => unary(a, leaves, env)(erfOf(_).getOrElse(Double.NaN))
  case Erfc(a)      => unary(a, leaves, env)(erfcOf(_).getOrElse(Double.NaN))
  case Digamma(a)   => unary(a, leaves, env)(digammaOf(_).getOrElse(Double.NaN))
  case GammaP(a, b) => binary(a, b, leaves, env)(lowerGammaP(_, _).getOrElse(Double.NaN))
  case GammaQ(a, b) => binary(a, b, leaves, env)(upperGammaQ(_, _).getOrElse(Double.NaN))
  case BetaI(a, b, c) => ternary(a, b, c, leaves, env)(incompleteBetaOf(_, _, _).getOrElse(Double.NaN))
  case _             => None  // _Derivative, _Integral, _DefIntegral, unresolvable

package it.grypho.scala.leonardo
package statistics

import core.*
import matrix._Matrix


/** [[https://en.wikipedia.org/wiki/Recursive_least_squares_filter Recursive least squares]]
 *  (F_0057, MPC requirement T1-06): one update of a least-squares estimate as a new observation
 *  arrives, with a forgetting factor so a drifting parameter can be tracked.
 *
 *  **State in, state out** (X-04): the caller passes `theta` and `P` and receives their updated
 *  values; nothing is remembered between calls. For `theta` (`n×m`), `P` (`n×n`), the regressor
 *  `phi` (`n×1`), the observation `y` (`m×1`, or a number when `m = 1`) and `λ ∈ (0, 1]`:
 *
 *  {{{
 *  d      = λ + φᵀPφ                (must be positive)
 *  K      = Pφ / d
 *  theta' = theta + K·(y − thetaᵀφ)ᵀ
 *  P'     = sym((P − K·φᵀP) / λ)     sym(A) = (A + Aᵀ)/2
 *  }}}
 *
 *  **`P'` is symmetrised explicitly** rather than trusted: the update is symmetric in exact
 *  arithmetic only, and in floating point the two triangles drift apart over thousands of steps
 *  until `P` stops being a covariance at all. `(A + Aᵀ)/2` is exactly symmetric in `Double`,
 *  since `a + b == b + a`.
 *
 *  **One algorithm, both tiers.** It is written once over a minimal `Field` — rational when every
 *  input is exact, `Double` otherwise — so the exact path is the same code, not a second copy
 *  that could drift from the first (the `Descriptive` precedent, without its duplication).
 */

/** The field operations recursive least squares needs. */
private trait Field[T]:
  def plus(a: T, b: T): T
  def minus(a: T, b: T): T
  def times(a: T, b: T): T
  def div(a: T, b: T): Option[T]
  def positive(a: T): Boolean
  def two: T

private object DoubleField extends Field[Double]:
  def plus(a: Double, b: Double): Double  = a + b
  def minus(a: Double, b: Double): Double = a - b
  def times(a: Double, b: Double): Double = a * b
  def div(a: Double, b: Double): Option[Double] = Option.when(b != 0)(a / b)
  def positive(a: Double): Boolean = a > 0
  def two: Double = 2.0

private final class RationalField(policy: GcdPolicy) extends Field[_Rational]:
  def plus(a: _Rational, b: _Rational): _Rational  = a.add(b, policy)
  def minus(a: _Rational, b: _Rational): _Rational = a.subtract(b, policy)
  def times(a: _Rational, b: _Rational): _Rational = a.multiply(b, policy)
  def div(a: _Rational, b: _Rational): Option[_Rational] = a.divide(b, policy)
  def positive(a: _Rational): Boolean = a.signum > 0
  def two: _Rational = _Rational(2)

/** Row-major rows of a small matrix. */
private type Rows[T] = Vector[Vector[T]]

/** `Some` of every value when none is missing. */
private def sequence[A](xs: Vector[Option[A]]): Option[Vector[A]] =
  xs.foldRight(Option(Vector.empty[A]))((o, acc) => for v <- acc; a <- o yield a +: v)

/** The update itself, generic in the field; `None` when the denominator is not positive. */
private def rlsStep[T](f: Field[T], theta: Rows[T], p: Rows[T], phi: Vector[T], y: Vector[T],
                       lambda: T): Option[(Rows[T], Rows[T])] =
  def dot(a: Vector[T], b: Vector[T]): T = a.lazyZip(b).map(f.times).reduce(f.plus)
  def matrix(n: Int)(cell: (Int, Int) => Option[T]): Option[Rows[T]] =
    sequence(Vector.tabulate(n)(i => sequence(Vector.tabulate(n)(j => cell(i, j)))))
  val n    = phi.size
  val pPhi = p.map(row => dot(row, phi))                                  // P·φ
  val phiP = Vector.tabulate(n)(j => dot(phi, p.map(_(j))))               // φᵀ·P
  val d    = f.plus(lambda, dot(phi, pPhi))
  if !f.positive(d) then None
  else
    for
      k     <- sequence(pPhi.map(f.div(_, d)))
      err    = y.indices.toVector.map(c => f.minus(y(c), dot(theta.map(_(c)), phi)))
      theta1 = Vector.tabulate(n, y.size)((i, c) => f.plus(theta(i)(c), f.times(k(i), err(c))))
      raw   <- matrix(n)((i, j) => f.div(f.minus(p(i)(j), f.times(k(i), phiP(j))), lambda))
      sym   <- matrix(n)((i, j) => f.div(f.plus(raw(i)(j), raw(j)(i)), f.two))
    yield (theta1, sym)

/** An evaluated operand as `(rows, cols, cells)` of values; a number is `1×1`. */
private def valueGrid(r: Either[_Expression, _Value]): Option[(Int, Int, Vector[_Value])] = r match
  case Right(m: _MatrixValue) => Some((m.rows, m.cols, m.toVector.map(_Number(_))))
  case Left(m: _Matrix) =>
    val vs = m.elems.collect { case v: _Value => v }
    Option.when(vs.size == m.elems.size)((m.rows, m.cols, vs))
  case Right(v @ _Number(_)) => Some((1, 1, Vector(v)))
  case _ => None

/** One recursive-least-squares update.
 *
 *  @param theta  the estimate, `n×m` (`n×1` for one output)
 *  @param p      the covariance, `n×n`
 *  @param phi    the regressor, `n×1`
 *  @param y      the observation, `m×1` (a number for one output)
 *  @param lambda the forgetting factor, in `(0, 1]`
 *  @param env    the environment the operands evaluate in, and the exact reduction policy
 *  @return `(theta', P')` — exact when every input is, dense otherwise — or `None` when λ is out
 *          of range, a shape disagrees, an operand is not numeric, or `λ + φᵀPφ` is not positive
 */
def rlsUpdate(theta: _Expression, p: _Expression, phi: _Expression, y: _Expression, lambda: _Expression,
              env: Environment): Option[(_Expression, _Expression)] =
  for
    (n, m, th)   <- valueGrid(theta.eval(env))
    (pr, pc, pv) <- valueGrid(p.eval(env))
    (fr, fc, fv) <- valueGrid(phi.eval(env))
    (yr, yc, yv) <- valueGrid(y.eval(env))
    lam          <- lambda.eval(env).toOption.collect { case v @ _Number(d) if d > 0 && d <= 1 => v }
    if pr == n && pc == n && fr == n && fc == 1 && yr == m && yc == 1
    result       <- inTier(n, m, th ++ pv ++ fv ++ yv :+ lam, env)
  yield result

/** Runs the update in the rational field when every value is exact, in `Double` otherwise.
 *  `values` is theta, P, phi, y (row-major) and λ, in that order. */
private def inTier(n: Int, m: Int, values: Vector[_Value], env: Environment): Option[(_Expression, _Expression)] =
  def split[T](xs: Vector[T]): (Rows[T], Rows[T], Vector[T], Vector[T], T) =
    def take(from: Int, len: Int) = xs.slice(from, from + len)
    (take(0, n * m).grouped(m).toVector, take(n * m, n * n).grouped(n).toVector,
     take(n * m + n * n, n), take(n * m + n * n + n, m), xs.last)
  if values.forall(_.isInstanceOf[_Rational]) then
    val (t, p, f, y, l) = split(values.collect { case r: _Rational => r })
    rlsStep(RationalField(env.rationalPolicy), t, p, f, y, l)
      .map((t1, p1) => (_Matrix(n, m, t1.flatten), _Matrix(n, n, p1.flatten)))
  else
    val (t, p, f, y, l) = split(values.collect { case _Number(d) => d })
    rlsStep(DoubleField, t, p, f, y, l)
      .filter((t1, p1) => (t1.flatten ++ p1.flatten).forall(d => !d.isNaN && !d.isInfinite))
      .map((t1, p1) => (_MatrixValue(n, m, t1.flatten.toArray), _MatrixValue(n, n, p1.flatten.toArray)))

/** `rls(theta, P, phi, y, lambda)` — one update, as the row `[[theta', P']]` (the `lu` shape), so
 *  tuple assignment carries the state to the next call.
 *
 *  @param theta  the estimate
 *  @param p      the covariance
 *  @param phi    the regressor
 *  @param y      the observation
 *  @param lambda the forgetting factor
 */
case class _Rls(theta: _Expression, p: _Expression, phi: _Expression, y: _Expression, lambda: _Expression)
    extends _Expression:
  override def toString: String = s"rls($theta, $p, $phi, $y, $lambda)"
  override def children: List[_Expression] = List(theta, p, phi, y, lambda)
  override def rebuild(c: List[_Expression]): _Expression = _Rls(c.head, c(1), c(2), c(3), c(4))
  override def eval(env: Environment): Either[_Expression, _Value] =
    rlsUpdate(theta, p, phi, y, lambda, env) match
      case Some((t, q)) => Left(_Matrix(1, 2, Vector(t, q)))
      case None         => Left(rebuild(children.map(_.eval(env).toExpression)))

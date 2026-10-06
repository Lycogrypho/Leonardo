package it.grypho.scala.leonardo
package optimize

import core.*
import scalar.*
import matrix.{_Matrix, Determinant}


/** The most variables [[convexity]] accepts.
 *
 *  The cofactor expansion behind a symbolic `det` stops at six (`matrix`'s own cap, private
 *  there); past it the full-size minor stays symbolic and no `true` could ever be proved, so a
 *  higher cap here would buy nothing but a slower `None`.  Kept as a separate constant
 *  because the other is not visible, and documented as its mirror so the two move together.
 */
val MaxConvexityVariables: Int = 6

/** Whether `f` is convex in `vars` (F_0010 slice 2): `Some(true)`, `Some(false)`, or `None`
 *  when neither can be established.
 *
 *  **`true` needs a proof, `false` needs a witness**, and anything short of either stays
 *  symbolic — failing to prove that a minor is non-negative is not a proof that it is
 *  negative.
 *
 *  - **`true`**: every *principal* minor of the symbolic Hessian is provably non-negative, so
 *    the Hessian is positive semidefinite everywhere.  **Principal, not leading**: Sylvester's
 *    leading-minor test proves *definiteness*; semidefiniteness needs every principal minor,
 *    and the leading ones alone report `diag(0, −1)` as convex.  The proof is also required to
 *    hold on a convex set, so `true` is answered only when `f` is defined everywhere — for
 *    `1/x²` the Hessian is positive wherever it exists, but the domain `x ≠ 0` is not convex
 *    and neither is the function on it.
 *  - **`false`**: a principal minor provably negative, or a point where `f` is real-valued
 *    and the numeric Hessian has a negative eigenvalue, found on a small fixed grid.  The
 *    point must be one where `f` is *defined*: `x·ln(x)` has a negative second derivative at
 *    `x = −1`, where it is not real, and is convex where it is.
 *
 *  Capped at [[MaxConvexityVariables]], the cofactor expansion's own cap.
 *
 *  @param f    the function
 *  @param vars the variables, an ordered tuple
 *  @param env  bindings and precision
 *  @return the verdict, or `None` when it cannot be established
 */
def convexity(f: _Expression, vars: Vector[_Variable], env: Environment): Option[Boolean] =
  if !distinctVariables(vars) || vars.size > MaxConvexityVariables then None
  else hessianCells(f, vars, env).flatMap { cells =>
    val n      = vars.size
    val minors = principalSubsets(n).map(s => principalMinor(cells, n, s, env))
    if minors.exists(m => sign(m, env).contains(-1)) then Some(false)
    else if minors.forall(m => nonNegative(m, env)) && definedEverywhere(f, vars, env) then Some(true)
    else Option.when(hasNegativeCurvature(f, vars, env))(false)
  }


/** Every non-empty subset of `0 until n`, as ascending index lists. */
private def principalSubsets(n: Int): List[List[Int]] =
  (1 to n).toList.flatMap(k => (0 until n).toList.combinations(k))

/** The determinant of the Hessian restricted to the rows and columns in `s`, simplified. */
private def principalMinor(cells: Vector[_Expression], n: Int, s: List[Int],
                           env: Environment): _Expression =
  val sub = for i <- s.toVector; j <- s yield cells(i * n + j)
  val det = if s.sizeIs == 1 then sub.head else Determinant(_Matrix(s.size, s.size, sub)).eval(env).toExpression
  simplifyFully(det)

/** Whether `e` is provably `≥ 0`.
 *
 *  Extends `scalar.isNonNegative` with the product and quotient rules a minor needs —
 *  `12·x²` is a product of non-negatives, which the shared prover does not read.  Kept local
 *  rather than widening the shared one: that prover also drives the inequality solver and the
 *  domain analysis, and widening it would change their answers, which is a unit of work of
 *  its own. */
private def nonNegative(e: _Expression, env: Environment): Boolean =
  isNonNegative(e, env) || (e match
    case Product(a, b) => (nonNegative(a, env) && nonNegative(b, env)) ||
                          (nonPositive(a, env) && nonPositive(b, env))
    case Sum(a, b)     => nonNegative(a, env) && nonNegative(b, env)
    case Ratio(a, b)   => nonNegative(a, env) && sign(b, env).contains(1)
    case _             => false)

/** Whether `e` is provably `≤ 0`, the mirror of [[nonNegative]]. */
private def nonPositive(e: _Expression, env: Environment): Boolean =
  isNonPositive(e, env) || (e match
    case Product(a, b) => (nonNegative(a, env) && nonPositive(b, env)) ||
                          (nonPositive(a, env) && nonNegative(b, env))
    case Sum(a, b)     => nonPositive(a, env) && nonPositive(b, env)
    case _             => false)

/** Whether `f` has no domain restriction in any of the variables. */
private def definedEverywhere(f: _Expression, vars: Vector[_Variable], env: Environment): Boolean =
  vars.forall(v => domainOf(f, v, DomainKind.Real, env).isUnrestricted)

/** The coordinates each variable takes on the witness grid.
 *
 *  Five values per axis up to four variables, three beyond, so the grid stays below a
 *  thousand points (`5⁴ = 625`, `3⁶ = 729`) at the [[MaxConvexityVariables]] cap. */
private def gridAxis(n: Int): List[Double] =
  if n <= 4 then List(-2.0, -1.0, 0.0, 1.0, 2.0) else List(-1.0, 0.0, 1.0)

/** Whether some grid point, where `f` is real-valued, has a Hessian with a negative eigenvalue.
 *
 *  Negative relative to the largest eigenvalue's magnitude, so a rounding-sized value does
 *  not count as a witness. */
private def hasNegativeCurvature(f: _Expression, vars: Vector[_Variable], env: Environment): Boolean =
  val axis   = gridAxis(vars.size)
  val points = vars.foldLeft(List(Map.empty[String, Double])) { (acc, v) =>
    for p <- acc; x <- axis yield p + (v.variable -> x)
  }
  points.exists { p =>
    val bound = p.foldLeft(env) { case (acc, (k, d)) => acc.withBinding(k, _Number(d)) }
    val real  = f.eval(bound) match
      case Right(_Number(d)) => !d.isNaN && !d.isInfinite
      case _                 => false
    real && numericHessian(f, vars, p, env).flatMap(eigenvaluesOf).exists { ev =>
      val scale = ev.map(math.abs).max
      ev.min < -1e-9 * scale
    }
  }

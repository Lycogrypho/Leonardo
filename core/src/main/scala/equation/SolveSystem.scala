package it.grypho.scala.leonardo
package equation

import core.*
import scalar.*

import scala.util.boundary, boundary.break


/** Linear system solver for square n-by-n systems.
 *
 *  [[solveSystem]] returns the unique solution as a list of `v = expr` equations,
 *  or `None` when the system is non-square, nonlinear, or singular.
 *
 *  Coefficient extraction uses `scalar.collect` (the same prerequisite used by
 *  [[solve]]'s linear and quadratic tiers): for each equation `i` and variable `j`,
 *  `collect(lhs_i - rhs_i, v_j)` yields the polynomial coefficients in `v_j`; for a
 *  linear system the result has length <= 2 (length > 2 means nonlinear -> `None`).
 *  A linearity guard rejects any coefficient `A_ij` that depends on a solve variable.
 *  The constant term `b_i` is `-(lhs_i - rhs_i evaluated with all v_j = 0)`.
 *
 *  **Dense path** (all `A_ij` and `b_i` fold to `_Number` in `env`): Gaussian
 *  elimination with partial pivoting on `Double` arrays; returns `None` when the
 *  pivot drops below `1e-12` (singular or near-singular).
 *
 *  **Symbolic path** (some coefficient or constant stays symbolic): row reduction
 *  using `Sum`/`Product`/`Ratio`/`_Expression` arithmetic with `simplifyFully` at
 *  every step; returns `None` when any diagonal pivot simplifies to `_Number(0)`.
 *
 *  Solution right-hand sides are folded through `env` so bound symbols produce
 *  numeric answers (same convention as [[solve]]).
 */

/** Returns the unique solution of a square linear system, or `None`.
 *
 *  @param equations the list of equations (must be n equations for n variables)
 *  @param variables the solve variables (must match the number of equations)
 *  @param env       the evaluation environment (variable bindings + precision)
 *  @return `Some(list of v_i = expr_i)` on success; `None` when non-square,
 *          nonlinear, singular, or near-singular
 */
def solveSystem(
    equations: List[_Equation],
    variables: List[_Variable],
    env:       Environment = new Environment()
): Option[List[_Equation]] =
  val n = equations.size
  if n == 0 || n != variables.size then return None

  // Normalise: move everything to the left side (expr = 0)
  val exprs: Vector[_Expression] =
    equations.map(eq => simplifyFully(Sum(eq.lhs, Product(_Number(-1), eq.rhs)))).toVector

  // Extract coefficient matrix A[i][j] via collect(expr_i, v_j)
  val coeffOpt: Option[Vector[Vector[_Expression]]] = allOpt(exprs.map { expr =>
    allOpt(variables.map { v =>
      collect(expr, v) match
        case Some(cs) if cs.size == 1 => Some(_Number(0))    // v absent
        case Some(cs) if cs.size == 2 => Some(cs(1))         // linear in v
        case Some(_)                  => None                 // degree >= 2
        case None                     => None                 // non-polynomial
    })
  })

  val A: Vector[Vector[_Expression]] = coeffOpt match
    case None    => return None
    case Some(m) => m

  // Linearity guard: no A_ij may depend on any solve variable
  if A.exists(_.exists(c => variables.exists(v => dependsOn(c, v)))) then return None

  // Constant vector b[i] = -(expr_i with all solve variables bound to 0)
  val zeroEnv = variables.foldLeft(env)((e, v) => e.withBinding(v.variable, _Number(0)))
  val b: Vector[_Expression] =
    exprs.map(expr => simplifyFully(Product(_Number(-1), expr.eval(zeroEnv).toExpression)))

  // Dense path: try to fold every coefficient and constant to a Double
  def toDouble(e: _Expression): Option[Double] = e.eval(env) match
    case Right(_Number(d)) => Some(d)
    case _                 => None

  val denseA: Option[Vector[Vector[Double]]] = allOpt(A.map(row => allOpt(row.map(toDouble))))
  val denseB: Option[Vector[Double]]         = allOpt(b.map(toDouble))

  (denseA, denseB) match
    case (Some(da), Some(db)) =>
      gaussDense(da, db).map(xs =>
        variables.zip(xs).map((v, x) => _Equation(v, _Number(x)))
      )
    case _ =>
      gaussSymbolic(A, b).map(xs =>
        variables.zip(xs).map((v, x) => _Equation(v, simplifyFully(x).eval(env).toExpression))
      )


/** Sequences an iterable of `Option`s into `Option[Vector]`; `None` if any element is `None`.
 *
 *  @param xs the iterable of options to sequence
 *  @return `Some(vector of unwrapped values)` when all elements are `Some`, `None` otherwise
 */
private def allOpt[A](xs: Iterable[Option[A]]): Option[Vector[A]] =
  val seq = xs.toVector
  val flat = seq.flatten
  if flat.size == seq.size then Some(flat) else None


/** Gaussian elimination with partial pivoting on `Double` arrays.
 *
 *  Solves `A * x = b` in-place on an augmented `[A|b]` matrix.
 *  Early exits use `boundary`/`break` (a `return` inside a `for` body desugars to
 *  a lambda in Scala 3, which makes non-local returns deprecated).
 *
 *  @param A the n-by-n coefficient matrix (as a vector of rows)
 *  @param b the right-hand-side vector of length n
 *  @return `Some(solution vector)` on success; `None` when any pivot is near-zero
 */
private def gaussDense(A: Vector[Vector[Double]], b: Vector[Double]): Option[Vector[Double]] =
  val n   = A.size
  val aug = Array.tabulate(n)(i => A(i).toArray :+ b(i))

  boundary:
    for pivot <- 0 until n do
      // Partial pivoting: swap the row with the largest absolute value in this column
      val maxRow = (pivot until n).maxBy(r => math.abs(aug(r)(pivot)))
      val tmp = aug(pivot); aug(pivot) = aug(maxRow); aug(maxRow) = tmp
      if math.abs(aug(pivot)(pivot)) < 1e-12 then break(None)
      for row <- pivot + 1 until n do
        val factor = aug(row)(pivot) / aug(pivot)(pivot)
        for col <- pivot to n do aug(row)(col) -= factor * aug(pivot)(col)

    // Back substitution
    val x = new Array[Double](n)
    for i <- n - 1 to 0 by -1 do
      x(i) = aug(i)(n)
      for j <- i + 1 until n do x(i) -= aug(i)(j) * x(j)
      x(i) /= aug(i)(i)
      if x(i).isNaN || x(i).isInfinite then break(None)
    Some(x.toVector)


/** Symbolic Gaussian elimination using `_Expression` arithmetic and `simplifyFully`.
 *
 *  Early exits via `boundary`/`break` like [[gaussDense]].
 *
 *  @param A the n-by-n symbolic coefficient matrix
 *  @param b the symbolic right-hand-side vector of length n
 *  @return `Some(solution vector of expressions)` on success;
 *          `None` when any pivot simplifies to `_Number(0)` (structurally singular)
 */
private def gaussSymbolic(A: Vector[Vector[_Expression]], b: Vector[_Expression]): Option[Vector[_Expression]] =
  val n   = A.size
  val aug = Array.tabulate(n)(i => (A(i) :+ b(i)).toArray)

  boundary:
    for pivot <- 0 until n do
      val pivVal = simplifyFully(aug(pivot)(pivot))
      if pivVal == _Number(0) then break(None)
      for row <- pivot + 1 until n do
        val factor = simplifyFully(Ratio(aug(row)(pivot), pivVal))
        for col <- pivot to n do
          aug(row)(col) = simplifyFully(
            Sum(aug(row)(col), Product(_Number(-1), Product(factor, aug(pivot)(col))))
          )

    // Back substitution
    val x = new Array[_Expression](n)
    for i <- n - 1 to 0 by -1 do
      var rhs: _Expression = aug(i)(n)
      for j <- i + 1 until n do
        rhs = simplifyFully(Sum(rhs, Product(_Number(-1), Product(aug(i)(j), x(j)))))
      val denom = simplifyFully(aug(i)(i))
      if denom == _Number(0) then break(None)
      x(i) = simplifyFully(Ratio(rhs, denom))
    Some(x.toVector)

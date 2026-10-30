package it.grypho.scala.leonardo
package equation

import core.*
import scalar.*

import scala.util.boundary, boundary.break


/** Linear system solver for `m` equations in `n` unknowns, `m >= n` (F_0242 for `m > n`).
 *
 *  [[solveSystem]] returns the unique solution as a list of `v = expr` equations, or `None`
 *  when there are fewer equations than unknowns, the system is nonlinear, it does not determine
 *  every unknown, or it is inconsistent; [[solveSystemDetailed]] says which.
 *
 *  **More equations than unknowns** is how "is `b` a combination of these vectors?" is asked,
 *  so a consistent over-determined system is solved.  Exactly: incremental elimination over the
 *  rationals, or over `Q(√d)` when the coefficients hold surds of one radical (F_0247), which names
 *  the first equation contradicting the ones before it.  Numerically: the least-squares solution
 *  by QR (never the normal equations, which square the condition number), after a rank test that
 *  declines nearly dependent columns (F_0245), accepted only when every equation holds within
 *  `0.5·10⁻ᵖ` of **its own terms** -- an absolute tolerance would pass any inconsistent system
 *  scaled down far enough (the units rule).  A
 *  symbolic over-determined system declines: deciding its consistency means deciding whether an
 *  expression vanishes.
 *
 *  Coefficient extraction uses `scalar.collect` (the same prerequisite used by
 *  [[solve]]'s linear and quadratic tiers): for each equation `i` and variable `j`,
 *  `collect(lhs_i - rhs_i, v_j)` yields the polynomial coefficients in `v_j`; for a
 *  linear system the result has length <= 2 (length > 2 means nonlinear -> `None`).
 *  A linearity guard rejects any coefficient `A_ij` that depends on a solve variable.
 *  The constant term `b_i` is `-(lhs_i - rhs_i evaluated with all v_j = 0)`.
 *
 *  **Dense path** (all `A_ij` and `b_i` fold to `_Number` in `env`): Gaussian
 *  elimination with partial pivoting on `Double` arrays; returns `None` when a pivot
 *  drops below `PivotTolerance` **times the largest entry of `A`** (singular or
 *  near-singular). Relative, not absolute (F_0075): an absolute `1e-12` made the verdict
 *  depend on the units of the coefficients, declining `1e-13*x + 1e-13*y = 2e-13` while
 *  solving the same system scaled to `x + y = 2` -- the 2.11 rule.
 *
 *  **Symbolic path** (some coefficient or constant stays symbolic): row reduction
 *  using `Sum`/`Product`/`Ratio`/`_Expression` arithmetic with `simplifyFully` at
 *  every step; returns `None` when any diagonal pivot simplifies to `_Number(0)`.
 *
 *  Solution right-hand sides are folded through `env` so bound symbols produce
 *  numeric answers (same convention as [[solve]]).
 */

/** Returns the unique solution of a linear system, or `None`.
 *
 *  @param equations the equations, at least as many as `variables`
 *  @param variables the solve variables
 *  @param env       the evaluation environment (variable bindings + precision)
 *  @return `Some(list of v_i = expr_i)` on success; `None` when under-determined, nonlinear,
 *          singular or inconsistent ([[solveSystemDetailed]] says which)
 */
def solveSystem(
    equations: List[_Equation],
    variables: List[_Variable],
    env:       Environment = new Environment()
): Option[List[_Equation]] =
  solveSystemDetailed(equations, variables, env).toOption

/** [[solveSystem]] with the reason for a decline (F_0242), which the REPL appends as a note.
 *
 *  @param equations the equations, at least as many as `variables`
 *  @param variables the solve variables
 *  @param env       the evaluation environment
 *  @return the solution, or why there is none
 */
private[leonardo] def solveSystemDetailed(
    equations: List[_Equation],
    variables: List[_Variable],
    env:       Environment
): Either[String, List[_Equation]] =
  val m = equations.size
  val n = variables.size
  if m == 0 || n == 0 then Left("no equations or no unknowns")
  else if m < n then Left(s"under-determined: ${counted(m, "equation")} for ${counted(n, "unknown")}")
  else solveDetermined(equations, variables, env)

/** `k` and a noun, made plural when `k` is not 1. */
private def counted(k: Int, noun: String): String = if k == 1 then s"1 $noun" else s"$k ${noun}s"

private val NotLinear       = "not linear in the unknowns"
private val NoUniqueExact   = "no unique solution: the equations do not determine every unknown"
private val NoUniqueDense   = "no unique solution: the coefficients are linearly dependent"

/** Why an over-determined numeric system with nearly dependent columns is declined (F_0245). */
private def nearlyDependent(rank: Int, n: Int): String =
  s"no unique solution: the coefficients are nearly dependent (numerical rank $rank of $n at the session precision), " +
    "so a least-squares fit cannot tell a consistent system from an inconsistent one"
private val SymbolicOverdetermined =
  "over-determined with symbolic coefficients: more equations than unknowns are solved only when every coefficient is a number"

/** The body of [[solveSystemDetailed]] for `m >= n` equations. */
private def solveDetermined(
    equations: List[_Equation],
    variables: List[_Variable],
    env:       Environment
): Either[String, List[_Equation]] =
  val m = equations.size
  val n = variables.size

  // Normalise: move everything to the left side (expr = 0).  The -1 and the 0 the unknowns are
  // bound to are invented constants, so they join the system's own tier (F_0166): inexact ones
  // demoted an exact system before elimination began.
  val system   = matrix._Matrix(1, m, equations.toVector)
  val minusOne = _Rational.literalLike(-1, system)
  val zero     = _Rational.literalLike(0, system)
  val exprs: Vector[_Expression] =
    equations.map(eq => simplifyFully(Sum(eq.lhs, Product(minusOne, eq.rhs)))).toVector

  // Extract coefficient matrix A[i][j] via collect(expr_i, v_j)
  // `collect` invents the unit coefficient of a bare `v` in the tier IT reads off its own
  // expression, and `u` alone holds nothing exact, so `u = 0` in an exact system had an inexact
  // coefficient and sent the whole system down the `Double` path (F_0242).  Adding the system's
  // exact zero carries the system's tier into the call; an inexact system is collected as before.
  def inTier(expr: _Expression): _Expression = zero match
    case _: _Rational => Sum(expr, zero)
    case _            => expr
  val coeffOpt: Option[Vector[Vector[_Expression]]] = allOpt(exprs.map { expr =>
    allOpt(variables.map { v =>
      collect(inTier(expr), v) match
        case Some(cs) if cs.size == 1 => Some(zero)          // v absent: an invented zero, in tier
        case Some(cs) if cs.size == 2 => Some(cs(1))         // linear in v
        case Some(_)                  => None                 // degree >= 2
        case None                     => None                 // non-polynomial
    })
  })

  val A: Vector[Vector[_Expression]] = coeffOpt match
    case None    => return Left(NotLinear)
    case Some(c) => c

  // Linearity guard: no A_ij may depend on any solve variable
  if A.exists(_.exists(c => variables.exists(v => dependsOn(c, v)))) then return Left(NotLinear)

  // Constant vector b[i] = -(expr_i with all solve variables bound to 0)
  val zeroEnv = variables.foldLeft(env)((e, v) => e.withBinding(v.variable, zero))
  val b: Vector[_Expression] =
    exprs.map(expr => simplifyFully(Product(minusOne, expr.eval(zeroEnv).toExpression)))

  // Exact path (F_0166): every coefficient and constant exact, so elimination over an exact
  // field -- before the dense path, whose widening `_Number` reading would take a `_Rational`
  // as the Double it approximates.  No tolerance decides anything here (F_0242: incremental, so
  // a contradiction is named where it arises).  Surds of one radical are eliminated over Q(√d),
  // the field the matrix kernels use (F_0247): read each as its own working-precision rational,
  // two equal surds from different expressions differed in the last digit and a consistent
  // system was reported contradictory.
  def solveOver[T](f: ExactField[T], cells: Vector[T], isZero: T => Boolean): Either[String, List[_Equation]] =
    val rows = Vector.tabulate(m)(i => cells.slice(i * n, (i + 1) * n))
    exactSolve(f, rows, cells.drop(m * n), equations, n, isZero).map(xs => variables.zip(xs.map(f.toValue)).map(_Equation(_, _)))

  val exactValues = allOpt((A.flatten ++ b).map(_.eval(env).toOption))
  val policy      = env.rationalPolicy
  val exactAnswer = exactValues.filter(_.forall(_Surd.isExact)).flatMap { vs =>
    fieldCells(vs) match
      case Some(FieldCells.OverQ(c))       => Some(solveOver(RationalField(policy), c, (r: _Rational) => r.isZero))
      case Some(FieldCells.OverSurd(d, c)) => Some(solveOver(SurdField(d, policy), c, (q: Quad) => q.a.isZero && q.b.isZero))
      case None =>
        // Two radicals that do not combine (√2 beside √3): the cells are read as rationals at the
        // working precision, as before F_0231, and a value is zero when it agrees with 0 at that
        // precision (F_0177) -- an exact zero test would read the reading's last digit as a
        // contradiction.
        val rs = vs.flatMap(_Surd.exactOf(_, env.workingPrecision))
        Option.when(rs.size == vs.size)(
          solveOver(RationalField(policy), rs, (r: _Rational) => agreeToWorkingPrecision(r, _Rational.Zero, env)))
  }
  exactAnswer match
    case Some(answer) => return answer
    case None         => ()

  // Dense path: try to fold every coefficient and constant to a Double
  def toDouble(e: _Expression): Option[Double] = e.eval(env) match
    case Right(_Number(d)) => Some(d)
    case _                 => None

  val denseA: Option[Vector[Vector[Double]]] = allOpt(A.map(row => allOpt(row.map(toDouble))))
  val denseB: Option[Vector[Double]]         = allOpt(b.map(toDouble))

  (denseA, denseB) match
    case (Some(da), Some(db)) =>
      val solved = if m == n then gaussDense(da, db).toRight(NoUniqueDense) else leastSquaresConsistent(da, db, equations, env)
      solved.map(xs => variables.zip(xs).map((v, x) => _Equation(v, _Number(x))))
    case _ if m > n => Left(SymbolicOverdetermined)
    case _ =>
      gaussSymbolic(A, b, minusOne).toRight(NoUniqueExact).map(xs =>
        variables.zip(xs).map((v, x) => _Equation(v, simplifyFully(x).eval(env).toExpression))
      )


/** Exact elimination of `[A | b]` over a field, one equation at a time (F_0242; any exact field
 *  since F_0247).
 *
 *  The basis is kept in reduced row-echelon form, so each new row is reduced against it in one
 *  pass.  A row that reduces to `0 = c` with `c != 0` **contradicts the equations before it**,
 *  which is the equation named; one that reduces to `0 = 0` is redundant.  At the end fewer pivots
 *  than unknowns means a consistent system that does not determine them all.
 *
 *  @tparam T        the field's element type
 *  @param f         the field
 *  @param a         the `m × n` coefficients
 *  @param b         the `m` constants
 *  @param equations the equations, for the message
 *  @param n         the number of unknowns
 *  @param isZero    what counts as zero: exact, or agreement with `0` at the working precision
 *                   when the cells are readings of radicals that do not combine
 *  @return the solution, or why there is none
 */
private def exactSolve[T](f: ExactField[T], a: Vector[Vector[T]], b: Vector[T], equations: List[_Equation],
                          n: Int, isZero: T => Boolean): Either[String, Vector[T]] =
  def eliminate(target: Vector[T], unit: Vector[T], p: Int): Vector[T] =
    val c = target(p)
    if f.isZero(c) then target else target.lazyZip(unit).map((x, y) => f.sub(x, f.mul(c, y)))

  val rows = a.lazyZip(b).map(_ :+ _)
  // Each basis row is its pivot column and the row itself, the pivot entry 1.
  rows.zipWithIndex.foldLeft[Either[String, Vector[(Int, Vector[T])]]](Right(Vector.empty)) {
    case (failed @ Left(_), _) => failed
    case (Right(basis), (row, i)) =>
      val r = basis.foldLeft(row)((acc, pr) => eliminate(acc, pr._2, pr._1))
      r.take(n).indexWhere(c => !isZero(c)) match
        case -1 if isZero(r(n)) => Right(basis)
        case -1                 => Left(s"inconsistent: equation ${i + 1} (${equations(i)}) contradicts the equations before it")
        case p => f.div(f.one, r(p)) match
          case None      => Left(NoUniqueExact)   // unreachable: r(p) is not zero
          case Some(inv) =>
            val unit = r.map(f.mul(_, inv))
            Right(basis.map((q, br) => (q, eliminate(br, unit, p))) :+ (p, unit))
  }.flatMap { basis =>
    if basis.size < n then Left(NoUniqueExact)
    else Right(Vector.tabulate(n)(j => basis.collectFirst { case (`j`, r) => r(n) }.getOrElse(f.zero)))
  }

/** The solution of `m > n` numeric equations, when they are consistent (F_0242).
 *
 *  The least-squares solution by QR (`R·x = Qᵀ·b`), then **every equation checked against its
 *  own terms**: equation `i` holds when `|aᵢ·x − bᵢ| <= 0.5·10⁻ᵖ·max(Σⱼ|aᵢⱼ·xⱼ|, |bᵢ|)`.
 *  Scaling an equation or rescaling an unknown leaves that verdict alone, which an absolute
 *  tolerance would not: `1e-8` times an inconsistent system would pass.  A residual within the
 *  solve's own rounding level -- `10·m·κ·ε` of the row's size at the solution's scale -- also
 *  counts as satisfied, since a component whose true value is zero comes back as rounding noise
 *  and would otherwise miss its own term by 100%.  `qrDecompose`'s `None` is the rank test (2.11).
 *
 *  **Nearly dependent columns are declined first** (F_0245), at the relative tolerance `rank`
 *  uses (`svd` + `numericalRank`, the `arx` arrangement): there the least-squares terms are huge
 *  and cancel, so a residual the size of an inconsistency passes as small against them --
 *  `x + y = 1`, `x + 1.000001·y = 1`, `x + y = 1.001` was answered `x ≈ 501, y ≈ −500`, which
 *  satisfies none of the three.  QR's own rank test refuses only exact dependence.
 *
 *  @param a         the `m × n` coefficients
 *  @param b         the `m` constants
 *  @param equations the equations, for the message
 *  @param env       the environment, whose precision sets the tolerances and whose limits the SVD's work
 *  @return the solution, or why there is none
 */
private def leastSquaresConsistent(a: Vector[Vector[Double]], b: Vector[Double], equations: List[_Equation],
                                   env: Environment): Either[String, Vector[Double]] =
  val m = a.size
  val n = a.head.size
  val precision = env.precision
  val design    = _MatrixValue(m, n, a.flatten.toArray)
  val deficient = design.svd(env.limits)
    .map((_, sigma, _) => matrix.numericalRank(sigma, matrix.rankTolerance(env))).filter(_ < n)
  def fit: Either[String, Vector[Double]] = design.qrDecompose match
    case None => Left(NoUniqueDense)
    case Some((q, r)) =>
      val qtb = Vector.tabulate(n)(j => (0 until m).map(i => q(i, j) * b(i)).sum)
      val x   = (n - 1 to 0 by -1).foldLeft(Vector.fill(n)(0.0)) { (acc, i) =>
        acc.updated(i, (qtb(i) - (i + 1 until n).map(j => r(i, j) * acc(j)).sum) / r(i, i))
      }
      if !x.forall(_.isFinite) then Left(NoUniqueDense)
      else
        val tol   = 0.5 * math.pow(10, -precision)
        val xMax  = x.map(math.abs).max
        val diag  = (0 until n).map(i => math.abs(r(i, i)))
        // The solve's own rounding level: a component whose true value is 0 comes back as
        // ~kappa*eps*|x| (u = 5e-16 for `u = 0` beside `u + v = 1.5`), which is a 100% miss of its
        // own term.  A residual at that level is rounding, not inconsistency.  kappa is estimated
        // from R's diagonal, and the factor 10*max(m, n) is QR's own (`QrDependenceFactor`, F_0091).
        val kappa = diag.max / diag.min
        val floor = 10.0 * m * kappa * Math.ulp(1.0)
        val misses = a.indices.map { i =>
          val terms    = a(i).lazyZip(x).map(_ * _)
          val residual = terms.sum - b(i)
          val scale    = math.max(terms.map(math.abs).sum, math.abs(b(i)))
          val noise    = floor * (a(i).map(math.abs).sum * xMax + math.abs(b(i)))
          val relative = if math.abs(residual) <= noise || scale == 0.0 then 0.0 else math.abs(residual) / scale
          (i, residual, relative)
        }
        val (worst, residual, relative) = misses.maxBy(_._3)
        if relative <= tol then Right(x)
        else
          val off = BigDecimal(residual.abs).round(new java.math.MathContext(3)).toString
          Left(s"inconsistent: no values satisfy every equation; at the least-squares fit equation ${worst + 1} (${equations(worst)}) is off by $off")
  deficient.fold(fit)(rk => Left(nearlyDependent(rk, n)))


/** Sequences an iterable of `Option`s into `Option[Vector]`; `None` if any element is `None`.
 *
 *  @param xs the iterable of options to sequence
 *  @return `Some(vector of unwrapped values)` when all elements are `Some`, `None` otherwise
 */
private def allOpt[A](xs: Iterable[Option[A]]): Option[Vector[A]] =
  val seq = xs.toVector
  val flat = seq.flatten
  if flat.size == seq.size then Some(flat) else None


/** The pivot threshold of [[gaussDense]], as a fraction of the largest entry of `A`.
 *
 *  Relative on purpose (F_0075): a pivot is "zero" only compared with the matrix it came
 *  from, so scaling every coefficient by `k` -- a change of units -- cannot move the verdict
 *  (the 2.11 rule, and `_MatrixValue.CholeskyTolerance`'s arrangement). `1e-12` keeps the
 *  previous threshold for a matrix of unit scale, where every existing test lives. */
private val PivotTolerance: Double = 1e-12

/** Gaussian elimination with partial pivoting on `Double` arrays.
 *
 *  Solves `A * x = b` in-place on an augmented `[A|b]` matrix.
 *  Early exits use `boundary`/`break` (a `return` inside a `for` body desugars to
 *  a lambda in Scala 3, which makes non-local returns deprecated).
 *
 *  @param A the n-by-n coefficient matrix (as a vector of rows)
 *  @param b the right-hand-side vector of length n
 *  @return `Some(solution vector)` on success; `None` when any pivot is near-zero
 *          relative to the largest entry of `A`, or `A` is all zeros
 */
private def gaussDense(A: Vector[Vector[Double]], b: Vector[Double]): Option[Vector[Double]] =
  val n   = A.size
  val aug = Array.tabulate(n)(i => A(i).toArray :+ b(i))
  val scale = A.iterator.flatten.foldLeft(0.0)((m, v) => math.max(m, math.abs(v)))
  val tol   = PivotTolerance * scale

  boundary:
    if scale == 0.0 || !scale.isFinite then break(None)
    for pivot <- 0 until n do
      // Partial pivoting: swap the row with the largest absolute value in this column
      val maxRow = (pivot until n).maxBy(r => math.abs(aug(r)(pivot)))
      val tmp = aug(pivot); aug(pivot) = aug(maxRow); aug(maxRow) = tmp
      if !(math.abs(aug(pivot)(pivot)) > tol) then break(None)
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
 *  @param minusOne the `-1` of the system's tier, so an exact system stays exact (F_0167)
 *  @return `Some(solution vector of expressions)` on success;
 *          `None` when any pivot simplifies to `_Number(0)` (structurally singular)
 */
private def gaussSymbolic(A: Vector[Vector[_Expression]], b: Vector[_Expression], minusOne: _Expression): Option[Vector[_Expression]] =
  val n   = A.size
  val aug = Array.tabulate(n)(i => (A(i) :+ b(i)).toArray)

  boundary:
    for pivot <- 0 until n do
      // A zero pivot is swapped with the first row below that has a non-zero entry in the column,
      // as the dense path does; declining outright refused `[[y = a, x + y = 3]]` (F_0205).
      if isZeroLiteral(simplifyFully(aug(pivot)(pivot))) then
        (pivot + 1 until n).find(r => !isZeroLiteral(simplifyFully(aug(r)(pivot)))).foreach { r =>
          val tmp = aug(pivot); aug(pivot) = aug(r); aug(r) = tmp
        }
      val pivVal = simplifyFully(aug(pivot)(pivot))
      if isZeroLiteral(pivVal) then break(None)   // by value: an exact zero is a zero (F_0073)
      for row <- pivot + 1 until n do
        val factor = simplifyFully(Ratio(aug(row)(pivot), pivVal))
        for col <- pivot to n do
          aug(row)(col) = simplifyFully(
            Sum(aug(row)(col), Product(minusOne, Product(factor, aug(pivot)(col))))
          )

    // Back substitution
    val x = new Array[_Expression](n)
    for i <- n - 1 to 0 by -1 do
      var rhs: _Expression = aug(i)(n)
      for j <- i + 1 until n do
        rhs = simplifyFully(Sum(rhs, Product(minusOne, Product(aug(i)(j), x(j)))))
      val denom = simplifyFully(aug(i)(i))
      if isZeroLiteral(denom) then break(None)
      x(i) = simplifyFully(Ratio(rhs, denom))
    Some(x.toVector)

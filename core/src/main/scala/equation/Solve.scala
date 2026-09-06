package it.grypho.scala.leonardo
package equation

import core.*
import scalar.*
import matrix.*

import scala.annotation.tailrec


/** Multi-tier equation solver.
 *
 *  [[solve]] returns the solutions of `eq` in `v` as a list of `[[_Equation]]`s in
 *  the shape `v = expr`.  A solution set does not fit `eval`'s
 *  `Either[_Expression, _Value]`, so like `derive`/`integrate` this is a
 *  package-level algorithm (the [[_Solve]] node presents its result at eval time).
 *
 *  Tiers, tried in order (most specific first):
 *  1. **Matrix unknown** (`solveMatrixUnknown`) -- if `v` names a matrix-valued
 *     unknown, recognises linear shapes `A*v = B`, `v*A = B`, `A*v*D = B`,
 *     `A*v + C = B`, and `v = B` via the inverse kernel (dense) or cofactor
 *     expansion (symbolic, capped at 6x6); general Sylvester/Lyapunov shapes via
 *     Kronecker vectorization (capped at a 20x20 unknown).
 *  2. **Scalar v inside a matrix equation** (`solveElementwise`) -- when both sides
 *     reduce to matrices, decomposes per-cell, pools candidates, and keeps those
 *     satisfying the whole equation.
 *  3. **Scalar linear** -- degree-1 polynomial via `scalar.collect`: `v = -c0/c1`.
 *  4. **Scalar quadratic** -- degree-2 polynomial: discriminant; 0/1/2 real roots for
 *     numeric coefficients, the two +/-sqrt(delta) closed forms for symbolic ones.
 *  5. **Numeric bisection** -- compiles `lhs - rhs` to a `Double => Double` closure,
 *     scans for sign changes over `[-100, 100]`, and refines by bisection (up to
 *     `MaxNumericRoots` roots).  Returns `Nil` when the expression is not compilable.
 *
 *  Each solution's right-hand side is folded through `env`, so bound coefficients
 *  produce numeric answers: with `a := 2`, `solve(a*x = 4, x)` yields `x = 2`.
 */

/** Search range for the numeric bisection tier. */
private val SearchLo         = -100.0
/** Search range for the numeric bisection tier. */
private val SearchHi         = 100.0
/** Number of uniform samples in `[SearchLo, SearchHi]` for sign-change detection. */
private val SearchSamples    = 10000
/** Maximum number of roots returned by the numeric tier (periodic functions have infinitely many). */
private val MaxNumericRoots  = 8
/** Bisection refinement iterations per detected sign-change interval. */
private val BisectIterations = 200

/** Returns the solutions of `eq` in `v` as a list of `v = expr` equations.
 *
 *  An empty list means no solution was found (or the equation does not constrain `v`).
 *  The caller ([[_Solve]].eval) interprets the list: empty -> stays symbolic, one ->
 *  single equation, many -> a row-vector `_Matrix` of equations.
 *
 *  @param eq  the equation to solve
 *  @param v   the variable to solve for
 *  @param env the evaluation environment (variable bindings + precision)
 *  @return    the solution list, possibly empty
 */
def solve(eq: _Equation, v: _Variable, env: Environment = new Environment()): List[_Equation] =
  solveMatrixUnknown(eq, v, env) match
    case Some(solution) => solution
    case None =>
      matrixSides(eq, env) match
        case Some((lhs, rhs)) =>
          if lhs.rows == rhs.rows && lhs.cols == rhs.cols then solveElementwise(eq, lhs, rhs, v, env)
          else Nil
        case None => solveScalar(eq, v, env)

/** Scalar linear/quadratic/numeric tiers for a non-matrix equation. */
private def solveScalar(eq: _Equation, v: _Variable, env: Environment): List[_Equation] =
  // The -1 must join the equation's own tier.  A hard-coded `_Number(-1)` here would demote
  // an exactly-stated equation through float contagion before the solver ever ran, and the
  // exact quadratic branch below would then never see an exact coefficient (issue 4.N).
  val minusOne   = _Rational.literalLike(-1, eq)
  val difference = Sum(eq.lhs, Product(minusOne, eq.rhs))

  val roots: List[_Expression] = collect(difference, v) match
    case Some(cs) if cs.size == 1 => Nil   // constant in v: nothing to solve for
    case Some(cs) if cs.size == 2 =>
      List(simplifyFully(Ratio(Product(minusOne, cs(0)), cs(1))))
    case Some(cs) if cs.size == 3 => quadraticRoots(cs(0), cs(1), cs(2), env)
    case _                        => numericRoots(difference, v, env)

  roots.map(r => _Equation(v, r.eval(env).toExpression))

/** Solves for a scalar `v` inside a matrix equation by element-wise decomposition.
 *
 *  Pools roots from each cell equation; keeps candidates that satisfy the whole
 *  matrix equation under `env`.  Symbolic roots that do not bind to a concrete value
 *  are conservatively skipped (no false positives).
 */
private def solveElementwise(eq: _Equation, lhs: _Matrix, rhs: _Matrix, v: _Variable, env: Environment): List[_Equation] =
  val elementEqs: List[_Equation] = lhs.elems.toList.zip(rhs.elems).map((l, r) => _Equation(l, r))
  val candidates: List[_Value]    =
    elementEqs.flatMap(e => solve(e, v, env)).flatMap(sol => sol.rhs.eval(env).toOption)
  val verified = candidates.filter(root => eq.eval(env.withBinding(v.variable, root)).contains(_Bool(true)))
  dedupeRoots(verified, env).map(root => _Equation(v, root))

/** Returns both sides re-inflated to `_Matrix` literals, or `None` when either is not matrix-shaped. */
private def matrixSides(eq: _Equation, env: Environment): Option[(_Matrix, _Matrix)] =
  (asMatrix(eq.lhs.eval(env)), asMatrix(eq.rhs.eval(env))) match
    case (Some(l), Some(r)) => Some((l, r))
    case _                  => None

/** Converts an already-evaluated result to a `_Matrix` literal, or `None`. */
private def asMatrix(r: Either[_Expression, _Value]): Option[_Matrix] = r match
  case Left(m: _Matrix)        => Some(m)
  case Right(mv: _MatrixValue) => Some(_Matrix.fromValue(mv))
  case _                       => None

/** Tries to solve for a MATRIX-valued unknown `v` in a linear matrix equation.
 *
 *  Recognises the shapes `A*v = B`, `v*A = B`, `A*v*D = B`, `A*v + C = B`, and
 *  `v = B`.  First tries the single-inverse (`linearMatrixSolve`) tier; then the
 *  Kronecker vectorization (`vectorizedMatrixSolve`) tier for multi-term shapes
 *  (Sylvester, Lyapunov, etc.).  `Some(Nil)` when the shape is recognised but
 *  singular/non-conforming (no solution); `None` when `v` is not a matrix unknown
 *  (so the scalar tiers still run).
 */
private def solveMatrixUnknown(eq: _Equation, v: _Variable, env: Environment): Option[List[_Equation]] =
  for
    (withV, constant) <- sideWith(v, eq)
    b                 <- asMatrixExpr(constant.eval(env))
    solution          <- linearMatrixSolve(withV, b, v, env)
                           .orElse(vectorizedMatrixSolve(withV, b, v, env))
  yield solution

/** Recognises and solves single-inverse matrix-unknown shapes.
 *
 *  Shapes handled (most specific first):
 *  - affine term: `A*X + C = B` -> peel `C`, recurse on `A*X = B - C`
 *  - two-sided:   `A*X*D = B`   -> `X = A^-1 * B * D^-1`
 *  - one-sided:   `A*X = B`     -> `X = A^-1 * B`; `X*A = B` -> `X = B * A^-1`
 *  - bare:        `X = B`       -> `X = B`
 *
 *  Both the scalar `Sum`/`Product` nodes and the matrix `MatSum`/`MatProduct` nodes
 *  are accepted (a bound-variable operand may parse as a scalar node).
 */
@tailrec
private def linearMatrixSolve(withV: _Expression, b: _Expression, v: _Variable, env: Environment): Option[List[_Equation]] =
  withV match
    // Affine: peel a v-free additive operand across (A*X + C = B -> A*X = B - C).
    case MatSum(l, r) if !dependsOn(l, v) => linearMatrixSolve(r, matSub(b, l), v, env)
    case MatSum(l, r) if !dependsOn(r, v) => linearMatrixSolve(l, matSub(b, r), v, env)
    case Sum(l, r)    if !dependsOn(l, v) => linearMatrixSolve(r, matSub(b, l), v, env)
    case Sum(l, r)    if !dependsOn(r, v) => linearMatrixSolve(l, matSub(b, r), v, env)
    // Two-sided product: A*X*D = B -> X = A^-1 * B * D^-1.
    case MatProduct(MatProduct(a, m), d) if m == v => twoSidedSolve(a, d, b, v, env)
    case MatProduct(a, MatProduct(m, d)) if m == v => twoSidedSolve(a, d, b, v, env)
    case Product(Product(a, m), d)       if m == v => twoSidedSolve(a, d, b, v, env)
    case Product(a, Product(m, d))       if m == v => twoSidedSolve(a, d, b, v, env)
    // One-sided product: A*X = B -> X = A^-1 * B ;  X*A = B -> X = B * A^-1.
    case Product(a, r)    if r == v => matrixDivide(a, b, v, env, aOnLeft = true)
    case MatProduct(a, r) if r == v => matrixDivide(a, b, v, env, aOnLeft = true)
    case Product(l, a)    if l == v => matrixDivide(a, b, v, env, aOnLeft = false)
    case MatProduct(l, a) if l == v => matrixDivide(a, b, v, env, aOnLeft = false)
    case r                if r == v =>                                   // v = B (or B - C)
      val sol = b.eval(env) match
        case Right(value) => value
        case Left(expr)   => simplifyFully(expr)
      Some(List(_Equation(v, sol)))
    case _                          => None

/** Solves `A*v = B` (aOnLeft=true) or `v*A = B` (aOnLeft=false) via matrix inverse.
 *
 *  Dense fast path: both `A` and `B` reduce to `_MatrixValue` -- uses the LU
 *  inverse kernel; `Some(Nil)` when singular/non-square or dimensions do not conform.
 *  Symbolic path: `A` or `B` is a `_Matrix` literal -- uses the `Inverse` node
 *  (cofactor expansion, capped at `MaxSymbolicDim`); returns `None` when the inverse
 *  cannot be determined so the element-wise / scalar tiers still get a chance.
 */
private def matrixDivide(a: _Expression, b: _Expression, v: _Variable, env: Environment, aOnLeft: Boolean): Option[List[_Equation]] =
  (asMatrixValue(a.eval(env)), asMatrixValue(b.eval(env))) match
    case (Some(aMatrix), Some(bMatrix)) =>
      val solution = aMatrix.inverse.flatMap(aInv =>
        if aOnLeft then matMul(aInv, bMatrix) else matMul(bMatrix, aInv))
      Some(solution.map(m => _Equation(v, m)).toList)
    case _ =>
      Inverse(a).eval(env) match
        case Left(_: Inverse) => None   // singular, non-square, or above symbolic cap
        case aInvResult =>
          val product =
            if aOnLeft then MatProduct(aInvResult.toExpression, b).eval(env)
            else MatProduct(b, aInvResult.toExpression).eval(env)
          product match
            case Left(_: MatProduct) => None   // dimensions don't conform -- fall through
            case Left(expr)          => Some(List(_Equation(v, simplifyFully(expr))))
            case Right(mv)           => Some(List(_Equation(v, mv)))

/** Solves `A*X*D = B` -> `X = A^-1 * B * D^-1` (two-sided inversion).
 *
 *  Both flanks are inverted via the `Inverse` node (dense LU kernel for concrete
 *  operands, cofactor expansion for symbolic ones capped at `MaxSymbolicDim`).
 *  Always answers `Some`: `Some(Nil)` when either inverse cannot be determined
 *  (singular / non-square / above the cofactor cap / non-conforming dimensions).
 *  Never falls through to the scalar tiers since the shape has been recognised.
 */
private def twoSidedSolve(a: _Expression, d: _Expression, b: _Expression, v: _Variable, env: Environment): Option[List[_Equation]] =
  Some(finalizeSolution(MatProduct(MatProduct(Inverse(a), b), Inverse(d)).eval(env), v).getOrElse(Nil))

/** Returns `B - C` as a matrix expression for peeling an affine term to the constant side. */
private def matSub(b: _Expression, c: _Expression): _Expression =
  MatSum(b, MatScale(_Number(-1), c))

/** Returns `Some(List(v = <matrix>))` when `result` reduced to a matrix shape, `None` otherwise. */
private def finalizeSolution(result: Either[_Expression, _Value], v: _Variable): Option[List[_Equation]] =
  result match
    case Right(mv: _MatrixValue) => Some(List(_Equation(v, mv)))
    case Left(m: _Matrix)        => Some(List(_Equation(v, simplifyFully(m))))
    case _                       => None

// ── Vectorized tier: general linear matrix equations (Sylvester shapes) ──────
//
// When the unknown matrix v appears in SEVERAL additive terms -- A*v + v*B = C
// (Sylvester), A*v + v*A^T = C (Lyapunov), k*v + A*v*D = C, etc. -- no single-inverse
// closed form exists. Each v-term is classified as s * L * v * R (absent L/R = identity,
// s a scalar factor) and the equation is vectorized with the Kronecker identity
//   vec(L*v*R) = (R^T ⊗ L) * vec(v),
// assembling the (p*q) x (p*q) dense system M * vec(v) = vec(C'), then solved via
// the inverse kernel and reshaped back with unvec. Dense-only.

/** Dimension cap for the Kronecker system (`MaxVectorizedSize = p*q`; up to a 20x20 unknown). */
private val MaxVectorizedSize = 400

/** One additive term linear in `v`: `scale * left * v * right` (absent side = identity).
 *
 *  @param scale scalar multiplier
 *  @param left  optional left matrix coefficient (`None` = identity)
 *  @param right optional right matrix coefficient (`None` = identity)
 */
private final case class LinearTerm(scale: Double, left: Option[_MatrixValue], right: Option[_MatrixValue])

/** Flattens a `Sum`/`MatSum` tree into a list of additive operands.
 *  Uses a worklist accumulator to guarantee O(1) stack depth regardless of tree height.
 */
@annotation.tailrec
private def flattenSum(pending: List[_Expression], acc: List[_Expression]): List[_Expression] =
  pending match
    case Nil                  => acc.reverse
    case Sum(a, b)    :: rest => flattenSum(a :: b :: rest, acc)
    case MatSum(a, b) :: rest => flattenSum(a :: b :: rest, acc)
    case other        :: rest => flattenSum(rest, other :: acc)

private def flattenSum(e: _Expression): List[_Expression] = flattenSum(List(e), Nil)

/** Classifies one additive term as `s * L * v * R`; `None` when not linear in `v` or a
 *  coefficient does not reduce to a dense matrix / number under `env`.
 */
private def classifyTerm(t: _Expression, v: _Variable, env: Environment): Option[LinearTerm] = t match
  case x if x == v      => Some(LinearTerm(1.0, None, None))
  case Product(a, b)    => classifyFactor(a, b, v, env)
  case MatProduct(a, b) => classifyFactor(a, b, v, env)
  case MatScale(k, m)   => classifyFactor(k, m, v, env)
  case _                => None

/** Classifies a binary product with `v` on exactly one side into a [[LinearTerm]].
 *  The v-free factor is composed into the inner term's left/right coefficient or
 *  into its scalar factor when it reduces to a `_Number`.
 */
private def classifyFactor(l: _Expression, r: _Expression, v: _Variable, env: Environment): Option[LinearTerm] =
  (dependsOn(l, v), dependsOn(r, v)) match
    case (false, true) =>
      classifyTerm(r, v, env).flatMap(inner => l.eval(env) match
        case Right(_Number(k))      => Some(inner.copy(scale = inner.scale * k))
        case Right(a: _MatrixValue) => inner.left match
          case None                          => Some(inner.copy(left = Some(a)))
          case Some(li) if a.cols == li.rows => Some(inner.copy(left = Some(a.multiply(li))))
          case _                             => None
        case _ => None)
    case (true, false) =>
      classifyTerm(l, v, env).flatMap(inner => r.eval(env) match
        case Right(_Number(k))      => Some(inner.copy(scale = inner.scale * k))
        case Right(d: _MatrixValue) => inner.right match
          case None                          => Some(inner.copy(right = Some(d)))
          case Some(ri) if ri.cols == d.rows => Some(inner.copy(right = Some(ri.multiply(d))))
          case _                             => None
        case _ => None)
    case _ => None   // v on both sides (nonlinear) -- not classifiable

/** Entry point for the vectorized tier.
 *
 *  `None` when the equation is not a recognisable dense linear matrix equation (later
 *  tiers run); `Some(Nil)` when recognised but unsolvable (singular / non-conforming).
 */
private def vectorizedMatrixSolve(withV: _Expression, b: _Expression, v: _Variable, env: Environment): Option[List[_Equation]] =
  val (vTerms, cTerms) = flattenSum(withV).partition(dependsOn(_, v))
  for
    bDense    <- asMatrixValue(b.eval(env))
    constants <- allOpt(cTerms.map(t => asMatrixValue(t.eval(env))))
    terms     <- allOpt(vTerms.map(classifyTerm(_, v, env)))
    if terms.nonEmpty
  yield solveVectorized(terms, bDense, constants, v).getOrElse(Nil)

/** Assembles and solves `M * vec(v) = vec(C')` via the Kronecker identity; `None` on failure. */
private def solveVectorized(terms: Vector[LinearTerm], b: _MatrixValue,
                            constants: Vector[_MatrixValue], v: _Variable): Option[List[_Equation]] =
  val p = b.rows
  val q = b.cols
  if constants.exists(c => c.rows != p || c.cols != q) then None
  else
    val cPrime = constants.foldLeft(b)((acc, c) => acc.add(c.scale(-1.0)))
    // Unknown dimensions: L is p x rX and R is cX x q; absent coefficients imply rX = p / cX = q.
    val rX = terms.collectFirst { case LinearTerm(_, Some(l), _) => l.cols }.getOrElse(p)
    val cX = terms.collectFirst { case LinearTerm(_, _, Some(r)) => r.rows }.getOrElse(q)
    val conforming = terms.forall { t =>
      t.left.forall(l => l.rows == p && l.cols == rX)  && (t.left.nonEmpty  || rX == p) &&
      t.right.forall(r => r.rows == cX && r.cols == q) && (t.right.nonEmpty || cX == q)
    }
    val n = p * q
    if !conforming || rX * cX != n || n > MaxVectorizedSize then None
    else
      val m = terms
        .map { t =>
          val lm = t.left.getOrElse(_MatrixValue.identity(p))
          val rm = t.right.getOrElse(_MatrixValue.identity(q))
          rm.transpose.kronecker(lm).scale(t.scale)
        }
        .reduce(_.add(_))
      for
        mInv <- m.inverse
        x    <- _MatrixValue.unvec(mInv.multiply(cPrime.vec), rX, cX)
      yield List(_Equation(v, x))

/** Multiplies two conforming matrices; `None` when dimensions do not match. */
private def matMul(p: _MatrixValue, q: _MatrixValue): Option[_MatrixValue] =
  if p.cols == q.rows then Some(p.multiply(q)) else None

/** Returns the unique side of `eq` that contains `v`, paired with the v-free side.
 *  `None` when `v` occurs on both sides or neither (not a linear matrix-unknown shape).
 */
private def sideWith(v: _Variable, eq: _Equation): Option[(_Expression, _Expression)] =
  (dependsOn(eq.lhs, v), dependsOn(eq.rhs, v)) match
    case (true, false) => Some((eq.lhs, eq.rhs))
    case (false, true) => Some((eq.rhs, eq.lhs))
    case _             => None

/** Extracts a `_MatrixValue` from an already-evaluated result, or `None`. */
private def asMatrixValue(r: Either[_Expression, _Value]): Option[_MatrixValue] = r match
  case Right(m: _MatrixValue) => Some(m)
  case _                      => None

/** Accepts a fully-reduced `_MatrixValue` or a partially-symbolic `_Matrix` literal; `None` otherwise. */
private def asMatrixExpr(r: Either[_Expression, _Value]): Option[_Expression] = r match
  case Right(mv: _MatrixValue) => Some(mv)
  case Left(m: _Matrix)        => Some(m)
  case _                       => None

/** Removes duplicate roots equal within the display tolerance (`0.5 * 10^(-env.precision)`). */
private def dedupeRoots(roots: List[_Value], env: Environment): List[_Value] =
  val tol = 0.5 * math.pow(10, -env.precision)
  roots.foldLeft(List.empty[_Value]) { (acc, r) =>
    if acc.exists(a => sameValue(a, r, tol)) then acc else acc :+ r
  }

/** Returns `true` when two concrete values are equal within `tol`. */
private def sameValue(a: _Value, b: _Value, tol: Double): Boolean = (a, b) match
  case (_Number(x), _Number(y)) => math.abs(x - y) <= tol
  case _                        => a == b

/** Returns the real roots of `c2*x^2 + c1*x + c0 = 0`.
 *  Numeric coefficients: 0, 1, or 2 `_Number` roots.
 *  Symbolic coefficients: both +/-sqrt(delta) closed forms.
 *
 *  The numeric branch uses the **stable** form rather than the textbook
 *  `(-b +/- sqrt(delta)) / 2a`.  When `b*b` dominates `4ac` the square root is nearly
 *  equal to `|b|`, so whichever of the `+/-` branches *subtracts* them loses almost all
 *  of its significant digits -- for `x^2 + 1e8*x + 1` the small root came out 25% wrong.
 *  Forming the root of larger magnitude first (where the two terms have the same sign and
 *  therefore add) and recovering the other from the root product `x1 * x2 = c/a` avoids
 *  the subtraction entirely.
 */
private def quadraticRoots(c0: _Expression, c1: _Expression, c2: _Expression,
                           env: Environment): List[_Expression] =
  (c0, c1, c2) match
    // Exact coefficients first -- `_Number` is a widening extractor, so without this the
    // exact tier's coefficients would be read as Doubles and the roots would be no better
    // than they ever were, however high the working precision (issue 4.N).
    case (a0: _Rational, a1: _Rational, a2: _Rational)
      if env.workingPrecision > _Rational.DoubleReliableDigits =>
      exactQuadraticRoots(a0, a1, a2, env.workingPrecision)
        .getOrElse(quadraticRoots(_Number(a0.toDouble), _Number(a1.toDouble), _Number(a2.toDouble), env))
    case (_Number(a0), _Number(a1), _Number(a2)) =>
      val delta = a1 * a1 - 4.0 * a2 * a0
      if delta < 0.0 then Nil
      else if delta == 0.0 then List(_Number(-a1 / (2.0 * a2)))
      else
        val sq = math.sqrt(delta)
        // signum(0) is 0, which would collapse q; with b = 0 the two terms cannot
        // cancel anyway, so take the plain form there.
        val q  = if a1 == 0.0 then -sq / 2.0 else -(a1 + math.signum(a1) * sq) / 2.0
        // q is never 0 here: delta > 0 forces sq > 0, and a1 + signum(a1)*sq has the
        // magnitude of |a1| + sq.  So both divisions are safe.
        val r1 = q / a2
        val r2 = a0 / q
        List(_Number(math.min(r1, r2)), _Number(math.max(r1, r2)))
    case _ =>
      // Symbolic coefficients: both +-sqrt(delta) closed forms (sign of delta unknown).
      val delta = Sum(Power(c1, _Number(2)), Product(_Number(-4), Product(c2, c0)))
      val sqrtD = Power(delta, _Number(0.5))
      val denom = Product(_Number(2), c2)
      List(
        simplifyFully(Ratio(Sum(Product(_Number(-1), c1), Product(_Number(-1), sqrtD)), denom)),
        simplifyFully(Ratio(Sum(Product(_Number(-1), c1), sqrtD), denom))
      )

/** The roots of `a2·x² + a1·x + a0 = 0` with exact coefficients, at the working precision.
 *
 *  The same numerically stable rearrangement the `Double` branch uses, but carried out in
 *  exact rational arithmetic with only `√Δ` approximated.  That is what makes the result
 *  improve **monotonically** as the working precision rises: the stable form has no
 *  subtraction of near-equal quantities left in it, so the error in each root is just the
 *  error in `√Δ`, and that is exactly what the precision controls.
 *
 *  The discriminant's *sign* is decided exactly, before any approximation — so "no real
 *  roots" and "a repeated root" are conclusions here, not guesses about a rounded value.
 *
 *  @param a0     the constant coefficient
 *  @param a1     the linear coefficient
 *  @param a2     the quadratic coefficient
 *  @param digits the working precision in decimal digits
 *  @return the roots in ascending order, or `None` if the root of the discriminant is
 *          unavailable (the caller then falls back to the `Double` branch)
 */
private def exactQuadraticRoots(a0: _Rational, a1: _Rational, a2: _Rational,
                                digits: Int): Option[List[_Expression]] =
  val policy = _Rational.thresholdFor(digits)
  val four   = _Rational(4)
  val two    = _Rational(2)
  val delta  = a1.multiply(a1, policy).subtract(four.multiply(a2, policy).multiply(a0, policy), policy)

  if delta.signum < 0 then Some(Nil)
  else if delta.isZero then
    a1.negate.divide(two.multiply(a2, policy), policy).map(r => List(r))
  else
    for
      sq <- exactSqrt(delta, digits)
      // Form the larger-magnitude root first, where the two terms share a sign and so ADD.
      signed = if a1.signum >= 0 then sq else sq.negate
      q  <- a1.add(signed, policy).negate.divide(two, policy)
      r1 <- q.divide(a2, policy)
      // ...and recover the other from the root product x1*x2 = c/a, never by subtracting.
      r2 <- a0.divide(q, policy)
    yield if r1 <= r2 then List(r1, r2) else List(r2, r1)

/** Sign-change scan over `[SearchLo, SearchHi]` followed by bisection; up to `MaxNumericRoots` roots. */
private def numericRoots(f: _Expression, v: _Variable, env: Environment): List[_Expression] =
  compile(f, v, env) match
    case None     => Nil
    case Some(fn) =>
      val step  = (SearchHi - SearchLo) / SearchSamples
      val found = scala.collection.mutable.ListBuffer[Double]()
      var a  = SearchLo
      var fa = fn(a)
      var i  = 0
      while i < SearchSamples && found.size < MaxNumericRoots do
        val b  = a + step
        val fb = fn(b)
        if fa == 0.0 then
          // An exact grid-point zero is a genuine root only if the function changes
          // sign around it.  When f is identically zero (e.g. sin(x) = sin(x) ->
          // f = 0), every grid point triggers fa == 0.0 and both neighbourhood
          // samples are also zero -- their product is 0, not negative, so no root
          // is collected and the identity case returns Nil instead of 8 fake roots.
          val eps     = step * 0.5
          val fBefore = fn(a - eps)
          val fAfter  = fn(a + eps)
          if !fBefore.isNaN && !fAfter.isNaN && fBefore * fAfter < 0.0 then
            found += a
        else if !fa.isNaN && !fb.isNaN && fa * fb < 0.0 then found += bisect(fn, a, b, fa)
        a = b; fa = fb; i += 1
      found.toList.map(_Number(_))

/** Bisection refinement: narrows `[lo, hi]` around a sign change for `BisectIterations` steps. */
@annotation.tailrec
private def bisect(fn: Double => Double, lo: Double, hi: Double, flo: Double, i: Int = 0): Double =
  if i >= BisectIterations then (lo + hi) / 2.0
  else
    val mid  = (lo + hi) / 2.0
    val fmid = fn(mid)
    if fmid == 0.0 then mid
    else if flo * fmid < 0.0 then bisect(fn, lo, mid, flo, i + 1)
    else bisect(fn, mid, hi, fmid, i + 1)

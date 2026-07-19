package it.grypho.scala.leonardo
package equation

import core.*
import scalar.*
import matrix.*

import scala.annotation.tailrec


// Equation solver: solve(eq, v) returns the solutions of eq in v as a list of
// equations in the shape "v = expr" — a solution set does not fit eval's
// Either[_Expression, _Value], so like derive/integrate this is a package-level
// algorithm (the _Solve node presents its result at eval time).
//
// Tiers:
//   - polynomial via scalar.collect (like-term collection):
//       degree 1 → v = -c₀/c₁ (symbolic coefficients welcome)
//       degree 2 → discriminant; 0, 1 or 2 real roots for numeric coefficients,
//                  the two ±√Δ closed forms for symbolic ones
//       degree 0 → Nil (the equation does not constrain v)
//   - anything else (transcendental forms, degree ≥ 3) falls back to numeric
//     root-finding: compile(lhs − rhs, v, env) gives a Double => Double closure,
//     scanned for sign changes over [-100, 100] and refined by bisection — up to
//     MaxNumericRoots roots (periodic functions have infinitely many). If the
//     difference is not compilable (unbound symbols besides v), Nil.
//
// Each solution's right-hand side is folded through env, so bound coefficients
// produce numeric answers: with a := 2, solve(a·x = 4, x) yields x = 2.
//
// Matrix equations take two separate paths, tried before the scalar tiers:
//   - unknown MATRIX v (A·v = B): v = A⁻¹·B via the inverse kernel (solveMatrixUnknown);
//   - scalar v inside a matrix equation (both sides reduce to matrices): element-wise
//     decomposition keeping the values that satisfy every cell (solveElementwise).

private val SearchLo         = -100.0
private val SearchHi         = 100.0
private val SearchSamples    = 10000
private val MaxNumericRoots  = 8
private val BisectIterations = 200

def solve(eq: _Equation, v: _Variable, env: Environment = new Environment()): List[_Equation] =
  // Tiers, most specific first:
  //   4.3b — unknown MATRIX v (A·v = B): v = A⁻¹·B via the inverse kernel;
  //   4.3a — scalar v inside a matrix equation (both sides reduce to matrices):
  //          element-wise decomposition, a dimension mismatch has no solution;
  //   otherwise the scalar linear/quadratic/numeric tiers.
  solveMatrixUnknown(eq, v, env) match
    case Some(solution) => solution
    case None =>
      matrixSides(eq, env) match
        case Some((lhs, rhs)) =>
          if lhs.rows == rhs.rows && lhs.cols == rhs.cols then solveElementwise(eq, lhs, rhs, v, env)
          else Nil
        case None => solveScalar(eq, v, env)

private def solveScalar(eq: _Equation, v: _Variable, env: Environment): List[_Equation] =
  val difference = Sum(eq.lhs, Product(_Number(-1), eq.rhs))

  val roots: List[_Expression] = collect(difference, v) match
    case Some(cs) if cs.size == 1 => Nil   // constant in v: nothing to solve for
    case Some(cs) if cs.size == 2 =>
      List(simplifyFully(Ratio(Product(_Number(-1), cs(0)), cs(1))))
    case Some(cs) if cs.size == 3 => quadraticRoots(cs(0), cs(1), cs(2))
    case _                        => numericRoots(difference, v, env)

  roots.map(r => _Equation(v, r.eval(env).toExpression))

// Issue 4.3a — scalar unknown v inside a matrix equation lhs = rhs. The two sides are
// reduced element-wise (a MatSum / matrix literal folds to an n×m grid of scalar
// expressions), giving n·m scalar equations Aᵢⱼ = Bᵢⱼ in v. Their roots are pooled as
// candidates, then each candidate is checked against the WHOLE matrix equation — a value
// is a solution only when every element holds. Verification reuses _Equation.eval's
// precision-tolerant matrix comparison, so a candidate that satisfies some elements but
// not others is dropped: the reported [[1+x, 2+x], [1+2x, 3+3x]] = [[1,3],[3,6]] forces
// x = 0 in cell (1,1) and x = 1 elsewhere, so no candidate verifies and the result is
// empty (the solve node then stays symbolic). Symbolic roots that do not reduce to a
// bindable value under env are conservatively skipped (no false positives).
private def solveElementwise(eq: _Equation, lhs: _Matrix, rhs: _Matrix, v: _Variable, env: Environment): List[_Equation] =
  val elementEqs: List[_Equation] = lhs.elems.toList.zip(rhs.elems).map((l, r) => _Equation(l, r))
  val candidates: List[_Value]    =
    elementEqs.flatMap(e => solve(e, v, env)).flatMap(sol => sol.rhs.eval(env).toOption)
  val verified = candidates.filter(root => eq.eval(env.withBinding(v.variable, root)).contains(_Bool(true)))
  dedupeRoots(verified, env).map(root => _Equation(v, root))

// Both sides re-inflated to matrix literals, or None when either is not matrix-shaped.
private def matrixSides(eq: _Equation, env: Environment): Option[(_Matrix, _Matrix)] =
  (asMatrix(eq.lhs.eval(env)), asMatrix(eq.rhs.eval(env))) match
    case (Some(l), Some(r)) => Some((l, r))
    case _                  => None

private def asMatrix(r: Either[_Expression, _Value]): Option[_Matrix] = r match
  case Left(m: _Matrix)        => Some(m)
  case Right(mv: _MatrixValue) => Some(_Matrix.fromValue(mv))
  case _                       => None

// Issue 4.3b / 4.5 — the unknown v is a MATRIX in a linear matrix equation. Recognises
// the shapes A·v = B, v·A = B, A·v·D = B, A·v + C = B (and permutations) and v = B where
// the *known* operands (A, C, D, B) are either concrete matrices (dense fast path: A⁻¹ via
// LU kernel) or symbolic matrix literals (symbolic path: A⁻¹ via cofactor expansion, capped
// at MaxSymbolicDim = 6):
//   A·v = B → v = A⁻¹·B   v·A = B → v = B·A⁻¹   A·v·D = B → v = A⁻¹·B·D⁻¹   v = B → v = B
// An affine v-free term is peeled to the constant side first (A·v + C = B → A·v = B − C).
// Some(Nil) when A is singular / non-square or the shapes do not conform (no solution →
// the _Solve node stays symbolic). None when the equation is not a matrix-unknown form,
// so the element-wise (4.3a) and scalar tiers still run. Products are the scalar `Product`
// node because a bare unknown parses as a scalar variable (`A * X` with A and B bound
// matrices → `Product(A, X)`); the `MatProduct` shapes are accepted too for completeness.
private def solveMatrixUnknown(eq: _Equation, v: _Variable, env: Environment): Option[List[_Equation]] =
  for
    (withV, constant) <- sideWith(v, eq)
    b                 <- asMatrixExpr(constant.eval(env))
    solution          <- linearMatrixSolve(withV, b, v, env)
                           .orElse(vectorizedMatrixSolve(withV, b, v, env))
  yield solution

// withV is the side containing v; b is the known right-hand matrix expression (concrete
// or symbolic). Returns None when withV is not one of the recognised matrix-unknown shapes.
//
// Shapes handled (4.3b + 4.5), most specific first:
//   - affine term      A·X + C = B → recurse on A·X with B − C   (issue 4.5)
//   - two-sided        A·X·D = B   → X = A⁻¹·B·D⁻¹                (issue 4.5)
//   - one-sided        A·X = B     → X = A⁻¹·B ;  X·A = B → X = B·A⁻¹
//   - bare unknown     X = B       → X = B
// The additive-peel and two-sided cases accept both the scalar Sum/Product nodes (a
// bound-variable operand parses as a scalar node) and the MatSum/MatProduct nodes (a
// matrix-literal operand parses as a matrix node).
@tailrec
private def linearMatrixSolve(withV: _Expression, b: _Expression, v: _Variable, env: Environment): Option[List[_Equation]] =
  withV match
    // Affine: peel a v-free additive operand across (A·X + C = B → A·X = B − C).
    case MatSum(l, r) if !dependsOn(l, v) => linearMatrixSolve(r, matSub(b, l), v, env)
    case MatSum(l, r) if !dependsOn(r, v) => linearMatrixSolve(l, matSub(b, r), v, env)
    case Sum(l, r)    if !dependsOn(l, v) => linearMatrixSolve(r, matSub(b, l), v, env)
    case Sum(l, r)    if !dependsOn(r, v) => linearMatrixSolve(l, matSub(b, r), v, env)
    // Two-sided product: A·X·D = B → X = A⁻¹·B·D⁻¹.
    case MatProduct(MatProduct(a, m), d) if m == v => twoSidedSolve(a, d, b, v, env)
    case MatProduct(a, MatProduct(m, d)) if m == v => twoSidedSolve(a, d, b, v, env)
    case Product(Product(a, m), d)       if m == v => twoSidedSolve(a, d, b, v, env)
    case Product(a, Product(m, d))       if m == v => twoSidedSolve(a, d, b, v, env)
    // One-sided product: A·X = B → X = A⁻¹·B ;  X·A = B → X = B·A⁻¹.
    case Product(a, r)    if r == v => matrixDivide(a, b, v, env, aOnLeft = true)
    case MatProduct(a, r) if r == v => matrixDivide(a, b, v, env, aOnLeft = true)
    case Product(l, a)    if l == v => matrixDivide(a, b, v, env, aOnLeft = false)
    case MatProduct(l, a) if l == v => matrixDivide(a, b, v, env, aOnLeft = false)
    case r                if r == v =>                                   // v = B (or B − C)
      val sol = b.eval(env) match
        case Right(value) => value
        case Left(expr)   => simplifyFully(expr)
      Some(List(_Equation(v, sol)))
    case _                          => None

// A·v = B → v = A⁻¹·B; v·A = B → v = B·A⁻¹.
// Dense fast path: both A and B reduce to _MatrixValue — uses the LU inverse kernel;
// Some(Nil) when singular/non-square or dimensions do not conform.
// Symbolic path: A or B is a _Matrix literal — uses the Inverse node (cofactor expansion,
// capped at MaxSymbolicDim); the product is computed via MatProduct. Returns None when the
// inverse cannot be determined (so element-wise/scalar tiers still get a chance).
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
            case Left(_: MatProduct) => None   // dimensions don't conform — fall through
            case Left(expr)          => Some(List(_Equation(v, simplifyFully(expr))))
            case Right(mv)           => Some(List(_Equation(v, mv)))

// Two-sided linear matrix equation A·X·D = B → X = A⁻¹·B·D⁻¹ (issue 4.5). Both flanks
// are inverted via the Inverse node (dense LU kernel for concrete operands, cofactor
// expansion for symbolic ones capped at MaxSymbolicDim), then the triple product is
// evaluated. This is a recognised matrix-unknown shape, so it always answers Some:
// Some(Nil) when either inverse cannot be determined (singular / non-square / above the
// cofactor cap / non-conforming) — no solution — never falling through to the scalar
// tiers (which cannot solve for a matrix unknown).
private def twoSidedSolve(a: _Expression, d: _Expression, b: _Expression, v: _Variable, env: Environment): Option[List[_Equation]] =
  Some(finalizeSolution(MatProduct(MatProduct(Inverse(a), b), Inverse(d)).eval(env), v).getOrElse(Nil))

// B − C as a matrix expression, used to peel a v-free additive term to the constant
// side. Kept symbolic; the caller's matrixDivide / finalizeSolution evaluates it.
private def matSub(b: _Expression, c: _Expression): _Expression =
  MatSum(b, MatScale(_Number(-1), c))

// A reduced right-hand side as the single matrix solution v = <matrix>, or None when it
// did not collapse to a matrix shape (an unreduced MatProduct/MatSum/Inverse means a
// singular or non-conforming operand).
private def finalizeSolution(result: Either[_Expression, _Value], v: _Variable): Option[List[_Equation]] =
  result match
    case Right(mv: _MatrixValue) => Some(List(_Equation(v, mv)))
    case Left(m: _Matrix)        => Some(List(_Equation(v, simplifyFully(m))))
    case _                       => None

// ── 4.5 vectorized tier: general linear matrix equations (Sylvester shapes) ──────
//
// When the unknown matrix v appears in SEVERAL additive terms — A·v + v·B = C
// (Sylvester), A·v + v·Aᵀ = C (Lyapunov), k·v + A·v·D = C, … — no single-inverse
// closed form exists. Each v-term is classified as  s · L · v · R  (absent L/R =
// identity, s a scalar factor) and the equation is vectorized with the Kronecker
// identity  vec(L·v·R) = (Rᵀ ⊗ L) · vec(v),  assembling the (p·q)×(p·q) dense system
//   M · vec(v) = vec(C′)    with  M = Σ sᵢ·(Rᵢᵀ ⊗ Lᵢ)   and  C′ = C − (v-free terms).
// Solved via the inverse kernel and reshaped back with unvec. Dense-only: every
// coefficient must reduce to a _MatrixValue or _Number under env — a symbolic
// coefficient yields None so the equation stays symbolic. Some(Nil) when the shape is
// recognised but singular / non-conforming (no solution). This tier also covers the
// single-term scalar coefficient (k·v = B → v = B/k), which the one-sided tier cannot
// invert as a matrix.

// M is (p·q)×(p·q): cap the vectorized dimension so the Kronecker system stays small.
private val MaxVectorizedSize = 400   // up to a 20×20 unknown

// One additive term linear in v: scale · left · v · right (absent side = identity).
private final case class LinearTerm(scale: Double, left: Option[_MatrixValue], right: Option[_MatrixValue])

// Additive operands of a Sum/MatSum tree, in order.
private def flattenSum(e: _Expression): List[_Expression] = e match
  case Sum(a, b)    => flattenSum(a) ++ flattenSum(b)
  case MatSum(a, b) => flattenSum(a) ++ flattenSum(b)
  case other        => List(other)

// Classify one additive term as s·L·v·R; None when the term is not linear in v or a
// coefficient does not reduce to a dense matrix / number under env.
private def classifyTerm(t: _Expression, v: _Variable, env: Environment): Option[LinearTerm] = t match
  case x if x == v      => Some(LinearTerm(1.0, None, None))
  case Product(a, b)    => classifyFactor(a, b, v, env)
  case MatProduct(a, b) => classifyFactor(a, b, v, env)
  case MatScale(k, m)   => classifyFactor(k, m, v, env)
  case _                => None

// A binary product with v on exactly one side: the v-free factor composes into the
// inner term's left/right coefficient (l · (L·v·R) = (l·L)·v·R and mirror) or into
// its scalar factor when it reduces to a _Number.
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
    case _ => None   // v on both sides (nonlinear in v) — not classifiable

// Entry point of the tier: None when the equation is not a recognisable dense linear
// matrix equation (later tiers run); Some(Nil) when recognised but unsolvable.
private def vectorizedMatrixSolve(withV: _Expression, b: _Expression, v: _Variable, env: Environment): Option[List[_Equation]] =
  val (vTerms, cTerms) = flattenSum(withV).partition(dependsOn(_, v))
  for
    bDense    <- asMatrixValue(b.eval(env))
    constants <- allOpt(cTerms.map(t => asMatrixValue(t.eval(env))))
    terms     <- allOpt(vTerms.map(classifyTerm(_, v, env)))
    if terms.nonEmpty
  yield solveVectorized(terms, bDense, constants, v).getOrElse(Nil)

// Assemble and solve M·vec(v) = vec(C′); None → no solution (singular/non-conforming).
private def solveVectorized(terms: Vector[LinearTerm], b: _MatrixValue,
                            constants: Vector[_MatrixValue], v: _Variable): Option[List[_Equation]] =
  val p = b.rows
  val q = b.cols
  if constants.exists(c => c.rows != p || c.cols != q) then None
  else
    val cPrime = constants.foldLeft(b)((acc, c) => acc.add(c.scale(-1.0)))
    // Unknown dimensions: L is p×rX and R is cX×q; absent coefficients imply rX = p / cX = q.
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

private def matMul(p: _MatrixValue, q: _MatrixValue): Option[_MatrixValue] =
  if p.cols == q.rows then Some(p.multiply(q)) else None

// The unique side of eq that contains v (with the other, v-free side), or None when v
// occurs on both sides or neither.
private def sideWith(v: _Variable, eq: _Equation): Option[(_Expression, _Expression)] =
  (dependsOn(eq.lhs, v), dependsOn(eq.rhs, v)) match
    case (true, false) => Some((eq.lhs, eq.rhs))
    case (false, true) => Some((eq.rhs, eq.lhs))
    case _             => None

private def asMatrixValue(r: Either[_Expression, _Value]): Option[_MatrixValue] = r match
  case Right(m: _MatrixValue) => Some(m)
  case _                      => None

// Accepts both a fully reduced _MatrixValue and a partially-symbolic _Matrix literal.
private def asMatrixExpr(r: Either[_Expression, _Value]): Option[_Expression] = r match
  case Right(mv: _MatrixValue) => Some(mv)
  case Left(m: _Matrix)        => Some(m)
  case _                       => None

// Collapse roots equal within the display tolerance (the same 0.5·10⁻ᵖ used by
// _Equation) so ±√Δ duplicates and repeated element roots print once.
private def dedupeRoots(roots: List[_Value], env: Environment): List[_Value] =
  val tol = 0.5 * math.pow(10, -env.precision)
  roots.foldLeft(List.empty[_Value]) { (acc, r) =>
    if acc.exists(a => sameValue(a, r, tol)) then acc else acc :+ r
  }

private def sameValue(a: _Value, b: _Value, tol: Double): Boolean = (a, b) match
  case (_Number(x), _Number(y)) => math.abs(x - y) <= tol
  case _                        => a == b

private def quadraticRoots(c0: _Expression, c1: _Expression, c2: _Expression): List[_Expression] =
  (c0, c1, c2) match
    case (_Number(a0), _Number(a1), _Number(a2)) =>
      val delta = a1 * a1 - 4.0 * a2 * a0
      if delta < 0.0 then Nil
      else if delta == 0.0 then List(_Number(-a1 / (2.0 * a2)))
      else
        val sq = math.sqrt(delta)
        List(_Number((-a1 - sq) / (2.0 * a2)), _Number((-a1 + sq) / (2.0 * a2)))
    case _ =>
      // Symbolic coefficients: both ±√Δ closed forms (the sign of Δ is unknown).
      val delta = Sum(Power(c1, _Number(2)), Product(_Number(-4), Product(c2, c0)))
      val sqrtD = Power(delta, _Number(0.5))
      val denom = Product(_Number(2), c2)
      List(
        simplifyFully(Ratio(Sum(Product(_Number(-1), c1), Product(_Number(-1), sqrtD)), denom)),
        simplifyFully(Ratio(Sum(Product(_Number(-1), c1), sqrtD), denom))
      )

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
          // sign around it.  When f is identically zero (e.g. sin(x) = sin(x) →
          // f ≡ 0), every grid point triggers fa == 0.0 and both neighbourhood
          // samples are also zero — their product is 0, not negative, so no root
          // is collected and the identity case returns Nil instead of 8 fake roots.
          val eps     = step * 0.5
          val fBefore = fn(a - eps)
          val fAfter  = fn(a + eps)
          if !fBefore.isNaN && !fAfter.isNaN && fBefore * fAfter < 0.0 then
            found += a
        else if !fa.isNaN && !fb.isNaN && fa * fb < 0.0 then found += bisect(fn, a, b, fa)
        a = b; fa = fb; i += 1
      found.toList.map(_Number(_))

private def bisect(fn: Double => Double, lo0: Double, hi0: Double, flo0: Double): Double =
  var lo  = lo0
  var hi  = hi0
  var flo = flo0
  var i   = 0
  while i < BisectIterations do
    val mid  = (lo + hi) / 2.0
    val fmid = fn(mid)
    if fmid == 0.0 then return mid
    if flo * fmid < 0.0 then hi = mid
    else
      lo = mid
      flo = fmid
    i += 1
  (lo + hi) / 2.0

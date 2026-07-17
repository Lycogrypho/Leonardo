package it.grypho.scala.leonardo
package equation

import core.*
import scalar.*
import matrix.*


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

// Issue 4.3b — the unknown v is a MATRIX in a linear matrix equation. Recognises the
// shapes A·v = B, v·A = B and v = B where the *known* operands (A and B) reduce to
// concrete matrices, and returns the unique solution via 4.2's inverse kernel:
//   A·v = B → v = A⁻¹·B      v·A = B → v = B·A⁻¹      v = B → v = B
// Some(Nil) when A is singular / non-square or the shapes do not conform (no solution →
// the _Solve node stays symbolic). None when the equation is not a matrix-unknown form,
// so the element-wise (4.3a) and scalar tiers still run. Products are the scalar `Product`
// node because a bare unknown parses as a scalar variable (`A * X` with A and B bound
// matrices → `Product(A, X)`); the `MatProduct` shapes are accepted too for completeness.
// Symbolic (non-dense) coefficients are out of scope and fall through as well.
private def solveMatrixUnknown(eq: _Equation, v: _Variable, env: Environment): Option[List[_Equation]] =
  for
    (withV, constant) <- sideWith(v, eq)
    b                 <- asMatrixValue(constant.eval(env))
    solution          <- linearMatrixSolve(withV, b, v, env)
  yield solution

// withV is the side containing v; b is the known right-hand matrix. Returns None when
// withV is not one of the recognised matrix-unknown shapes (fall through to other tiers).
private def linearMatrixSolve(withV: _Expression, b: _MatrixValue, v: _Variable, env: Environment): Option[List[_Equation]] =
  withV match
    case Product(a, r)    if r == v => matrixDivide(a, b, v, env, aOnLeft = true)
    case MatProduct(a, r) if r == v => matrixDivide(a, b, v, env, aOnLeft = true)
    case Product(l, a)    if l == v => matrixDivide(a, b, v, env, aOnLeft = false)
    case MatProduct(l, a) if l == v => matrixDivide(a, b, v, env, aOnLeft = false)
    case r                if r == v => Some(List(_Equation(v, b)))   // v = B
    case _                          => None

// A·v = B → v = A⁻¹·B (A on the left of v); v·A = B → v = B·A⁻¹. None when A is not a
// concrete matrix (not a matrix-unknown equation); Some(Nil) when A is singular/non-square
// or the shapes do not conform.
private def matrixDivide(a: _Expression, b: _MatrixValue, v: _Variable, env: Environment, aOnLeft: Boolean): Option[List[_Equation]] =
  asMatrixValue(a.eval(env)).map { aMatrix =>
    val solution = aMatrix.inverse.flatMap(aInv => if aOnLeft then matMul(aInv, b) else matMul(b, aInv))
    solution.map(m => _Equation(v, m)).toList
  }

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

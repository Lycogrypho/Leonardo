package it.grypho.scala.leonardo
package scalar

import core.*

import scala.annotation.tailrec


/** Symbolic indefinite integration.
 *
 *  [[integrate]] returns an antiderivative of `e` with respect to `v` (the `+ C`
 *  constant is dropped), implemented as a rule table over expression shapes -- the dual
 *  of [[derive]] in `Derive.scala`.  Powers the [[_Integral]] node's `eval`.
 *
 *  When no rule applies, [[integrate]] returns the original `_Integral(e, v)` node
 *  unchanged; `_Integral.eval` treats that fixpoint as "stays symbolic", exactly as
 *  `_Derivative` does when [[derive]] cannot reduce.
 *
 *  Chain-rule coverage is limited to a linear inner argument `u = a*v + b`:
 *  there the substitution `t = u` has constant `dt/dv = a`, so the integral of
 *  `f(u)` w.r.t. `v` equals `F(u)/a`.  Non-linear inner arguments (which would need a
 *  general substitution) are left symbolic.  [[linearSlope]] returns `Some(a)`
 *  exactly when `u` is linear in `v`.
 *
 *  Integration by parts (`∫ u dv = u·V − ∫ V du`, `V = ∫ dv`) covers products of a
 *  polynomial with `exp`/`sin`/`cos` (the polynomial is repeatedly differentiated away)
 *  and a standalone logarithm (`∫ ln(x) dx`).  The `u`/`dv` split follows the LIATE
 *  heuristic (see [[liatePriority]]).  Forms whose parts expansion needs rational-function
 *  cancellation (`∫ x·ln(x) dx`, `∫ arctan(x) dx`) or algebraic solving of a cyclic
 *  integral (`∫ eˣ·sin(x) dx`) are left symbolic; those await the partial-fractions tier.
 *
 *  A dedicated reduction formula (see [[reduceSinCosPower]]) handles the trigonometric
 *  powers `∫ sinⁿ(u) dx` and `∫ cosⁿ(u) dx` (integer `n` in `[2, MaxReductionPower]`,
 *  `u` linear in `v`), which parts cannot reach because the power is not a product.
 */

/** Shared empty environment used by [[linearSlope]] and [[constValue]] to fold
 *  constant-only expressions without allocating a throwaway instance per call. */
private val EmptyEnv = new Environment()

/** Returns the slope `a` when `u` is linear in `v` (i.e. `d(u)/dv` folds to a nonzero
 *  constant); `None` for non-linear or constant `u`.
 *
 *  @param u the expression to test for linearity
 *  @param v the variable
 *  @return `Some(a)` when `u = a*v + b` with `a != 0`, `None` otherwise
 */
private def linearSlope(u: _Expression, v: _Variable): Option[Double] =
  derive(u, v).eval(EmptyEnv) match
    case Right(_Number(a)) if a != 0.0 => Some(a)
    case _                             => None

/** Maximum nested integration-by-parts applications before giving up.
 *
 *  Bounds cyclic integrals (e.g. `∫ eˣ·sin(x) dx`, which reproduces itself after two
 *  applications) so they stay symbolic instead of recursing forever.  Polynomial times
 *  `exp`/`sin`/`cos` terminates well within this bound (the polynomial degree drops by
 *  one per level).
 */
private val MaxPartsDepth = 4

/** Strips `v`-independent factors from a product, returning the `v`-dependent core.
 *
 *  A parts sub-integrand often bundles a numeric coefficient inside a product
 *  (`-cos(x)·2x` parses as `Product(Product(-1, cos(x)), Product(2, x))`), which would
 *  hide the `cos`/polynomial factors from [[liatePriority]].  Peeling the constants
 *  exposes the shape that decides the LIATE class.
 *
 *  @param e the factor to strip
 *  @param v the integration variable
 *  @return `e` with its `v`-independent factors removed
 */
@tailrec
private def stripConstantFactors(e: _Expression, v: _Variable): _Expression = e match
  case Product(a, b) if !dependsOn(a, v) => stripConstantFactors(b, v)
  case Product(a, b) if !dependsOn(b, v) => stripConstantFactors(a, v)
  case _                                 => e

/** LIATE priority of a factor for the integration-by-parts `u`/`dv` split.
 *
 *  The factor with the HIGHER priority is chosen as `u` (the part that gets
 *  differentiated), the other as `dv` (the part that gets integrated):
 *  Logarithmic (5) > Inverse-trig (4) > Algebraic/polynomial (3) > Trig (2) >
 *  Exponential (1).  The factor is classified by its `v`-dependent core (see
 *  [[stripConstantFactors]]).  Returns `None` for shapes parts cannot use.
 *
 *  @param f the factor to classify
 *  @param v the integration variable
 *  @return the LIATE priority, or `None` when `f` is not a usable parts factor
 */
private def liatePriority(f: _Expression, v: _Variable): Option[Int] =
  stripConstantFactors(f, v) match
    case Ln(_)                             => Some(5)
    case LogBase(_, b) if !dependsOn(b, v) => Some(5)
    case Asin(_) | Acos(_) | Atan(_)       => Some(4)
    case core if collect(core, v).isDefined => Some(3)   // polynomial in v
    case Sin(_) | Cos(_)                   => Some(2)
    case Exp(_)                            => Some(1)
    case _                                 => None

/** Splits `e` into a `(numerator, denominator)` pair, flattening nested products and
 *  ratios so that reciprocal factors cancel under [[simplify]].
 *
 *  The `∫ V du` integrand produced by parts often carries a reciprocal (e.g.
 *  `d/dx ln(x) = 1/x`); collecting the whole product into one fraction lets `x·(1/x)`
 *  fold to `1` via the existing `x/x → 1` rule.
 *
 *  @param e the expression to split
 *  @return `(numerator, denominator)` whose ratio equals `e`
 */
private def asFraction(e: _Expression): (_Expression, _Expression) = e match
  case Product(a, b) =>
    val (na, da) = asFraction(a)
    val (nb, db) = asFraction(b)
    (Product(na, nb), Product(da, db))
  case Ratio(a, b) =>
    val (na, da) = asFraction(a)
    val (nb, db) = asFraction(b)
    (Product(na, db), Product(da, nb))
  case _ => (e, _Number(1))

/** Rebuilds `e` as a single fully-simplified fraction (see [[asFraction]]). */
private def combineFraction(e: _Expression): _Expression =
  val (num, den) = asFraction(e)
  simplifyFully(Ratio(num, den))

/** True when `e` still contains an unresolved [[_Integral]] node.
 *
 *  Signals that a parts attempt could not reduce some sub-integral to a closed form,
 *  so the whole attempt must stay symbolic.
 */
private def containsIntegral(e: _Expression): Boolean = e match
  case _: _Integral => true
  case _            => e.children.exists(containsIntegral)

/** Applies one step of integration by parts: `∫ u dv = u·V − ∫ V du` with `V = ∫ dv`.
 *
 *  Returns `None` (stay symbolic) when either `∫ dv` or the resulting `∫ V du` cannot
 *  be reduced to a closed form (its result still contains an [[_Integral]]).
 *
 *  @param u     the factor to differentiate
 *  @param dv    the factor to integrate
 *  @param v     the integration variable
 *  @param depth current parts-recursion depth (guards against cyclic integrals)
 *  @return `Some(antiderivative)` on success, `None` to stay symbolic
 */
private def applyParts(u: _Expression, dv: _Expression, v: _Variable, depth: Int): Option[_Expression] =
  val vInt = integrateImpl(dv, v, depth)
  if containsIntegral(vInt) then None
  else
    val du        = simplifyFully(derive(u, v))
    val integrand = combineFraction(Product(vInt, du))
    val rest      = integrateImpl(integrand, v, depth + 1)
    if containsIntegral(rest) then None
    else Some(simplifyFully(Sum(Product(u, vInt), Product(_Number(-1), rest))))

/** Integration by parts for a product of two `v`-dependent factors.
 *
 *  Selects `u` and `dv` by the LIATE heuristic (see [[liatePriority]]); returns `None`
 *  when either factor is a shape parts cannot use, or the depth cap is hit.
 *
 *  @param f     the first factor
 *  @param g     the second factor
 *  @param v     the integration variable
 *  @param depth current parts-recursion depth
 *  @return `Some(antiderivative)` on success, `None` to stay symbolic
 */
private def partsProduct(f: _Expression, g: _Expression, v: _Variable, depth: Int): Option[_Expression] =
  if depth >= MaxPartsDepth then None
  else
    (liatePriority(f, v), liatePriority(g, v)) match
      case (Some(pf), Some(pg)) =>
        val (u, dv) = if pf >= pg then (f, g) else (g, f)
        applyParts(u, dv, v, depth)
      case _ => None

/** Largest exponent accepted by the trigonometric power-reduction rule.
 *
 *  Matches the Laplace power-rule convention (`transform.LaplaceTransform`): a bound
 *  keeps a pathological `∫ sin^1000(x) dx` from expanding into a huge tree, even though
 *  the recursion always terminates (the exponent drops by two each step).
 */
private val MaxReductionPower = 20

/** True when `n` is an integer in `[2, MaxReductionPower]` -- the range the trig-power
 *  reduction handles (`n = 0` and `n = 1` are the base cases already in the table). */
private def isReduciblePower(n: Double): Boolean =
  n.toInt.toDouble == n && n >= 2 && n <= MaxReductionPower

/** Reduction formula for `∫ sin^n(u) dx` and `∫ cos^n(u) dx` with `u = a*v + b` linear.
 *
 *  Derived by parts (`∫ sin^n = ∫ sin^(n-1)·sin`):
 *  {{{
 *  ∫ sin^n(u) dx = -sin^(n-1)(u)·cos(u)/(a·n) + (n-1)/n · ∫ sin^(n-2)(u) dx
 *  ∫ cos^n(u) dx =  cos^(n-1)(u)·sin(u)/(a·n) + (n-1)/n · ∫ cos^(n-2)(u) dx
 *  }}}
 *  The `1/a` factor comes from the linear inner argument (chain rule).  The recursion
 *  drops `n` by two per step and bottoms out at `∫ sin(u) dx` / `∫ cos(u) dx` (`n = 1`)
 *  or `∫ 1 dx` (`n = 0`), both handled by the surrounding rule table.
 *
 *  @param isSin `true` for a sine power, `false` for a cosine power
 *  @param u     the (linear) inner argument
 *  @param n     the integer exponent (guaranteed in `[2, MaxReductionPower]` by the caller)
 *  @param v     the integration variable
 *  @param depth current integration-by-parts recursion depth (threaded through unchanged)
 *  @return the antiderivative, or `None` when `u` is not linear in `v`
 */
private def reduceSinCosPower(isSin: Boolean, u: _Expression, n: Int, v: _Variable, depth: Int): Option[_Expression] =
  linearSlope(u, v).flatMap { a =>
    val base     = if isSin then Sin(u) else Cos(u)
    val other    = if isSin then Cos(u) else Sin(u)
    val sign     = if isSin then _Number(-1) else _Number(1)
    val boundary = Ratio(Product(sign, Product(Power(base, _Number(n - 1)), other)), _Number(a * n))
    val lower    = integrateImpl(simplifyFully(Power(base, _Number(n - 2))), v, depth)
    if containsIntegral(lower) then None
    else Some(simplifyFully(Sum(boundary, Product(Ratio(_Number(n - 1), _Number(n)), lower))))
  }

/** Reduction formula for a tangent or cotangent power (issue 3.11).
 *
 *  {{{
 *  ∫ tan^n(u) dx =  tan^(n-1)(u)/((n-1)·a) − ∫ tan^(n-2)(u) dx
 *  ∫ cot^n(u) dx = −cot^(n-1)(u)/((n-1)·a) − ∫ cot^(n-2)(u) dx
 *  }}}
 *  The boundary term differs only by sign; both recur with coefficient `−1`, dropping `n`
 *  by two per step.  Unlike [[reduceSinCosPower]], the `n = 1` base case (`∫ tan`, `∫ cot`)
 *  lives in the data-driven table, not in [[integrateImpl]], so the recursive step is taken
 *  through [[resolve]] (which consults the table) rather than `integrateImpl` directly.
 *
 *  @param isTan `true` for a tangent power, `false` for a cotangent power
 *  @param u     the (linear) inner argument
 *  @param n     the integer exponent (guaranteed in `[2, MaxReductionPower]` by the caller)
 *  @param v     the integration variable
 *  @return the antiderivative, or `None` when `u` is not linear or the recursion fails to close
 */
private def reduceTanCotPower(isTan: Boolean, u: _Expression, n: Int, v: _Variable): Option[_Expression] =
  linearSlope(u, v).flatMap { a =>
    val base     = if isTan then Tg(u) else Cot(u)
    val sign     = if isTan then _Number(1) else _Number(-1)
    val boundary = Ratio(Product(sign, Power(base, _Number(n - 1))), _Number((n - 1) * a))
    val lower    = resolve(simplifyFully(Power(base, _Number(n - 2))), v, 0)
    if containsIntegral(lower) then None
    else Some(simplifyFully(Sum(boundary, Product(_Number(-1), lower))))
  }

/** Reduction formula for a secant or cosecant power (issue 3.11).
 *
 *  {{{
 *  ∫ sec^n(u) dx =  sec^(n-2)(u)·tan(u)/((n-1)·a) + (n-2)/(n-1)·∫ sec^(n-2)(u) dx
 *  ∫ csc^n(u) dx = −csc^(n-2)(u)·cot(u)/((n-1)·a) + (n-2)/(n-1)·∫ csc^(n-2)(u) dx
 *  }}}
 *  The `n = 1` base case (`∫ sec`, `∫ csc`) lives in the table, so the recursive step goes
 *  through [[resolve]] as in [[reduceTanCotPower]].
 *
 *  @param isSec `true` for a secant power, `false` for a cosecant power
 *  @param u     the (linear) inner argument
 *  @param n     the integer exponent (guaranteed in `[2, MaxReductionPower]` by the caller)
 *  @param v     the integration variable
 *  @return the antiderivative, or `None` when `u` is not linear or the recursion fails to close
 */
private def reduceSecCscPower(isSec: Boolean, u: _Expression, n: Int, v: _Variable): Option[_Expression] =
  linearSlope(u, v).flatMap { a =>
    val base     = if isSec then Sec(u) else Csc(u)
    val other    = if isSec then Tg(u) else Cot(u)
    val sign     = if isSec then _Number(1) else _Number(-1)
    val boundary = Ratio(Product(sign, Product(Power(base, _Number(n - 2)), other)), _Number((n - 1) * a))
    val lower    = resolve(simplifyFully(Power(base, _Number(n - 2))), v, 0)
    if containsIntegral(lower) then None
    else Some(simplifyFully(Sum(boundary, Product(Ratio(_Number(n - 2), _Number(n - 1)), lower))))
  }

/** Reduction formula for a hyperbolic sine or cosine power (the hyperbolic mirror of
 *  [[reduceSinCosPower]], needed by the trig-substitution tier's `√(v²±a²)` results).
 *
 *  {{{
 *  ∫ sinh^n(u) dx = sinh^(n-1)(u)·cosh(u)/(a·n) − (n-1)/n·∫ sinh^(n-2)(u) dx
 *  ∫ cosh^n(u) dx = cosh^(n-1)(u)·sinh(u)/(a·n) + (n-1)/n·∫ cosh^(n-2)(u) dx
 *  }}}
 *  The `n = 1` base cases (`∫ sinh`, `∫ cosh`) live in the table, so the recursive step goes
 *  through [[resolve]].
 *
 *  @param isSinh `true` for a `sinh` power, `false` for a `cosh` power
 *  @param u      the (linear) inner argument
 *  @param n      the integer exponent (guaranteed in `[2, MaxReductionPower]` by the caller)
 *  @param v      the integration variable
 *  @return the antiderivative, or `None` when `u` is not linear or the recursion fails to close
 */
private def reduceSinhCoshPower(isSinh: Boolean, u: _Expression, n: Int, v: _Variable): Option[_Expression] =
  linearSlope(u, v).flatMap { a =>
    val base     = if isSinh then Sinh(u) else Cosh(u)
    val other    = if isSinh then Cosh(u) else Sinh(u)
    val boundary = Ratio(Product(Power(base, _Number(n - 1)), other), _Number(a * n))
    val coeff    = if isSinh then Ratio(_Number(-(n - 1)), _Number(n)) else Ratio(_Number(n - 1), _Number(n))
    val lower    = resolve(simplifyFully(Power(base, _Number(n - 2))), v, 0)
    if containsIntegral(lower) then None
    else Some(simplifyFully(Sum(boundary, Product(coeff, lower))))
  }


// ── Rational-function integration by partial fractions (issue 4.C) ──────────────
//
// integrateRational handles Ratio(N(v), D(v)) where N and D are polynomials in v with
// numeric coefficients: polynomial long division peels an improper fraction into a
// polynomial part plus a proper remainder, then the proper part is decomposed by the
// structure of D — linear denominators integrate to a log, quadratic denominators via
// completing the square (log + arctan, or two logs / a log-plus-reciprocal for real /
// repeated roots), and degree >= 3 denominators with DISTINCT REAL roots via residues
// (sum of logs). Repeated roots at degree >= 3, any complex root at degree >= 3, and
// symbolic (non-numeric) coefficients stay symbolic — the same boundary the inverse
// Laplace transform draws (transform.InverseLaplaceTransform).

// `RationalEps`, `polyDegree`, `derivCoeffs` and `polyRoots` live in `Polynomial.scala`
// (issue 3.1) — they are shared with the inverse Laplace transform's residue rule.

/** Evaluates `e` in an empty environment, returning `Some(d)` for a concrete number. */
private def constValue(e: _Expression): Option[Double] =
  e.eval(EmptyEnv) match
    case Right(_Number(d)) => Some(d)
    case _                 => None

/** Dense numeric coefficient vector `[c0, c1, ..., cn]` of `e` as a polynomial in `v`,
 *  or `None` when `e` is not polynomial or any coefficient is not a concrete number. */
private def polyCoeffs(e: _Expression, v: _Variable): Option[Vector[Double]] =
  collect(e, v).flatMap { cs =>
    cs.foldRight(Option(Vector.empty[Double])) { (c, acc) =>
      for tail <- acc; d <- constValue(c) yield d +: tail
    }
  }

/** Horner evaluation of a real-coefficient polynomial at a real point. */
private def evalCoeffsReal(cs: Vector[Double], x: Double): Double =
  cs.foldRight(0.0)((c, acc) => acc * x + c)

/** Polynomial long division `num / den` -> `(quotient, remainder)` coefficient vectors
 *  (`num = quotient * den + remainder`, `deg remainder < deg den`). */
private def polyDivide(num: Vector[Double], den: Vector[Double]): (Vector[Double], Vector[Double]) =
  val dd   = polyDegree(den)
  val lc   = den(dd)
  val dnum = polyDegree(num)
  if dnum < dd then (Vector(0.0), num)
  else
    val rr = num.toArray.clone()
    val q  = Array.fill(dnum - dd + 1)(0.0)
    var k  = dnum - dd
    while k >= 0 do
      val coef = rr(dd + k) / lc
      q(k) = coef
      var i = 0
      while i <= dd do
        rr(k + i) -= coef * den(i)
        i += 1
      k -= 1
    (q.toVector, rr.toVector)

/** Builds `k * ln(arg)`, folding `k = 0` to `0` and `k = 1` to `ln(arg)`. */
private def logTerm(k: Double, arg: _Expression): _Expression = scaleBy(k, Ln(arg))

/** Builds `k * e`, folding `k = 0` to `0` and `k = 1` to `e`. */
private def scaleBy(k: Double, e: _Expression): _Expression =
  if math.abs(k) < 1e-15 then _Number(0)
  else if math.abs(k - 1.0) < 1e-15 then e
  else Product(_Number(k), e)

/** Integrates the polynomial `sum(cs(i) * v^i)` term by term to `sum(cs(i)/(i+1) * v^(i+1))`. */
private def integratePolyCoeffs(cs: Vector[Double], v: _Variable): _Expression =
  val terms = cs.zipWithIndex.collect {
    case (c, i) if math.abs(c) > RationalEps => scaleBy(c / (i + 1), Power(v, _Number(i + 1)))
  }
  if terms.isEmpty then _Number(0) else terms.reduce((a, b) => Sum(a, b))

/** Integrates a proper rational `num/den` (`deg num < deg den`, numeric coefficients).
 *
 *  @param num the proper numerator coefficients
 *  @param den the denominator coefficients (degree >= 1)
 *  @param v   the integration variable
 *  @return the antiderivative, or `None` when the denominator shape is out of scope
 *          (repeated/complex roots at degree >= 3)
 */
private def integrateProperRational(num: Vector[Double], den: Vector[Double], v: _Variable): Option[_Expression] =
  polyDegree(den) match
    case 1 =>
      // n0 / (d1*v + d0) = (n0/d1) * ln(d1*v + d0)
      val d1 = den(1)
      val d0 = den(0)
      val n0 = num.headOption.getOrElse(0.0)
      Some(logTerm(n0 / d1, simplifyFully(Sum(Product(_Number(d1), v), _Number(d0)))))
    case 2           => integrateQuadraticDen(num, den, v)
    case d if d >= 3 => integratePartialFractions(num, den, v)
    case _           => None

/** Integrates `(n1*v + n0)/(c2*v^2 + c1*v + c0)` by completing the square.
 *
 *  Complex roots -> log + arctan; repeated real root -> log + reciprocal; distinct real
 *  roots -> two logs (partial fractions).
 */
private def integrateQuadraticDen(num: Vector[Double], den: Vector[Double], v: _Variable): Option[_Expression] =
  val c2 = den(2)
  val p  = den(1) / c2
  val q  = den(0) / c2
  val n1 = num.lift(1).getOrElse(0.0) / c2
  val n0 = num.headOption.getOrElse(0.0) / c2
  val a  = -p / 2.0
  val w2 = q - p * p / 4.0             // (v - a)^2 + w2
  val xa = Sum(v, _Number(-a))         // v - a
  if w2 > RationalEps then
    // complex conjugate roots: (n1/2)*ln((v-a)^2 + w2) + ((n0 + n1*a)/w)*atan((v-a)/w)
    val w = math.sqrt(w2)
    val logPart = logTerm(n1 / 2.0, Sum(Power(xa, _Number(2)), _Number(w2)))
    val atanArg = Atan(Ratio(xa, _Number(w)))
    Some(simplifyFully(Sum(logPart, scaleBy((n0 + n1 * a) / w, atanArg))))
  else if math.abs(w2) <= RationalEps then
    // repeated real root a: n1*ln(v-a) - (n0 + n1*a)/(v-a)
    Some(simplifyFully(Sum(logTerm(n1, xa), scaleBy(-(n0 + n1 * a), Ratio(_Number(1), xa)))))
  else
    // distinct real roots a +/- r: A*ln(v-r1) + B*ln(v-r2)
    val r  = math.sqrt(-w2)
    val r1 = a + r
    val r2 = a - r
    val a1 = (n1 * r1 + n0) / (r1 - r2)
    val a2 = (n1 * r2 + n0) / (r2 - r1)
    Some(simplifyFully(Sum(logTerm(a1, Sum(v, _Number(-r1))), logTerm(a2, Sum(v, _Number(-r2))))))

// ── Full partial-fraction decomposition (issue 3.12) ────────────────────────────
//
// Generalises the old residue-only path (distinct real roots) to the complete real
// decomposition: repeated real roots (v - r)^m, and irreducible quadratics (v^2 + p v + q)
// from complex conjugate pairs, repeated or not. The unknown numerators are found by
// undetermined coefficients — a dense linear system solved with `core._MatrixValue.inverse`,
// the same cross-layer choice `padeApproximant` makes (`equation.solveSystem` is unreachable
// from `scalar`). Multiplicities come from SQUARE-FREE FACTORISATION (Yun's algorithm), not
// from root-finding: `polyRoots` (QR iteration) does not converge on a repeated root, so the
// denominator is first split by repeated-factor multiplicity via polynomial GCDs — a purely
// arithmetic step — and only the resulting square-free parts (simple roots, well-conditioned)
// are handed to `polyRoots`.

/** Real polynomial multiplication (convolution). */
private def polyMul(a: Vector[Double], b: Vector[Double]): Vector[Double] =
  val out = Array.fill(a.length + b.length - 1)(0.0)
  for i <- a.indices; j <- b.indices do out(i + j) += a(i) * b(j)
  out.toVector

/** Trims trailing (high-order) negligible coefficients to the true degree; `[0.0]` for zero. */
private def polyTrim(a: Vector[Double]): Vector[Double] =
  val d = polyDegree(a)
  if d < 0 then Vector(0.0) else a.take(d + 1)

/** Coefficient-wise difference `a - b`, trimmed. */
private def polySub(a: Vector[Double], b: Vector[Double]): Vector[Double] =
  val n = math.max(a.length, b.length)
  polyTrim(Vector.tabulate(n)(i => a.lift(i).getOrElse(0.0) - b.lift(i).getOrElse(0.0)))

/** Normalises to a monic polynomial (leading coefficient 1); unchanged for the zero/constant. */
private def polyMonic(a: Vector[Double]): Vector[Double] =
  val d = polyDegree(a)
  if d < 0 then a else a.take(d + 1).map(_ / a(d))

/** Monic polynomial GCD by the Euclidean algorithm (tolerance via [[polyDegree]]). */
private def polyGcd(a0: Vector[Double], b0: Vector[Double]): Vector[Double] =
  var a = polyTrim(a0)
  var b = polyTrim(b0)
  while polyDegree(b) >= 0 do
    val (_, r) = polyDivide(a, b)
    a = b
    b = polyTrim(r)
  polyMonic(a)

/** Square-free factorisation (Yun): each returned `(poly, k)` is the product of the distinct
 *  factors of `d0` that occur with multiplicity exactly `k`, so `poly` has only simple roots.
 *  This separates the multiplicities arithmetically, before any root-finding. */
private def squareFreeFactors(d0: Vector[Double]): Vector[(Vector[Double], Int)] =
  val dd = polyTrim(d0)
  if polyDegree(dd) < 1 then Vector.empty
  else
    val g  = polyGcd(dd, polyTrim(derivCoeffs(dd)))
    var c  = polyTrim(polyDivide(dd, g)._1)
    var w  = polySub(polyTrim(polyDivide(polyTrim(derivCoeffs(dd)), g)._1), polyTrim(derivCoeffs(c)))
    val out = scala.collection.mutable.ListBuffer[(Vector[Double], Int)]()
    var k  = 1
    while polyDegree(c) >= 1 do
      val p = polyGcd(c, w)
      if polyDegree(p) >= 1 then out += ((polyMonic(p), k))
      c = polyTrim(polyDivide(c, p)._1)
      w = polySub(polyTrim(polyDivide(w, p)._1), polyTrim(derivCoeffs(c)))
      k += 1
    out.toVector

/** Roots of a square-free polynomial as `(re, im)` pairs; degree 1 and 2 are solved
 *  analytically (robust), degree ≥ 3 via [[polyRoots]] (its simple roots converge). */
private def rootsOfSquareFree(poly: Vector[Double]): Option[Vector[(Double, Double)]] =
  polyDegree(poly) match
    case 1 => Some(Vector((-poly(0) / poly(1), 0.0)))
    case 2 =>
      val a = poly(2); val b = poly(1); val c = poly(0)
      val disc = b * b - 4 * a * c
      if disc >= 0 then
        val s = math.sqrt(disc)
        Some(Vector(((-b + s) / (2 * a), 0.0), ((-b - s) / (2 * a), 0.0)))
      else
        val s = math.sqrt(-disc)
        Some(Vector((-b / (2 * a), s / (2 * a)), (-b / (2 * a), -s / (2 * a))))
    case d if d >= 3 => polyRoots(poly).map(_.flatMap(_Complex.parts))
    case _           => None

/** Factorises `den` into real linear factors `(v - r)^m` and irreducible quadratics
 *  `(v^2 + p v + q)^m` (from complex conjugate pairs), via square-free factorisation so
 *  repeated roots are handled without root-finding.  Accepted only when the reconstructed
 *  product matches `den`, rejecting a mis-factorisation rather than integrating a wrong one.
 *
 *  @return `(realFactors, quadFactors)` as `((root, mult), (p, q, mult))`, or `None`
 */
private def factorDenominator(den: Vector[Double]): Option[(Vector[(Double, Int)], Vector[(Double, Double, Int)])] =
  val sqf = squareFreeFactors(den)
  if sqf.isEmpty then None
  else
    val realB = scala.collection.mutable.ListBuffer[(Double, Int)]()
    val quadB = scala.collection.mutable.ListBuffer[(Double, Double, Int)]()
    val ok = sqf.forall { (poly, k) =>
      rootsOfSquareFree(poly) match
        case None => false
        case Some(parts) =>
          val (reals, comps) = parts.partition((_, im) => math.abs(im) < 1e-6)
          reals.foreach((re, _) => realB += ((re, k)))
          comps.filter(_._2 > 0.0).foreach((re, im) => quadB += ((-2.0 * re, re * re + im * im, k)))
          true
    }
    val deg      = polyDegree(den)
    val totalMul = realB.map(_._2).sum + 2 * quadB.map(_._3).sum
    if !ok || totalMul != deg then None
    else
      // reconstruction check: lead * Π factors must match den (guards a mis-factorisation)
      val lead  = den(deg)
      val recon = (realB.flatMap((r, m) => Vector.fill(m)(Vector(-r, 1.0))) ++
                   quadB.flatMap((p, q, m) => Vector.fill(m)(Vector(q, p, 1.0))))
        .foldLeft(Vector(lead))(polyMul)
      val matches = recon.length == den.length &&
                    recon.indices.forall(i => math.abs(recon(i) - den(i)) <= 1e-3 * (1.0 + math.abs(den(i))))
      if matches then Some((realB.toVector, quadB.toVector)) else None

/** `∫ dv/(v^2 + p v + q)^j` for an irreducible quadratic (`w2 = q - p^2/4 > 0`), by the
 *  standard reduction on `j` down to `I_1 = atan((v + p/2)/w)/w`. */
private def integralInvQuadPower(p: Double, q: Double, j: Int, quadExpr: _Expression, v: _Variable): _Expression =
  val w2 = q - p * p / 4.0
  val s  = simplifyFully(Sum(v, _Number(p / 2.0)))          // v + p/2
  if j == 1 then
    val w = math.sqrt(w2)
    Ratio(Atan(Ratio(s, _Number(w))), _Number(w))
  else
    val term = scaleBy(1.0 / (2.0 * (j - 1) * w2), Ratio(s, Power(quadExpr, _Number(j - 1))))
    val rec  = scaleBy((2.0 * j - 3.0) / (2.0 * (j - 1) * w2), integralInvQuadPower(p, q, j - 1, quadExpr, v))
    Sum(term, rec)

/** `∫ (B v + C)/(v^2 + p v + q)^j dv` for an irreducible quadratic — the `(2v+p)` part
 *  (derivative of the quadratic) integrates directly, the constant remainder via
 *  [[integralInvQuadPower]]. */
private def integrateQuadPowerTerm(bCoef: Double, cCoef: Double, p: Double, q: Double, j: Int, v: _Variable): _Expression =
  val quadExpr = simplifyFully(Sum(Sum(Power(v, _Number(2)), Product(_Number(p), v)), _Number(q)))
  // B v + C = (B/2)(2v + p) + (C - B p/2)
  val derivPart =
    if j == 1 then logTerm(bCoef / 2.0, quadExpr)
    else scaleBy(-(bCoef / 2.0) / (j - 1), Ratio(_Number(1), Power(quadExpr, _Number(j - 1))))
  val constPart = scaleBy(cCoef - bCoef * p / 2.0, integralInvQuadPower(p, q, j, quadExpr, v))
  Sum(derivPart, constPart)

/** Integrates a proper rational `num/den` (`deg den >= 3`, numeric coefficients) by the full
 *  real partial-fraction decomposition (issue 3.12).
 *
 *  @return the antiderivative, or `None` when the denominator cannot be factored/reconstructed
 *          or the coefficient system is singular
 */
private def integratePartialFractions(num: Vector[Double], den: Vector[Double], v: _Variable): Option[_Expression] =
  factorDenominator(den).flatMap { (realFactors, quadFactors) =>
    val deg  = polyDegree(den)
    val lead = den(deg)
    // All factor polynomials, in a fixed order (reals first, then quadratics), with mult.
    val factorPolys = realFactors.map((r, m) => (Vector(-r, 1.0), m)) ++
                      quadFactors.map((p, q, m) => (Vector(q, p, 1.0), m))
    // Product of every factor^mult, but with the `reduce`-th factor's exponent lowered by `by`.
    def cofactor(reduce: Int, by: Int): Vector[Double] =
      factorPolys.zipWithIndex.foldLeft(Vector(lead)) { case (acc, ((poly, m), idx)) =>
        val exp = if idx == reduce then m - by else m
        (0 until exp).foldLeft(acc)((p, _) => polyMul(p, poly))
      }
    // Build one column per unknown; remember each column's term so the solution can be read.
    enum Term:
      case Real(r: Double, j: Int)
      case Quad(p: Double, q: Double, j: Int, ofV: Boolean)   // ofV: numerator is v (else 1)
    val columns = scala.collection.mutable.ListBuffer[(Term, Vector[Double])]()
    realFactors.zipWithIndex.foreach { case ((r, m), i) =>
      for j <- 1 to m do columns += ((Term.Real(r, j), cofactor(i, j)))
    }
    val quadBase = realFactors.length
    quadFactors.zipWithIndex.foreach { case ((p, q, m), k) =>
      for j <- 1 to m do
        val cof = cofactor(quadBase + k, j)
        columns += ((Term.Quad(p, q, j, false), cof))               // constant numerator
        columns += ((Term.Quad(p, q, j, true), polyMul(Vector(0.0, 1.0), cof)))  // v * cofactor
    }
    if columns.sizeIs != deg then None
    else
      // Dense system M x = b: row = power of v (0..deg-1), column = unknown.
      val mat = Array.fill(deg * deg)(0.0)
      columns.zipWithIndex.foreach { case ((_, col), c) =>
        for r <- 0 until deg do mat(r * deg + c) = col.lift(r).getOrElse(0.0)
      }
      val rhs = Array.tabulate(deg)(i => num.lift(i).getOrElse(0.0))
      _MatrixValue(deg, deg, mat).inverse.map { inv =>
        val x     = inv.multiply(_MatrixValue(deg, 1, rhs)).toVector
        val terms = columns.zipWithIndex.collect {
          case ((Term.Real(r, j), _), i) if math.abs(x(i)) > RationalEps =>
            if j == 1 then logTerm(x(i), Sum(v, _Number(-r)))
            else scaleBy(-x(i) / (j - 1), Ratio(_Number(1), Power(Sum(v, _Number(-r)), _Number(j - 1))))
        }.toVector
        // Quadratic terms pair a constant-numerator unknown with its v-numerator unknown.
        val quadTerms = columns.zipWithIndex.collect {
          case ((Term.Quad(p, q, j, false), _), i) => (p, q, j, x(i), i)
        }.map { (p, q, j, cCoef, ci) =>
          val bCoef = columns.zipWithIndex.collectFirst {
            case ((Term.Quad(pp, qq, jj, true), _), bi) if pp == p && qq == q && jj == j => x(bi)
          }.getOrElse(0.0)
          integrateQuadPowerTerm(bCoef, cCoef, p, q, j, v)
        }
        val all = terms ++ quadTerms
        if all.isEmpty then _Number(0) else simplifyFully(all.reduce((a, b) => Sum(a, b)))
      }
  }

/** Integrates a rational function `numE / denE` when both are polynomials in `v`.
 *
 *  Improper fractions (`deg num >= deg den`) are split by long division into a polynomial
 *  part plus a proper remainder; the proper part is decomposed by [[integrateProperRational]].
 *
 *  @param numE the numerator expression
 *  @param denE the denominator expression (already known to depend on `v`)
 *  @param v    the integration variable
 *  @return the antiderivative, or `None` to stay symbolic
 */
private def integrateRational(numE: _Expression, denE: _Expression, v: _Variable): Option[_Expression] =
  for
    numC <- polyCoeffs(numE, v)
    denC <- polyCoeffs(denE, v)
    if polyDegree(denC) >= 1
    res  <-
      if polyDegree(numC) >= polyDegree(denC) then
        val (quot, rem) = polyDivide(numC, denC)
        val polyPart    = integratePolyCoeffs(quot, v)
        if polyDegree(rem) < 0 then Some(polyPart)
        else integrateProperRational(rem, denC, v).map(pr => Sum(polyPart, pr))
      else integrateProperRational(numC, denC, v)
  yield simplifyFully(res)


/** Returns an antiderivative of `e` with respect to `v`, or `_Integral(e, v)` when
 *  no rule applies.
 *
 *  `_ElementWise` containers (matrices, equations) are integrated element-wise.
 *  Chain-rule support is limited to linear inner arguments; integration by parts
 *  covers polynomial times `exp`/`sin`/`cos` and the standalone logarithm; rational
 *  functions are integrated by partial fractions (see [[integrateRational]]).
 *
 *  @param e the integrand
 *  @param v the integration variable
 *  @return an antiderivative of `e` (constant of integration omitted),
 *          or `_Integral(e, v)` when no rule fires
 */
def integrate(e: _Expression, v: _Variable): _Expression =
  resolve(e, v, 0)

/** Resolves `∫ e dv` through the full pipeline: the compiled tiers first, then — only when
 *  they give up — the data-driven table (6.21) and the u-substitution driver (3.10).
 *
 *  Both last resorts are hooked HERE rather than at `integrateImpl`'s fallthrough, because
 *  an arm that claims a shape and then gives up inside itself (the rational tier, or a
 *  `Product` whose parts attempt fails) never reaches that fallthrough.  `containsIntegral`
 *  is the give-up signal the compiled tiers already use, so consulting either extension on
 *  it means neither can ever change an integral that already closes.  The table is tried
 *  before substitution so its canonical closed forms are preferred where both would fire.
 *
 *  @param e        the integrand
 *  @param v        the integration variable
 *  @param subDepth current substitution-recursion depth (bounds nested u-substitution)
 *  @return an antiderivative of `e`, or `_Integral(e, v)` when nothing closes it
 */
private def resolve(e: _Expression, v: _Variable, subDepth: Int): _Expression =
  val compiled = integrateImpl(e, v, 0)
  if !containsIntegral(compiled) then compiled
  else
    // Second compiled attempt on the node-normalised form: a user-typed ratio such as
    // `1/cos(x)^2` reaches `integrateImpl` un-normalised and misses the reciprocal-node
    // tiers (e.g. the `Power(Sec, n)` reduction, 3.11), which `simplify` would have exposed
    // (`1/cos^2 -> sec^2`).  Run only on give-up, so nothing that already closed changes.
    val es        = simplifyFully(e)
    val compiled2 = if es != e then integrateImpl(es, v, 0) else compiled
    if !containsIntegral(compiled2) then compiled2
    else
      integralRules.applyTo(e, v)
        .orElse(integrateBySubstitution(e, v, subDepth))
        .orElse(integrateByTrigSub(e, v))
        .getOrElse(compiled)

/** Maximum nesting of u-substitutions before giving up (a substituted integral may itself
 *  need a further substitution; the bound stops the recursion from running away). */
private val MaxSubstitutionDepth = 2

/** Maximum number of candidate inner functions tried per substitution attempt. */
private val MaxSubstitutionCandidates = 8

/** Number of nodes in `e` — used to try the most specific (largest) candidate `g` first. */
private def treeSize(e: _Expression): Int = 1 + e.children.map(treeSize).sum

/** Flattens a product into its factor list (`a·b·c` → `[a, b, c]`); a non-product is a
 *  single factor.  A power of a product is split so its factors surface too
 *  (`(2·sin w)^2` → `[2^2, sin(w)^2]`), which is what lets factor cancellation and the
 *  `[cos w, cos w]`-style grouping reach a substituted power. */
private def flattenFactors(e: _Expression): List[_Expression] = e match
  case Product(a, b)          => flattenFactors(a) ++ flattenFactors(b)
  case Power(Product(a, b), n) => flattenFactors(Power(a, n)) ++ flattenFactors(Power(b, n))
  case _                       => List(e)

/** Rebuilds a product from a factor list; the empty list is the unit `1`. */
private def productOf(fs: List[_Expression]): _Expression =
  fs.foldLeft(_Number(1): _Expression)((acc, f) => Product(acc, f))

/** Forms `num / den` and cancels structurally-equal factors between them, then simplifies.
 *
 *  `simplify` does no common-factor cancellation, so `∫ x·e^(x²) / 2x` would keep its `x`
 *  and defeat the "free of v" test; cancelling the shared factors first is what lets the
 *  quotient reduce to `e^(x²)/2`.  Cancellation is per-factor by structural equality (a
 *  multiset intersection), so it clears exactly the factors the two share — enough for
 *  u-substitution, without a full polynomial gcd.
 *
 *  @param num the numerator expression
 *  @param den the denominator expression
 *  @return the simplified, factor-cancelled quotient `num / den`
 */
private def cancelRatio(num: _Expression, den: _Expression): _Expression =
  def isOne(f: _Expression): Boolean = f == _Number(1)
  val numF = flattenFactors(num).map(simplifyFully).filterNot(isOne)
  val denF = scala.collection.mutable.ListBuffer(flattenFactors(den).map(simplifyFully).filterNot(isOne)*)
  val keptNum = numF.filter { f =>
    val i = denF.indexOf(f)
    if i >= 0 then { denF.remove(i); false } else true
  }
  simplifyFully(Ratio(productOf(groupIdentical(keptNum)), productOf(groupIdentical(denF.toList))))

/** Collapses repeated identical factors into a power (`[cos w, cos w] -> [cos(w)^2]`), so a
 *  product of equal factors folds — `simplify` does not combine equals nested in a product,
 *  which would leave e.g. `(4·cos w)·cos w` unrecognised by the `cos^n` reduction tier. */
private def groupIdentical(fs: List[_Expression]): List[_Expression] =
  fs.groupBy(identity).toList.map((f, occ) => if occ.sizeIs == 1 then f else Power(f, _Number(occ.size)))

/** Collects candidate inner functions `g` for u-substitution from the integrand.
 *
 *  The plausible inner functions are the arguments of function nodes, the bases (radicands)
 *  of powers, and the denominators of ratios (see issue 3.10).  Each is simplified, the bare
 *  variable and `v`-independent terms dropped, duplicates removed, and the list ordered most
 *  specific (largest) first and capped.
 *
 *  @param e the integrand
 *  @param v the integration variable
 *  @return the candidate inner functions, most specific first
 */
private def substitutionCandidates(e: _Expression, v: _Variable): List[_Expression] =
  val buf = scala.collection.mutable.ListBuffer[_Expression]()
  def walk(x: _Expression): Unit =
    x match
      case f: _Function => f.children.foreach(buf += _)   // arguments of exp/sin/…
      case Power(b, _)  => buf += b                        // base / radicand
      case Ratio(_, d)  => buf += d                        // denominator
      case _            =>
    x.children.foreach(walk)
  walk(e)
  buf.toList
    .map(simplifyFully)
    .filter(g => dependsOn(g, v) && !g.isInstanceOf[_Variable])
    .distinct
    .sortBy(g => -treeSize(g))
    .take(MaxSubstitutionCandidates)

/** Attempts non-linear u-substitution `∫ f(g(v))·g'(v) dv = ∫ f(u) du` (issue 3.10).
 *
 *  For each candidate inner function `g` (see [[substitutionCandidates]]): form
 *  `integrand / g'`, cancel the shared factors, and replace every `g` with a fresh `u`.
 *  The **crux is the decidable "free of v" test** — if the result still depends on `v` the
 *  substitution is not valid and the candidate is skipped; otherwise integrate in `u` (via
 *  the full [[resolve]] pipeline, so nested substitution and the table are available) and
 *  back-substitute `u → g`.  Gives up (`None`) when no candidate closes, keeping the
 *  integrand symbolic — the give-up convention shared with the rest of the engine.
 *
 *  @param e        the integrand
 *  @param v        the integration variable
 *  @param subDepth current substitution depth (bounds nested substitution)
 *  @return `Some(antiderivative)` on success, `None` to stay symbolic
 */
private def integrateBySubstitution(e: _Expression, v: _Variable, subDepth: Int): Option[_Expression] =
  if subDepth >= MaxSubstitutionDepth then None
  else
    substitutionCandidates(e, v).iterator.flatMap { g =>
      val gPrime = simplifyFully(derive(g, v))
      gPrime match
        case _Number(0.0) => None   // g locally constant — no substitution
        case _ =>
          val u        = freshVar(e.freeVars + v.variable)
          val quotient = cancelRatio(e, gPrime)
          val replaced = replaceSubexpr(quotient, g, u)
          if dependsOn(replaced, v) then None   // the "free of v" test failed
          else
            val inner = resolve(replaced, u, subDepth + 1)
            if containsIntegral(inner) then None
            else Some(simplifyFully(substitute(inner, Map(u.variable -> g))))
    }.nextOption()


// ── Trigonometric / hyperbolic substitution (issue 3.13) ────────────────────────
//
// Radical integrands √(a²−v²), √(a²+v²), √(v²−a²) close by the substitution that turns the
// radical into a single trig/hyperbolic factor. The ± cases use the HYPERBOLIC substitution
// (v = a·sinh t / a·cosh t) rather than tan/sec, so the "angle" back-substitutes through the
// 3.9 inverse-hyperbolic nodes (asinh / acosh) and the result is written in them directly:
//   √(a²−v²): v = a·sinθ,  dv = a·cosθ dθ,  radical → a·cosθ,   θ = asin(v/a)
//   √(a²+v²): v = a·sinh t, dv = a·cosh t dt, radical → a·cosh t, t = asinh(v/a)
//   √(v²−a²): v = a·cosh t, dv = a·sinh t dt, radical → a·sinh t, t = acosh(v/a)
// The radical node is replaced explicitly (`simplify` does not know the Pythagorean identity),
// v is substituted in the rest, the θ-integral is taken through `resolve`, and the inverse map
// is substituted back — leaving the result numerically exact even where `sin(asin(x))` and the
// like do not algebraically collapse.

/** Attempts a trig/hyperbolic substitution for an integrand containing `√(c2·v² + c0)`.
 *
 *  Scans for a square-root node whose radicand is quadratic in `v` (no linear term) with
 *  numeric coefficients, classifies the three radical forms, and integrates in a fresh
 *  variable via [[resolve]].  `None` when no radical matches or the transformed integral does
 *  not close.
 *
 *  @param e the integrand
 *  @param v the integration variable
 *  @return `Some(antiderivative)` on success, `None` to stay symbolic
 */
private def integrateByTrigSub(e: _Expression, v: _Variable): Option[_Expression] =
  val radicals = scala.collection.mutable.ListBuffer[_Expression]()
  def walk(x: _Expression): Unit =
    x match
      case pw @ Power(base, _Number(ex)) if math.abs(ex - 0.5) < 1e-12 && dependsOn(base, v) => radicals += pw
      case _                                                                                 =>
    x.children.foreach(walk)
  walk(e)
  radicals.iterator.flatMap(r => trigSubWith(e, v, r)).nextOption()

/** Performs the trig/hyperbolic substitution for one radical node (see [[integrateByTrigSub]]). */
private def trigSubWith(e: _Expression, v: _Variable, radical: _Expression): Option[_Expression] =
  val radicand = radical match
    case Power(b, _) => b
    case _           => radical
  for
    cs  <- collect(radicand, v)
    if cs.length == 3
    c0  <- constValue(cs(0)); c1 <- constValue(cs(1)); c2 <- constValue(cs(2))
    if math.abs(c1) < RationalEps && math.abs(c2) > RationalEps
    w    = freshVar(e.freeVars + v.variable)
    scale = math.sqrt(math.abs(c2))
    ratio = c0 / c2
    // (v = h(w), dv/dw = hp(w), radical → gExpr(w), w = inverseAngle(v))
    setup <-
      if c2 < 0.0 && c0 > 0.0 then
        val a = math.sqrt(-ratio)                     // √(a² − v²), a² = c0/|c2|
        Some((Product(_Number(a), Sin(w)), Product(_Number(a), Cos(w)),
              Product(_Number(scale * a), Cos(w)), Asin(Ratio(v, _Number(a)))))
      else if c2 > 0.0 && c0 > 0.0 then
        val a = math.sqrt(ratio)                      // √(v² + a²)
        Some((Product(_Number(a), Sinh(w)), Product(_Number(a), Cosh(w)),
              Product(_Number(scale * a), Cosh(w)), Asinh(Ratio(v, _Number(a)))))
      else if c2 > 0.0 && c0 < 0.0 then
        val a = math.sqrt(-ratio)                     // √(v² − a²)
        Some((Product(_Number(a), Cosh(w)), Product(_Number(a), Sinh(w)),
              Product(_Number(scale * a), Sinh(w)), Acosh(Ratio(v, _Number(a)))))
      else None
    (hw, hpw, gExpr, inverse) = setup
    // Replace the radical FIRST (simplify cannot reduce √(a²−a²sin²w)), then substitute v.
    // Combine into one factor-cancelled fraction — `simplify` alone does not cancel the
    // common `cos w` in e.g. (1/(2cos w))·(2cos w), which would leave the θ-integral open.
    substituted  = substitute(replaceSubexpr(e, radical, gExpr), Map(v.variable -> hw))
    (nn, dd)     = asFraction(Product(substituted, hpw))
    newIntegrand = cancelRatio(nn, dd)
    fW           = resolve(newIntegrand, w, 0)
    if !containsIntegral(fW)
  yield simplifyFully(substitute(fW, Map(w.variable -> inverse)))

/** Rule-table implementation of [[integrate]], threading the parts-recursion `depth`.
 *
 *  @param e     the integrand
 *  @param v     the integration variable
 *  @param depth current integration-by-parts recursion depth
 *  @return an antiderivative of `e`, or `_Integral(e, v)` when no rule fires
 */
private def integrateImpl(e: _Expression, v: _Variable, depth: Int): _Expression = e match
  // Element-wise containers (see core._ElementWise): integrate each child. Must
  // precede the constant rule below, which would otherwise wrap a v-independent
  // matrix in the scalar node Product(matrix, v) instead of integrating per element.
  case ew: _ElementWise => ew.rebuild(ew.children.map(integrateImpl(_, v, depth)))

  // integral(c, v) = c*v          (c independent of v)
  case _ if !dependsOn(e, v) => Product(e, v)

  // linearity: integral(a + b, v) = integral(a, v) + integral(b, v)
  case Sum(a, b) => Sum(integrateImpl(a, v, depth), integrateImpl(b, v, depth))

  // constant multiple: integral(c*f, v) = c * integral(f, v)
  case Product(a, b) if !dependsOn(a, v) => Product(a, integrateImpl(b, v, depth))
  case Product(a, b) if !dependsOn(b, v) => Product(b, integrateImpl(a, v, depth))

  // constant denominator: integral(f/c, v) = (integral(f, v))/c
  case Ratio(a, b) if !dependsOn(b, v) => Ratio(integrateImpl(a, v, depth), b)

  // the bare variable: integral(v, v) = v^2/2   (Power rule below only sees v wrapped in Power)
  case x: _Variable if x.variable == v.variable => Ratio(Power(v, _Number(2)), _Number(2))

  // trigonometric power reduction (must precede the generic power rule, which would
  // otherwise see the non-linear base sin(u)/cos(u) and give up):
  //   integral(sin^n(u), v) and integral(cos^n(u), v) for integer n in [2, MaxReductionPower],
  //   u linear in v. Recurses down to the n=1 (Sin/Cos primitives) or n=0 (constant) cases.
  case Power(Sin(u), _Number(n)) if isReduciblePower(n) =>
    reduceSinCosPower(isSin = true, u, n.toInt, v, depth).getOrElse(_Integral(e, v))
  case Power(Cos(u), _Number(n)) if isReduciblePower(n) =>
    reduceSinCosPower(isSin = false, u, n.toInt, v, depth).getOrElse(_Integral(e, v))

  // tan/cot/sec/csc power reduction (issue 3.11), same placement rationale as sin/cos:
  //   integer n in [2, MaxReductionPower], u linear in v. The n=1 base cases live in the
  //   data-driven table, so the reducers recurse through `resolve` (which consults it).
  case Power(Tg(u), _Number(n)) if isReduciblePower(n) =>
    reduceTanCotPower(isTan = true, u, n.toInt, v).getOrElse(_Integral(e, v))
  case Power(Cot(u), _Number(n)) if isReduciblePower(n) =>
    reduceTanCotPower(isTan = false, u, n.toInt, v).getOrElse(_Integral(e, v))
  case Power(Sec(u), _Number(n)) if isReduciblePower(n) =>
    reduceSecCscPower(isSec = true, u, n.toInt, v).getOrElse(_Integral(e, v))
  case Power(Csc(u), _Number(n)) if isReduciblePower(n) =>
    reduceSecCscPower(isSec = false, u, n.toInt, v).getOrElse(_Integral(e, v))

  // hyperbolic sinh/cosh power reduction (issue 3.13's helper) — same placement rationale.
  case Power(Sinh(u), _Number(n)) if isReduciblePower(n) =>
    reduceSinhCoshPower(isSinh = true, u, n.toInt, v).getOrElse(_Integral(e, v))
  case Power(Cosh(u), _Number(n)) if isReduciblePower(n) =>
    reduceSinhCoshPower(isSinh = false, u, n.toInt, v).getOrElse(_Integral(e, v))

  // power rule over a linear argument u (slope a):
  //   integral(u^n, v) = u^(n+1) / (a*(n+1))   for n != -1
  //   integral(u^-1, v) = log(u) / a            for n = -1
  case Power(u, _Number(n)) => linearSlope(u, v) match
    case Some(a) if n == -1.0 => Ratio(Ln(u), _Number(a))
    case Some(a)              => Ratio(Power(u, _Number(n + 1)), _Number(a * (n + 1)))
    case None                 => _Integral(e, v)

  // integral(a/(1 + u^2), v) = a*atan(u)/slope   (standard atan primitive; linear-arg chain rule)
  // Both orderings of the denominator sum are matched (1+u^2 and u^2+1).
  case Ratio(a, Sum(_Number(1.0), Power(u, _Number(2.0)))) if !dependsOn(a, v) =>
    linearSlope(u, v) match
      case Some(s) => Product(a, Ratio(Atan(u), _Number(s)))
      case None    => _Integral(e, v)

  case Ratio(a, Sum(Power(u, _Number(2.0)), _Number(1.0))) if !dependsOn(a, v) =>
    linearSlope(u, v) match
      case Some(s) => Product(a, Ratio(Atan(u), _Number(s)))
      case None    => _Integral(e, v)

  // integral(a/sqrt(1 - u^2), v) = a*asin(u)/slope  (standard asin primitive; linear-arg chain rule)
  case Ratio(a, Power(Sum(_Number(1.0), Product(_Number(-1.0), Power(u, _Number(2.0)))), _Number(0.5)))
      if !dependsOn(a, v) =>
    linearSlope(u, v) match
      case Some(s) => Product(a, Ratio(Asin(u), _Number(s)))
      case None    => _Integral(e, v)

  // General rational function N(v)/D(v) (both polynomials in v): long division for
  // improper fractions, then partial fractions on the proper part (log for linear
  // denominators, completing the square for quadratics, residue logs for deg>=3 with
  // distinct real roots). Placed after the specific atan/asin ratio rules (which give
  // cleaner closed forms for their shapes) but before the constant-numerator log rule,
  // which would otherwise capture a polynomial-denominator ratio and give up on it.
  // Non-polynomial operands / out-of-scope root structures -> None -> stays symbolic.
  case Ratio(num, den) if dependsOn(den, v) =>
    integrateRational(num, den, v).getOrElse(_Integral(e, v))

  // integral(c/u, v) = c*log(u)/a   (reciprocal written as a Ratio rather than Power(u, -1))
  case Ratio(a, u) if !dependsOn(a, v) => linearSlope(u, v) match
    case Some(s) => Product(a, Ratio(Ln(u), _Number(s)))
    case None    => _Integral(e, v)

  // exponential / trigonometric primitives over a linear argument u (slope a):
  case Exp(u) => linearSlope(u, v) match
    case Some(a) => Ratio(Exp(u), _Number(a))
    case None    => _Integral(e, v)

  case Sin(u) => linearSlope(u, v) match
    case Some(a) => Ratio(Product(_Number(-1), Cos(u)), _Number(a))
    case None    => _Integral(e, v)

  case Cos(u) => linearSlope(u, v) match
    case Some(a) => Ratio(Sin(u), _Number(a))
    case None    => _Integral(e, v)

  // Heaviside step: ∫ step(u) dv = u·step(u) / a  (u = a·v + b linear, chain rule 1/a).
  // Verified by differentiation: d/dv [u·step(u)/a] = (du/dv)·step(u)/a + u·dirac(u)/a
  // and u·δ(u) = 0 in the distributional sense, leaving step(u).
  case _Heaviside(u) => linearSlope(u, v) match
    case Some(a) => Ratio(Product(u, _Heaviside(u)), _Number(a))
    case None    => _Integral(e, v)

  // Integration by parts for a product of two v-dependent factors (the earlier
  // constant-multiple Product cases already peeled any v-free factor, so both
  // factors here depend on v). LIATE selects u/dv; stays symbolic if parts cannot
  // reduce the resulting sub-integral.
  case Product(f, g) => partsProduct(f, g, v, depth).getOrElse(_Integral(e, v))

  // Standalone logarithm: integral(ln(u), v) via parts with dv = 1 (u linear in v).
  case Ln(u) if depth < MaxPartsDepth && linearSlope(u, v).isDefined =>
    applyParts(e, _Number(1), v, depth).getOrElse(_Integral(e, v))
  case LogBase(u, b) if depth < MaxPartsDepth && !dependsOn(b, v) && linearSlope(u, v).isDefined =>
    applyParts(e, _Number(1), v, depth).getOrElse(_Integral(e, v))

  // Standalone arctan: integral(atan(u), v) via parts with dv = 1 (u linear in v). The
  // resulting integral(u'/(1 + u^2)) closes through the rational-function tier above.
  // (asin/acos are not added: their parts step needs integral(x/sqrt(1 - x^2)), which is
  // not a rational function and has no rule -- their parts attempt would just bail.)
  case Atan(u) if depth < MaxPartsDepth && linearSlope(u, v).isDefined =>
    applyParts(e, _Number(1), v, depth).getOrElse(_Integral(e, v))

  // No rule (tan, standalone inverse trig, non-linear compositions, cyclic parts):
  // stay symbolic.
  case _ => _Integral(e, v)

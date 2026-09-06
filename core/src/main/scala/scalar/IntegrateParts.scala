package it.grypho.scala.leonardo
package scalar

import core.*

import scala.annotation.tailrec


/** Integration by parts and the power-reduction formulas.
 *
 *  Split out of `Integrate.scala` by issue 2.6, which left that file the dispatcher only.
 *  Everything here is a top-level definition in `package scalar`, so the split is a pure
 *  file reorganisation: no visibility changed and no call site moved.
 *
 *  Two families live here.  **Integration by parts** (`∫ u dv = u·V − ∫ V du`) with the
 *  LIATE split, and the **power reductions** — `sin`/`cos` (4.C), `tan`/`cot`/`sec`/`csc`
 *  (3.11) and `sinh`/`cosh` (3.13) — which recurse on the exponent rather than on shape.
 */

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


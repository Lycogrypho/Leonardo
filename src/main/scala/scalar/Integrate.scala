package it.grypho.scala.leonardo
package scalar

import core.*


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

/** Returns the slope `a` when `u` is linear in `v` (i.e. `d(u)/dv` folds to a nonzero
 *  constant); `None` for non-linear or constant `u`.
 *
 *  @param u the expression to test for linearity
 *  @param v the variable
 *  @return `Some(a)` when `u = a*v + b` with `a != 0`, `None` otherwise
 */
private def linearSlope(u: _Expression, v: _Variable): Option[Double] =
  derive(u, v).eval(new Environment()) match
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


/** Returns an antiderivative of `e` with respect to `v`, or `_Integral(e, v)` when
 *  no rule applies.
 *
 *  `_ElementWise` containers (matrices, equations) are integrated element-wise.
 *  Chain-rule support is limited to linear inner arguments; integration by parts
 *  covers polynomial times `exp`/`sin`/`cos` and the standalone logarithm.
 *
 *  @param e the integrand
 *  @param v the integration variable
 *  @return an antiderivative of `e` (constant of integration omitted),
 *          or `_Integral(e, v)` when no rule fires
 */
def integrate(e: _Expression, v: _Variable): _Expression = integrateImpl(e, v, 0)

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

  // No rule (tan, standalone inverse trig, non-linear compositions, cyclic parts):
  // stay symbolic.
  case _ => _Integral(e, v)

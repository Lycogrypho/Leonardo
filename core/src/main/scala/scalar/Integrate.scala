package it.grypho.scala.leonardo
package scalar

import core.*

import scala.annotation.tailrec


/** Symbolic indefinite integration — the entry point and the tier dispatcher.
 *
 *  [[integrate]] returns an antiderivative of `e` with respect to `v` (the `+ C` constant is
 *  dropped), the dual of [[derive]] in `Derive.scala`.  Powers the [[_Integral]] node's
 *  `eval`.  When nothing applies it returns the original `_Integral(e, v)` unchanged;
 *  `_Integral.eval` treats that fixpoint as "stays symbolic", exactly as `_Derivative` does.
 *  **Every tier keeps that give-up rule** — the property the whole integration programme was
 *  built to preserve is that Leonardo declines rather than answering wrongly.
 *
 *  This file holds only the dispatch order (see [[resolve]]) and the compiled rule table
 *  [[integrateImpl]].  Issue 2.6 moved the algorithms themselves into three siblings, all
 *  top-level definitions in the same package, so the split changed no visibility and no call
 *  site:
 *
 *  - `IntegrateParts.scala` — integration by parts (LIATE) and the `sin`/`cos`,
 *    `tan`/`cot`/`sec`/`csc` and `sinh`/`cosh` power reductions;
 *  - `IntegrateRational.scala` — long division, completing the square, and the full
 *    partial-fraction decomposition;
 *  - `IntegrateSubstitution.scala` — non-linear u-substitution, trigonometric/hyperbolic
 *    substitution, and the Weierstrass half-angle substitution.
 *
 *  Chain-rule coverage in the compiled arms is limited to a linear inner argument
 *  `u = a*v + b`: there `t = u` has constant `dt/dv = a`, so `∫ f(u) dv = F(u)/a`, and
 *  [[linearSlope]] returns `Some(a)` exactly when `u` is linear in `v`.  A non-linear inner
 *  argument is the substitution tiers' business, not this table's.
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
      // `es` is exactly the form `applyTo` would compute for itself, so it is threaded in.
      integralRules.applyTo(e, v, Some(es))
        .orElse(integrateBySubstitution(e, v, subDepth))
        .orElse(integrateByTrigSub(e, v))
        .orElse(integrateByWeierstrass(e, v))
        .getOrElse(compiled)

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

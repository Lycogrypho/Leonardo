package it.grypho.scala.leonardo
package transform

import core.*
import scalar.*


// Inverse Laplace transform rule set — the dual of LaplaceTransform.scala.
// inverseLaplaceOf(F, s, t) computes L⁻¹{F(s)} as a function of t. Returns
// _InverseLaplace(F, s, t) unchanged when no rule applies (fixpoint / stay-symbolic
// convention, matching the forward transform and derive/integrate).
//
// Strategy: linearity peels sums and s-free constant factors; the core is a rational
// matcher for F(s) = N(s)/D(s) with deg N < deg D ≤ 2. The denominator's numeric
// coefficients (via scalar.collect) decide the pole structure by completing the square:
//   deg D = 1:  b/(s − a)                → b·e^{a·t}
//   deg D = 2, distinct real roots r₁,r₂ → A·e^{r₁·t} + B·e^{r₂·t}   (partial fractions)
//   deg D = 2, repeated real root a      → e^{a·t}·(N₁ + (N₀ + N₁·a)·t)
//   deg D = 2, complex roots a ± i·w     → e^{a·t}·(N₁·cos(w·t) + ((N₀ + N₁·a)/w)·sin(w·t))
// These cover the images of the forward table (constants, t, e^{a·t}, sin, cos, and
// their damped / first-shift products). Symbolic coefficients (the sign of the
// discriminant is then unknown) and denominators of degree ≥ 3 (general factoring
// required) stay symbolic.

def inverseLaplaceOf(f: _Expression, s: _Variable, t: _Variable): _Expression =
  val result = inverseImpl(f, s, t)
  if result.isInstanceOf[_InverseLaplace] then result else simplifyFully(result)

private def inverseImpl(f: _Expression, s: _Variable, t: _Variable): _Expression = f match

  // Linearity: L⁻¹{A + B} = L⁻¹{A} + L⁻¹{B}. If either half is unresolved, the whole
  // sum stays symbolic (a partially-inverted sum would be misleading).
  case Sum(a, b) =>
    val ia = inverseImpl(a, s, t)
    val ib = inverseImpl(b, s, t)
    if ia.isInstanceOf[_InverseLaplace] || ib.isInstanceOf[_InverseLaplace] then _InverseLaplace(f, s, t)
    else Sum(ia, ib)

  // Constant multiple: L⁻¹{c · G} = c · L⁻¹{G} when c is free of s (both orderings).
  case Product(c, g) if !dependsOn(c, s) =>
    val ig = inverseImpl(g, s, t)
    if ig.isInstanceOf[_InverseLaplace] then _InverseLaplace(f, s, t) else Product(c, ig)
  case Product(g, c) if !dependsOn(c, s) =>
    val ig = inverseImpl(g, s, t)
    if ig.isInstanceOf[_InverseLaplace] then _InverseLaplace(f, s, t) else Product(c, ig)

  // Rational core: N(s) / D(s).
  case Ratio(num, den) =>
    invRational(num, den, s, t).getOrElse(_InverseLaplace(f, s, t))

  case _ => _InverseLaplace(f, s, t)

// Numeric coefficient vector of a polynomial in s (c₀, c₁, …), or None when the
// expression is not polynomial in s or a coefficient is not a concrete number — in
// which case the pole structure cannot be decided numerically and we stay symbolic.
private def numericCoeffs(e: _Expression, s: _Variable): Option[Vector[Double]] =
  collect(e, s).flatMap { cs =>
    cs.foldRight(Option(Vector.empty[Double])) { (c, acc) =>
      for tail <- acc; d <- asNumber(c) yield d +: tail
    }
  }

private def asNumber(e: _Expression): Option[Double] =
  e.eval(new Environment()) match
    case Right(_Number(d)) => Some(d)
    case _                 => None

private def invRational(num: _Expression, den: _Expression, s: _Variable, t: _Variable): Option[_Expression] =
  for
    ns <- numericCoeffs(num, s)
    ds <- numericCoeffs(den, s)
    if ns.size <= ds.size - 1     // strictly proper: deg N < deg D (else poly division needed)
    result <- ds.size match
      case 2 => Some(invLinear(ns, ds, t))
      case 3 => invQuadratic(ns, ds, t)
      case _ => None               // deg D = 0 (no pole) or ≥ 3 (needs factoring): symbolic
  yield result

// b / (d₁·s + d₀)  →  (n₀/d₁) · e^{a·t},  a = −d₀/d₁.
private def invLinear(ns: Vector[Double], ds: Vector[Double], t: _Variable): _Expression =
  val a     = -ds(0) / ds(1)
  val coeff = ns.headOption.getOrElse(0.0) / ds(1)
  scaleExp(coeff, a, t)

// (n₁·s + n₀) / (d₂·s² + d₁·s + d₀), reduced to monic and split by completing the square.
private def invQuadratic(ns: Vector[Double], ds: Vector[Double], t: _Variable): Option[_Expression] =
  val d2 = ds(2)
  val p  = ds(1) / d2
  val q  = ds(0) / d2
  val n1 = ns.lift(1).getOrElse(0.0) / d2
  val n0 = ns.lift(0).getOrElse(0.0) / d2
  val a  = -p / 2.0
  val w2 = q - p * p / 4.0        // (s − a)² + w²  with  w² = q − p²/4
  if w2 > 0.0 then
    // Complex conjugate poles a ± i·w → damped oscillation.
    val w = math.sqrt(w2)
    Some(damped(a, t, sum(mul(n1, Cos(mulNum(w, t))), mul((n0 + n1 * a) / w, Sin(mulNum(w, t))))))
  else if w2 == 0.0 then
    // Repeated real root a: n₁/(s−a) + (n₀+n₁·a)/(s−a)² → e^{a·t}(n₁ + (n₀+n₁·a)·t).
    Some(damped(a, t, sum(_Number(n1), mul(n0 + n1 * a, t))))
  else
    // Distinct real roots a ± √(−w²) → partial fractions A/(s−r₁) + B/(s−r₂).
    val r  = math.sqrt(-w2)
    val r1 = a + r
    val r2 = a - r
    val a1 = (n1 * r1 + n0) / (r1 - r2)
    val a2 = (n1 * r2 + n0) / (r2 - r1)
    Some(sum(scaleExp(a1, r1, t), scaleExp(a2, r2, t)))

// ── expression builders (fold trivial 0/1 coefficients so output stays readable) ──

private def mul(k: Double, e: _Expression): _Expression = k match
  case 0.0 => _Number(0)
  case 1.0 => e
  case _   => Product(_Number(k), e)

private def mulNum(k: Double, e: _Expression): _Expression =
  if k == 1.0 then e else Product(_Number(k), e)

private def sum(a: _Expression, b: _Expression): _Expression = (a, b) match
  case (_Number(0.0), _) => b
  case (_, _Number(0.0)) => a
  case _                 => Sum(a, b)

// coeff · e^{a·t}  (a = 0 collapses the exponential to the bare coefficient).
private def scaleExp(coeff: Double, a: Double, t: _Variable): _Expression =
  if a == 0.0 then _Number(coeff) else mul(coeff, Exp(mulNum(a, t)))

// e^{a·t} · inner  (a = 0 → inner).
private def damped(a: Double, t: _Variable, inner: _Expression): _Expression =
  if a == 0.0 then inner else Product(Exp(mulNum(a, t)), inner)

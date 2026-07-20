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
// matcher for F(s) = N(s)/D(s) with deg N < deg D. The denominator's numeric coefficients
// (via scalar.collect) decide the pole structure:
//   deg D = 1:  b/(s − a)                → b·e^{a·t}
//   deg D = 2, distinct real roots r₁,r₂ → A·e^{r₁·t} + B·e^{r₂·t}
//   deg D = 2, repeated real root a      → e^{a·t}·(N₁ + (N₀ + N₁·a)·t)
//   deg D = 2, complex roots a ± i·w     → e^{a·t}·(N₁·cos(w·t) + ((N₀+N₁·a)/w)·sin(w·t))
//   deg D ≥ 3, distinct roots only       → residue partial fractions via companion-matrix
//                                           root-finding (eigenDecompose); repeated roots
//                                           detected by near-zero D'(root) → stay symbolic.
//   Second-shift: L⁻¹{e^{-as}·F(s)} = u(t-a)·f(t-a)  where f = L⁻¹{F}; matched for
//                 Product(Exp(-as), F), Product(F, Exp(-as)), and Ratio(Exp(-as), D(s)).
// Symbolic-coefficient denominators always stay symbolic (numericCoeffs returns None).

def inverseLaplaceOf(f: _Expression, s: _Variable, t: _Variable): _Expression =
  val result = inverseImpl(f, s, t)
  if result.isInstanceOf[_InverseLaplace] then result else simplifyFully(result)

// Returns Some(a) for a positive numeric a when inner matches -a*sv
// (the exponent of e^{-as} in the second-shift factor).
private def negShiftOf(inner: _Expression, sv: String): Option[Double] = inner match
  case Product(_Number(c), vv: _Variable) if vv.variable == sv && c < 0 => Some(-c)
  case Product(vv: _Variable, _Number(c)) if vv.variable == sv && c < 0 => Some(-c)
  case _                                                                  => None

// L⁻¹{e^{-as}·F(s)} = u(t-a)·f(t-a) where f = L⁻¹{F}.
// Returns None when a cannot be extracted or F cannot be inverted.
private def inverseSecondShift(
    inner: _Expression, F: _Expression, s: _Variable, t: _Variable
): Option[_Expression] =
  negShiftOf(inner, s.variable).flatMap { a =>
    val ft = inverseImpl(F, s, t)
    if ft.isInstanceOf[_InverseLaplace] then None
    else
      val ftShifted = simplifyFully(substitute(ft, Map(t.variable -> Sum(t, _Number(-a)))))
      Some(Product(_Heaviside(Sum(t, _Number(-a))), ftShifted))
  }

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

  // Second-shift theorem: L⁻¹{e^{-as}·F(s)} = u(t-a)·f(t-a).
  // Matched in three forms that arise in practice.
  case Product(Exp(inner), g)   => inverseSecondShift(inner, g, s, t).getOrElse(_InverseLaplace(f, s, t))
  case Product(g, Exp(inner))   => inverseSecondShift(inner, g, s, t).getOrElse(_InverseLaplace(f, s, t))
  case Ratio(Exp(inner), den)   => inverseSecondShift(inner, Ratio(_Number(1), den), s, t)
                                     .getOrElse(invRational(_Number(1), den, s, t)
                                     .getOrElse(_InverseLaplace(f, s, t)))

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
      case _ => invHigherDegree(ns, ds, s, t)   // deg D ≥ 3: residue partial fractions
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
  val n0 = ns.headOption.getOrElse(0.0) / d2
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

// ── high-degree rational inverse: companion-matrix root-finding + residue partial fractions ──

// [c₀, c₁, …, cₙ] → [c₁, 2c₂, …, n·cₙ]  (differentiate polynomial coefficient vector).
private def derivPoly(coeffs: Vector[Double]): Vector[Double] =
  coeffs.zipWithIndex.tail.map { (c, i) => i.toDouble * c }

// Horner evaluation of a real-coefficient polynomial at a complex point r = (rRe, rIm).
private def evalPolyAt(coeffs: Vector[Double], r: (Double, Double)): (Double, Double) =
  val (rre, rim) = r
  coeffs.foldRight((0.0, 0.0)) { (c, acc) =>
    val (are, aim) = acc
    (are * rre - aim * rim + c, are * rim + aim * rre)
  }

// Roots of a polynomial (coeffs(i) = coefficient of sⁱ) via its companion matrix.
// Builds the n×n Frobenius companion and calls _MatrixValue.eigenDecompose.
private def polyRoots(coeffs: Vector[Double]): Option[Vector[_Value]] =
  if coeffs.isEmpty then None
  else
    val n  = coeffs.size - 1
    val cn = coeffs(n)
    if n < 1 || math.abs(cn) < 1e-12 then None
    else
      val mat = Array.fill(n * n)(0.0)
      for i <- 0 until n do mat(i * n + (n - 1)) = -coeffs(i) / cn  // last column
      for i <- 1 until n do mat(i * n + (i - 1)) = 1.0              // sub-diagonal
      _MatrixValue(n, n, mat).eigenDecompose

// Pair complex roots into conjugate pairs (positive-im, negative-im).
// None when any root cannot be matched (implies repeated or defective).
private def pairConjugates(cs: Vector[_Complex]): Option[Vector[(_Complex, _Complex)]] =
  val pos = cs.filter(_.im > 0)
  val neg = cs.filter(_.im < 0)
  if pos.size != neg.size then None
  else
    val pairs = pos.flatMap { p =>
      neg.find(n => math.abs(n.re - p.re) < 1e-8 && math.abs(n.im + p.im) < 1e-8)
         .map(n => (p, n))
    }
    if pairs.size == pos.size then Some(pairs) else None

// Inverse Laplace for strictly proper N(s)/D(s) with deg D ≥ 3, distinct roots only.
// Residue formula: A_r = N(r)/D'(r) for each root r.  For complex conjugate pairs
// (α ± βi) the combined term is  2·Re(A)·e^{αt}·cos(βt) − 2·Im(A)·e^{αt}·sin(βt).
// Returns None when root-finding fails or any root is repeated (|D'(root)| < 1e-12).
private def invHigherDegree(
    ns: Vector[Double], ds: Vector[Double], s: _Variable, t: _Variable
): Option[_Expression] =
  val dp = derivPoly(ds)
  polyRoots(ds).flatMap { roots =>
    val realRoots    = roots.collect { case _Number(re) => re }
    val complexRoots = roots.collect { case c: _Complex => c }
    if realRoots.size + complexRoots.size != roots.size then None
    else
      val realTermsOpt: Option[Vector[_Expression]] =
        realRoots.foldLeft(Option(Vector.empty[_Expression])) { (accOpt, re) =>
          accOpt.flatMap { acc =>
            val (nv, _) = evalPolyAt(ns, (re, 0.0))
            val (dv, _) = evalPolyAt(dp, (re, 0.0))
            if math.abs(dv) < 1e-12 then None
            else Some(acc :+ scaleExp(nv / dv, re, t))
          }
        }
      pairConjugates(complexRoots).flatMap { pairs =>
        val complexTermsOpt: Option[Vector[_Expression]] =
          pairs.foldLeft(Option(Vector.empty[_Expression])) { (accOpt, pair) =>
            val (c, _) = pair
            accOpt.flatMap { acc =>
              val (nnRe, nnIm) = evalPolyAt(ns, (c.re, c.im))
              val (ddRe, ddIm) = evalPolyAt(dp, (c.re, c.im))
              val denom = ddRe * ddRe + ddIm * ddIm
              if denom < 1e-24 then None
              else
                val aRe   = (nnRe * ddRe + nnIm * ddIm) / denom
                val aIm   = (nnIm * ddRe - nnRe * ddIm) / denom
                val inner = sum(mul(2.0 * aRe, Cos(mulNum(c.im, t))),
                                mul(-2.0 * aIm, Sin(mulNum(c.im, t))))
                Some(acc :+ damped(c.re, t, inner))
            }
          }
        for
          rTerms <- realTermsOpt
          cTerms <- complexTermsOpt
          all     = rTerms ++ cTerms
          if all.nonEmpty
        yield all.reduce(sum)
      }
  }

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

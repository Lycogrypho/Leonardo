package it.grypho.scala.leonardo
package transform

import core.*
import scalar.*


/** Inverse Laplace transform rule set.
 *
 *  [[inverseLaplaceOf]] computes `L^-1{F(s)}` as a function of `t`.  Returns
 *  [[_InverseLaplace]]`(F, s, t)` unchanged when no rule applies (fixpoint /
 *  stay-symbolic convention, matching the forward transform and `derive`/`integrate`).
 *
 *  Strategy: linearity peels sums and `s`-free constant factors; the core matches
 *  rational `N(s)/D(s)` with `deg N < deg D`.  The denominator's numeric coefficients
 *  (via `scalar.collect`) decide the pole structure:
 *  - `deg D = 1`:  `b/(s - a)` -> `b * exp(a*t)`
 *  - `deg D = 2`, distinct real roots `r1, r2` -> `A*exp(r1*t) + B*exp(r2*t)`
 *  - `deg D = 2`, repeated real root `a`        -> `exp(a*t) * (N1 + (N0 + N1*a)*t)`
 *  - `deg D = 2`, complex roots `a +/- i*w`     -> `exp(a*t) * (N1*cos(w*t) + ((N0+N1*a)/w)*sin(w*t))`
 *  - `deg D >= 3`, distinct roots only           -> residue partial fractions via
 *                                                   companion-matrix root-finding;
 *                                                   repeated roots stay symbolic.
 *  - **Second-shift**: `L^-1{exp(-a*s)*F(s)} = u(t-a)*f(t-a)` where `f = L^-1{F}`;
 *    matched for `Product(Exp(-a*s), F)`, `Product(F, Exp(-a*s))`, and
 *    `Ratio(Exp(-a*s), D(s))`.
 *
 *  Symbolic-coefficient denominators always stay symbolic (`numericCoeffs` returns `None`).
 */

/** Computes `L^-1{f}` with Laplace variable `s` and time variable `t`.
 *
 *  Applies `simplifyFully` to the result when a rule fires, keeping the output readable.
 *
 *  @param f the frequency-domain expression to invert
 *  @param s the Laplace frequency variable
 *  @param t the time variable
 *  @return the inverse Laplace transform of `f`, or [[_InverseLaplace]]`(f, s, t)` if no rule applies
 */
def inverseLaplaceOf(f: _Expression, s: _Variable, t: _Variable): _Expression =
  val result = inverseImpl(f, s, t)
  if result.isInstanceOf[_InverseLaplace] then result else simplifyFully(result)

/** Returns `Some(a)` for a positive `a` when `inner` matches `-a * sv` (the exponent of `exp(-a*s)`). */
private def negShiftOf(inner: _Expression, sv: String): Option[Double] = inner match
  case Product(_Number(c), vv: _Variable) if vv.variable == sv && c < 0 => Some(-c)
  case Product(vv: _Variable, _Number(c)) if vv.variable == sv && c < 0 => Some(-c)
  case _                                                                  => None

/** Applies the second-shift theorem: `L^-1{exp(-a*s) * F(s)} = u(t-a) * f(t-a)`.
 *
 *  @param inner the exponent of the `exp` factor (must match `-a * s`)
 *  @param F     the remaining frequency-domain factor
 *  @param s     the Laplace variable
 *  @param t     the time variable
 *  @return `Some(u(t-a) * f(t-a))` on success; `None` when `a` cannot be extracted
 *          or `F` cannot be inverted
 */
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

/** Recursive rule dispatcher for the inverse Laplace transform. */
private def inverseImpl(f: _Expression, s: _Variable, t: _Variable): _Expression = f match

  // Linearity: L^-1{A + B} = L^-1{A} + L^-1{B}. If either half is unresolved, the whole
  // sum stays symbolic (a partially-inverted sum would be misleading).
  case Sum(a, b) =>
    val ia = inverseImpl(a, s, t)
    val ib = inverseImpl(b, s, t)
    if ia.isInstanceOf[_InverseLaplace] || ib.isInstanceOf[_InverseLaplace] then _InverseLaplace(f, s, t)
    else Sum(ia, ib)

  // Constant multiple: L^-1{c * G} = c * L^-1{G} when c is free of s (both orderings).
  case Product(c, g) if !dependsOn(c, s) =>
    val ig = inverseImpl(g, s, t)
    if ig.isInstanceOf[_InverseLaplace] then _InverseLaplace(f, s, t) else Product(c, ig)
  case Product(g, c) if !dependsOn(c, s) =>
    val ig = inverseImpl(g, s, t)
    if ig.isInstanceOf[_InverseLaplace] then _InverseLaplace(f, s, t) else Product(c, ig)

  // Second-shift theorem: L^-1{exp(-a*s) * F(s)} = u(t-a) * f(t-a).
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

/** Returns the numeric coefficient vector `[c0, c1, ..., cn]` of a polynomial in `s`,
 *  or `None` when any coefficient is not a concrete number (stays symbolic).
 */
private def numericCoeffs(e: _Expression, s: _Variable): Option[Vector[Double]] =
  collect(e, s).flatMap { cs =>
    cs.foldRight(Option(Vector.empty[Double])) { (c, acc) =>
      for tail <- acc; d <- asNumber(c) yield d +: tail
    }
  }

/** Evaluates `e` in an empty environment and returns the numeric value, or `None`. */
private def asNumber(e: _Expression): Option[Double] =
  e.eval(new Environment()) match
    case Right(_Number(d)) => Some(d)
    case _                 => None

/** Inverts a strictly proper rational `N(s)/D(s)` (`deg N < deg D`); `None` when unsupported. */
private def invRational(num: _Expression, den: _Expression, s: _Variable, t: _Variable): Option[_Expression] =
  for
    ns <- numericCoeffs(num, s)
    ds <- numericCoeffs(den, s)
    if ns.size <= ds.size - 1     // strictly proper: deg N < deg D (else poly division needed)
    result <- ds.size match
      case 2 => Some(invLinear(ns, ds, t))
      case 3 => invQuadratic(ns, ds, t)
      case _ => invHigherDegree(ns, ds, s, t)   // deg D >= 3: residue partial fractions
  yield result

/** Inverts `b / (d1*s + d0)` -> `(n0/d1) * exp(a*t)` where `a = -d0/d1`. */
private def invLinear(ns: Vector[Double], ds: Vector[Double], t: _Variable): _Expression =
  val a     = -ds(0) / ds(1)
  val coeff = ns.headOption.getOrElse(0.0) / ds(1)
  scaleExp(coeff, a, t)

/** Inverts `(n1*s + n0) / (d2*s^2 + d1*s + d0)` by completing the square.
 *  Handles complex conjugate poles, repeated real root, and distinct real roots.
 */
private def invQuadratic(ns: Vector[Double], ds: Vector[Double], t: _Variable): Option[_Expression] =
  val d2 = ds(2)
  val p  = ds(1) / d2
  val q  = ds(0) / d2
  val n1 = ns.lift(1).getOrElse(0.0) / d2
  val n0 = ns.headOption.getOrElse(0.0) / d2
  val a  = -p / 2.0
  val w2 = q - p * p / 4.0        // (s - a)^2 + w^2  with  w^2 = q - p^2/4
  if w2 > 0.0 then
    // Complex conjugate poles a +/- i*w -> damped oscillation.
    val w = math.sqrt(w2)
    Some(damped(a, t, sum(mul(n1, Cos(mulNum(w, t))), mul((n0 + n1 * a) / w, Sin(mulNum(w, t))))))
  else if w2 == 0.0 then
    // Repeated real root a: n1/(s-a) + (n0+n1*a)/(s-a)^2 -> exp(a*t)*(n1 + (n0+n1*a)*t).
    Some(damped(a, t, sum(_Number(n1), mul(n0 + n1 * a, t))))
  else
    // Distinct real roots a +/- sqrt(-w^2) -> partial fractions A/(s-r1) + B/(s-r2).
    val r  = math.sqrt(-w2)
    val r1 = a + r
    val r2 = a - r
    val a1 = (n1 * r1 + n0) / (r1 - r2)
    val a2 = (n1 * r2 + n0) / (r2 - r1)
    Some(sum(scaleExp(a1, r1, t), scaleExp(a2, r2, t)))

// ── High-degree rational inverse: square-free factorisation + partial fractions ──
//
// `derivPoly` and `polyRoots` were local copies of `scalar.derivCoeffs` / `scalar.polyRoots`;
// both now come from `scalar/Polynomial.scala` (issue 3.1), which issue 2.5 extended with the
// coefficient arithmetic and square-free factorisation this tier now shares with the
// integrator's rational tier.
//
// Issue 3.17 replaced the residue formula `A_r = N(r)/D'(r)` used here: it is defined only
// for a SIMPLE pole (it divides by `D'(r)`, which vanishes exactly when the pole repeats), so
// `evalPolyAt` and `pairConjugates` — the complex-root residue machinery — went with it.

/** Inverts a strictly proper `N(s)/D(s)` with `deg D >= 3`, repeated poles included.
 *
 *  **Multiplicities come from square-free factorisation, not from root-finding** (issue
 *  3.17, reusing the `scalar` helpers issue 2.5 made shareable).  The former residue
 *  formula `A_r = N(r)/D'(r)` is defined only for a *simple* pole — it divides by `D'(r)`,
 *  which vanishes exactly when the pole repeats — so repeated poles had to be refused.
 *  `squareFreeFactors` splits `D` by multiplicity arithmetically first, which also sidesteps
 *  the QR iteration's failure to converge on a repeated root.
 *
 *  The partial-fraction numerators are then found by undetermined coefficients (a dense
 *  system solved with `core._MatrixValue.inverse`, the cross-layer choice the integrator's
 *  rational tier makes for the same reason), and each piece is inverted by its standard
 *  transform pair:
 *  {{{
 *  A/(s-a)^k                -> A * t^(k-1) * exp(a*t) / (k-1)!
 *  (B*s + C)/((s-a)^2+w^2)^k -> the k = 1 damped sine/cosine; k >= 2 via the same
 *                               t^(j) * exp(a t) * {cos,sin}(w t) family
 *  }}}
 *
 *  @return the inverse transform, or `None` when the factorisation or the system fails
 */
private def invHigherDegree(
    ns: Vector[Double], ds: Vector[Double], s: _Variable, t: _Variable
): Option[_Expression] =
  partialFractionTerms(ns, ds, t).map { terms =>
    if terms.isEmpty then _Number(0) else terms.reduce(sum)
  }

/** Decomposes `N(s)/D(s)` into partial fractions and inverts each piece (issue 3.17).
 *
 *  Mirrors `scalar.integratePartialFractions`: factor `D` into real linear factors and
 *  irreducible quadratics with their multiplicities, build one unknown per basis element
 *  (`1/(s-r)^j`, and `s`/`1` numerators over `(s^2+ps+q)^j`), solve the dense system, then
 *  map each surviving coefficient to its inverse-transform pair.
 */
private def partialFractionTerms(ns: Vector[Double], ds: Vector[Double], t: _Variable): Option[Vector[_Expression]] =
  val deg  = polyDegree(ds)
  val lead = ds(deg)
  // Square-free factorisation first: multiplicities arithmetically, roots only per part.
  val factored = squareFreeFactors(ds).foldLeft(Option((Vector.empty[(Double, Int)], Vector.empty[(Double, Double, Int)]))) {
    case (acc, (poly, k)) =>
      acc.zip(rootsOfSquareFree(poly)).map { case ((reals, quads), parts) =>
        val (rs, cs) = parts.partition((_, im) => math.abs(im) < 1e-6)
        (reals ++ rs.map((re, _) => (re, k)),
         quads ++ cs.filter(_._2 > 0.0).map((re, im) => (-2.0 * re, re * re + im * im, k)))
      }
  }
  factored.flatMap { (realFactors, quadFactors) =>
    if realFactors.map(_._2).sum + 2 * quadFactors.map(_._3).sum != deg then None
    else
      val factorPolys = realFactors.map((r, m) => (Vector(-r, 1.0), m)) ++
                        quadFactors.map((p, q, m) => (Vector(q, p, 1.0), m))
      // D with the `reduce`-th factor's exponent lowered by `by` — the column multiplier.
      def cofactor(reduce: Int, by: Int): Vector[Double] =
        factorPolys.zipWithIndex.foldLeft(Vector(lead)) { case (acc, ((poly, m), idx)) =>
          val exp = if idx == reduce then m - by else m
          (0 until exp).foldLeft(acc)((p, _) => polyMul(p, poly))
        }
      // One column per unknown, tagged with the term it stands for.
      val columns = scala.collection.mutable.ListBuffer[(PfTerm, Vector[Double])]()
      realFactors.zipWithIndex.foreach { case ((r, m), i) =>
        for j <- 1 to m do columns += ((PfTerm.Real(r, j), cofactor(i, j)))
      }
      val quadBase = realFactors.length
      quadFactors.zipWithIndex.foreach { case ((p, q, m), k) =>
        for j <- 1 to m do
          val cof = cofactor(quadBase + k, j)
          columns += ((PfTerm.Quad(p, q, j, false), cof))
          columns += ((PfTerm.Quad(p, q, j, true), polyMul(Vector(0.0, 1.0), cof)))
      }
      if columns.sizeIs != deg then None
      else
        val mat = Array.fill(deg * deg)(0.0)
        columns.zipWithIndex.foreach { case ((_, col), c) =>
          for r <- 0 until deg do mat(r * deg + c) = col.lift(r).getOrElse(0.0)
        }
        val rhs = Array.tabulate(deg)(i => ns.lift(i).getOrElse(0.0))
        _MatrixValue(deg, deg, mat).inverse.flatMap { inv =>
          val x = inv.multiply(_MatrixValue(deg, 1, rhs)).toVector
          val realTerms = columns.zipWithIndex.collect {
            case ((PfTerm.Real(r, j), _), i) if math.abs(x(i)) > 1e-12 => invRealPower(x(i), r, j, t)
          }.toVector
          // Each quadratic power pairs its constant-numerator unknown with its s-numerator one.
          val quadTerms = columns.zipWithIndex.collect {
            case ((PfTerm.Quad(p, q, j, false), _), i) => (p, q, j, x(i))
          }.map { (p, q, j, cCoef) =>
            val bCoef = columns.zipWithIndex.collectFirst {
              case ((PfTerm.Quad(pp, qq, jj, true), _), bi) if pp == p && qq == q && jj == j => x(bi)
            }.getOrElse(0.0)
            invQuadPower(bCoef, cCoef, p, q, j, t)
          }.toVector
          if quadTerms.contains(None) then None
          else Some(realTerms ++ quadTerms.flatten)
        }
  }

/** One partial-fraction basis element: `A/(s-r)^j`, or a numerator over `(s^2+ps+q)^j`
 *  (`ofS` distinguishes the `s` numerator from the constant one). */
private enum PfTerm:
  case Real(r: Double, j: Int)
  case Quad(p: Double, q: Double, j: Int, ofS: Boolean)

/** `L-1{A/(s-r)^k} = A · t^(k-1) · e^(r t) / (k-1)!`.
 *
 *  Both trivial factors collapse: `t^0` disappears for a simple pole and the exponential
 *  for a pole at the origin, so `A/s` inverts to the bare constant `A` and `A/s^2` to `A·t`.
 */
private def invRealPower(coeffA: Double, root: Double, k: Int, t: _Variable): _Expression =
  val c       = coeffA / (1 to (k - 1)).product      // (k-1)! — the empty product is 1
  val tPow    = Option.when(k > 1)(if k == 2 then t else Power(t, _Number(k - 1)))
  val expPart = Option.when(root != 0.0)(Exp(mulNum(root, t)))
  (tPow, expPart) match
    case (None, None)       => _Number(c)
    case (Some(p), None)    => mul(c, p)
    case (None, Some(e))    => mul(c, e)
    case (Some(p), Some(e)) => mul(c, Product(p, e))

/** `L-1{(B·s + C)/((s-a)^2 + w^2)^k}` for `k = 1` and `k = 2`.
 *
 *  Writing `s = (s-a) + a` splits the numerator into its `(s-a)` and constant parts, whose
 *  transforms are the damped cosine/sine (`k = 1`) and the `t`-weighted pair (`k = 2`):
 *  {{{
 *  k=1: e^(at)·[A1·cos(wt) + (A0/w)·sin(wt)]
 *  k=2: e^(at)·[A1·t·sin(wt)/(2w) + A0·(sin(wt) − w·t·cos(wt))/(2w³)]
 *  }}}
 *  `k >= 3` returns `None` — the family continues, but this tier does not build it, and
 *  declining is the house rule (a wrong inverse is worse than a symbolic one).
 */
private def invQuadPower(bCoef: Double, cCoef: Double, p: Double, q: Double,
                         k: Int, t: _Variable): Option[_Expression] =
  val a  = -p / 2.0
  val w2 = q - p * p / 4.0
  if w2 <= 0.0 then None                              // not an irreducible quadratic
  else
    val w  = math.sqrt(w2)
    val a1 = bCoef                                    // coefficient of (s - a)
    val a0 = bCoef * a + cCoef                        // constant coefficient
    val wt = mulNum(w, t)
    val inner = k match
      case 1 => Some(sum(mul(a1, Cos(wt)), mul(a0 / w, Sin(wt))))
      case 2 => Some(sum(mul(a1 / (2 * w), Product(t, Sin(wt))),
                         mul(a0 / (2 * w * w2),
                             sum(Sin(wt), mul(-w, Product(t, Cos(wt)))))))
      case _ => None
    inner.map(i => damped(a, t, i))

// ── Expression builders (fold trivial 0/1 coefficients so output stays readable) ──

/** Returns `k * e`, folding `0 * e -> 0` and `1 * e -> e`. */
private def mul(k: Double, e: _Expression): _Expression = k match
  case 0.0 => _Number(0)
  case 1.0 => e
  case _   => Product(_Number(k), e)

/** Returns `k * e`, folding `1 * e -> e`. */
private def mulNum(k: Double, e: _Expression): _Expression =
  if k == 1.0 then e else Product(_Number(k), e)

/** Returns `a + b`, folding `0 + b -> b` and `a + 0 -> a`. */
private def sum(a: _Expression, b: _Expression): _Expression = (a, b) match
  case (_Number(0.0), _) => b
  case (_, _Number(0.0)) => a
  case _                 => Sum(a, b)

/** Returns `coeff * exp(a * t)`, collapsing the exponential when `a = 0`. */
private def scaleExp(coeff: Double, a: Double, t: _Variable): _Expression =
  if a == 0.0 then _Number(coeff) else mul(coeff, Exp(mulNum(a, t)))

/** Returns `exp(a * t) * inner`, omitting the exponential factor when `a = 0`. */
private def damped(a: Double, t: _Variable, inner: _Expression): _Expression =
  if a == 0.0 then inner else Product(Exp(mulNum(a, t)), inner)

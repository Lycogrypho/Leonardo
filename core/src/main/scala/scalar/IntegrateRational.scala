package it.grypho.scala.leonardo
package scalar

import core.*

import scala.annotation.tailrec


/** Rational-function integration: long division, completing the square, and the full
 *  partial-fraction decomposition.
 *
 *  Split out of `Integrate.scala` by issue 2.6 (see that file for the dispatch order).
 *  The coefficient arithmetic this tier runs on lives in `Polynomial.scala` (issue 2.5),
 *  shared with `Singularity` and the inverse Laplace transform.
 */


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

// `polyMul`, `polyTrim`, `polySub`, `polyMonic`, `polyGcd`, `polyDivide`, `evalCoeffsReal`,
// `squareFreeFactors` and `rootsOfSquareFree` live in `Polynomial.scala` (issue 2.5) — they
// are shared with `Singularity` and the inverse Laplace transform.

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



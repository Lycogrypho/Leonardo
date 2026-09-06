package it.grypho.scala.leonardo
package scalar

import core.*

import scala.annotation.tailrec


/** The substitution tiers: non-linear u-substitution, trigonometric/hyperbolic
 *  substitution, and the Weierstrass half-angle substitution.
 *
 *  Split out of `Integrate.scala` by issue 2.6.  All three are consulted from `resolve`
 *  only after every compiled arm has declined, in the order written there — each is a
 *  fallback, and the later ones are progressively more general and more expensive.
 */

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
  val numG = groupByBase(keptNum)
  val denG = groupByBase(denF.toList)
  // A negative exponent belongs on the other side of the fraction: `cos(w)^-2` in the
  // numerator is `cos(w)^2` in the denominator, which is the spelling the reciprocal-node
  // normalisation and the power-reduction tiers recognise (issue 3.18).
  val (numPos, numNeg) = numG.partition(_._2 > 0.0)
  val (denPos, denNeg) = denG.partition(_._2 > 0.0)
  def flip(ps: List[(_Expression, Double)]): List[(_Expression, Double)] = ps.map((b, n) => (b, -n))
  val denExpr = rebuildFactors(denPos ++ flip(numNeg))
  // Numeric factors ride outside the fraction, so a bare `1` is left over the denominator:
  // `simplify`'s `1/cos^2 -> sec^2` normalisation keys on a unit numerator, and without this
  // `0.25/cos(w)^2` would never reach the `sec^n` reduction.
  val (numConst, numRest) = (numPos ++ flip(denNeg)).partition((b, _) => b.isInstanceOf[_Number])
  simplifyFully(Product(rebuildFactors(numConst), Ratio(rebuildFactors(numRest), denExpr)))

/** Groups factors by base, summing exponents (`[cos w, cos w] -> cos(w)^2`, `[x^3, x^-1] ->
 *  x^2`), in order of first appearance.
 *
 *  `simplify` does not combine equal factors nested in a product, so `(4·cos w)·cos w` would
 *  otherwise stay unrecognised by the `cos^n` reduction tier.  Summing *exponents* rather
 *  than counting duplicates is what lets a half-integer radical power decompose: `(2·cos w)^3`
 *  flattens to `cos(w)^3` beside the measure's `cos w`, and the two must fold to `cos(w)^4`.
 *  The fold preserves first-seen order, so a rebuilt product is stable across runs.
 */
private def groupByBase(fs: List[_Expression]): List[(_Expression, Double)] =
  def split(f: _Expression): (_Expression, Double) = f match
    case Power(b, _Number(n)) => (b, n)
    case _                    => (f, 1.0)
  fs.foldLeft(Vector.empty[(_Expression, Double)]) { (acc, f) =>
    val (base, n) = split(f)
    acc.indexWhere(_._1 == base) match
      case -1 => acc :+ (base, n)
      case i  => acc.updated(i, (base, acc(i)._2 + n))
  }.toList

/** Rebuilds a product from `(base, exponent)` pairs, dropping unit exponents. */
private def rebuildFactors(ps: List[(_Expression, Double)]): _Expression =
  productOf(ps.map((b, n) => if n == 1.0 then b else Power(b, _Number(n))))

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
      case pw @ Power(base, _Number(ex)) if isHalfInteger(ex) && dependsOn(base, v) => radicals += pw
      case _                                                                        =>
    x.children.foreach(walk)
  walk(e)
  radicals.iterator.flatMap(r => trigSubWith(e, v, r)).nextOption()

/** True when `ex` is a half-integer (`2·ex` an odd integer): `±0.5`, `±1.5`, `±2.5`, … —
 *  every power whose reduced form still carries one square root (issue 3.18).  A whole
 *  exponent is excluded: it needs no substitution and the power rule owns it. */
private def isHalfInteger(ex: Double): Boolean =
  val twice = ex * 2.0
  twice == math.rint(twice) && math.abs(twice.toLong % 2) == 1 && math.abs(twice) <= MaxReductionPower

/** Performs the trig/hyperbolic substitution for one radical node (see [[integrateByTrigSub]]). */
private def trigSubWith(e: _Expression, v: _Variable, radical: _Expression): Option[_Expression] =
  val (radicand, halves) = radical match
    case Power(b, _Number(ex)) => (b, math.rint(ex * 2.0).toInt)   // odd: radicand^(halves/2)
    case _                     => (radical, 1)
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
    (hw, hpw, gExpr0, inverse) = setup
    // `radicand^(halves/2) = (√radicand)^halves`, so the whole power is replaced by the
    // θ-form raised to `halves` — `^0.5` is the `halves = 1` case (issue 3.18).
    gExpr = if halves == 1 then gExpr0 else Power(gExpr0, _Number(halves))
    // Replace the radical FIRST (simplify cannot reduce √(a²−a²sin²w)), then substitute v.
    // Combine into one factor-cancelled fraction — `simplify` alone does not cancel the
    // common `cos w` in e.g. (1/(2cos w))·(2cos w), which would leave the θ-integral open.
    substituted  = substitute(replaceSubexpr(e, radical, gExpr), Map(v.variable -> hw))
    (nn, dd)     = asFraction(Product(substituted, hpw))
    newIntegrand = cancelRatio(nn, dd)
    fW           = resolve(newIntegrand, w, 0)
    if !containsIntegral(fW)
  yield simplifyFully(substitute(fW, Map(w.variable -> inverse)))


// ── Weierstrass (half-angle) substitution (issue 3.14) ──────────────────────────
//
// t = tan(v/2) turns ANY rational function of sin v / cos v into a rational function of t
// (sin = 2t/(1+t²), cos = (1−t²)/(1+t²), dv = 2/(1+t²)dt), which the 3.12 rational tier
// finishes. Dispatched last: it is the general fallback for trig rationals that none of the
// specific trig rules closed. The rationality test is DECIDABLE: the substituted integrand is
// normalised into one polynomial fraction over t (`ratNormalize`); any node that is not
// +/·/÷/integer-power over t (an exp, a bare v, another variable) makes it fail, which is the
// refusal — nothing is ever half-transformed.

/** Degree cap for the normalised Weierstrass fraction, keeping the linear system solvable. */
private val MaxWeierstrassDegree = 24

/** Normalises an arithmetic expression over `t` into one polynomial fraction `num/den`
 *  (dense coefficient vectors).  `None` when the expression is not rational in `t` (another
 *  variable, a function node, a non-integer power) or a degree exceeds the cap — the
 *  decidable "is it rational" test the Weierstrass tier rests on. */
private def ratNormalize(e: _Expression, t: _Variable): Option[(Vector[Double], Vector[Double])] =
  def capped(n: Vector[Double], d: Vector[Double]): Option[(Vector[Double], Vector[Double])] =
    if polyDegree(n) > MaxWeierstrassDegree || polyDegree(d) > MaxWeierstrassDegree then None
    else if polyDegree(d) < 0 then None      // zero denominator
    else Some((polyTrim(n), polyTrim(d)))
  e match
    case _Number(c)                               => Some((Vector(c), Vector(1.0)))
    case x: _Variable if x.variable == t.variable => Some((Vector(0.0, 1.0), Vector(1.0)))
    case Sum(a, b) =>
      for (na, da) <- ratNormalize(a, t); (nb, db) <- ratNormalize(b, t)
          r <- capped(polyAdd(polyMul(na, db), polyMul(nb, da)), polyMul(da, db)) yield r
    case Product(a, b) =>
      for (na, da) <- ratNormalize(a, t); (nb, db) <- ratNormalize(b, t)
          r <- capped(polyMul(na, nb), polyMul(da, db)) yield r
    case Ratio(a, b) =>
      for (na, da) <- ratNormalize(a, t); (nb, db) <- ratNormalize(b, t)
          if polyDegree(nb) >= 0
          r <- capped(polyMul(na, db), polyMul(da, nb)) yield r
    case Power(b, _Number(n)) if n.toInt.toDouble == n && math.abs(n) <= MaxWeierstrassDegree =>
      ratNormalize(b, t).flatMap { (nb, db) =>
        val k = math.abs(n.toInt)
        val (pn, pd) = (0 until k).foldLeft((Vector(1.0), Vector(1.0))) {
          case ((an, ad), _) => (polyMul(an, nb), polyMul(ad, db))
        }
        if n >= 0 then capped(pn, pd) else capped(pd, pn)
      }
    case _ => None

/** Integrates a rational function given directly as coefficient vectors: long division for an
 *  improper fraction, then [[integrateProperRational]] on the proper part. */
private def integrateCoeffRational(num: Vector[Double], den: Vector[Double], t: _Variable): Option[_Expression] =
  val dn = polyDegree(den)
  if dn < 0 then None
  else if dn == 0 then Some(integratePolyCoeffs(num.map(_ / den(0)), t))
  else if polyDegree(num) >= dn then
    val (q, r) = polyDivide(num, den)
    val polyPart = integratePolyCoeffs(q, t)
    if polyDegree(r) < 0 then Some(polyPart)
    else integrateProperRational(r, den, t).map(pr => simplifyFully(Sum(polyPart, pr)))
  else integrateProperRational(num, den, t)

/** Attempts the Weierstrass substitution `t = tan(v/2)` on a rational-in-trig integrand.
 *
 *  Requires every circular-trig node in `e` (`sin`/`cos`/`tan`/`sec`/`csc`/`cot`) to have the
 *  bare variable as its argument; each is replaced by its half-angle form, the `2/(1+t²)`
 *  measure appended, and the result normalised by [[ratNormalize]] — the decidable check that
 *  the integrand really is rational in `sin v`/`cos v`.  The rational-in-`t` fraction is
 *  finished by the 3.12 tier and `t = tan(v/2)` substituted back.
 *
 *  @param e the integrand
 *  @param v the integration variable
 *  @return `Some(antiderivative)` on success, `None` to stay symbolic
 */
private def integrateByWeierstrass(e: _Expression, v: _Variable): Option[_Expression] =
  def trigArg(x: _Expression): Option[_Expression] = x match
    case Sin(a) => Some(a)
    case Cos(a) => Some(a)
    case Tg(a)  => Some(a)
    case Sec(a) => Some(a)
    case Csc(a) => Some(a)
    case Cot(a) => Some(a)
    case _      => None
  val trigNodes = scala.collection.mutable.ListBuffer[_Expression]()
  def walk(x: _Expression): Unit =
    if trigArg(x).isDefined then trigNodes += x
    x.children.foreach(walk)
  walk(e)
  if trigNodes.isEmpty || !trigNodes.forall(n => trigArg(n).contains(v)) then None
  else
    val t     = freshVar(e.freeVars + v.variable)
    val t2    = Power(t, _Number(2))
    val plus  = Sum(_Number(1), t2)                          // 1 + t²
    val minus = Sum(_Number(1), Product(_Number(-1), t2))    // 1 − t²
    val twoT  = Product(_Number(2), t)
    val forms = List[(_Expression, _Expression)](
      Sin(v) -> Ratio(twoT, plus),  Cos(v) -> Ratio(minus, plus), Tg(v)  -> Ratio(twoT, minus),
      Sec(v) -> Ratio(plus, minus), Csc(v) -> Ratio(plus, twoT),  Cot(v) -> Ratio(minus, twoT))
    val replaced = forms.foldLeft(e) { case (acc, (from, to)) => replaceSubexpr(acc, from, to) }
    if dependsOn(replaced, v) then None   // a bare v outside the trig nodes — not rational in them
    else
      for
        (num, den) <- ratNormalize(Product(replaced, Ratio(_Number(2), plus)), t)
        anti       <- integrateCoeffRational(num, den, t)
      yield simplifyFully(substitute(anti, Map(t.variable -> Tg(Ratio(v, _Number(2))))))


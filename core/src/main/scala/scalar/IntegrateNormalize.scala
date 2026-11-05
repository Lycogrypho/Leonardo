package it.grypho.scala.leonardo
package scalar

import core.*


// ── Standard forms after give-up (F_0287) ──────────────────────────────────────────────
//
// Everything here runs only when the whole pipeline (`resolve`) left an `_Integral`, so no
// integral that already closed changes.  Most of the forms LT's course probe found open were
// spellings rather than missing rules (an unfolded exponent, a constant inside a denominator,
// powers of one base unmerged, a polynomial power unexpanded), so the integrand is first
// normalised and retried term by term; the rest are the textbook substitutions a first course
// teaches, each one integrating its new integrand through this same pipeline, one level deeper.

/** The pipeline with the F_0287 retries, at substitution depth `d`.
 *
 *  @param e the integrand
 *  @param v the integration variable
 *  @param d the substitution depth, bounded by `MaxSubstitutionDepth`
 *  @return an antiderivative, or what [[resolve]] answered when nothing here closes it
 */
private[scalar] def integrateFull(e: _Expression, v: _Variable, d: Int)(using t: Tier): _Expression =
  val first = resolve(e, v, d)
  if !containsIntegral(first) then first
  else
    val n = normalizeIntegrand(e, v)
    val pieces = terms(n)
    val each = pieces.map { x =>
      val r = if x == e then first else resolve(x, v, d)
      if containsIntegral(r) then techniques(x, v, d).getOrElse(r) else r
    }
    Some(each.reduceLeft(Sum(_, _))).filterNot(containsIntegral).getOrElse(first)

private def terms(x: _Expression): List[_Expression] = x match
  case Sum(a, b) => terms(a) ++ terms(b)
  case other     => List(other)

/** The substitutions, tried in turn on one normalised term. */
private def techniques(x: _Expression, v: _Variable, d: Int)(using t: Tier): Option[_Expression] =
  expTrigProduct(x, v)
    .orElse(asQuotient(x, v, d))
    .orElse(if d >= MaxSubstitutionDepth then None
            else lnByParts(x, v, d)
              .orElse(oddTrigPower(x, v, d))
              .orElse(sinCosProduct(x, v, d))
              .orElse(linearBase(x, v, d))
              .orElse(squareSubstitution(x, v, d))
              .orElse(rationalising(x, v, d))
              .orElse(expOfLog(x, v, d)))

/** The integrand normalised for a second attempt: every subtree free of `v` folded to its value;
 *  a polynomial with constant coefficients written out by `normalize`; otherwise a product or
 *  quotient flattened into a coefficient and the powers of its bases, exponents summed and a
 *  denominator's negated; a sum normalised term by term.
 *
 *  @param e the integrand
 *  @param v the integration variable
 *  @return the normalised integrand, `e` itself when nothing changed
 */
private[scalar] def normalizeIntegrand(e: _Expression, v: _Variable)(using t: Tier): _Expression =
  val folded = foldFree(e, v)
  polynomial(folded, v).getOrElse(merged(folded, v))

/** Every maximal subtree free of `v` replaced by its value, where it has one. */
private def foldFree(e: _Expression, v: _Variable): _Expression =
  if !dependsOn(e, v) then e.eval(EmptyEnv).fold(_ => e, x => x)
  else e.rebuild(e.children.map(foldFree(_, v)))

/** A polynomial in `v` of degree one or more with constant coefficients, written out, or `None`. */
private def polynomial(e: _Expression, v: _Variable)(using t: Tier): Option[_Expression] =
  collect(e, v).filter(cs => cs.sizeIs >= 2 && cs.forall(c => !dependsOn(c, v)))
    .map(_ => simplifyFullyInTier(normalize(e, v), t.exact))

/** A product or quotient as `(constant factors, (base, exponent) of the others)`, the exponents of
 *  one base summed and simplified, a denominator's negated. */
private def powersOf(e: _Expression, v: _Variable)(using t: Tier)
    : (List[_Expression], List[(_Expression, _Expression)]) =
  def factors(x: _Expression, sign: Int): List[(_Expression, _Expression)] = x match
    case Product(a, b)                   => factors(a, sign) ++ factors(b, sign)
    case Ratio(a, b)                     => factors(a, sign) ++ factors(b, -sign)
    case Power(b, k) if !dependsOn(k, v) => List((b, if sign > 0 then k else Product(lit(-1), k)))
    case other                           => List((other, lit(sign)))
  val (consts, fs) = factors(e, 1).partition((b, _) => !dependsOn(b, v))
  val grouped = fs.foldLeft(List.empty[(_Expression, _Expression)]) { case (acc, (b, k)) =>
    acc.indexWhere(_._1 == b) match
      case -1 => acc :+ (b, k)
      case i  => acc.updated(i, (b, Sum(acc(i)._2, k)))
  }.map((b, k) => (b, simplifyFullyInTier(k, t.exact))).filterNot((_, k) => isZeroLiteral(k))
  (consts.map((b, k) => if isOneLiteral(k) then b else Power(b, k)), grouped)

/** `c · Π b^k` rebuilt, unit exponents and a unit coefficient dropped. */
private def rebuilt(consts: List[_Expression], fs: List[(_Expression, _Expression)])(using t: Tier): _Expression =
  val c     = simplifyFullyInTier(consts.foldLeft[_Expression](lit(1))(Product(_, _)), t.exact)
  val body  = fs.map((b, k) => if isOneLiteral(k) then b else Power(b, k))
  val parts = if isOneLiteral(c) then body else c :: body
  parts.reduceLeftOption[_Expression](Product(_, _)).getOrElse(lit(1))

/** A product or quotient with the powers of each base merged; a sum term by term. */
private def merged(e: _Expression, v: _Variable)(using t: Tier): _Expression = e match
  case Sum(a, b)             => Sum(merged(a, v), merged(b, v))
  case _: Product | _: Ratio =>
    val (consts, fs) = powersOf(e, v)
    rebuilt(consts, fs)
  case other                 => other

/** The integer `n` in the integrator's tier. */
private def lit(n: Int)(using t: Tier): _Expression = t.n(n)

/** A whole exponent, read as an `Int`. */
private def whole(k: _Expression): Option[Int] = k match
  case _Number(d) if d == math.rint(d) && math.abs(d) <= MaxReductionPower => Some(d.toInt)
  case _                                                                     => None

/** A term carrying negative powers as one quotient `Π positive / Π negative`, the shape the
 *  rational tier reads (`2·u²·(u⁴ + u²)^−1`). */
private def asQuotient(x: _Expression, v: _Variable, d: Int)(using t: Tier): Option[_Expression] =
  val (consts, fs) = powersOf(x, v)
  val (neg, pos)   = fs.partition((_, k) => k match
    case _Number(n) => n < 0
    case _          => false)
  if neg.isEmpty || pos.isEmpty then None
  else
    val flipped = neg.map((b, k) => (b, simplifyFullyInTier(Product(lit(-1), k), t.exact)))
    val r = resolve(Ratio(rebuilt(consts, pos), rebuilt(Nil, flipped)), v, d)
    Option.when(!containsIntegral(r))(r)

/** `∫ ln(u)·g = ln(u)·G − ∫ G·u′/u` with `G = ∫ g` (`ln(x)/x²`, `x³·ln(2x + 1)`). */
private def lnByParts(x: _Expression, v: _Variable, d: Int)(using t: Tier): Option[_Expression] =
  val (consts, fs) = powersOf(x, v)
  fs.collectFirst { case (l @ Ln(arg), k) if isOneLiteral(k) && dependsOn(arg, v) => (l, arg) }.flatMap { (l, arg) =>
    val g  = rebuilt(Nil, fs.filterNot(_._1 == l))
    val gI = integrateFull(g, v, d + 1)
    if containsIntegral(gI) || !dependsOn(g, v) then None
    else
      val rest = integrateFull(cancelRatio(Product(gI, deriveIn(arg, v)), arg), v, d + 1)
      Option.when(!containsIntegral(rest))(
        Product(rebuilt(consts, Nil), Sum(Product(l, gI), Product(lit(-1), rest))))
  }

/** `sin(u)ᵐ·cos(u)ⁿ`, `u` linear, one exponent an odd natural: `w = cos u` (or `sin u`) turns it into
 *  a polynomial in `w` (`sin⁵·cos⁸`). */
private def oddTrigPower(x: _Expression, v: _Variable, d: Int)(using t: Tier): Option[_Expression] =
  val (consts, fs) = powersOf(x, v)
  val parts = fs.map {
    case (Sin(u), k) => whole(k).map(m => (u, m, 0))
    case (Cos(u), k) => whole(k).map(n => (u, 0, n))
    case _           => None
  }
  if parts.isEmpty || parts.exists(_.isEmpty) || parts.flatten.map(_._1).distinct.sizeIs != 1 then None
  else
    val u = parts.flatten.head._1
    val m = parts.flatten.map(_._2).sum
    val n = parts.flatten.map(_._3).sum
    for
      a <- linearSlope(u, v)
      if m >= 0 && n >= 0 && (m % 2 == 1 || n % 2 == 1)
      w  = freshVar(x.freeVars + v.variable)
      oneMinus = Sum(lit(1), Product(lit(-1), Power(w, lit(2))))
      (poly, back, sign) =
        if m % 2 == 1 then (Product(Power(oneMinus, lit((m - 1) / 2)), Power(w, lit(n))), Cos(u), -1)
        else               (Product(Power(w, lit(m)), Power(oneMinus, lit((n - 1) / 2))), Sin(u), 1)
      inner = integrateFull(expand(poly), w, d + 1)
      if !containsIntegral(inner)
    yield Product(rebuilt(consts, Nil),
                  Ratio(Product(lit(sign), substitute(inner, Map(w.variable -> back))), a))

/** `sin(u)·cos(u) = sin(2u)/2`, so the product reaches the parts and substitution rules
 *  (`x·sin(x)·cos(x)`). */
private def sinCosProduct(x: _Expression, v: _Variable, d: Int)(using t: Tier): Option[_Expression] =
  val (consts, fs) = powersOf(x, v)
  fs.collectFirst { case (Sin(u), k) if isOneLiteral(k) && fs.contains((Cos(u), k)) => u }.flatMap { u =>
    val others = fs.filterNot((b, _) => b == Sin(u) || b == Cos(u))
    val r = integrateFull(rebuilt(Ratio(lit(1), lit(2)) :: consts, others :+ (Sin(Product(lit(2), u)), lit(1))), v, d + 1)
    Option.when(!containsIntegral(r))(r)
  }

/** `P(v)·(a·v + b)^r`, `b ≠ 0`: `u = a·v + b`, so `v = (u − b)/a` and the integrand is a sum of
 *  powers of `u` once expanded (`x·(1 − x)^(1/2)`). */
private def linearBase(x: _Expression, v: _Variable, d: Int)(using t: Tier): Option[_Expression] =
  val (consts, fs) = powersOf(x, v)
  fs.iterator.collect {
    case (base, k) if whole(k).isEmpty && collect(base, v).exists(cs =>
      cs.sizeIs == 2 && cs.forall(c => !dependsOn(c, v)) && !isZeroLiteral(cs(0))) => base
  }.toList.headOption.flatMap { base =>
    val cs = collect(base, v).getOrElse(Vector.empty)
    val u  = freshVar(x.freeVars + v.variable)
    val vOfU = Ratio(Sum(u, Product(lit(-1), cs(0))), cs(1))
    val body = substitute(replaceSubexpr(rebuilt(consts, fs), base, u), Map(v.variable -> vOfU))
    if dependsOn(body, v) then None
    else
      val r = integrateFull(expand(Ratio(body, cs(1))), u, d + 1)
      Option.when(!containsIntegral(r))(substitute(r, Map(u.variable -> base)))
  }

/** `v·F(v²)`: `w = v²`, `dv = dw/(2v)`, when `v` occurs only in even powers once one factor `v` is
 *  taken (`x³·(1 + x²)^(1/2)`). */
private def squareSubstitution(x: _Expression, v: _Variable, d: Int)(using t: Tier): Option[_Expression] =
  val w = freshVar(x.freeVars + v.variable)
  def even(e: _Expression): Option[_Expression] = e match
    case Power(b: _Variable, k) if b.variable == v.variable => whole(k).filter(_ % 2 == 0).map(n => Power(w, lit(n / 2)))
    case b: _Variable if b.variable == v.variable           => None
    case other => other.children.foldRight(Option(List.empty[_Expression]))((c, acc) =>
                    acc.flatMap(cs => even(c).map(_ :: cs))).map(other.rebuild)
  val (consts, fs) = powersOf(x, v)
  val taken = fs.map {
    case (b: _Variable, k) if b.variable == v.variable => (b, simplifyFullyInTier(Sum(k, lit(-1)), t.exact))
    case other                                         => other
  }
  if taken == fs then None
  else
    even(rebuilt(Ratio(lit(1), lit(2)) :: consts, taken.filterNot((_, k) => isZeroLiteral(k)))).flatMap { body =>
      val r = integrateFull(body, w, d + 1)
      Option.when(!containsIntegral(r))(substitute(r, Map(w.variable -> Power(v, lit(2)))))
    }

/** `v` only in powers `v^(p/q)` (and whole ones): `v = u^q` makes the integrand rational in `u`
 *  (`x^(1/2)/(x² + x)`). */
private def rationalising(x: _Expression, v: _Variable, d: Int)(using t: Tier): Option[_Expression] =
  val exponents = scala.collection.mutable.ListBuffer[Double]()
  def scan(e: _Expression): Unit = e match
    case Power(b: _Variable, _Number(k)) if b.variable == v.variable => exponents += k
    case other => other.children.foreach(scan)
  scan(x)
  val fractional = exponents.filter(k => k != math.rint(k))
  (2 to 12).find(q => fractional.nonEmpty && exponents.forall(k => math.abs(k * q - math.rint(k * q)) < 1e-12))
    .flatMap { q =>
      val u = freshVar(x.freeVars + v.variable)
      def put(e: _Expression): _Expression = e match
        case Power(b: _Variable, _Number(k)) if b.variable == v.variable => Power(u, lit(math.rint(k * q).toInt))
        case b: _Variable if b.variable == v.variable                    => Power(u, lit(q))
        case other                                                       => other.rebuild(other.children.map(put))
      val body = Product(Product(lit(q), Power(u, lit(q - 1))), put(x))
      val r    = integrateFull(normalizeIntegrand(body, u), u, d + 1)
      Option.when(!containsIntegral(r))(substitute(r, Map(u.variable -> Power(v, Ratio(lit(1), lit(q))))))
    }

/** `v` only inside `ln(v)` and as a power of itself: `v = e^u` (`cos(ln(x))`). */
private def expOfLog(x: _Expression, v: _Variable, d: Int)(using t: Tier): Option[_Expression] =
  val log = Ln(v)
  def has(e: _Expression): Boolean = e == log || e.children.exists(has)
  if !has(x) then None
  else
    val u    = freshVar(x.freeVars + v.variable)
    val body = Product(substitute(replaceSubexpr(x, log, u), Map(v.variable -> Exp(u))), Exp(u))
    if dependsOn(body, v) then None
    else
      val r = integrateFull(simplifyFullyInTier(body, t.exact), u, d + 1)
      Option.when(!containsIntegral(r))(simplifyFullyInTier(substitute(r, Map(u.variable -> log)), t.exact))

/** `∫ k·e^(av + c)·sin(bv + d)` and `·cos(bv + d)` in closed form, `a` and `b` numeric:
 *  `e^u·(a·sin w − b·cos w)/(a² + b²)` and `e^u·(a·cos w + b·sin w)/(a² + b²)`.  The table's
 *  entries matched only some spellings of the slopes (`exp(-x)`, `exp(x/2)` missed them).
 *
 *  @param e the integrand
 *  @param v the integration variable
 *  @return the antiderivative, or `None` when `e` is not that shape
 */
private[scalar] def expTrigProduct(e: _Expression, v: _Variable)(using t: Tier): Option[_Expression] =
  def flat(x: _Expression): List[_Expression] = x match
    case Product(a, b) => flat(a) ++ flat(b)
    case other         => List(other)
  def slope(u: _Expression): Option[_Expression] =
    collect(u, v).filter(cs => cs.sizeIs == 2 && cs.forall(c => !dependsOn(c, v))).map(_(1))
      .filter(_.eval(EmptyEnv) match
        case Right(_Number(a)) => a != 0 && a.isFinite
        case _                 => false)
  val (consts, fs) = flat(e).partition(x => !dependsOn(x, v))
  val pair = fs match
    case List(Exp(u), w) => Some((u, w))
    case List(w, Exp(u)) => Some((u, w))
    case _               => None
  for
    (u, w)        <- pair
    (arg, cosine) <- w match
                       case Sin(x) => Some((x, false))
                       case Cos(x) => Some((x, true))
                       case _      => None
    a             <- slope(u)
    b             <- slope(arg)
  yield
    val den = Sum(Power(a, lit(2)), Power(b, lit(2)))
    val num =
      if cosine then Sum(Product(a, Cos(arg)), Product(b, Sin(arg)))
      else Sum(Product(a, Sin(arg)), Product(lit(-1), Product(b, Cos(arg))))
    val k = consts.foldLeft[_Expression](lit(1))(Product(_, _))
    simplifyFullyInTier(Product(k, Ratio(Product(Exp(u), num), den)), t.exact)

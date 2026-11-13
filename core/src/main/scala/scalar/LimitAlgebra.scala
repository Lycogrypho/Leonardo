package it.grypho.scala.leonardo
package scalar

import core.*


/** The limit algebra (F_0286): the limits of the parts combined while the form is determinate.
 *
 *  The engine's other tiers sample numerically, and a limit `0` reached only by samples declines by
 *  design (F_0211) — so a free parameter, a growth comparison at infinity or an infinite one-sided
 *  limit could not be answered at all.  This tier reads the expression instead: a finite limit with
 *  the side it is approached from, or `±∞`; `c/0±` is `±∞`; an indeterminate quotient at a finite
 *  point is read off the leading terms of two Taylor series (F_0277's engine), at infinity off
 *  **growth classes** (exponential rate, polynomial degree, log power, coefficient).  It is consulted
 *  only when every other tier declined, so no answer the engine gave before changes.
 *
 *  **A generic answer records what it assumed** (the owner's decision): dividing by a symbolic finite
 *  value notes it `≠ 0`, a logarithm of one notes it `> 0`, and the REPL appends them.
 *
 *  @param v     the limit variable
 *  @param env   bindings, the tier and the working precision
 *  @param exact whether the constants are built exact
 */
private[scalar] final class LimitAlgebra(v: _Variable, env: Environment, exact: Boolean):

  /** The conditions a generic answer assumed, as text. */
  val conditions: scala.collection.mutable.LinkedHashSet[String] = scala.collection.mutable.LinkedHashSet()

  /** A limit value: finite (possibly symbolic) with the sign of `f − value` near the point (`Some(0)`:
   *  identically the value, `None`: unknown), or an infinity. */
  enum LVal:
    case Fin(value: _Expression, side: Option[Int])
    case Inf(sign: Int)
  import LVal.*

  /** Where `v` goes: a finite point from one side, or an infinity. */
  enum Pt:
    case At(p: _Expression, side: Int)
    case Infinity(sign: Int)
  import Pt.*

  private def lit(n: Int): _Expression = if exact then _Rational(n) else _Number(n)
  private def fold(e: _Expression): _Expression = e.eval(env).fold(simplifyFullyInTier(_, exact), x => x)
  private def numeric(e: _Expression): Option[Double] = e.eval(env) match
    case Right(_Number(d)) if d.isFinite => Some(d)
    case _                                => None
  private def isZero(e: _Expression): Boolean = isZeroLiteral(e) || numeric(e).contains(0.0)
  private def sgn(d: Double): Int = if d > 0 then 1 else if d < 0 then -1 else 0

  /** The limit of `e` as `v` goes to `pt`, or `None` when this algebra cannot decide it. */
  def lim(e: _Expression, pt: Pt, depth: Int = 0): Option[LVal] =
    if depth > MaxDepth then None
    else if !dependsOn(e, v) then Some(Fin(fold(e), Some(0)))
    else pt match
      case Infinity(_) => growthAt(e, pt).orElse(structural(e, pt, depth))
      case _           => structural(e, pt, depth)

  private val MaxDepth = 6

  private def structural(e: _Expression, pt: Pt, depth: Int): Option[LVal] = e match
    case x: _Variable if x == v => pt match
      case At(p, s)    => Some(Fin(fold(p), Some(s)))
      case Infinity(s) => Some(Inf(s))
    case Sum(a, b)     =>
      (for la <- lim(a, pt, depth); lb <- lim(b, pt, depth); r <- add(la, lb) yield r).orElse {
        // ∞ − ∞: expanded, like terms fold (`x⁻² − (x⁻² − 1)` is `1`), which simplify does not do
        val ex = expand(e)
        if ex != e && depth < MaxDepth then lim(ex, pt, depth + 1) else None
      }
    case Product(a, b) => for la <- lim(a, pt, depth); lb <- lim(b, pt, depth); r <- times(a, b, la, lb, pt, depth) yield r
    case Ratio(a, b)   => for la <- lim(a, pt, depth); lb <- lim(b, pt, depth); r <- over(a, b, la, lb, pt, depth) yield r
    case Power(b, k) if !dependsOn(k, v) => lim(b, pt, depth).flatMap(power(_, fold(k)))
    case Power(b, k)   => lim(Exp(Product(k, Ln(b))), pt, depth)
    case Exp(u)        => lim(u, pt, depth).map {
      case Inf(1)     => Inf(1)
      case Inf(_)     => Fin(lit(0), Some(1))
      case Fin(c, _)  => Fin(fold(Exp(c)), None)
    }
    case Ln(u)         => lim(u, pt, depth).flatMap {
      case Inf(1)                         => Some(Inf(1))
      case Fin(c, s) if isZero(c)         => Option.when(s.contains(1))(Inf(-1))
      case Fin(c, _) => numeric(c) match
        case Some(d) if d > 0 => Some(Fin(fold(Ln(c)), None))
        case Some(_)          => None
        case None             => conditions += s"$c > 0"; Some(Fin(fold(Ln(c)), None))
      case _ => None
    }
    case Sin(u) => lim(u, pt, depth).flatMap(trig(_, cosine = false))
    case Cos(u) => lim(u, pt, depth).flatMap(trig(_, cosine = true))
    case Tg(u)  => lim(Ratio(Sin(u), Cos(u)), pt, depth)
    case Sec(u) => lim(Ratio(lit(1), Cos(u)), pt, depth)
    case Csc(u) => lim(Ratio(lit(1), Sin(u)), pt, depth)
    case Cot(u) => lim(Ratio(Cos(u), Sin(u)), pt, depth)
    case Atan(u) => lim(u, pt, depth).map {
      case Inf(s)    => Fin(fold(Product(lit(s), Ratio(_Number(math.Pi), lit(2)))), None)
      case Fin(c, _) => Fin(fold(Atan(c)), None)
    }
    // The values at an infinity of the functions that have one (F_0288: ∫ e^(−x²) over ℝ needs erf(±∞)).
    case Erf(u)  => lim(u, pt, depth).flatMap {
      case Inf(s)    => Some(Fin(lit(s), None))
      case Fin(c, _) => Some(Fin(fold(Erf(c)), None))
    }
    case Erfc(u) => lim(u, pt, depth).flatMap {
      case Inf(s)    => Some(Fin(lit(if s > 0 then 0 else 2), None))
      case Fin(c, _) => Some(Fin(fold(Erfc(c)), None))
    }
    case Tanh(u) => lim(u, pt, depth).flatMap {
      case Inf(s)    => Some(Fin(lit(s), None))
      case Fin(c, _) => Some(Fin(fold(Tanh(c)), None))
    }
    // F_0332: the integer parts jump, so the side the argument comes from decides.
    case f @ (_: Floor | _: Ceil | _: Round) =>
      val u = f.children.head
      // the size of the terms that form the argument at the point, for the rounding of its limit (F_0342)
      lazy val magnitude = pt match
        case At(p, _) => magnitudeOf(substitute(u, Map(v.variable -> p)), env)
        case _        => None
      lim(u, pt, depth).flatMap(integerPart(f, _, magnitude))
    // Any other function on the allowlist, continuous at a finite value it is defined at (F_0332, the
    // owner's decision: continuity is declared, so a function added later is declined by default).
    case f: _Function if f.children.sizeIs == 1 && continuousWhereDefined(f) => lim(f.children.head, pt, depth).flatMap {
      case Fin(c, s) => valueAt(f, c, s).map(Fin(_, None))
      case _         => None
    }
    case _ => None

  /** `f(c)` as the limit of `f` where its argument tends to the finite `c` from `side`, or `None` (F_0341).
   *  The value must be a finite number or an expression holding free names — never a closed node that did
   *  not fold (`digamma(0.0)`, a failed computation: the third Laurent trap) — and at a numeric `c` the
   *  samples beside it must agree with it or approach it, as substitution is asked to ([[continuousAt]]):
   *  `sec` at the reading of `π/2` is `1.6E16`, a pole at its rounding.  Both function arms, at a point and
   *  in the growth classes at infinity, read their value here. */
  private def valueAt(f: _Function, c: _Expression, side: Option[Int]): Option[_Expression] =
    f.rebuild(List(c)).eval(env) match
      // a closed `c` that stays a symbol (`pi/2` in the exact tier) is read at its value, or the check is skipped
      case Right(r @ _Number(d)) if d.isFinite => numeric(c).orElse(evalNumeric(c, env).toOption.collect {
          case _Number(x) if x.isFinite => x
        }) match
        case Some(cd) =>
          val dir = side match
            case Some(1)  => LimitDir.FromRight
            case Some(-1) => LimitDir.FromLeft
            case _        => LimitDir.Both
          if !(side.contains(0) || continuousAt(f.rebuild(List(v)), v, cd, d, dir, env)) then None
          // a reading of zero (`cos` at the reading of `π/2` is `6.1E-17`) is zero, not a growth coefficient
          // whose reciprocal is `1.6E16`: within four ulps of `c` times the slope there
          else if d != 0 && numericAt(derive(f.rebuild(List(v)), v), v, cd, env)
                              .exists(s => math.abs(d) <= 4 * math.ulp(cd) * math.abs(s)) then Some(lit(0))
          else Some(r)
        case None => Some(r)
      case Left(x) if !dependsOn(x, v) && x.freeVars.nonEmpty => Some(simplifyFullyInTier(x, exact))
      case _ => None

  /** `floor`, `ceil` or `round` of a limit `l` (F_0332): the value at `c` where the function does not jump
   *  there; at a jump `j`, the side decides — from above `floor` is `j`, `ceil` `j + 1`, `round` `j + 1/2`;
   *  from below `j − 1`, `j` and `j − 1/2` — and with no side known, or a value that is not a number,
   *  `None`.  The answer is locally constant, so its side is `0`.
   *  @param magnitude the size of the terms that form the argument at the point, when known */
  private def integerPart(f: _Function, l: LVal, magnitude: => Option[Double]): Option[LVal] = l match
    case Inf(_) => None
    case Fin(c, s) => numeric(c).flatMap { d =>
      def at(x: _Expression) = Some(Fin(fold(x), Some(0)))
      val half = Ratio(lit(1), lit(2))
      jumpAt(f, c, d, magnitude) match
        case Some(j) if !s.contains(0) => (f, s) match
          case (_: Floor, Some(1))  => at(j)
          case (_: Floor, Some(-1)) => at(Sum(j, lit(-1)))
          case (_: Ceil, Some(1))   => at(Sum(j, lit(1)))
          case (_: Ceil, Some(-1))  => at(j)
          case (_: Round, Some(1))  => at(Sum(j, half))
          case (_: Round, Some(-1)) => at(Sum(j, Product(lit(-1), half)))
          case _                    => None
        case _ => at(f.rebuild(List(c)))
    }

  /** The jump of `f` at `c` (an integer for `floor`/`ceil`, a half-integer for `round`), or `None` when `f`
   *  does not jump there.  An exact `c` is tested exactly; a `Double` reading `d` is at a jump when it lies
   *  within four ulps of one, at the size of the terms that formed it (F_0342: `0.3/0.1` is
   *  `2.9999999999999996`, which an exact comparison read as no jump, so `limit(floor(x/0.1), x, 0.3)`
   *  answered `2.0` where the sides give `2` and `3`). */
  private def jumpAt(f: _Function, c: _Expression, d: Double, magnitude: => Option[Double]): Option[_Expression] =
    val round = f.isInstanceOf[Round]
    if _Surd.isExact(c) then
      val t     = if round then fold(Product(_Rational(2), c)) else c
      val whole = isZeroLiteral(fold(Sum(t, Product(_Rational(-1), Floor(t)))))
      val odd   = !round || !isZeroLiteral(fold(Mod(t, _Rational(2))))
      Option.when(whole && odd)(c)
    else
      val t     = if round then 2 * d else d
      val k     = math.rint(t)
      val scale = math.max(math.abs(t), magnitude.getOrElse(0.0) * (if round then 2 else 1))
      val near  = math.abs(t - k) <= 4 * math.ulp(scale)
      Option.when(near && (!round || math.abs(k % 2) == 1))(_Number(if round then k / 2 else k))

  /** `sin c` or `cos c`; a zero (to the function's range) is read with its side from the derivative. */
  private def trig(l: LVal, cosine: Boolean): Option[LVal] = l match
    case Inf(_)    => None
    case Fin(c, s) => numeric(c) match
      case Some(d) =>
        val value = if cosine then math.cos(d) else math.sin(d)
        val slope = if cosine then -math.sin(d) else math.cos(d)
        if math.abs(value) <= 1e-12 then Some(Fin(lit(0), s.map(_ * sgn(slope))))
        else Some(Fin(fold(if cosine then Cos(c) else Sin(c)), None))
      case None => Some(Fin(fold(if cosine then Cos(c) else Sin(c)), None))

  private def add(la: LVal, lb: LVal): Option[LVal] = (la, lb) match
    case (Fin(a, sa), Fin(b, sb)) =>
      val side = (sa, sb) match
        case (Some(0), s)              => s
        case (s, Some(0))              => s
        case (Some(x), Some(y)) if x == y => Some(x)
        case _                         => None
      Some(Fin(fold(Sum(a, b)), side))
    case (Fin(_, _), Inf(s))  => Some(Inf(s))
    case (Inf(s), Fin(_, _))  => Some(Inf(s))
    case (Inf(s), Inf(t)) if s == t => Some(Inf(s))
    case _ => None                                           // ∞ − ∞

  private def times(a: _Expression, b: _Expression, la: LVal, lb: LVal, pt: Pt, depth: Int): Option[LVal] =
    (la, lb) match
      case (Fin(x, sx), Fin(y, sy)) =>
        val side = (numeric(x), numeric(y)) match
          case (Some(0.0), Some(q)) if q != 0 => sx.map(_ * sgn(q))
          case (Some(p), Some(0.0)) if p != 0 => sy.map(_ * sgn(p))
          case (Some(0.0), Some(0.0))        => for s <- sx; t <- sy yield s * t
          // a constant factor scales the other's side by its sign (F_0342: `x/0.1` from above)
          case (Some(_), Some(q)) if sy.contains(0) && q != 0 => sx.map(_ * sgn(q))
          case (Some(p), Some(_)) if sx.contains(0) && p != 0 => sy.map(_ * sgn(p))
          case _                              => None
        Some(Fin(fold(Product(x, y)), side))
      case (Inf(s), Inf(t)) => Some(Inf(s * t))
      case (Fin(c, _), Inf(s)) => infTimes(c, s, a, b, pt, depth, zeroFirst = true)
      case (Inf(s), Fin(c, _)) => infTimes(c, s, b, a, pt, depth, zeroFirst = true)

  /** `c·(±∞)`: decided by `c`'s sign; `0·∞` is rewritten as the quotient `zero/(1/inf)`. */
  private def infTimes(c: _Expression, s: Int, finite: _Expression, infinite: _Expression, pt: Pt, depth: Int,
                       zeroFirst: Boolean): Option[LVal] =
    numeric(c) match
      case Some(0.0) => lim(Ratio(finite, reciprocal(infinite)), pt, depth + 1)
      case Some(d)   => Some(Inf(s * sgn(d)))
      case None      => None

  /** `1/e` written as the series engine reads it: a quotient flipped, a constant power negated —
   *  `1/(1/x²)` would read as a pole at 0 where `x²` expands. */
  private def reciprocal(e: _Expression): _Expression = e match
    case Ratio(n, d) if isOneLiteral(n) => d
    case Ratio(n, d)                    => Ratio(d, n)
    case Power(b, k) if !dependsOn(k, v) => Power(b, fold(Product(lit(-1), k)))
    case other                          => Ratio(lit(1), other)

  private def over(a: _Expression, b: _Expression, la: LVal, lb: LVal, pt: Pt, depth: Int): Option[LVal] =
    (la, lb) match
      case (Fin(n, sn), Fin(d, sd)) if isZero(d) =>
        if isZero(n) then indeterminate(a, b, pt, depth)
        else (numeric(n), sd) match
          case (Some(q), Some(s)) if s != 0 => Some(Inf(sgn(q) * s))
          case _                            => None
      case (Fin(n, sn), Fin(d, sd)) =>
        if numeric(d).isEmpty then conditions += s"${fold(d)} != 0"
        val side = (sn, numeric(d)) match
          case (Some(s), Some(q)) if isZero(n) => Some(s * sgn(q))
          // over a constant, the numerator's side scaled by its sign (F_0342)
          case (Some(s), Some(q)) if sd.contains(0) && q != 0 => Some(s * sgn(q))
          case _                  => None
        Some(Fin(fold(Ratio(n, d)), side))
      case (Inf(s), Fin(d, _)) => numeric(d) match
        case Some(q) if q != 0 => Some(Inf(s * sgn(q)))
        case _                 => None
      case (Fin(n, _), Inf(s)) =>
        val side = numeric(n).filter(_ != 0).map(q => sgn(q) * s)
        Some(Fin(lit(0), side))
      case (Inf(_), Inf(_)) => indeterminate(a, b, pt, depth)

  private def power(lb: LVal, k: _Expression): Option[LVal] =
    numeric(k).flatMap { kv =>
      val whole = kv == math.rint(kv)
      lb match
        case Fin(c, s) if isZero(c) => s match
          case Some(1) => Some(if kv > 0 then Fin(lit(0), Some(1)) else if kv < 0 then Inf(1) else Fin(lit(1), Some(0)))
          case Some(-1) if whole =>
            val odd = (kv.toLong % 2) != 0
            Some(if kv > 0 then Fin(lit(0), Some(if odd then -1 else 1)) else if kv < 0 then Inf(if odd then -1 else 1)
                 else Fin(lit(1), Some(0)))
          case _ => None
        case Fin(c, _) => numeric(c) match
          case Some(d) if d < 0 && !whole => None          // the principal value is complex (F_0297)
          case _                         => Some(Fin(fold(Power(c, k)), None))
        case Inf(1)  => Some(if kv > 0 then Inf(1) else if kv < 0 then Fin(lit(0), Some(1)) else Fin(lit(1), Some(0)))
        case Inf(_) if whole =>
          val odd = (kv.toLong % 2) != 0
          Some(if kv > 0 then Inf(if odd then -1 else 1) else if kv < 0 then Fin(lit(0), None) else Fin(lit(1), Some(0)))
        case _ => None
    }

  /** `0/0` or `∞/∞`: at a finite point the leading terms of two Taylor series, at infinity the growth
   *  classes; failing those, L'Hôpital's rule. */
  private def indeterminate(a: _Expression, b: _Expression, pt: Pt, depth: Int): Option[LVal] =
    val bySeries = pt match
      case At(p, s) => seriesQuotient(a, b, p, s)
      case _        => growthAt(Ratio(a, b), pt)
    // L'Hôpital is held to the environment's step budget, as the engine's own tier is (F_0124).
    bySeries.orElse(Option.when(depth < env.limits.count(Limit.MaxHopitalSteps))(()).flatMap(_ =>
      lim(Ratio(simplifyFullyInTier(deriveInTier(a, v, exact), exact),
                simplifyFullyInTier(deriveInTier(b, v, exact), exact)), pt, depth + 1)))

  /** `a/b` at `p` from its leading Taylor terms: orders `k` and `m`, coefficients `cₖ`, `dₘ`. */
  private def seriesQuotient(a: _Expression, b: _Expression, p: _Expression, side: Int): Option[LVal] =
    val order = 10
    def leading(e: _Expression): Option[(Int, _Expression)] =
      powerSeriesIn(e, v, p, order, MaxDerivativeNodes, exact).toOption.flatMap { cs =>
        val scale = cs.flatMap(numeric).map(math.abs).maxOption.getOrElse(0.0)
        cs.zipWithIndex.find((c, _) => numeric(c) match
          case Some(d) => math.abs(d) > 1e-12 * scale
          case None    => !isZeroLiteral(c)
        ).map((c, k) => (k, c))
      }
    for
      (k, ck) <- leading(a)
      (m, dm) <- leading(b)
      r <- if k > m then Some(Fin(lit(0), None))
           else if k == m then
             if numeric(dm).isEmpty then conditions += s"${fold(dm)} != 0"
             Some(Fin(fold(Ratio(ck, dm)), None))
           else numeric(fold(Ratio(ck, dm))).map(r => Inf(sgn(r) * (if (m - k) % 2 == 1 then side else 1)))
    yield r

  // ── growth classes at infinity ──

  /** `coef · e^(rate·v) · v^deg · ln(v)^logp`, the leading behaviour of a factor at `+∞`. */
  private case class G(coef: _Expression, rate: Double, deg: Double, logp: Double):
    def *(o: G): G = G(fold(Product(coef, o.coef)), rate + o.rate, deg + o.deg, logp + o.logp)
    def /(o: G): G = G(fold(Ratio(coef, o.coef)), rate - o.rate, deg - o.deg, logp - o.logp)
    def order: (Double, Double, Double) = (rate, deg, logp)

  /** The limit at an infinity from growth classes, or `None`. */
  private def growthAt(e: _Expression, pt: Pt): Option[LVal] = pt match
    case Infinity(sign) =>
      val f = if sign > 0 then e else substitute(e, Map(v.variable -> Product(lit(-1), v)))
      growth(f).flatMap { g =>
        val o = compare(g.order, (0.0, 0.0, 0.0))
        if o > 0 then numeric(g.coef).filter(_ != 0).map(c => Inf(sgn(c)))
        else if o < 0 then Some(Fin(lit(0), None))
        else Some(Fin(g.coef, None))
      }
    case _ => None

  private def compare(a: (Double, Double, Double), b: (Double, Double, Double)): Int =
    def c(x: Double, y: Double) = if math.abs(x - y) <= 1e-12 * math.max(1.0, math.max(math.abs(x), math.abs(y))) then 0
                                  else if x > y then 1 else -1
    val r = c(a._1, b._1)
    if r != 0 then r else { val d = c(a._2, b._2); if d != 0 then d else c(a._3, b._3) }

  /** The growth class of `e` at `+∞`, or `None` when it has none this algebra reads. */
  private def growth(e: _Expression): Option[G] =
    if !dependsOn(e, v) then Option.when(!isZero(fold(e)))(G(fold(e), 0, 0, 0))
    else e match
      case x: _Variable if x == v => Some(G(lit(1), 0, 1, 0))
      case Product(a, b) => for ga <- growth(a); gb <- growth(b) yield ga * gb
      case Ratio(a, b)   => for ga <- growth(a); gb <- growth(b) yield ga / gb
      case Power(b, k) if !dependsOn(k, v) => numeric(k).flatMap { kv =>
        growth(b).filter(g => kv == math.rint(kv) || numeric(g.coef).exists(_ > 0))
          .map(g => G(fold(Power(g.coef, k)), g.rate * kv, g.deg * kv, g.logp * kv))
      }
      case Power(a, u) if !dependsOn(a, v) => (numeric(a), linear(u)) match
        case (Some(base), Some((slope, icpt))) if base > 0 => Some(G(fold(Power(a, icpt)), slope * math.log(base), 0, 0))
        case _                                              => growth(Exp(Product(u, Ln(a))))
      case Power(b, u) => growth(Exp(Product(u, Ln(b))))
      case Exp(u) => linear(u) match
        case Some((slope, icpt)) => Some(G(fold(Exp(icpt)), slope, 0, 0))
        case None => lim(u, Infinity(1)).collect { case Fin(c, _) => G(fold(Exp(c)), 0, 0, 0) }
      case Ln(u) => growth(u).flatMap { g =>
        if g.rate == 0 && g.deg > 0 then Some(G(_Number(g.deg), 0, 0, 1))
        else if g.rate == 0 && g.deg == 0 && g.logp == 0 then
          // ln u with u → c: ln c if c ≠ 1, else ln(1 + (u − 1)) ~ u − 1
          numeric(g.coef) match
            case Some(c) if math.abs(c - 1) <= 1e-12 => growth(Sum(u, lit(-1)))
            case Some(c) if c > 0                    => Some(G(fold(Ln(g.coef)), 0, 0, 0))
            case _                                   => None
        else None
      }
      // u → 0: sin u ~ tan u ~ asin u ~ atan u ~ sinh u ~ tanh u ~ u
      case f @ (_: Sin | _: Tg | _: Asin | _: Atan | _: Sinh | _: Tanh) =>
        val u = f.children.head
        lim(u, Infinity(1)) match
          case Some(Fin(c, _)) if isZero(c) => growth(u)
          case Some(Fin(c, s))              => valueAt(f, c, s).filter(x => !isZero(x)).map(G(_, 0, 0, 0))
          case _                            => None
      case Sum(_, _) => sumGrowth(e)
      case f: _Function if f.children.sizeIs == 1 && continuousWhereDefined(f) =>
        lim(f.children.head, Infinity(1)).flatMap {
          case Fin(c, s) => valueAt(f, c, s)
          case _         => None
        }.filter(x => !isZero(x)).map(G(_, 0, 0, 0))
      case _ => None

  /** A sum's growth: a polynomial by its leading term, else the strictly dominant term's, ties summed. */
  private def sumGrowth(e: _Expression): Option[G] =
    collect(e, v).filter(_.forall(c => !dependsOn(c, v))).flatMap { cs =>
      cs.zipWithIndex.reverse.find((c, _) => !isZero(fold(c))).map((c, k) => G(fold(c), 0, k, 0))
    }.orElse {
      def terms(x: _Expression): List[_Expression] = x match
        case Sum(a, b) => terms(a) ++ terms(b)
        case other     => List(other)
      core.sequence(terms(e).map(growth)).flatMap { gs =>
        val top  = gs.reduce((a, b) => if compare(a.order, b.order) >= 0 then a else b)
        val tied = gs.filter(g => compare(g.order, top.order) == 0)
        val coef = fold(tied.map(_.coef).reduce(Sum.apply))
        Option.when(!isZero(coef))(top.copy(coef = coef))
      }
    }

  /** `u = slope·v + intercept` with a numeric slope, or `None`. */
  private def linear(u: _Expression): Option[(Double, _Expression)] =
    collect(u, v).filter(cs => cs.sizeIs == 2).flatMap(cs => numeric(cs(1)).filter(_ != 0).map(s => (s, cs(0))))


/** Whether a one-argument function is **continuous wherever it is defined** (F_0332), so that its limit is
 *  its value at the limit of its argument.  An allowlist, by the owner's decision: the generic arm used to
 *  assume it of every `_Function`, and `floor`, `ceil` and `round` (F_0324) then answered a two-sided limit
 *  at their jumps — as did the functionals whose one child is their operand (`derive`, `integral`, the
 *  transforms), for which the property means nothing.  A function added later is declined by default; add
 *  it here when it is continuous where defined.  A pole (`Gamma` at `0`) is not a point of the domain and
 *  does not evaluate, so the arm declines it. */
private def continuousWhereDefined(f: _Function): Boolean = f match
  case _: Exp | _: Ln | _: Abs | _: Sin | _: Cos | _: Tg | _: Asin | _: Acos | _: Atan
     | _: Sinh | _: Cosh | _: Tanh | _: Asinh | _: Acosh | _: Atanh
     | _: Sec | _: Csc | _: Cot | _: Sech | _: Csch | _: Coth
     | _: Factorial | _: Gamma | _: LogGamma | _: Digamma | _: Erf | _: Erfc
     | _: Si | _: Ci | _: Ei | _: Li | _: FresnelS | _: FresnelC => true
  case _ => false

/** The limit-algebra answer for `e` as `v → point` (F_0286), with the conditions it assumed, or `None`.
 *  A two-sided limit at a finite point is answered when both sides agree. */
private[scalar] def limitAlgebra(e: _Expression, v: _Variable, point: _Expression, dir: LimitDir,
                                 env: Environment): Option[(_Expression, List[String])] =
  val exact = _Rational.containsExactIn(e, env) || _Rational.containsExact(point)
  val alg   = LimitAlgebra(v, env, exact)
  val body  = simplifyFullyInTier(e, exact)
  def value(l: alg.LVal): _Expression = l match
    case alg.LVal.Fin(x, _) => x
    case alg.LVal.Inf(s)    => _Number(if s > 0 then Double.PositiveInfinity else Double.NegativeInfinity)
  val answer: Option[_Expression] = point.eval(env) match
    case Right(_Number(p)) if p.isInfinite => alg.lim(body, alg.Pt.Infinity(if p > 0 then 1 else -1)).map(value)
    case _ =>
      def side(s: Int) = alg.lim(body, alg.Pt.At(point, s)).map(value)
      dir match
        case LimitDir.FromRight => side(1)
        case LimitDir.FromLeft  => side(-1)
        case LimitDir.Both      =>
          for r <- side(1); l <- side(-1) if r == l || (for x <- r.eval(env).toOption; y <- l.eval(env).toOption yield (x, y)).exists {
                // infinities by equality alone: |−∞ − ∞| ≤ 10⁻¹²·∞ would pass a relative test
                case (_Number(x), _Number(y)) => x == y ||
                  (x.isFinite && y.isFinite && math.abs(x - y) <= 1e-12 * math.max(math.abs(x), math.abs(y)))
                case (x, y)                   => x == y
              } yield r
  answer.map(a => (a, alg.conditions.toList))

/** The conditions a generic limit answer assumed (F_0286), for the REPL's note; empty when there were
 *  none or the algebra did not answer.
 *  @param e     the expression
 *  @param v     the limit variable
 *  @param point the limit point
 *  @param dir   the direction
 *  @param env   bindings
 *  @return the conditions, as text */
private[leonardo] def limitConditions(e: _Expression, v: _Variable, point: _Expression, dir: LimitDir,
                                      env: Environment): List[String] =
  limitAlgebra(e, v, point, dir, env).fold(Nil)(_._2)

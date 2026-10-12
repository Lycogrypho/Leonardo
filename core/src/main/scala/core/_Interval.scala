package it.grypho.scala.leonardo
package core

import spire.math.Real
import spire.math.Rational as SpireRational


/** A closed real interval `[lo, hi]` whose endpoints are **rigorous** bounds (F_0008).
 *
 *  The answer to the accuracy question the exact tier does not ask: what a computation's
 *  result can be, given that its inputs are only known to lie in ranges.  Every operation
 *  returns an interval guaranteed to contain every value the operation can take on its
 *  operands — the *containment* property — by rounding each endpoint outward.
 *
 *  **The first sibling of `_Number` that deliberately does NOT widen** (Decision A).
 *  `_Complex`, `_Truth`, `_Rational` and `_Based` all read as numbers through `_Number`'s
 *  widening extractor; an interval must not, because one read as a single `Double` would
 *  silently become one endpoint — the worst failure this library has.  So every
 *  `case _Number(x)` site simply does not match an interval, and every pass without an
 *  interval rule leaves the expression symbolic *by construction*.  Intervals are results
 *  of evaluation; they are not operands of symbolic algebra (F_0014 is that).
 *
 *  **The endpoint type chooses the bounds** (Decision B): two `_Rational` endpoints are an
 *  exact interval, combined exactly and bounded through spire at the working precision where an
 *  operation leaves the rationals; `Double` endpoints are combined in IEEE-754 and rounded
 *  outward.  Mixing the two demotes, outward — float contagion, as everywhere.
 *
 *  **Rounding, stated because it is where interval code goes wrong**: `+ − × ÷` and the square
 *  root detect an exact `Double` result with error-free transformations and widen by one ulp
 *  only on an inexact side, so `[1, 2] − [1, 2]` is exactly `[−1, 1]`.  A transcendental is
 *  widened by two ulps on both platforms, since ECMAScript does not specify the accuracy of
 *  `Math.sin` the way the JDK does — one figure everywhere, never a platform branch.  A value
 *  known exactly (`exp(0) = 1`) is not widened.
 *
 *  Endpoints are finite: a half-infinite interval is declined, not handled.  `lo == hi`
 *  collapses to the number itself.  Built only through [[_Interval.of]].
 *
 *  @param lo the lower endpoint, a `_Rational` or a `_Number`
 *  @param hi the upper endpoint, of the same kind
 */
final class _Interval private (val lo: _Value, val hi: _Value) extends _Value:

  /** True when both endpoints are exact rationals. */
  def isExact: Boolean = lo.isInstanceOf[_Rational]

  /** The lower endpoint as a `Double`, rounded down when it is an exact rational. */
  def loDouble: Double = _Interval.toDoubleDown(lo)

  /** The upper endpoint as a `Double`, rounded up when it is an exact rational. */
  def hiDouble: Double = _Interval.toDoubleUp(hi)

  /** Renders `[lo, hi]` at `precision` decimals, rounded **outward**: the lower end down and
   *  the upper end up, so the displayed interval still contains the computed one.  An exact
   *  endpoint that prints as a short fraction is shown as that fraction, unrounded.
   *  @param precision decimal places
   */
  def display(precision: Int): String =
    s"[${_Interval.endpointText(lo, precision, down = true)}, ${_Interval.endpointText(hi, precision, down = false)}]"

  /** The display at [[Environment.DefaultPrecision]], outward; `:save` writes the endpoints
   *  in full instead (the REPL's `serializeValue`). */
  override def toString: String = display(Environment.DefaultPrecision)

  override def eval(env: Environment): Either[_Expression, _Value] = Right(this)
  override def children: List[_Expression] = List.empty
  override def rebuild(c: List[_Expression]): _Expression = this

  override def equals(that: Any): Boolean = that match
    case i: _Interval => lo == i.lo && hi == i.hi
    case _            => false
  override def hashCode: Int = (lo, hi).##


/** Construction, the bound algebra, and the interval kernels of the elementary functions. */
object _Interval:

  /** The bounds an interval kernel works on: exact rationals, or `Double`s. */
  private enum B:
    case E(lo: _Rational, hi: _Rational)
    case D(lo: Double, hi: Double)

  import B.*

  /** An interval from two endpoints, or `None` when they do not make one: an endpoint that is
   *  not a real number, a non-finite or NaN one, or `lo > hi`.  Equal endpoints give the
   *  number itself.
   *  @param lo the lower endpoint
   *  @param hi the upper endpoint
   *  @return the interval, the number, or `None`
   */
  def of(lo: _Value, hi: _Value): Option[_Value] =
    (lo, hi) match
      case (a: _Rational, b: _Rational) => make(E(a, b))
      case (_: _Interval, _) | (_, _: _Interval) => None
      case (_Number(_), _Number(_))     => make(D(toDoubleDown(lo), toDoubleUp(hi)))
      case _                            => None

  /** The bounds of an operand: an interval's own, or a number as a point. */
  private def bounds(v: _Value): Option[B] = v match
    case i: _Interval =>
      (i.lo, i.hi) match
        case (a: _Rational, b: _Rational) => Some(E(a, b))
        case (_Number(a), _Number(b))     => Some(D(a, b))
        case _                            => None
    case r: _Rational => Some(E(r, r))
    case _Number(d) if !d.isNaN && !d.isInfinite => Some(D(d, d))
    case _            => None

  /** The bounds as `Double`s, an exact one rounded outward. */
  private def toD(b: B): (Double, Double) = b match
    case D(l, h) => (l, h)
    case E(l, h) => (toDoubleDown(l), toDoubleUp(h))

  /** The value the bounds describe; `None` when they are not an interval. */
  private def make(b: B): Option[_Value] = b match
    case E(l, h) =>
      val c = l.compare(h)
      if c > 0 then None else if c == 0 then Some(l) else Some(new _Interval(l, h))
    case D(l, h) =>
      if l.isNaN || h.isNaN || l.isInfinite || h.isInfinite || l > h then None
      else if l == h then Some(_Number(l))
      else Some(new _Interval(_Number(l), _Number(h)))

  /** Whether one operand is an interval and both have bounds: the arms' common gate. */
  private def operands(a: _Value, b: _Value): Option[(B, B)] =
    if !a.isInstanceOf[_Interval] && !b.isInstanceOf[_Interval] then None
    else for x <- bounds(a); y <- bounds(b) yield (x, y)

  // ── endpoints ──

  /** An endpoint as a `Double` no greater than it. */
  private[core] def toDoubleDown(v: _Value): Double = v match
    case r: _Rational =>
      val d = r.toDouble
      if _Rational.fromDouble(d).exists(_.compare(r) <= 0) then d else Math.nextDown(d)
    case _Number(d)   => d
    case _            => Double.NaN

  /** An endpoint as a `Double` no less than it. */
  private[core] def toDoubleUp(v: _Value): Double = v match
    case r: _Rational =>
      val d = r.toDouble
      if _Rational.fromDouble(d).exists(_.compare(r) >= 0) then d else Math.nextUp(d)
    case _Number(d)   => d
    case _            => Double.NaN

  /** An endpoint at `precision` decimals, rounded toward `down`'s side. */
  private[core] def endpointText(v: _Value, precision: Int, down: Boolean): String =
    val mode = if down then java.math.RoundingMode.FLOOR else java.math.RoundingMode.CEILING
    v match
      case r: _Rational if r.display(precision).contains('/') || r.den == 1 => r.display(precision)
      case r: _Rational =>
        val exact = new java.math.BigDecimal(r.num.bigInteger)
          .divide(new java.math.BigDecimal(r.den.bigInteger), precision + 2, mode)
        _Number(exact.setScale(precision, mode).doubleValue).display(precision)
      case _Number(d) =>
        _Number(new java.math.BigDecimal(d).setScale(precision, mode).doubleValue).display(precision)
      case other => other.toString

  // ── directed Double arithmetic ──

  /** Veltkamp's splitter, `2²⁷ + 1`: splits a `Double` into two halves whose products are exact. */
  private val Splitter = 134217729.0
  /** Below this magnitude a product may be subnormal, where the error terms are not exact. */
  private val Tiny = 1e-290
  /** Above this magnitude the splitter may overflow. */
  private val Huge = 1e290

  /** The rounding error of `s = a + b` (Knuth's TwoSum): `a + b = s + err` exactly. */
  private def sumError(a: Double, b: Double, s: Double): Double =
    val bb = s - a
    (a - (s - bb)) + (b - bb)

  private def sumDown(a: Double, b: Double): Double =
    val s = a + b
    if s.isInfinite || s.isNaN then s else if sumError(a, b, s) < 0 then Math.nextDown(s) else s

  private def sumUp(a: Double, b: Double): Double =
    val s = a + b
    if s.isInfinite || s.isNaN then s else if sumError(a, b, s) > 0 then Math.nextUp(s) else s

  /** The rounding error of `p = a·b` (Dekker's TwoProduct, no fused multiply-add): `a·b = p + err`
   *  exactly, or `None` where over- or underflow makes the error terms inexact. */
  private def productError(a: Double, b: Double, p: Double): Option[Double] =
    if p == 0.0 && (a == 0.0 || b == 0.0) then Some(0.0)
    else if math.abs(p) < Tiny || math.abs(a) > Huge || math.abs(b) > Huge then None
    else
      def split(x: Double): (Double, Double) =
        val c = Splitter * x
        val h = c - (c - x)
        (h, x - h)
      val (ah, al) = split(a)
      val (bh, bl) = split(b)
      Some(((ah * bh - p) + ah * bl + al * bh) + al * bl)

  private def mulDown(a: Double, b: Double): Double =
    val p = a * b
    if p.isInfinite || p.isNaN then p
    else productError(a, b, p) match
      case Some(e) => if e < 0 then Math.nextDown(p) else p
      case None    => Math.nextDown(p)

  private def mulUp(a: Double, b: Double): Double =
    val p = a * b
    if p.isInfinite || p.isNaN then p
    else productError(a, b, p) match
      case Some(e) => if e > 0 then Math.nextUp(p) else p
      case None    => Math.nextUp(p)

  /** The sign of `a/b − q` for the computed quotient `q`: from the residual `a − q·b`, exact by
   *  TwoProduct (and Sterbenz for `a − p`); `None` where that cannot be trusted. */
  private def quotientSide(a: Double, b: Double, q: Double): Option[Int] =
    val p = q * b
    productError(q, b, p).map { e =>
      val r = (a - p) - e
      if r == 0.0 then 0 else (if (r > 0) == (b > 0) then 1 else -1)
    }

  private def divDown(a: Double, b: Double): Double =
    val q = a / b
    if q.isInfinite || q.isNaN then q
    else quotientSide(a, b, q) match
      case Some(s) => if s < 0 then Math.nextDown(q) else q
      case None    => Math.nextDown(q)

  private def divUp(a: Double, b: Double): Double =
    val q = a / b
    if q.isInfinite || q.isNaN then q
    else quotientSide(a, b, q) match
      case Some(s) => if s > 0 then Math.nextUp(q) else q
      case None    => Math.nextUp(q)

  /** The correctly rounded square root, bounded on each side by its exact residual. */
  private def sqrtBounds(x: Double): (Double, Double) =
    val s = math.sqrt(x)
    val p = s * s
    productError(s, s, p) match
      case Some(e) =>
        val r = (x - p) - e   // sign of x − s²
        if r == 0.0 then (s, s) else if r > 0 then (s, Math.nextUp(s)) else (Math.nextDown(s), s)
      case None => (Math.nextDown(s), Math.nextUp(s))

  /** Two ulps below and above a transcendental's computed value. */
  private def down2(x: Double): Double = Math.nextDown(Math.nextDown(x))
  private def up2(x: Double): Double = Math.nextUp(Math.nextUp(x))

  // ── the field operations ──

  /** `a + b`, when at least one is an interval; `None` otherwise or when not representable. */
  def add(a: _Value, b: _Value, env: Environment): Option[_Value] =
    operands(a, b).flatMap {
      case (E(a1, a2), E(b1, b2)) => make(E(a1.add(b1, env.rationalPolicy), a2.add(b2, env.rationalPolicy)))
      case (x, y) =>
        val ((a1, a2), (b1, b2)) = (toD(x), toD(y))
        make(D(sumDown(a1, b1), sumUp(a2, b2)))
    }

  /** `a · b`, when at least one is an interval. */
  def multiply(a: _Value, b: _Value, env: Environment): Option[_Value] =
    operands(a, b).flatMap {
      case (E(a1, a2), E(b1, b2)) =>
        val ps = for x <- List(a1, a2); y <- List(b1, b2) yield x.multiply(y, env.rationalPolicy)
        make(E(ps.reduce((p, q) => if p.compare(q) <= 0 then p else q),
               ps.reduce((p, q) => if p.compare(q) >= 0 then p else q)))
      case (x, y) =>
        val ((a1, a2), (b1, b2)) = (toD(x), toD(y))
        val pairs = List((a1, b1), (a1, b2), (a2, b1), (a2, b2))
        make(D(pairs.map(mulDown(_, _)).min, pairs.map(mulUp(_, _)).max))
    }

  /** `a / b`, when at least one is an interval; `None` when the divisor contains zero, since
   *  the quotient is then two disjoint rays, not an interval. */
  def divide(a: _Value, b: _Value, env: Environment): Option[_Value] =
    operands(a, b).flatMap {
      case (E(a1, a2), E(b1, b2)) =>
        if b1.signum <= 0 && b2.signum >= 0 then None
        else
          val qs = for x <- List(a1, a2); y <- List(b1, b2) yield x.divide(y, env.rationalPolicy)
          sequence(qs).flatMap { ps =>
            make(E(ps.reduce((p, q) => if p.compare(q) <= 0 then p else q),
                   ps.reduce((p, q) => if p.compare(q) >= 0 then p else q)))
          }
      case (x, y) =>
        val ((a1, a2), (b1, b2)) = (toD(x), toD(y))
        if b1 <= 0.0 && b2 >= 0.0 then None
        else
          val pairs = List((a1, b1), (a1, b2), (a2, b1), (a2, b2))
          make(D(pairs.map(divDown(_, _)).min, pairs.map(divUp(_, _)).max))
    }

  // ── powers ──

  /** The largest integer exponent an interval is raised to by repeated squaring. */
  private val MaxIntervalPower = 100000

  /** `a ^ e` for an interval `a` and a numeric exponent; `None` for an interval exponent, a
   *  negative power of an interval containing zero, a fractional power of an interval reaching
   *  below zero, or a result that is not representable.
   *
   *  An integer power keeps the even/odd distinction (`[−2, 1]² = [0, 4]`, not `[1, 4]`); a
   *  fractional one is monotone on `[0, ∞)`. */
  def power(a: _Value, e: _Value, env: Environment): Option[_Value] =
    if !a.isInstanceOf[_Interval] then None
    else (bounds(a), e) match
      case (Some(b), r: _Rational) =>
        r.toBigIntExact.filter(k => k.abs <= MaxIntervalPower) match
          case Some(k) => integerPower(b, k.toInt, env)
          case None    => fractionalPower(b, Left(r), env)
      case (Some(b), _Number(d)) if !d.isNaN && !d.isInfinite =>
        if d == math.rint(d) && math.abs(d) <= MaxIntervalPower then integerPower(toDBounds(b), d.toInt, env)
        else fractionalPower(toDBounds(b), Right(d), env)
      case _ => None

  /** Demotes exact bounds to `Double` ones, outward — an inexact exponent is float contagion. */
  private def toDBounds(b: B): B = b match
    case e: E => val (l, h) = toD(e); D(l, h)
    case d: D => d

  private def integerPower(b: B, n: Int, env: Environment): Option[_Value] =
    if n == 0 then Some(b match { case _: E => _Rational(1); case _: D => _Number(1) })
    else if n < 0 then
      if containsZero(b) then None else integerPower(b, -n, env).flatMap(reciprocal(_, env))
    else b match
      case E(l, h) =>
        for
          pl <- l.pow(n, env.rationalPolicy)
          ph <- h.pow(n, env.rationalPolicy)
          v  <- {
            val (lo, hi) = if pl.compare(ph) <= 0 then (pl, ph) else (ph, pl)
            if n % 2 == 0 && l.signum < 0 && h.signum > 0 then make(E(_Rational(0), hi)) else make(E(lo, hi))
          }
        yield v
      case D(l, h) =>
        val (ll, lu) = powBounds(l, n)
        val (hl, hu) = powBounds(h, n)
        if n % 2 == 1 then make(D(ll, hu))
        else if l >= 0 then make(D(ll, hu))
        else if h <= 0 then make(D(hl, lu))
        else make(D(0.0, math.max(lu, hu)))

  /** `1/p` for a value whose bounds do not contain zero: `1/x` is decreasing on each side. */
  private def reciprocal(p: _Value, env: Environment): Option[_Value] =
    bounds(p).flatMap {
      case E(l, h) =>
        for lo <- _Rational(1).divide(h, env.rationalPolicy); hi <- _Rational(1).divide(l, env.rationalPolicy); v <- make(E(lo, hi))
        yield v
      case D(l, h) => make(D(divDown(1.0, h), divUp(1.0, l)))
    }

  private def containsZero(b: B): Boolean = b match
    case E(l, h) => l.signum <= 0 && h.signum >= 0
    case D(l, h) => l <= 0.0 && h >= 0.0

  /** Rigorous `(lower, upper)` of `xⁿ`, `n ≥ 1`, by repeated squaring with directed rounding. */
  private def powBounds(x: Double, n: Int): (Double, Double) =
    val a = math.abs(x)
    @annotation.tailrec
    def go(base: (Double, Double), k: Int, acc: (Double, Double)): (Double, Double) =
      if k == 0 then acc
      else
        val next = if (k & 1) == 1 then (mulDown(acc._1, base._1), mulUp(acc._2, base._2)) else acc
        if k == 1 then next
        else go((mulDown(base._1, base._1), mulUp(base._2, base._2)), k >> 1, next)
    val (lo, hi) = go((a, a), n, (1.0, 1.0))
    if x >= 0 || n % 2 == 0 then (lo, hi) else (-hi, -lo)

  private def fractionalPower(b: B, e: Either[_Rational, Double], env: Environment): Option[_Value] =
    b match
      case E(l, h) =>
        e match
          case Left(r) =>
            if l.signum < 0 || (r.signum < 0 && l.signum == 0) then None
            else
              val f = (x: Real) => x.fpow(SpireRational(r.num, r.den))
              val (a, z) = if r.signum > 0 then (l, h) else (h, l)
              for lo <- exactBound(f, a, env, down = true); hi <- exactBound(f, z, env, down = false); v <- make(E(lo, hi)) yield v
          case Right(_) => None
      case D(l, h) =>
        val r = e match { case Left(q) => q.toDouble; case Right(d) => d }
        if l < 0 || (r < 0 && l == 0) then None
        else if r == 0.5 then make(D(sqrtBounds(l)._1, sqrtBounds(h)._2))
        else
          def known(x: Double): Option[Double] = if x == 1.0 then Some(1.0) else if x == 0.0 && r > 0 then Some(0.0) else None
          def lowAt(x: Double)  = known(x).getOrElse(math.max(0.0, down2(math.pow(x, r))))
          def highAt(x: Double) = known(x).getOrElse(up2(math.pow(x, r)))
          if r > 0 then make(D(lowAt(l), highAt(h))) else make(D(lowAt(h), highAt(l)))

  // ── exact bounds through spire ──

  /** A rational bound on `f(x)`: `f(x)` approximated to `p` bits by spire, then moved two units
   *  in the last bit outward — spire's `toRational(p)` is within `2⁻ᵖ` of the value. */
  private def exactBound(f: Real => Real, x: _Rational, env: Environment, down: Boolean): Option[_Rational] =
    val bits = Real.digitsToBits(env.workingPrecision + GuardDigits)
    val q    = f(toReal(x)).toRational(bits)
    val pad  = SpireRational(BigInt(1), BigInt(2).pow(bits - 1))
    val b    = if down then q - pad else q + pad
    _Rational.of(b.numerator.toBigInt, b.denominator.toBigInt)

  // ── the elementary functions ──

  /** A kernel's description of a function on one monotone piece. */
  private final case class Monotone(double: Double => Double, real: Real => Real, increasing: Boolean,
                                    known: Map[Double, Double])

  /** `f` on bounds where it is monotone, each end widened outward unless its value is known
   *  exactly; `clamp` cuts the result to the function's range. */
  private def monotone(b: B, f: Monotone, env: Environment, clamp: (Double, Double) => (Double, Double) = (l, h) => (l, h)): Option[_Value] =
    b match
      case E(l, h) =>
        val (a, z) = if f.increasing then (l, h) else (h, l)
        def at(x: _Rational, down: Boolean): Option[_Rational] =
          f.known.get(x.toDouble).filter(_ => _Rational.fromDouble(x.toDouble).exists(_.compare(x) == 0))
            .flatMap(_Rational.fromDouble)
            .orElse(exactBound(f.real, x, env, down))
        for lo <- at(a, down = true); hi <- at(z, down = false); v <- make(E(lo, hi)) yield v
      case D(l, h) =>
        val (a, z) = if f.increasing then (l, h) else (h, l)
        val lo = f.known.getOrElse(a, down2(f.double(a)))
        val hi = f.known.getOrElse(z, up2(f.double(z)))
        val (cl, ch) = clamp(lo, hi)
        make(D(cl, ch))

  /** Whether `phase + k·period` lies in `[lo, hi]` for some integer `k`, generously: a point
   *  within a relative `10⁻¹²` of an end counts, because counting a near extremum only widens
   *  the answer while missing a real one would break containment. */
  private def reaches(lo: Double, hi: Double, phase: Double, period: Double): Boolean =
    val m = 1e-12 * math.max(1.0, math.max(math.abs(lo), math.abs(hi)))
    val k = math.ceil((lo - m - phase) / period)
    phase + k * period <= hi + m

  private def unitClamp(l: Double, h: Double): (Double, Double) = (math.max(-1.0, l), math.min(1.0, h))

  /** `sin` or `cos` (`shift` = 0 or π/2): the ends' values, widened, then ±1 wherever a
   *  maximum or minimum lies inside — `sin([0, π])` is `[0, 1]`, not `[0, 0]`. */
  private def periodic(b: B, cosine: Boolean, env: Environment): Option[_Value] =
    val (l, h) = toD(b)
    val (maxPhase, minPhase) = if cosine then (0.0, math.Pi) else (math.Pi / 2, -math.Pi / 2)
    val fullTurn = h - l >= 2 * math.Pi
    val top = fullTurn || reaches(l, h, maxPhase, 2 * math.Pi)
    val bot = fullTurn || reaches(l, h, minPhase, 2 * math.Pi)
    val f    = if cosine then Monotone(math.cos, Real.cos, true, Map(0.0 -> 1.0)) else Monotone(math.sin, Real.sin, true, Map(0.0 -> 0.0))
    b match
      case E(el, eh) =>
        def at(x: _Rational, down: Boolean): Option[_Rational] =
          if x.signum == 0 then Some(_Rational(if cosine then 1 else 0)) else exactBound(f.real, x, env, down)
        for
          al <- at(el, down = true); ah <- at(el, down = false)
          zl <- at(eh, down = true); zh <- at(eh, down = false)
          lo = if bot then _Rational(-1) else (if al.compare(zl) <= 0 then al else zl)
          hi = if top then _Rational(1) else (if ah.compare(zh) >= 0 then ah else zh)
          v  <- make(E(lo, hi))
        yield v
      case D(_, _) =>
        def low(x: Double)  = f.known.getOrElse(x, down2(f.double(x)))
        def high(x: Double) = f.known.getOrElse(x, up2(f.double(x)))
        val lo = if bot then -1.0 else math.min(low(l), low(h))
        val hi = if top then 1.0 else math.max(high(l), high(h))
        val (cl, ch) = unitClamp(lo, hi)
        make(D(cl, ch))

  /** The interval image of a named elementary function, or `None` when the function has no
   *  interval rule here or the interval leaves its domain (a pole of `tan`, `ln` reaching zero,
   *  `asin` beyond `±1`).
   *  @param name the function's grammar name: `exp ln sin cos tan asin acos atan sinh cosh tanh abs`
   *  @param a    the argument, an interval
   *  @param env  supplies the working precision for an exact interval
   *  @return the image, or `None`
   */
  def function(name: String, a: _Value, env: Environment): Option[_Value] =
    if !a.isInstanceOf[_Interval] then None
    else bounds(a).flatMap { b =>
      val (l, h) = toD(b)
      def sinhR(x: Real): Real = (Real.exp(x) - Real.exp(-x)) / Real(2)
      def coshR(x: Real): Real = (Real.exp(x) + Real.exp(-x)) / Real(2)
      def tanhR(x: Real): Real = sinhR(x) / coshR(x)
      name match
        case "exp"  => monotone(b, Monotone(math.exp, Real.exp, true, Map(0.0 -> 1.0)), env, (lo, hi) => (math.max(0.0, lo), hi))
        case "ln" if l > 0 => monotone(b, Monotone(math.log, Real.log, true, Map(1.0 -> 0.0)), env)
        case "sin"  => periodic(b, cosine = false, env)
        case "cos"  => periodic(b, cosine = true, env)
        case "tan" if h - l < math.Pi && !reaches(l, h, math.Pi / 2, math.Pi) =>
          monotone(b, Monotone(math.tan, Real.tan, true, Map(0.0 -> 0.0)), env)
        case "atan" => monotone(b, Monotone(math.atan, Real.atan, true, Map(0.0 -> 0.0)), env)
        case "asin" if l >= -1 && h <= 1 => monotone(b, Monotone(math.asin, Real.asin, true, Map(0.0 -> 0.0)), env)
        case "acos" if l >= -1 && h <= 1 => monotone(b, Monotone(math.acos, Real.acos, false, Map(1.0 -> 0.0)), env)
        case "sinh" => monotone(b, Monotone(math.sinh, sinhR, true, Map(0.0 -> 0.0)), env)
        case "tanh" => monotone(b, Monotone(math.tanh, tanhR, true, Map(0.0 -> 0.0)), env, unitClamp)
        case "cosh" =>
          val f = Monotone(math.cosh, coshR, true, Map(0.0 -> 1.0))
          if l >= 0 then monotone(b, f, env)
          else if h <= 0 then monotone(b, f.copy(increasing = false), env)
          else
            // The minimum, 1, is inside: the larger of the two ends is the maximum.
            b match
              case E(el, eh) =>
                for
                  x <- exactBound(coshR, el, env, down = false)
                  y <- exactBound(coshR, eh, env, down = false)
                  v <- make(E(_Rational(1), if x.compare(y) >= 0 then x else y))
                yield v
              case D(_, _) => make(D(1.0, math.max(up2(math.cosh(l)), up2(math.cosh(h)))))
        case "abs" =>
          b match
            case E(el, eh) =>
              if el.signum >= 0 then make(b)
              else if eh.signum <= 0 then make(E(eh.negate, el.negate))
              else make(E(_Rational(0), if el.negate.compare(eh) >= 0 then el.negate else eh))
            case D(dl, dh) =>
              if dl >= 0 then make(b) else if dh <= 0 then make(D(-dh, -dl)) else make(D(0.0, math.max(-dl, dh)))
        case _ => None
    }

  /** `log_base(a)` as `ln(a) / ln(base)`, either of them an interval: a number's logarithm is
   *  itself enclosed, so the quotient stays rigorous.  `None` outside the domain, and for a
   *  base whose logarithm interval contains zero (a base at or around 1).
   *  @param a    the argument
   *  @param base the base
   *  @param env  supplies the working precision for exact operands
   *  @return the enclosure, or `None`
   */
  def logBase(a: _Value, base: _Value, env: Environment): Option[_Value] =
    if !a.isInstanceOf[_Interval] && !base.isInstanceOf[_Interval] then None
    else
      def ln(v: _Value): Option[_Value] = v match
        case i: _Interval => function("ln", i, env)
        case r: _Rational if r.signum > 0 =>
          if r.compare(_Rational(1)) == 0 then Some(_Rational(0))
          else for l <- exactBound(Real.log, r, env, down = true); h <- exactBound(Real.log, r, env, down = false); v <- make(E(l, h)) yield v
        case _Number(d) if d > 0 && !d.isInfinite =>
          if d == 1.0 then Some(_Number(0)) else make(D(down2(math.log(d)), up2(math.log(d))))
        case _ => None
      for num <- ln(a); den <- ln(base); v <- divide(num, den, env).orElse(quotientOfPoints(num, den, env)) yield v

  /** `a / b` for two values neither of which is an interval (both logarithms came out as
   *  points): an ordinary quotient, which `divide` declines by design. */
  private def quotientOfPoints(a: _Value, b: _Value, env: Environment): Option[_Value] =
    (a, b) match
      case (x: _Rational, y: _Rational) => x.divide(y, env.rationalPolicy)
      case (_Number(x), _Number(y)) if y != 0.0 => make(D(divDown(x, y), divUp(x, y)))
      case _ => None

  // ── comparisons ──

  /** Whether every point of `a` lies below every point of `b` (`strict`: `<`, else `≤`), with the
   *  `=` tolerance: `Some(true)` when it holds for every pair, `Some(false)` when it fails for
   *  every pair, `None` (unknown) otherwise.  At least one operand must be an interval.
   *  @param a      the left operand
   *  @param b      the right operand
   *  @param strict `<` rather than `≤`
   *  @param tol    the tolerance within which two points count as equal
   *  @return the verdict, or `None` when it depends on the points chosen
   */
  def below(a: _Value, b: _Value, strict: Boolean, tol: Double): Option[Boolean] =
    operands(a, b).flatMap { (x, y) =>
      (x, y) match
        case (E(a1, a2), E(b1, b2)) =>
          if strict then
            if a2.compare(b1) < 0 then Some(true) else if a1.compare(b2) >= 0 then Some(false) else None
          else
            if a2.compare(b1) <= 0 then Some(true) else if a1.compare(b2) > 0 then Some(false) else None
        case _ =>
          val ((a1, a2), (b1, b2)) = (toD(x), toD(y))
          if strict then
            if a2 < b1 - tol then Some(true) else if a1 >= b2 - tol then Some(false) else None
          else
            if a2 <= b1 + tol then Some(true) else if a1 > b2 + tol then Some(false) else None
    }

  /** Whether `a = b` for every pair of points (`Some(true)`), for none (`Some(false)`: the
   *  intervals are apart by more than `tol`), or neither (`None`, unknown). */
  def equal(a: _Value, b: _Value, tol: Double): Option[Boolean] =
    operands(a, b).flatMap { (x, y) =>
      (x, y) match
        case (E(a1, a2), E(b1, b2)) =>
          if a2.compare(b1) < 0 || b2.compare(a1) < 0 then Some(false) else None
        case _ =>
          val ((a1, a2), (b1, b2)) = (toD(x), toD(y))
          if a2 < b1 - tol || b2 < a1 - tol then Some(false)
          else if math.max(a2, b2) - math.min(a1, b1) <= tol then Some(true)
          else None
    }

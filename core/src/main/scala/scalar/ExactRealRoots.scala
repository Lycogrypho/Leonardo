package it.grypho.scala.leonardo
package scalar

import core.*
import spire.math.Rational as Q
import scala.util.boundary
import scala.util.boundary.break


// The real roots of an exact polynomial, found exactly (F_0343).
//
// The exact tier's sign charts read their cut points from `Double` roots, and a root of a cluster `g`
// wide is found in floating point only to about `ε/g`: `(x - 1)·(x - 1 - 1/10^6)` read as a double root,
// so `solve(… > 0, x)` answered `x != 1`, and the exact candidates `qRationalRoots` proposes from those
// roots missed the same roots, so nothing could tell.  Here nothing is read in floating point: a Sturm
// sequence counts the distinct real roots in any interval exactly, bisection isolates each in an interval
// of its own, and inside it
//
// - a **rational** root is the fraction of smallest denominator in the interval, once the interval is
//   narrower than `1/(2L²)`, `L` the leading coefficient of the primitive integer form: a rational root's
//   denominator divides `L`, and two fractions of denominator at most `L` are at least `1/L²` apart, so
//   the simplest fraction of so narrow an interval is the root or the root is irrational.  The candidate
//   is tried at every step and accepted only when it is an exact zero;
// - a root of the **quadratic** left after dividing out the rational ones is its surd;
// - any other is **isolated**: an interval known to hold exactly one root, read to `Double` precision.
//
// **Everything runs on integers.**  The Sturm sequence is a primitive pseudo-remainder sequence over
// `BigInt` (each remainder divided by its content, its sign arranged so that it is a positive multiple of
// the negated remainder); the bisection's points are dyadic, `n/2ᵏ` (the bound is a power of two), and a
// sign at one is that of the homogeneous `Σ cᵢ·nⁱ·2^{k(d−i)}`.  Over spire's rationals, which reduce
// every result by a gcd, a product of 24 linear factors took 23 s on the JVM and 138 s on Node (measured
// 2026-11-12).  **The budget is charged as the products are made**, each at the product of its operands'
// sizes in 64-bit words — what a schoolbook multiplication costs.

/** A real root of an exact polynomial (F_0343). */
private[leonardo] enum ExactRoot:
  /** A rational or a surd. */
  case Exact(value: _Value)
  /** A root with no exact form here, read to `Double` precision from its exact isolation. */
  case Isolated(reading: Double)

  /** The root as an expression: the exact value, or the reading. */
  def expression: _Expression = this match
    case Exact(x)    => x
    case Isolated(d) => _Number(d)

  /** The root as a `Double`. */
  def toDouble: Double = this match
    case Exact(_Number(d)) => d
    case Exact(_)          => Double.NaN
    case Isolated(d)       => d

/** The real roots of a polynomial with what a sign chart needs of some of its factors (F_0343).
 *  @param roots         the distinct real roots, ascending
 *  @param intervalSigns for each factor, its sign on each of the `roots.size + 1` open intervals
 *  @param vanishes      for each factor, whether it is zero at each root
 *  @param multiplicity  for each factor, its multiplicity at each root; empty unless asked for */
private[leonardo] final case class RootIsolation(roots: Vector[ExactRoot], intervalSigns: Vector[Vector[Int]],
                                                 vanishes: Vector[Vector[Boolean]],
                                                 multiplicity: Vector[Vector[Int]] = Vector.empty)

/** A polynomial over the integers, coefficient `i` of `xⁱ` at index `i`. */
private type ZPoly = Vector[BigInt]

/** The cost of a product of two integers, in units of a word-by-word product. */
private def productCost(a: BigInt, b: BigInt): Long = (a.bitLength / 64 + 1).toLong * (b.bitLength / 64 + 1)

private def zTrim(p: ZPoly): ZPoly =
  var n = p.length
  while n > 0 && p(n - 1).signum == 0 do n -= 1
  p.take(n)

private def zDeg(p: ZPoly): Int = zTrim(p).length - 1

private def zDeriv(p: ZPoly): ZPoly = zTrim(p.zipWithIndex.drop(1).map((c, i) => c * i))

/** `p` scaled by a positive integer to integer coefficients: the same sign everywhere. */
private def zInteger(p: QPoly): ZPoly =
  val t   = qTrim(p)
  val lcm = t.foldLeft(BigInt(1))((l, c) => { val d = c.denominator.toBigInt; l / l.gcd(d) * d })
  t.map(c => (c * Q(lcm)).numerator.toBigInt)

/** `p` divided by the gcd of its coefficients, its sign kept. */
private def zPrimitive(p: ZPoly, charge: Long => Unit): ZPoly =
  val t = zTrim(p)
  if t.isEmpty then t
  else
    charge(t.iterator.map(c => productCost(c, c)).sum)
    // folded from 0, never `reduce`: a lone negative coefficient would be its own "gcd" and flip the sign
    val g = t.foldLeft(BigInt(0))(_ gcd _)
    if g == BigInt(1) then t else t.map(_ / g)

/** Pseudo-division `lc(b)ˢ·a = q·b + r`, `deg r < deg b`, by `s` elimination steps.
 *  @return `(q, r, s)` */
private def zPseudoDivide(a: ZPoly, b: ZPoly, charge: Long => Unit): (ZPoly, ZPoly, Int) =
  val bt = zTrim(b)
  val db = bt.length - 1
  val lb = bt.last
  var r  = zTrim(a)
  var q  = Vector.fill(math.max(0, r.length - db))(BigInt(0))
  var s  = 0
  while r.length - 1 >= db do
    val shift = r.length - 1 - db
    val lr    = r.last
    charge(r.iterator.map(productCost(lb, _)).sum + bt.iterator.map(productCost(lr, _)).sum)
    q = q.map(_ * lb).updated(shift, lr)
    r = zTrim(Vector.tabulate(r.length)(i => lb * r(i) - (if i >= shift then lr * bt(i - shift) else BigInt(0))))
    s += 1
  (q, r, s)

/** The Sturm sequence of `p`: `p`, `p′`, then each a positive multiple of the negated remainder,
 *  primitive.  For a `p` that is not square-free its last element is `gcd(p, p′)`, and the count of sign
 *  changes still counts the distinct roots between two points that are not roots. */
private def zSturm(p: ZPoly, charge: Long => Unit): Vector[ZPoly] =
  @annotation.tailrec
  def go(a: ZPoly, b: ZPoly, acc: Vector[ZPoly]): Vector[ZPoly] =
    if zDeg(b) < 0 then acc
    else
      val (_, r, s) = zPseudoDivide(a, b, charge)
      // rem = r / lc(b)^s, so a positive multiple of −rem is −r·sign(lc(b))^s
      val flip = if zTrim(b).last.signum < 0 && s % 2 == 1 then BigInt(1) else BigInt(-1)
      go(b, zPrimitive(r.map(_ * flip), charge), acc :+ b)
  val p0 = zPrimitive(p, charge)
  go(p0, zPrimitive(zDeriv(p0), charge), Vector(p0))

/** The number of sign changes in a sequence of signs, zeros skipped. */
private def changes(signs: Iterator[Int]): Int =
  signs.filter(_ != 0).foldLeft((0, 0)) { case ((n, last), s) => (if last != 0 && s != last then n + 1 else n, s) }._1

/** A dyadic rational `num/2ᵏ`, the bisection's points. */
private final case class Dyadic(num: BigInt, k: Int):
  def den: BigInt = BigInt(1) << k
  def toQ: Q = Q(num, den)
  /** `this − that` as a numerator over `2^max(k)`, with that exponent. */
  def minus(that: Dyadic): (BigInt, Int) =
    val k2 = math.max(k, that.k)
    ((num << (k2 - k)) - (that.num << (k2 - that.k)), k2)

private def midpoint(a: Dyadic, b: Dyadic): Dyadic =
  val k = math.max(a.k, b.k)
  Dyadic((a.num << (k - a.k)) + (b.num << (k - b.k)), k + 1)

/** The sign of `p` at `h/k`, `k > 0`: that of the homogeneous `Σ cᵢ·hⁱ·k^(d−i)`, no fraction formed. */
private def zSignAt(p: ZPoly, h: BigInt, k: BigInt, charge: Long => Unit): Int =
  val d = p.length - 1
  if d < 0 then 0
  else
    var acc = p(d)
    var kp  = k
    var i   = d - 1
    while i >= 0 do
      charge(productCost(acc, h) + productCost(p(i), kp) + productCost(kp, k))
      acc = acc * h + p(i) * kp
      kp  = kp * k
      i -= 1
    acc.signum

/** `⌊a/b⌋` for `b > 0`. */
private def floorDiv(a: BigInt, b: BigInt): BigInt = if (a % b).signum < 0 then a / b - 1 else a / b

/** The fraction of smallest denominator in `[an/ad, bn/bd]`, the denominators positive: the continued
 *  fractions of the two ends followed while they agree. */
private def simplestBetween(an0: BigInt, ad0: BigInt, bn0: BigInt, bd0: BigInt): (BigInt, BigInt) =
  @annotation.tailrec
  def descend(an: BigInt, ad: BigInt, bn: BigInt, bd: BigInt, floors: List[BigInt]): (List[BigInt], BigInt) =
    val fl = floorDiv(an, ad)
    if fl * ad == an then (floors, fl)
    else if fl < floorDiv(bn, bd) then (floors, fl + 1)
    else descend(bd, bn - fl * bd, ad, an - fl * ad, fl :: floors)
  val (floors, last) = descend(an0, ad0, bn0, bd0, Nil)
  floors.foldLeft((last, BigInt(1))) { case ((h, k), fl) => (fl * h + k, h) }

/** Every distinct real root of `p`, ascending (F_0343).
 *  @param p       the polynomial, exact coefficients
 *  @param maxWork the budget ([[core.Limit.MaxExactRootWork]]), in word products
 *  @return the roots, or `None` for the zero polynomial or past the budget */
private[leonardo] def exactRealRoots(p: QPoly, maxWork: Long): Option[Vector[ExactRoot]] =
  isolateRoots(p, Vector.empty, maxWork).map(_.roots)

/** [[exactRealRoots]] with the work it spent, for a caller sharing one budget among several polynomials. */
private[leonardo] def exactRealRootsSpending(p: QPoly, maxWork: Long): (Option[Vector[ExactRoot]], Long) =
  val (iso, work) = isolateRootsMeasured(p, Vector.empty, maxWork)
  (iso.map(_.roots), work)

/** The real roots of `p`, with the sign of each of `factors` between them and whether it vanishes at
 *  each (F_0343).
 *  @param p       the polynomial, exact coefficients
 *  @param factors polynomials whose real roots are all roots of `p`
 *  @param maxWork the budget ([[core.Limit.MaxExactRootWork]]), in word products
 *  @param multiplicities whether to count each factor's multiplicity at each root
 *  @return the isolation, or `None` for the zero polynomial or past the budget */
private[leonardo] def isolateRoots(p: QPoly, factors: Vector[QPoly], maxWork: Long,
                                   multiplicities: Boolean = false): Option[RootIsolation] =
  isolateRootsMeasured(p, factors, maxWork, multiplicities)._1

/** [[isolateRoots]] with the work it spent, for measuring the budget. */
private[scalar] def isolateRootsMeasured(p: QPoly, factors: Vector[QPoly], maxWork: Long,
                                         multiplicities: Boolean = false): (Option[RootIsolation], Long) =
  val t = qTrim(p)
  if t.isEmpty then (None, 0L)
  else
    var work = 0L
    val result = boundary[Option[RootIsolation]] {
      def charge(units: Long): Unit =
        work += units
        if work > maxWork then break(None)
      def signAt(f: ZPoly, x: Dyadic): Int = zSignAt(f, x.num, x.den, charge)
      val fz = factors.map(zInteger)
      if qDeg(t) == 0 then
        Some(RootIsolation(Vector.empty, fz.map(f => Vector(signAt(f, Dyadic(0, 0)))), fz.map(_ => Vector.empty)))
      else
        // the primitive integer form, then its square-free part: p / gcd(p, p′), exactly
        val whole = zPrimitive(zInteger(t), charge)
        val first = zSturm(whole, charge)
        val chain =
          if zDeg(first.last) == 0 then first
          else zSturm(zPseudoDivide(whole, first.last, charge)._1, charge)
        val p0   = chain.head
        val lead = p0.last.abs
        def variations(c: Vector[ZPoly], x: Dyadic): Int = changes(c.iterator.map(signAt(_, x)))
        def count(lo: Dyadic, hi: Dyadic): Int = variations(chain, lo) - variations(chain, hi)
        def isolate(lo: Dyadic, hi: Dyadic, k: Int): List[(Dyadic, Dyadic)] =
          if k == 0 then Nil
          else if k == 1 then List((lo, hi))
          else
            val m    = midpoint(lo, hi)
            val left = count(lo, m)
            isolate(lo, m, left) ++ isolate(m, hi, k - left)
        /** The rational root in `(lo, hi]`, or the interval narrowed below `1/(2·lead²)`, proving none. */
        @annotation.tailrec
        def rationalIn(lo: Dyadic, hi: Dyadic): Either[(BigInt, BigInt), (Dyadic, Dyadic)] =
          val (h, k) = simplestBetween(lo.num, lo.den, hi.num, hi.den)
          if h * lo.den > lo.num * k && zSignAt(p0, h, k, charge) == 0 then Left((h, k))
          else
            val (w, e) = hi.minus(lo)
            if w * lead * lead * 2 < (BigInt(1) << e) then Right((lo, hi))
            else
              val m = midpoint(lo, hi)
              if count(lo, m) == 1 then rationalIn(lo, m) else rationalIn(m, hi)
        /** A reading to `Double` precision: narrowed until the ends are a few ulps apart. */
        @annotation.tailrec
        def reading(lo: Dyadic, hi: Dyadic): Double =
          val (l, h) = (spireToDouble(lo.toQ), spireToDouble(hi.toQ))
          if l == h || (math.signum(l) == math.signum(h) && math.abs(h - l) <= 4 * math.ulp(math.min(math.abs(l), math.abs(h)))) then
            spireToDouble(midpoint(lo, hi).toQ)
          else
            val m = midpoint(lo, hi)
            if count(lo, m) == 1 then reading(lo, m) else reading(m, hi)

        // Cauchy: every root lies strictly inside 1 + max|cᵢ/cₙ|, taken up to a power of two
        val cauchy = Q.one + p0.init.map(c => Q(c.abs, lead)).foldLeft(Q.zero)((a, b) => if b > a then b else a)
        val e      = Iterator.from(0).find(e => Q(BigInt(1) << e) >= cauchy).getOrElse(0)
        val (lo0, hi0) = (Dyadic(-(BigInt(1) << e), 0), Dyadic(BigInt(1) << e, 0))
        val ivs    = isolate(lo0, hi0, count(lo0, hi0)).toVector
        val found  = ivs.map((lo, hi) => rationalIn(lo, hi))

        // the quadratic left after the rational roots, by exact division by each k·x − h
        val linear = found.collect { case Left((h, k)) => Vector(-h, k) }
        val surds: List[_Value] =
          if p0.length - 1 - linear.size != 2 then Nil
          else zTrim(linear.foldLeft(p0)((g, f) => zPrimitive(zPseudoDivide(g, f, charge)._1, charge))) match
            case Vector(c0, c1, c2) =>
              (for
                d   <- _Rational.of(c1 * c1 - BigInt(4) * c2 * c0, BigInt(1))
                if d.signum > 0
                sq  <- _Surd.sqrtOf(d)
                a2  <- _Rational.of(c2 * 2, BigInt(1))
                neg <- _Surd.negate(sq)
                pn  <- _Surd.add(_Rational(-c1), sq).flatMap(_Surd.divide(_, a2))
                nn  <- _Surd.add(_Rational(-c1), neg).flatMap(_Surd.divide(_, a2))
              yield List(pn, nn)).getOrElse(Nil)
            case _ => Nil
        def inside(x: _Value, lo: Dyadic, hi: Dyadic): Boolean =
          (_Rational.of(lo.num, lo.den), _Rational.of(hi.num, hi.den)) match
            case (Some(l), Some(h)) => _Surd.compareExact(x, l).contains(1) && _Surd.compareExact(x, h).exists(_ <= 0)
            case _                  => false
        val roots = found.map {
          case Left((h, k))  => ExactRoot.Exact(_Rational.of(h, k).getOrElse(_Rational(0)))
          case Right((l, h)) => surds.find(inside(_, l, h)).fold(ExactRoot.Isolated(reading(l, h)))(ExactRoot.Exact(_))
        }

        // one point inside each open interval: below every root, between neighbours, above every root.
        // An interval's right end is at or past its root and before the next one; its left end is no root.
        val between = ivs.zip(ivs.drop(1)).map { case ((_, hj), (ln, hn)) =>
          if signAt(p0, hj) != 0 then hj
          else if ln.minus(hj)._1.signum > 0 then ln
          else Iterator.iterate(midpoint(hj, hn))(m => midpoint(hj, m)).find(m => count(hj, m) == 0).getOrElse(hn)
        }
        val samples = (lo0 +: between) :+ hi0
        val signs   = fz.map(f => samples.map(signAt(f, _)))
        // the narrowed ends of an irrational root are no roots of `p`, so none of a factor's
        def zeroAt(f: ZPoly, fc: => Vector[ZPoly])(r: Either[(BigInt, BigInt), (Dyadic, Dyadic)]): Boolean = r match
          case Left((h, k))  => zSignAt(f, h, k, charge) == 0
          case Right((l, h)) => variations(fc, l) - variations(fc, h) > 0
        val vanish  = fz.map { f =>
          if f.isEmpty then found.map(_ => true)
          else if f.length == 1 then found.map(_ => false)
          else
            lazy val fc = zSturm(f, charge)
            found.map(zeroAt(f, fc))
        }
        // a root's multiplicity in f: how many of f, gcd(f, f′), gcd of that with its derivative, … vanish there
        val mult = if !multiplicities then Vector.empty else fz.map { f =>
          val tower = Vector.unfold(f) { g => Option.when(zDeg(g) >= 1) { val c = zSturm(g, charge); ((c.head, c), c.last) } }
          found.map(r => tower.count((g, c) => zeroAt(g, c)(r)))
        }
        Some(RootIsolation(roots, signs, vanish, mult))
    }
    (result, work)

/** The singularities of an exact rational function, exactly (F_0343): each real root of the denominator
 *  with its order, the denominator's multiplicity there less the numerator's (`0` for a removable one).
 *  A multiplicity is counted exactly, over the integers (how many of `f`, `gcd(f, f′)`, … vanish at the
 *  root), so two poles `10⁻¹⁰` apart are two simple poles, not one double pole.
 *  @param e   the expression
 *  @param v   the variable
 *  @param env bindings and the budget
 *  @return the singularities, ascending, or `None` when `e` is not an exact rational function */
private[leonardo] def exactSingularities(e: _Expression, v: _Variable, env: Environment): Option[Vector[(ExactRoot, Int)]] =
  for
    (n, d) <- qFraction(e, v, env)
    if qDeg(n) >= 0 && qDeg(d) >= 1
    iso    <- isolateRoots(qMul(n, d), Vector(d, n), env.limits(Limit.MaxExactRootWork), multiplicities = true)
  yield
    iso.roots.indices.toVector.flatMap { j =>
      val md = iso.multiplicity(0)(j)
      Option.when(md > 0)((iso.roots(j), math.max(md - iso.multiplicity(1)(j), 0)))
    }

/** `e` as a fraction `N/D` of exact polynomials in `v`, or `None` (F_0343).  A subexpression free of `v`
 *  folds in `env` to its exact value, or the whole reading declines; only `+ × ÷` and integer powers
 *  are read, up to [[MaxRationalDegree]].
 *  @param e   the expression
 *  @param v   the variable
 *  @param env bindings
 *  @return the numerator and denominator, as written (no common factor is cancelled) */
private[leonardo] def qFraction(e: _Expression, v: _Variable, env: Environment): Option[(QPoly, QPoly)] =
  val one: QPoly = Vector(Q.one)
  def capped(n: QPoly, d: QPoly): Option[(QPoly, QPoly)] =
    Option.when(qDeg(n) <= MaxRationalDegree && qDeg(d) <= MaxRationalDegree && qDeg(d) >= 0)((qTrim(n), qTrim(d)))
  def go(x: _Expression): Option[(QPoly, QPoly)] = x match
    case w: _Variable if w.variable == v.variable => Some((Vector(Q.zero, Q.one), one))
    case _ if !dependsOn(x, v) => x.eval(env) match
      case Right(r: _Rational) => Some((qTrim(Vector(Q(r.num, r.den))), one))
      case _                   => None
    case Sum(a, b) =>
      for (na, da) <- go(a); (nb, db) <- go(b); r <- capped(qAdd(qMul(na, db), qMul(nb, da)), qMul(da, db)) yield r
    case Product(a, b) =>
      for (na, da) <- go(a); (nb, db) <- go(b); r <- capped(qMul(na, nb), qMul(da, db)) yield r
    case Ratio(a, b) =>
      for (na, da) <- go(a); (nb, db) <- go(b) if qDeg(nb) >= 0; r <- capped(qMul(na, db), qMul(da, nb)) yield r
    case Power(b, k) if !dependsOn(k, v) => k.eval(env) match
      case Right(r: _Rational) if r.den == 1 && r.num.abs <= MaxRationalDegree =>
        go(b).flatMap { (nb, db) =>
          val n = r.num.toInt
          val (pn, pd) = (0 until math.abs(n)).foldLeft((one, one)) { case ((an, ad), _) => (qMul(an, nb), qMul(ad, db)) }
          if n >= 0 then capped(pn, pd) else if qDeg(pn) < 0 then None else capped(pd, pn)
        }
      case _ => None
    case _ => None
  go(e)

/** The product of two polynomials over the rationals. */
private[leonardo] def qTimesPoly(a: QPoly, b: QPoly): QPoly = qMul(a, b)

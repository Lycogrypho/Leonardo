package it.grypho.scala.leonardo
package scalar

import core.*
import spire.math.Rational as Q


/** The rational-function tier over the rationals (F_0171 phase C).
 *
 *  `IntegrateRational.scala` decomposes a rational function in `Double` arithmetic — numeric
 *  roots, `Double` coefficient vectors — so an exact integrand came back inexact.  This file is
 *  its exact counterpart, tried first in the exact tier: long division, the gcd and Yun's
 *  square-free factorisation over `spire.math.Rational`; the rational roots of each square-free
 *  part found exactly (a numeric root only *proposes* a candidate, verified by exact
 *  evaluation); an irreducible quadratic integrated to `ln` + `atan` (complex roots) or to two
 *  logarithms in one quotient (irrational real roots), its square root kept exact as `^(1/2)`;
 *  and the partial-fraction numerators from a linear system solved over the rationals.
 *
 *  **Where it declines, the `Double` tier answers**: a square-free factor of degree three or
 *  more with no rational root is not factored over the rationals here, and a repeated quadratic
 *  with irrational real roots has no closed form in this tier.  The fallback is stated rather
 *  than hidden: an exact integrand with such a denominator integrates in `Double`s.
 */

private type QPoly = Vector[Q]

/** The coefficient vector with trailing zeros removed (index = power). */
private def qTrim(p: QPoly): QPoly =
  var n = p.length
  while n > 0 && p(n - 1).isZero do n -= 1
  p.take(n)

/** The degree; `-1` for the zero polynomial. */
private def qDeg(p: QPoly): Int = qTrim(p).length - 1

private def qAdd(a: QPoly, b: QPoly): QPoly =
  qTrim(Vector.tabulate(math.max(a.length, b.length))(i =>
    a.lift(i).getOrElse(Q.zero) + b.lift(i).getOrElse(Q.zero)))

private def qScale(a: QPoly, k: Q): QPoly = qTrim(a.map(_ * k))

private def qSub(a: QPoly, b: QPoly): QPoly = qAdd(a, qScale(b, -Q.one))

private def qMul(a: QPoly, b: QPoly): QPoly =
  if a.isEmpty || b.isEmpty then Vector.empty
  else qTrim(Vector.tabulate(a.length + b.length - 1)(k =>
    (math.max(0, k - b.length + 1) to math.min(k, a.length - 1)).foldLeft(Q.zero)((s, i) => s + a(i) * b(k - i))))

/** Long division `a = q·b + r`, `deg r < deg b`; `b` must not be zero. */
private def qDivMod(a: QPoly, b: QPoly): (QPoly, QPoly) =
  val bt = qTrim(b)
  val db = bt.length - 1
  var r  = qTrim(a)
  var q  = Vector.fill(math.max(0, r.length - db))(Q.zero)
  while r.length - 1 >= db do
    val shift = r.length - 1 - db
    val c     = r.last / bt.last
    q = q.updated(shift, c)
    r = qSub(r, Vector.fill(shift)(Q.zero) ++ bt.map(_ * c))
  (qTrim(q), r)

private def qDeriv(a: QPoly): QPoly = qTrim(a.zipWithIndex.drop(1).map((c, i) => c * Q(i)))

private def qMonic(a: QPoly): QPoly = { val t = qTrim(a); if t.isEmpty then t else qScale(t, Q.one / t.last) }

/** The monic greatest common divisor, by Euclid's algorithm over the rationals. */
private def qGcd(a: QPoly, b: QPoly): QPoly =
  var x = qTrim(a)
  var y = qTrim(b)
  while y.nonEmpty do
    val r = qDivMod(x, y)._2
    x = y; y = r
  qMonic(x)

private def qEval(p: QPoly, x: Q): Q = p.foldRight(Q.zero)((c, acc) => acc * x + c)

/** Yun's square-free factorisation of a monic `p`: `p = Π Pₖ^k`, each `Pₖ` square-free. */
private def qSquareFree(p: QPoly): List[(QPoly, Int)] =
  val one: QPoly = Vector(Q.one)
  val a0 = qGcd(p, qDeriv(p))
  var b  = qDivMod(p, a0)._1
  var c  = qDivMod(qDeriv(p), a0)._1
  var d  = qSub(c, qDeriv(b))
  var i  = 1
  val out = scala.collection.mutable.ListBuffer[(QPoly, Int)]()
  while qDeg(b) > 0 do
    val a = qGcd(b, d)
    if qDeg(a) > 0 then out += ((a, i))
    b = qDivMod(b, a)._1
    c = qDivMod(d, a)._1
    d = qSub(c, qDeriv(b))
    i += 1
  out.toList

/** `⌊√n⌋` for `n ≥ 0`, by Newton's iteration on `BigInt` (portable: no JDK square root). */
private def isqrtFloor(n: BigInt): BigInt =
  if n < 2 then n
  else
    var x = BigInt(1) << ((n.bitLength + 1) / 2)
    var y = (x + n / x) >> 1
    while y < x do { x = y; y = (x + n / x) >> 1 }
    x

/** The exact square root of a non-negative rational, when it is one. */
private def qSqrt(x: Q): Option[Q] =
  def isqrt(n: BigInt): Option[BigInt] =
    if n.signum < 0 then None else Some(isqrtFloor(n)).filter(r => r * r == n)
  for n <- isqrt(x.numerator.toBigInt); d <- isqrt(x.denominator.toBigInt) yield Q(n, d)

/** The positive divisors of `n`, or `None` past `10¹²` (trial division to `10⁶`). */
private def divisors(n: BigInt): Option[List[BigInt]] =
  val m = n.abs
  if m > BigInt("1000000000000") || m.signum == 0 then None
  else
    val small = (1L to isqrtFloor(m).toLong).filter(k => m % k == 0).map(BigInt(_)).toList
    Some((small ++ small.map(m / _)).distinct)

/** The rational roots of a square-free `p`, each verified by exact evaluation.  A numeric root
 *  proposes the candidate `round(x·q)/q` for each divisor `q` of the leading coefficient of the
 *  integer form; nothing is accepted that does not make `p` exactly zero. */
private def qRationalRoots(p: QPoly): List[Q] =
  val lcm   = p.foldLeft(BigInt(1))((l, c) => { val d = c.denominator.toBigInt; l / l.gcd(d) * d })
  val ints  = p.map(c => (c * Q(lcm)).numerator.toBigInt)
  rootsOfSquareFree(p.map(_.toDouble)) match
    case None => Nil
    case Some(parts) =>
      val reals = parts.filter(realAmong(parts, 1e-6)).map(_._1)
      divisors(ints.last).fold(Nil) { qs =>
        reals.flatMap { x =>
          qs.iterator.map(q => Q(BigInt(math.round(x * q.toDouble)), q)).find(c => qEval(p, c).isZero)
        }.distinct.toList
      }

/** The integer form of `p` (scaled by the lcm of its denominators) — its leading coefficient
 *  bounds, by Gauss's lemma, the denominators of a monic factor's coefficients. */
private def qLeadInteger(p: QPoly): BigInt =
  val lcm = p.foldLeft(BigInt(1))((l, c) => { val d = c.denominator.toBigInt; l / l.gcd(d) * d })
  (p.last * Q(lcm)).numerator.toBigInt

/** A monic quadratic factor `v² + p·v + q` of `poly` over the rationals, or `None`.
 *
 *  Each pair of numeric roots (a conjugate pair, or two real roots) proposes `p = −(r₁ + r₂)`,
 *  `q = r₁·r₂`; the exact coefficients have denominators dividing the integer form's leading
 *  coefficient (Gauss's lemma), so each candidate is rounded on that grid and accepted only when
 *  exact division leaves no remainder. */
private def qQuadraticFactor(poly: QPoly): Option[QPoly] =
  for
    parts <- rootsOfSquareFree(poly.map(_.toDouble))
    dens  <- divisors(qLeadInteger(poly))
    found <- {
      val pairs = for i <- parts.indices; j <- parts.indices if i < j yield (parts(i), parts(j))
      pairs.iterator.flatMap { case ((r1, i1), (r2, i2)) =>
        val (pr, qr) = (-(r1 + r2), r1 * r2 - i1 * i2)
        val imag     = math.abs(i1 + i2)
        if imag > 1e-6 * (1.0 + math.abs(r1) + math.abs(r2)) then Iterator.empty   // not a real-coefficient pair
        else dens.iterator.map { a =>
          Vector(Q(BigInt(math.round(qr * a.toDouble)), a), Q(BigInt(math.round(pr * a.toDouble)), a), Q.one)
        }.find(f => qDivMod(poly, f)._2.isEmpty)
      }.nextOption()
    }
  yield found

/** A factor of the denominator: `v − r` or the irreducible-over-the-rationals `v² + p·v + q`. */
private enum QFactor:
  case Linear(r: Q)
  case Quadratic(p: Q, q: Q)

  def poly: QPoly = this match
    case Linear(r)       => Vector(-r, Q.one)
    case Quadratic(p, q) => Vector(q, p, Q.one)

/** Factors a monic denominator over the rationals into linear and irreducible quadratic
 *  factors with their multiplicities, or `None` when an irreducible factor of degree ≥ 3
 *  remains (the `Double` tier's case). */
private def qFactor(monic: QPoly): Option[List[(QFactor, Int)]] =
  /** A square-free polynomial of degree ≤ 2 as its factors. */
  def small(rest: QPoly): Option[List[QFactor]] = qDeg(rest) match
    case 0 => Some(Nil)
    case 1 => Some(List(QFactor.Linear(-rest(0) / rest(1))))
    case 2 =>
      val m = qMonic(rest)
      val (p, q) = (m(1), m(0))
      val half   = p / Q(2)
      val disc   = half * half - q
      qSqrt(disc) match
        case Some(s) if !disc.isZero => Some(List(QFactor.Linear(-half + s), QFactor.Linear(-half - s)))
        case _                       => Some(List(QFactor.Quadratic(p, q)))
    case _ => None
  /** A square-free polynomial of degree ≥ 3 with no rational root, split into quadratics. */
  def quadratics(rest: QPoly): Option[List[QFactor]] =
    if qDeg(rest) <= 2 then small(rest)
    else qQuadraticFactor(rest).flatMap { f =>
      for a <- small(f); b <- quadratics(qDivMod(rest, f)._1) yield a ++ b
    }
  val pieces = qSquareFree(monic).map { (part, k) =>
    val roots = if qDeg(part) >= 3 then qRationalRoots(part) else Nil
    val rest  = qMonic(roots.foldLeft(part)((acc, r) => qDivMod(acc, Vector(-r, Q.one))._1))
    quadratics(rest).map(fs => (roots.map(QFactor.Linear(_)) ++ fs).map(_ -> k))
  }
  Option.when(pieces.forall(_.isDefined))(pieces.flatten.flatten)

/** Solves the square system `m·x = b` over the rationals by Gauss–Jordan elimination. */
private def qSolve(m: Vector[Vector[Q]], b: Vector[Q]): Option[Vector[Q]] =
  val n   = b.length
  val aug = Array.tabulate(n)(i => (m(i) :+ b(i)).toArray)
  var ok  = true
  var col = 0
  while ok && col < n do
    (col until n).find(r => !aug(r)(col).isZero) match
      case None => ok = false
      case Some(pr) =>
        val tmp = aug(col); aug(col) = aug(pr); aug(pr) = tmp
        val piv = aug(col)(col)
        for c <- col to n do aug(col)(c) = aug(col)(c) / piv
        for r <- 0 until n if r != col && !aug(r)(col).isZero do
          val f = aug(r)(col)
          for c <- col to n do aug(r)(c) = aug(r)(c) - f * aug(col)(c)
    col += 1
  Option.when(ok)(Vector.tabulate(n)(i => aug(i)(n)))

/** An exact rational as an expression. */
private def qe(q: Q): _Expression = _Rational.of(q.numerator.toBigInt, q.denominator.toBigInt).getOrElse(_Number(q.toDouble))

/** `k·e`, folding `0` and `1`. */
private def qTimes(k: Q, e: _Expression): Option[_Expression] =
  if k.isZero then None else if k == Q.one then Some(e) else Some(Product(qe(k), e))

/** The square root of a positive rational: exact when it is one, else `x^(1/2)` with `x` exact. */
private def qRoot(x: Q)(using t: Tier): _Expression = qSqrt(x).fold(Power(qe(x), t.half))(qe)

/** The exact coefficients of `e` as a polynomial in `v`, or `None` when one is not exact. */
private def qCoeffs(e: _Expression, v: _Variable): Option[QPoly] =
  collect(e, v).flatMap { cs =>
    val qs = cs.map(_.eval(EmptyEnv) match
      case Right(r: _Rational) => Some(Q(r.num, r.den))
      case _                   => None)
    Option.when(qs.forall(_.isDefined))(qTrim(qs.flatten))
  }

/** `∫ num/den dv` over the rationals, or `None` (the `Double` tier answers then). */
private def integrateQRational(num: QPoly, den: QPoly, v: _Variable)(using t: Tier): Option[_Expression] =
  val d = qTrim(den)
  if qDeg(d) < 1 || qDeg(d) > MaxRationalDegree || qDeg(num) > MaxRationalDegree then None
  else
    val lead       = d.last
    val monic      = qScale(d, Q.one / lead)
    val (quot, rem) = qDivMod(qScale(num, Q.one / lead), monic)
    val polyPart   = quot.zipWithIndex.flatMap((c, i) => qTimes(c / Q(i + 1), Power(v, t.n(i + 1))))
    val proper     = if rem.isEmpty then Some(Nil) else properQRational(rem, monic, v)
    proper.map { terms =>
      val all = polyPart ++ terms
      if all.isEmpty then t.n(0) else simplifyT(all.reduce(Sum(_, _)))
    }

/** The partial-fraction terms of a proper `rem/monic`, integrated. */
private def properQRational(rem: QPoly, monic: QPoly, v: _Variable)(using t: Tier): Option[Vector[_Expression]] =
  qFactor(monic).flatMap { factors =>
    val deg = qDeg(monic)
    def cofactor(reduce: Int, by: Int): QPoly =
      factors.zipWithIndex.foldLeft(Vector(Q.one): QPoly) { case (acc, ((f, m), idx)) =>
        (0 until (if idx == reduce then m - by else m)).foldLeft(acc)((p, _) => qMul(p, f.poly))
      }
    // One column per unknown: (factor index, power j, whether the numerator is v).
    val columns = factors.zipWithIndex.flatMap { case ((f, m), i) =>
      (1 to m).flatMap { j =>
        val cof = cofactor(i, j)
        f match
          case QFactor.Linear(_)       => List((i, j, false, cof))
          case QFactor.Quadratic(_, _) => List((i, j, false, cof), (i, j, true, qMul(Vector(Q.zero, Q.one), cof)))
      }
    }
    if columns.length != deg then None
    else
      val m = Vector.tabulate(deg)(r => columns.map(c => c._4.lift(r).getOrElse(Q.zero)).toVector)
      qSolve(m, Vector.tabulate(deg)(i => rem.lift(i).getOrElse(Q.zero))).flatMap { x =>
        def coef(i: Int, j: Int, ofV: Boolean): Q =
          columns.indexWhere(c => c._1 == i && c._2 == j && c._3 == ofV) match
            case -1 => Q.zero
            case k  => x(k)
        val terms = factors.zipWithIndex.flatMap { case ((f, mult), i) =>
          (1 to mult).map { j =>
            f match
              case QFactor.Linear(r) =>
                val base = simplifyT(Sum(v, qe(-r)))
                val a    = coef(i, j, false)
                Some(if j == 1 then qTimes(a, Ln(base))
                     else qTimes(-a / Q(j - 1), Ratio(t.n(1), Power(base, t.n(j - 1)))))
              case QFactor.Quadratic(p, q) =>
                quadTerm(coef(i, j, true), coef(i, j, false), p, q, j, v)
          }
        }
        Option.when(terms.forall(_.isDefined))(terms.flatMap(_.flatten).toVector)
      }
  }

/** `∫ (B·v + C)/(v² + p·v + q)^j dv` for a quadratic irreducible over the rationals: `Some(None)`
 *  for a zero term, `None` when this tier has no closed form (irrational real roots, `j > 1`). */
private def quadTerm(b: Q, c: Q, p: Q, q: Q, j: Int, v: _Variable)(using t: Tier): Option[Option[_Expression]] =
  if b.isZero && c.isZero then Some(None)
  else
    val quad = simplifyT(Sum(Sum(Power(v, t.n(2)), Product(qe(p), v)), qe(q)))
    val half = p / Q(2)
    val s    = simplifyT(Sum(v, qe(half)))                    // v + p/2
    val d    = q - half * half                                // (v + p/2)² + d
    val rest = c - b * half                                   // B·v + C = (B/2)(2v + p) + rest
    if d.signum > 0 then
      val w = qRoot(d)
      // Iⱼ = ∫ dv/quadʲ:  I₁ = atan(s/w)/w,  Iⱼ = s/(2(j−1)d·quad^(j−1)) + (2j−3)/(2(j−1)d)·Iⱼ₋₁
      def inv(k: Int): _Expression =
        if k == 1 then Ratio(Atan(Ratio(s, w)), w)
        else Sum(Product(qe(Q.one / (Q(2 * (k - 1)) * d)), Ratio(s, Power(quad, t.n(k - 1)))),
                 Product(qe(Q(2 * k - 3) / (Q(2 * (k - 1)) * d)), inv(k - 1)))
      val derivPart =
        if j == 1 then qTimes(b / Q(2), Ln(quad))
        else qTimes(-(b / Q(2)) / Q(j - 1), Ratio(t.n(1), Power(quad, t.n(j - 1))))
      val constPart = qTimes(rest, inv(j))
      Some((derivPart ++ constPart).reduceOption(Sum(_, _)))
    else if j == 1 then
      // Irrational real roots v = −p/2 ± r, r = √(−d):
      // ∫ dv/quad = ln((s − r)/(s + r))/(2r).
      val r = qRoot(-d)
      val logPart  = qTimes(b / Q(2), Ln(quad))
      val ratioLog = Ratio(Ln(Ratio(Sum(s, Product(t.n(-1), r)), Sum(s, r))), Product(t.n(2), r))
      Some((logPart ++ qTimes(rest, ratioLog)).reduceOption(Sum(_, _)))
    else None

/** The exact rational tier's entry: `∫ numE/denE dv`, or `None` (the `Double` tier answers). */
private def integrateRationalExact(numE: _Expression, denE: _Expression, v: _Variable)(using t: Tier): Option[_Expression] =
  for
    num <- qCoeffs(numE, v)
    den <- qCoeffs(denE, v)
    res <- integrateQRational(num, den, v)
  yield res

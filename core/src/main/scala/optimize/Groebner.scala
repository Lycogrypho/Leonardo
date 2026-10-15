package it.grypho.scala.leonardo
package optimize

import core.*
import scalar.*
import spire.math.Rational as Q


/** The largest basis [[groebnerBasis]] builds before declining.  **Measured** (F_0068, JVM /
 *  Node): the test systems end with 2–4 elements and solve in tens of milliseconds — the
 *  nine-point stationary problem in 0.19 s on Node, three dense quadratics in three unknowns in
 *  0.33 / 0.48 s, Katsura-3 in under 0.15 s.  The caps bound the cost of a decline, not only of
 *  an answer.
 *
 *  **24, re-measured by F_0122** (2026-10-15). It was 40, and cyclic-5 — whose lexicographic
 *  basis is notoriously large — then grew 32 elements and coefficients near the 4096-bit cap
 *  before declining, in 0.6–0.9 s on the JVM and 3–5 s on Node; F_0068 had recorded 0.13 /
 *  0.16 s, which no build reached (re-run at F_0068's own commit: 0.9–3.4 s). The last
 *  elements cost the most, since their coefficients are the widest. Every basis a successful
 *  solve builds, in the suite and in decimal-coefficient variants of its systems, holds at most
 *  6 elements before minimisation; at 24 cyclic-5 declines in 0.2–0.7 s on the JVM and
 *  0.75–0.9 s on Node, and every one of them still answers.  **The coefficient cap could not
 *  be the lever**: a 2×2 system with general decimal coefficients needs 1259 bits, since a
 *  `Double` is read as the dyadic rational it holds. */
val MaxGroebnerBasis: Int = 24

/** The highest total degree an element of the basis may reach before [[groebnerBasis]]
 *  declines — a lex basis's last element carries the whole system's Bézout number as its degree,
 *  which is what the elimination solver then root-finds. */
val MaxGroebnerDegree: Int = 24

/** The largest coefficient, in bits of numerator plus denominator, before [[groebnerBasis]]
 *  declines: exact arithmetic does not round, it grows, and a lex basis is where it grows.
 *  Checked at every reduction step, which is what bounds a decline's time: without it,
 *  cyclic-5's reduction ran for minutes (F_0122). A 2×2 system with decimal coefficients
 *  needs 1259 bits, so neither 512 nor 1024 is enough. */
val MaxGroebnerCoefficientBits: Int = 4096

/** The S-polynomial reductions [[groebnerBasis]] may perform before declining. */
val MaxGroebnerReductions: Int = 2000


/** The reduced Gröbner basis of the polynomials `eqs` in `unknowns`, under the lexicographic
 *  order `unknowns(0) > unknowns(1) > …` (F_0068), by Buchberger's algorithm.
 *
 *  **Why lexicographic**: a lex basis of a system with finitely many solutions is *triangular*
 *  — an element in the last unknown alone, then elements bringing in one more unknown at a time
 *  — which is exactly the shape [[eliminate]] finishes: a univariate equation to root-find, then
 *  substitution.  So the basis is a front end to the elimination solver, never a replacement.
 *
 *  **Exact arithmetic, always.** A coefficient is read as the rational it is: a `_Rational` as
 *  itself, a `Double` as the dyadic rational the hardware holds, so no tolerance ever decides
 *  that a coefficient is zero — the decision Buchberger's algorithm makes at every step, and the
 *  one a floating-point implementation gets wrong. The answer is written back in the input's
 *  tier: exact when every coefficient was exact, `Double` otherwise.
 *
 *  **Declines** (`None`) on anything that is not a polynomial in the unknowns with numeric
 *  coefficients — a free parameter, a transcendental function, a negative or fractional power —
 *  and past any of the caps on the basis's size, degree, coefficient size and work, which are
 *  what exact Buchberger needs, since its intermediate growth is notoriously unbounded.  An
 *  inconsistent system has the basis `[1]`.
 *
 *  @param eqs      the polynomials, each meaning `= 0`
 *  @param unknowns the unknowns, in decreasing lexicographic order
 *  @param env      bindings: a bound parameter is substituted
 *  @return the reduced, monic basis in increasing order of its leading terms, or `None`
 */
def groebnerBasis(eqs: List[_Expression], unknowns: List[_Variable],
                  env: Environment): Option[List[_Expression]] =
  val names = unknowns.map(_.variable).toVector
  val read  = eqs.map(e => GPoly.from(substitute(e, boundParameters(eqs, names.toSet, env)), names))
  sequence(read).flatMap { ps =>
    val exact = ps.forall(_._2)
    buchberger(ps.map(_._1).filterNot(_.isZero)).map(_.map(_.toExpression(unknowns, exact)))
  }


/** `e` with every coefficient that is rounding dropped, when `e` is a polynomial in `names` with
 *  numeric coefficients; otherwise `e` unchanged (F_0068).
 *
 *  **Why it is needed**: substituting an inexact root into a polynomial whose coefficient
 *  vanishes there exactly leaves a coefficient like `10⁻¹⁷` instead of `0`. Read as a value, it
 *  turns `0·x = 0` into `x = 0`, and the branch's other equations then find that inconsistent:
 *  the points are lost while the list still looks complete. A coefficient is rounding when it is
 *  at most [[RoundingLevel]] of the magnitudes of the terms that cancelled to form it, which is
 *  a measure without units. Dropping errs safe: an equation wrongly dropped only admits
 *  candidates, which [[eliminate]]'s final verification removes.
 *
 *  @param e     the expression, after substitution and before simplification
 *  @param names the unknowns still to solve for
 *  @return the cleaned polynomial, or `e` itself
 */
private[optimize] def withoutRounding(e: _Expression, names: Vector[String]): _Expression =
  GPoly.withMagnitude(e, names) match
    case Some((p, mag)) =>
      val kept = p.terms.filter((m, c) => c.abs.toDouble > RoundingLevel * mag.terms.getOrElse(m, Q.zero).toDouble)
      if kept.size == p.terms.size then e
      else GPoly(kept).toExpression(names.toList.map(_Variable(_)), exact = false)
    case None => e

/** An exponent vector, one entry per unknown. */
private type Mono = Vector[Int]

/** The lexicographic order: the first unknown whose exponents differ decides. */
private def lexCompare(a: Mono, b: Mono): Int =
  var i = 0
  while i < a.length && a(i) == b(i) do i += 1
  if i == a.length then 0 else Integer.compare(a(i), b(i))

private given Ordering[Mono] = (a, b) => lexCompare(a, b)

/** A polynomial with exact rational coefficients, no zero term stored. */
private final case class GPoly(terms: Map[Mono, Q]):
  def isZero: Boolean = terms.isEmpty
  def isConstant: Boolean = terms.keys.forall(_.forall(_ == 0))
  lazy val lead: (Mono, Q) = terms.maxBy(_._1)
  def degree: Int = terms.keys.map(_.sum).maxOption.getOrElse(0)
  def bits: Int = terms.valuesIterator.map(q => q.numerator.bitLength + q.denominator.bitLength).maxOption.getOrElse(0)

  def +(that: GPoly): GPoly =
    GPoly(that.terms.foldLeft(terms) { case (acc, (m, c)) =>
      val s = acc.getOrElse(m, Q.zero) + c
      if s.isZero then acc - m else acc.updated(m, s)
    })
  def unary_- : GPoly = GPoly(terms.view.mapValues(-_).toMap)
  def -(that: GPoly): GPoly = this + (-that)
  /** `c · x^m · this`. */
  def shifted(c: Q, m: Mono): GPoly =
    if c.isZero then GPoly(Map.empty)
    else GPoly(terms.map((k, v) => (k.lazyZip(m).map(_ + _), v * c)))
  def *(that: GPoly): GPoly =
    that.terms.foldLeft(GPoly(Map.empty)) { case (acc, (m, c)) => acc + shifted(c, m) }
  def monic: GPoly = if isZero then this else shifted(lead._2.reciprocal, Vector.fill(lead._1.length)(0))

  /** The polynomial as an expression in `unknowns`, terms in decreasing order, every invented
   *  constant — a coefficient, an exponent — built in the input's tier (the numeric tier rule). */
  def toExpression(unknowns: List[_Variable], exact: Boolean): _Expression =
    def number(q: Q): _Expression =
      if exact then _Rational.of(q.numerator.toBigInt, q.denominator.toBigInt).getOrElse(_Number(q.toDouble))
      else _Number(q.toDouble)
    def integer(k: Int): _Expression = if exact then _Rational(k) else _Number(k)
    val sorted = terms.toList.sortBy(_._1)(using summon[Ordering[Mono]].reverse)
    val parts = sorted.map { (m, c) =>
      val factors = unknowns.zip(m).collect {
        case (v, 1)          => v: _Expression
        case (v, k) if k > 1 => Power(v, integer(k))
      }
      factors match
        case Nil                    => number(c)
        case fs if c == Q.one       => fs.reduceLeft(Product(_, _))
        case fs                     => Product(number(c), fs.reduceLeft(Product(_, _)))
    }
    parts match
      case Nil => number(Q.zero)
      case ps  => ps.reduceLeft(Sum(_, _))

private object GPoly:
  private def constant(n: Int, q: Q): GPoly = GPoly(if q.isZero then Map.empty else Map(Vector.fill(n)(0) -> q))

  /** Reads `e` as a polynomial in `names`, with whether every coefficient was exact; `None`
   *  for anything else. `_Rational` is matched BEFORE `_Number`, whose extractor widens. */
  def from(e: _Expression, names: Vector[String]): Option[(GPoly, Boolean)] =
    val n = names.size
    def go(e: _Expression): Option[(GPoly, Boolean)] = e match
      case r: _Rational => Some((constant(n, Q(r.num, r.den)), true))
      case _Number(d)   => _Rational.fromDouble(d).map(r => (constant(n, Q(r.num, r.den)), false))
      case _Variable(name) =>
        names.indexOf(name) match
          case -1 => None
          case i  => Some((GPoly(Map(Vector.tabulate(n)(k => if k == i then 1 else 0) -> Q.one)), true))
      case Sum(a, b)     => for (p, x) <- go(a); (q, y) <- go(b) yield (p + q, x && y)
      case Product(a, b) => for (p, x) <- go(a); (q, y) <- go(b) yield (p * q, x && y)
      case Ratio(a, b) =>
        for
          (p, x) <- go(a)
          (q, y) <- go(b)
          if q.isConstant && !q.isZero
        yield (p.shifted(q.lead._2.reciprocal, Vector.fill(n)(0)), x && y)
      case Power(b, k) =>
        val exponent = k match
          case r: _Rational if r.den == 1 && r.num >= 0 && r.num <= MaxGroebnerDegree => Some((r.num.toInt, true))
          case _Number(d) if d == math.floor(d) && d >= 0 && d <= MaxGroebnerDegree     => Some((d.toInt, false))
          case _                                                                         => None
        for (p, x) <- go(b); (m, y) <- exponent yield ((1 to m).foldLeft(constant(n, Q.one))((acc, _) => acc * p), x && y)
      case _ => None
    go(e)

  /** Reads `e` as a polynomial together with its *magnitude*: the same polynomial built from
   *  the absolute values of every term, so each coefficient carries the size of what cancelled
   *  into it. `None` when `e` is not a polynomial in `names` with numeric coefficients. */
  def withMagnitude(e: _Expression, names: Vector[String]): Option[(GPoly, GPoly)] =
    val n = names.size
    def number(d: Q): (GPoly, GPoly) = (constant(n, d), constant(n, d.abs))
    def go(e: _Expression): Option[(GPoly, GPoly)] = e match
      case r: _Rational => Some(number(Q(r.num, r.den)))
      case _Number(d)   => _Rational.fromDouble(d).map(r => number(Q(r.num, r.den)))
      case _Variable(name) =>
        names.indexOf(name) match
          case -1 => None
          case i  =>
            val p = GPoly(Map(Vector.tabulate(n)(k => if k == i then 1 else 0) -> Q.one))
            Some((p, p))
      case Sum(a, b)     => for (p, pm) <- go(a); (q, qm) <- go(b) yield (p + q, pm + qm)
      case Product(a, b) => for (p, pm) <- go(a); (q, qm) <- go(b) yield (p * q, pm * qm)
      case Ratio(a, b) =>
        for
          (p, pm) <- go(a)
          (q, _)  <- go(b)
          if q.isConstant && !q.isZero
        yield
          val c = q.lead._2
          (p.shifted(c.reciprocal, Vector.fill(n)(0)), pm.shifted(c.abs.reciprocal, Vector.fill(n)(0)))
      case Power(b, k) =>
        val exponent = k match
          case r: _Rational if r.den == 1 && r.num >= 0 && r.num <= MaxGroebnerDegree => Some(r.num.toInt)
          case _Number(d) if d == math.floor(d) && d >= 0 && d <= MaxGroebnerDegree     => Some(d.toInt)
          case _                                                                         => None
        for (p, pm) <- go(b); m <- exponent yield
          ((1 to m).foldLeft(constant(n, Q.one))((acc, _) => acc * p),
           (1 to m).foldLeft(constant(n, Q.one))((acc, _) => acc * pm))
      case _ => None
    go(e)


/** Buchberger's algorithm with the coprime-leading-term criterion and the normal selection
 *  strategy (the pair with the smallest lcm first), then interreduction; `None` past a cap. */
private def buchberger(input: List[GPoly]): Option[List[GPoly]] =
  val n = input.headOption.map(_.lead._1.length).getOrElse(0)
  val one = GPoly(Map(Vector.fill(n)(0) -> Q.one))
  def lcm(a: Mono, b: Mono): Mono = a.lazyZip(b).map(math.max)
  def divides(a: Mono, b: Mono): Boolean = a.lazyZip(b).forall(_ <= _)
  def quotient(b: Mono, a: Mono): Mono = b.lazyZip(a).map(_ - _)
  def withinCaps(p: GPoly): Boolean = p.degree <= MaxGroebnerDegree && p.bits <= MaxGroebnerCoefficientBits

  /** The full remainder of `p` on division by `g`; `None` when a cap is passed on the way. */
  def reduce(p: GPoly, g: Vector[GPoly]): Option[GPoly] =
    @annotation.tailrec
    def loop(p: GPoly, r: GPoly): Option[GPoly] =
      if p.isZero then Some(r)
      else if !withinCaps(p) then None
      else
        val (m, c) = p.lead
        g.find(q => divides(q.lead._1, m)) match
          case Some(q) => loop(p - q.shifted(c / q.lead._2, quotient(m, q.lead._1)), r)
          case None    =>
            val t = GPoly(Map(m -> c))
            loop(p - t, r + t)
    loop(p, GPoly(Map.empty))

  def sPoly(f: GPoly, g: GPoly): GPoly =
    val l = lcm(f.lead._1, g.lead._1)
    f.shifted(f.lead._2.reciprocal, quotient(l, f.lead._1)) - g.shifted(g.lead._2.reciprocal, quotient(l, g.lead._1))

  @annotation.tailrec
  def run(basis: Vector[GPoly], pairs: List[(Int, Int)], work: Int): Option[Vector[GPoly]] =
    if basis.exists(_.isConstant) then Some(Vector(one))
    else pairs match
      case Nil => Some(basis)
      case _ if work >= MaxGroebnerReductions || basis.size > MaxGroebnerBasis => None
      case _ =>
        val (i, j) = pairs.minBy((a, b) => {
          val l = lcm(basis(a).lead._1, basis(b).lead._1)
          (l.sum, l)
        })(using Ordering.Tuple2(Ordering.Int, summon[Ordering[Mono]]))
        val rest = pairs.filterNot(_ == (i, j))
        val (fi, fj) = (basis(i).lead._1, basis(j).lead._1)
        // Coprime leading monomials: the S-polynomial reduces to zero (Buchberger's first criterion).
        if fi.lazyZip(fj).forall((a, b) => a == 0 || b == 0) then run(basis, rest, work)
        else reduce(sPoly(basis(i), basis(j)), basis) match
          case None                => None
          case Some(r) if r.isZero => run(basis, rest, work + 1)
          case Some(r) =>
            val next = basis :+ r.monic
            run(next, rest ++ basis.indices.map((_, basis.size)), work + 1)

  val start = input.map(_.monic).toVector
  run(start, (for j <- start.indices; i <- 0 until j yield (i, j)).toList, 0).flatMap { g =>
    // Minimal: drop an element whose leading monomial another's divides; then reduce each
    // element by the rest, which makes the basis unique.
    val minimal = g.indices.filterNot { i =>
      g.indices.exists(j => j != i && divides(g(j).lead._1, g(i).lead._1) &&
                            (g(j).lead._1 != g(i).lead._1 || j < i))
    }.map(g).toVector
    sequence(minimal.indices.toList.map(i => reduce(minimal(i), minimal.patch(i, Nil, 1)).map(_.monic)))
      .map(_.filterNot(_.isZero).sortBy(_.lead._1))
  }

package it.grypho.scala.leonardo
package scalar

import core.*


// A rational function read as the factors it is written with (F_0351).
//
// Every consumer of real roots — the inequality solver's sign chart, `solve`, the domain analysis,
// `singularities` — used to expand its input into one numerator and one denominator before finding
// roots.  Expanding a product is the ill-conditioned step: the roots of `(x - 1)·(x - 1.00001)` are
// exact in its two factors and a cancellation in its coefficients, where a discriminant or a gcd then
// has to tell them apart with a tolerance, and read them as one double root.  The numerical
// conditioning rule — prefer the form that never builds the quantity whose error dominates — says
// not to expand.  So the tree is read structurally: products, quotients and integer powers are kept,
// and only a factor that is itself a sum is turned into coefficients.  Each factor's roots are found
// alone; multiplicities come from the written powers and add where two factors share a root; and a
// sign is a product of the factors' signs, each read off the factor's own roots, so no value is ever
// evaluated beside a root.

/** One written factor: a polynomial and the power it is raised to, negative below the fraction bar. */
private[leonardo] final case class Factor(coeffs: Vector[Double], power: Int)

/** A real point where a factored function vanishes or is undefined.
 *  @param at        the point
 *  @param zeroOrder the multiplicity of the point among the numerator's factors
 *  @param poleOrder its multiplicity among the denominator's: positive means undefined there, even
 *                   when the numerator cancels it (a removable point) */
private[leonardo] final case class CutPoint(at: Double, zeroOrder: Int, poleOrder: Int)

/** A rational function as written factors, with its cut points and its sign between them.  Built by
 *  [[factoredIn]], which has read every factor's real roots ([[realRoots]]) and declines when one
 *  cannot be read (F_0370).
 *  @param factors the factors, constants included (their sign counts)
 *  @param roots   each factor's real roots, aligned with `factors` */
private[leonardo] final class Factored private[scalar] (val factors: Vector[Factor], roots: Vector[Vector[RealRoot]]):

  /** Whether the function is identically zero: a numerator factor is the zero polynomial. */
  val isZero: Boolean = factors.exists(f => f.power > 0 && degreeOf(f.coeffs) < 0)

  /** Whether no factor with a root sits below the bar. */
  val isPolynomial: Boolean = factors.forall(f => f.power > 0 || degreeOf(f.coeffs) < 1)

  // every factor's roots, tagged with the factor, in one ascending list; two roots are one point when
  // they are closer than their uncertainties together (a root of the same point computed twice)
  private val clusters: Vector[Vector[(RealRoot, Int)]] =
    val all = roots.zipWithIndex.flatMap((rs, i) => rs.map((_, i))).sortBy(_._1.at)
    all.foldLeft(Vector.empty[Vector[(RealRoot, Int)]]) { (acc, r) =>
      acc.lastOption match
        case Some(c) if c.exists((q, _) => math.abs(r._1.at - q.at) <= r._1.uncertainty + q.uncertainty) =>
          acc.init :+ (c :+ r)
        case _ => acc :+ Vector(r)
    }

  /** The cut points, ascending: every real root of every factor. */
  val cuts: Vector[CutPoint] = clusters.map { c =>
    def order(below: Boolean): Int =
      c.collect { case (r, i) if (factors(i).power < 0) == below => r.multiplicity * math.abs(factors(i).power) }.sum
    CutPoint(c.minBy(_._1.uncertainty)._1.at, order(false), order(true))
  }

  // for each factor, the multiplicity of its own roots at each cut point
  private val multiplicityAt: Vector[Array[Int]] = factors.indices.toVector.map { i =>
    clusters.map(_.collect { case (r, j) if j == i => r.multiplicity }.sum).toArray
  }

  /** The sign of the function on the open interval left of cut point `i` (`i = cuts.size`: right of
   *  them all): each factor has its leading coefficient's sign at `+∞` and changes it at each of its
   *  roots of odd multiplicity; a factor at an even power is positive.
   *  @param i the interval, `0` to `cuts.size`
   *  @return `-1`, `0` (only for the zero function) or `1` */
  def intervalSign(i: Int): Int =
    if isZero then 0
    else factors.indices.foldLeft(1) { (s, k) =>
      val f = factors(k)
      if f.power % 2 == 0 then s
      else
        val lead  = math.signum(f.coeffs(degreeOf(f.coeffs))).toInt
        val flips = (i until cuts.size).map(multiplicityAt(k)(_)).sum
        s * (if flips % 2 == 0 then lead else -lead)
    }

/** `e` as written factors in `v`, or `None` when a factor is not a rational function with numeric
 *  coefficients, the denominator is identically zero, the degrees pass [[MaxRationalDegree]], or a
 *  factor's real roots cannot be read ([[realRoots]], F_0370).
 *  @param e   the expression
 *  @param v   the variable
 *  @param env bindings, folded into the coefficients
 *  @return the factored reading */
private[leonardo] def factoredIn(e: _Expression, v: _Variable, env: Environment): Option[Factored] =
  def leaf(x: _Expression, p: Int): Option[Vector[Factor]] =
    rationalCoeffsIn(x, v, env).map { (num, den) =>
      Factor(num, p) +: (if den == Vector(1.0) then Vector.empty else Vector(Factor(den, -p)))
    }
  def whole(k: _Expression): Option[Int] = k.eval(env) match
    case Right(_Number(d)) if d == math.rint(d) && math.abs(d) <= MaxRationalDegree => Some(d.toInt)
    case _                                                                          => None
  def go(x: _Expression, p: Int): Option[Vector[Factor]] = x match
    case _ if !dependsOn(x, v) => leaf(x, p)
    case Product(a, b)         => for fa <- go(a, p); fb <- go(b, p) yield fa ++ fb
    case Ratio(a, b)           => for fa <- go(a, p); fb <- go(b, -p) yield fa ++ fb
    case Power(b, k) if !dependsOn(k, v) => whole(k) match
      case Some(0) => Some(Vector(Factor(Vector(1.0), 1)))
      case Some(n) => go(b, p * n)
      case None    => leaf(x, p)
    case _ => leaf(x, p)
  go(e, 1).filter { fs =>
    !fs.exists(f => f.power < 0 && degreeOf(f.coeffs) < 0) &&
      fs.map(f => math.max(degreeOf(f.coeffs), 0).toLong * math.abs(f.power)).sum <= MaxRationalDegree
  }.flatMap(fs => sequence(fs.map(f => realRoots(f.coeffs))).map(Factored(fs, _)))

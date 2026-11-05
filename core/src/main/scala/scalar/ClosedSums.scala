package it.grypho.scala.leonardo
package scalar

import core.*
import spire.math.{Rational => Q}


// Sums over a symbolic bound in closed form (F_0291).
//
// `sum(f, k, lo, hi)` with a bound that is not a number stayed as written, so a Riemann sum's limit
// could not be taken.  Two families close: a polynomial in `k` whose coefficients are free of `k`
// (Faulhaber's formula, its Bernoulli coefficients exact), and a geometric term `c*r^(a*k + b)` whose
// ratio `r^a` is a number other than 1.  A sum of such terms closes term by term; anything else
// stays unevaluated.  Only reached when a bound does not fold to a number, so no numeric sum changes.

/** The highest power summed by Faulhaber's formula: `collect`'s own cap. */
private val MaxFaulhaberPower: Int = 20

/** The Bernoulli numbers `B_0 .. B_m` with `B_1 = +1/2`, the convention Faulhaber's formula takes. */
private lazy val bernoulliPlus: Vector[Q] =
  val minus = (1 to MaxFaulhaberPower).foldLeft(Vector(Q.one)) { (bs, m) =>
    val s = (0 until m).foldLeft(Q.zero)((acc, k) => acc + Q(binomial(m + 1, k)) * bs(k))
    bs :+ (-s / Q(m + 1))
  }
  minus.updated(1, Q(1, 2))

private def binomial(n: Int, k: Int): BigInt =
  (1 to k).foldLeft(BigInt(1))((acc, i) => acc * (n - k + i) / i)

/** A spire rational as a literal in the tier. */
private def qLit(q: Q)(using t: Tier): _Expression =
  if t.exact then _Rational.of(q.numerator.toBigInt, q.denominator.toBigInt).getOrElse(_Number(spireToDouble(q)))
  else _Number(spireToDouble(q))

/** `S_p(x) = 1^p + 2^p + ... + x^p` as a polynomial in `x` (Faulhaber):
 *  `(1/(p+1)) * sum_j C(p+1, j) * B_j * x^(p+1-j)`, `B_1 = +1/2`. */
private def faulhaber(p: Int, x: _Expression)(using t: Tier): _Expression =
  (0 to p).map { j =>
    val c = Q(binomial(p + 1, j)) * bernoulliPlus(j) / Q(p + 1)
    (c, p + 1 - j)
  }.filterNot(_._1.isZero)
   .map((c, k) => Product(qLit(c), Power(x, t.n(k))): _Expression)
   .reduceLeft(Sum(_, _))

/** `sum(e, v, lo, hi)` in closed form, or `None` when the summand is not of a closing family.
 *  @param e   the summand
 *  @param v   the index
 *  @param lo  the first index
 *  @param hi  the last index
 *  @param env the environment
 *  @return the closed form, unevaluated
 */
private[scalar] def closedSum(e: _Expression, v: _Variable, lo: _Expression, hi: _Expression,
                              env: Environment): Option[_Expression] =
  val exact = _Rational.containsExactIn(e, env) || _Rational.containsExactIn(lo, env) ||
              _Rational.containsExactIn(hi, env)
  given t: Tier = Tier(exact)
  def flat(x: _Expression): List[_Expression] = x match
    case Sum(a, b) => flat(a) ++ flat(b)
    case other     => List(other)
  val whole = polynomialSum(e, v, lo, hi).map(List(_))
  val parts = whole.orElse(sequence(flat(expand(e)).map(x => polynomialSum(x, v, lo, hi).orElse(geometricSum(x, v, lo, hi, env)))))
  parts.map(ps => simplifyFullyInTier(ps.reduceLeft(Sum(_, _)), exact))

/** `sum(c_0 + c_1 k + ... , k, lo, hi)` with every `c_p` free of `k`: `sum_p c_p (S_p(hi) - S_p(lo - 1))`. */
private def polynomialSum(e: _Expression, v: _Variable, lo: _Expression, hi: _Expression)
                         (using t: Tier): Option[_Expression] =
  collect(e, v).filter(cs => cs.nonEmpty && cs.sizeIs <= MaxFaulhaberPower + 1 && cs.forall(c => !dependsOn(c, v)))
    .map { cs =>
      val below = Sum(lo, t.n(-1))
      cs.zipWithIndex.filterNot((c, _) => isZeroLiteral(c)).map { (c, p) =>
        Product(c, Sum(faulhaber(p, hi), Product(t.n(-1), faulhaber(p, below)))): _Expression
      }.reduceLeftOption(Sum(_, _)).getOrElse(t.n(0))
    }

/** `sum(c * r^(a k + b), k, lo, hi)` (or `c * exp(a k + b)`) with ratio `R = r^a` a number other than 1:
 *  `c * r^b * (R^(hi + 1) - R^lo) / (R - 1)`. */
private def geometricSum(e: _Expression, v: _Variable, lo: _Expression, hi: _Expression, env: Environment)
                        (using t: Tier): Option[_Expression] =
  def flat(x: _Expression): List[_Expression] = x match
    case Product(a, b) => flat(a) ++ flat(b)
    case other         => List(other)
  def linear(u: _Expression): Option[(_Expression, _Expression)] =
    collect(u, v).filter(cs => cs.sizeIs == 2 && cs.forall(c => !dependsOn(c, v))).map(cs => (cs(1), cs(0)))
  val (consts, fs) = flat(e).partition(x => !dependsOn(x, v))
  val shape: Option[(_Expression, _Expression)] = fs match   // (ratio, multiplier)
    case List(Power(r, u)) if !dependsOn(r, v) => linear(u).map((a, b) => (Power(r, a), Power(r, b)))
    case List(Exp(u))                         => linear(u).map((a, b) => (Exp(a), Exp(b)))
    case _                                    => None
  shape.filter((ratio, _) => evalNumeric(ratio, env) match
    case Right(_Number(d)) => d.isFinite && d != 1.0
    case _                 => false
  ).map { (ratio, mult) =>
    val c = consts.foldLeft[_Expression](mult)(Product(_, _))
    Product(c, Ratio(Sum(Power(ratio, Sum(hi, t.n(1))), Product(t.n(-1), Power(ratio, lo))),
                     Sum(ratio, t.n(-1))))
  }

/** `e` with every `sum` whose bounds involve `v` replaced by its closed form where it has one
 *  (F_0291), so a limit in `v` reads a rational function of `v` rather than sampling the sum.
 *  @param e the expression
 *  @param v the variable the bounds depend on
 *  @return the expression with those sums closed */
private[scalar] def closeSums(e: _Expression, v: _Variable): _Expression = e match
  case r @ _Reduction(ReduceKind.Sum, body, k, lo, hi) if dependsOn(lo, v) || dependsOn(hi, v) =>
    closedSum(body, k, lo, hi, new Environment()).getOrElse(r)
  case other if other.children.isEmpty => other
  case other => other.rebuild(other.children.map(closeSums(_, v)))

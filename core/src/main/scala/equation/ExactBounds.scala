package it.grypho.scala.leonardo
package equation

import core.*
import scalar.*


// The bounds of a solution set in the input's tier (F_0337).
//
// The inequality solver and the domain analysis find their cut points in `Double` — a sign chart is
// built by ordering roots, and the root finder works in floating point — and wrote them as decimals,
// so under `exact on` `solve(x^2 > 4, x)` answered `x < -2.0 or x > 2.0` and `domain(ln(x - 1/3), x)`
// `x > 0.33333`.  The cut points are roots of exact polynomials, so their exact values exist: a rational
// root of any degree (F_0228's `exactPolynomialRoots`), a quadratic's irrational one as a surd through
// `solve` (F_0231).  The chart keeps its `Double` arithmetic, which orders the points; only the leaves
// of the answer are replaced, each by the exact root it is a reading of.  A root with no exact form (a
// cubic's irrational one) stays a decimal, and the REPL's note says so.

/** The exact real roots of `p` in `v` (F_0337): every rational one, and the surds of a quadratic factor;
 *  empty when `p` is not a polynomial with exact coefficients.
 *  @param p   the polynomial
 *  @param v   the variable
 *  @param env bindings and precision
 *  @return the exact roots, as `_Rational` or `_Surd` */
private[leonardo] def exactRootsOf(p: _Expression, v: _Variable, env: Environment): Vector[_Expression] =
  exactPolynomialRoots(p, v) match
    case None     => Vector.empty
    case Some(rs) =>
      val exact = rs.filter(_Surd.isExact)
      if exact.size == rs.size then exact
      else
        // irrational roots left: a quadratic's are surds, which `solve` forms exactly
        val surds = solve(_Equation(p, _Rational.literalLike(0, p, env)), v, env).collect {
          case _Equation(_, r) if _Surd.isExact(r) => r
        }
        (exact ++ surds).distinct

/** The exact points where `e = t` can change sign in `v` (F_0337): the roots of `e − t` when it is a
 *  polynomial, and, for every quotient `n/d` in `e`, the roots of `n`, of `d` and of `n − t·d` — the cut
 *  points of a sign chart, the critical points of a rational function written as quotients.
 *  @param e   the expression
 *  @param t   the threshold
 *  @param v   the variable
 *  @param env bindings and precision
 *  @return the exact candidates, possibly empty */
private[leonardo] def exactCutPoints(e: _Expression, v: _Variable, t: Int, env: Environment): Vector[_Expression] =
  // read with the environment: a bare `x` has no exact content of its own, but `exact on` does (F_0273)
  def tier(n: Int): _Expression = _Rational.literalLike(n, e, env)
  def ratios(x: _Expression): Vector[Ratio] =
    (x match { case r: Ratio => Vector(r); case _ => Vector.empty }) ++ x.children.toVector.flatMap(ratios)
  val whole = exactRootsOf(Sum(e, Product(tier(-1), tier(t))), v, env)
  val parts = ratios(e).flatMap { case Ratio(n, d) =>
    exactRootsOf(n, v, env) ++ exactRootsOf(d, v, env) ++
      (if t == 0 then Vector.empty else exactRootsOf(Sum(n, Product(tier(-t), d)), v, env))
  }
  (whole ++ parts).distinct

/** `answer` with every decimal leaf that is, to rounding, one of `exact` replaced by it (F_0337).  Only a
 *  `_Number` is replaced — a leaf already exact stays — and only within `10⁻⁹` of the candidate,
 *  relatively, since a root found in `Double` carries rounding and nothing else.
 *  @param answer the answer, a relation built from `Double` bounds
 *  @param exact  the exact values its bounds may be readings of
 *  @return the answer in the exact tier where a bound matched */
private[leonardo] def withExactLeaves(answer: _Expression, exact: Vector[_Expression]): _Expression =
  if exact.isEmpty then answer
  else answer match
    case n: _Number =>
      val d = n.d
      exact.find {
        case _Number(x) => x == d || math.abs(x - d) <= 1e-9 * math.max(math.abs(x), math.abs(d))
        case _          => false
      }.getOrElse(n)
    case other if other.children.isEmpty => other
    case other => other.rebuild(other.children.map(withExactLeaves(_, exact)))

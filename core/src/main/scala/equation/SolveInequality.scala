package it.grypho.scala.leonardo
package equation

import core.*
import scalar.*
import logic.{And, Or}


/** Inequality solving.
 *
 *  `solve(x^2 - 4 > 0, x)` answers `x < -2 or x > 2`.  The counterpart of [[solve]], which
 *  handles `=`; this handles `<`, `>`, `<=`, `>=` and `!=`.
 *
 *  **No new carrier was needed.**  A solution set is a *set*, and the obvious worry is that
 *  it wants an interval-valued `_Value`.  With the comparison operators it does not: the answer is expressible in
 *  the language itself — a [[_Comparison]], an `or` of two, `and` for a bounded range, and
 *  `_Bool(true)` / `_Bool(false)` for the universal and empty sets.  This is the lesson
 *  applied again ("adding a `_Value` is cheap only when nothing has to read it as an existing
 *  one"), with the happier outcome that here one is avoided altogether.
 *
 *  **`equation` imports `logic` for this**, which is new but acyclic: `logic` imports only
 *  `core` and `scalar.compile`, never `equation`.  `probability.Predicate` already depends on
 *  both packages the same way.
 *
 *  **The direction flip is the whole problem.**  Dividing by a negative coefficient reverses
 *  the relation, so every tier asks [[scalar.sign]] first and refuses when the answer is
 *  `None`.  Refusing rather than assuming positivity is the same call the probability-predicate reader makes when
 *  it declined `prob(2*X < 6)`: the bad outcome is not an error, it is a confident answer to
 *  a different question.
 */

/** Solves `c` for `v`, or `None` when no tier applies (the caller then stays symbolic).
 *
 *  @param c   the comparison to solve
 *  @param v   the variable to solve for
 *  @param env bindings and precision
 *  @return the solution set written in the language, or `None`
 */
private[equation] def solveInequality(c: _Comparison, v: _Variable,
                                      env: Environment): Option[_Expression] =
  if !_Rational.containsExactIn(c, env) then absTier(c, v, env).orElse(polynomialOrRational(c, v, env))
  // F_0343: an exact rational function is charted exactly; anything else keeps the `Double` chart, its
  // bounds written in the comparison's tier where they read an exact root (F_0337)
  else absTier(c, v, env).orElse(exactChartTier(c, v, env)).orElse {
    val difference = Sum(c.lhs, Product(_Rational.literalLike(-1, c), c.rhs))
    polynomialOrRational(c, v, env).map(withExactLeaves(_, exactCutPoints(difference, v, 0, env)))
  }

/** An absolute value of the unknown against a bound (F_0290): `abs(u) < c` is `-c < u < c`,
 *  `abs(u) > c` is `u < -c or u > c`, `abs(u) < abs(w)` is `u^2 < w^2`; the parts are solved through the
 *  `and`/`or` path.  `c`'s sign must be decided (`scalar.sign`), or the tier declines: below zero
 *  `abs(u) < c` holds nowhere and `abs(u) > c` everywhere. */
private def absTier(c: _Comparison, v: _Variable, env: Environment): Option[_Expression] =
  val minusOne = _Rational.literalLike(-1, c)
  def neg(x: _Expression): _Expression = Product(minusOne, x)
  def solved(e: _Expression): Option[_Expression] = _Solve(e, v).eval(env).toExpression match
    case _: _Solve => None
    case other     => Some(other)
  val shape: Option[(Abs, _Expression, CompareOp)] = (c.lhs, c.rhs) match
    case (a: Abs, b) => Some((a, b, c.op))
    case (b, a: Abs) => Some((a, b, flip(c.op)))
    case _           => None
  shape.flatMap { (absSide, bound, op) => bound match
    case Abs(w) =>
      solved(_Comparison(Power(absSide.e, _Rational.literalLike(2, c)), op, Power(w, _Rational.literalLike(2, c))))
    case b if !dependsOn(b, v) =>
      val u = absSide.e
      sign(b, env).flatMap { s =>
        op match
          case CompareOp.Lt => if s <= 0 then Some(_Bool(false)) else solved(And(_Comparison(u, CompareOp.Gt, neg(b)), _Comparison(u, CompareOp.Lt, b)))
          case CompareOp.Le => if s < 0 then Some(_Bool(false))
                               else if s == 0 then solved(_Equation(u, b))
                               else solved(And(_Comparison(u, CompareOp.Ge, neg(b)), _Comparison(u, CompareOp.Le, b)))
          case CompareOp.Gt => if s < 0 then Some(_Bool(true)) else solved(Or(_Comparison(u, CompareOp.Lt, neg(b)), _Comparison(u, CompareOp.Gt, b)))
          case CompareOp.Ge => if s <= 0 then Some(_Bool(true)) else solved(Or(_Comparison(u, CompareOp.Le, neg(b)), _Comparison(u, CompareOp.Ge, b)))
          case CompareOp.Ne => None
      }
    case _ => None
  }

/** The constant and linear tiers, whose coefficients may be symbolic, then the sign chart. */
private def polynomialOrRational(c: _Comparison, v: _Variable, env: Environment): Option[_Expression] =
  // The -1 must join the comparison's own tier, or an exactly-stated inequality is demoted
  // through float contagion before the solver runs (the numeric-tier rule).
  val minusOne   = _Rational.literalLike(-1, c)
  val difference = Sum(c.lhs, Product(minusOne, c.rhs))

  // collect keeps the coefficients' exact tier; the degree is read from the magnitude-tracked
  // coefficients (F_0121), so a residue top coefficient is dropped and a genuine tiny one kept.
  collect(difference, v).map(atMagnitudeDegree(_, difference, v, env)) match
    case Some(cs) if cs.size == 1 => constantTier(cs(0), c.op, env)
    case Some(cs) if cs.size == 2 => linearTier(cs(0), cs(1), c.op, v, env)
    case _                        => chartTier(c, v, env)

/** `N(v)/D(v) <op> 0` by the sign chart of its **written factors** (F_0351): the quadratic, the
 *  higher degrees and the rational functions in one tier.
 *
 *  The cut points are the roots of each factor, found alone ([[scalar.factoredIn]]): expanded first,
 *  `(x - 1)*(x - 1.00001) > 0` went through a discriminant tolerance and answered `x != 1.000005`,
 *  and a cubic through a gcd that read the same pair as a double root.  The sign on each interval is
 *  the product of the factors' signs.  At a cut point the function is undefined where a denominator
 *  factor vanishes — a pole, or a point the numerator cancels — and zero otherwise, so **a non-strict
 *  relation is answered too**, on a rational function as on a polynomial: `(x - 1)^2/(x - 2) >= 0` is
 *  `x = 1 or x > 2` (it stayed unevaluated; the exact tier has answered it since F_0343).  A side that
 *  is zero is dropped rather than subtracted, which would make a sum of the product.
 */
private def chartTier(c: _Comparison, v: _Variable, env: Environment): Option[_Expression] =
  def zero(e: _Expression): Boolean = !dependsOn(e, v) && (e.eval(env) match
    case Right(_Number(d)) => d == 0.0
    case _                 => false)
  val (f, op) =
    if zero(c.rhs) then (c.lhs, c.op)
    else if zero(c.lhs) then (c.rhs, flip(c.op))
    else (Sum(c.lhs, Product(_Rational.literalLike(-1, c), c.rhs)), c.op)
  factoredIn(f, v, env).map { fd =>
    def holds(s: Int): Boolean = op match
      case CompareOp.Lt => s < 0
      case CompareOp.Le => s <= 0
      case CompareOp.Gt => s > 0
      case CompareOp.Ge => s >= 0
      case CompareOp.Ne => s != 0
    val in = Vector.tabulate(2 * fd.cuts.size + 1) { k =>
      if k % 2 == 0 then holds(fd.intervalSign(k / 2))
      else fd.cuts((k - 1) / 2).poleOrder == 0 && holds(0)
    }
    renderRuns(fd.cuts.map(p => _Number(p.at): _Expression), in, v, notEqual = true)
  }

/** `v` does not occur, so the relation is a constant truth: `2 < 3` is `true`. */
private def constantTier(c0: _Expression, op: CompareOp, env: Environment): Option[_Expression] =
  _Comparison(c0, op, _Rational.literalLike(0, c0)).eval(env) match
    case Right(b: _Bool) => Some(b)
    case _               => None

/** `c1*v + c0 <op> 0`  ->  `v <op'> -c0/c1`, with `op'` flipped when `c1` is negative. */
private def linearTier(c0: _Expression, c1: _Expression, op: CompareOp,
                       v: _Variable, env: Environment): Option[_Expression] =
  sign(c1, env) match
    case Some(s) if s != 0 =>
      val bound = simplifyFully(Ratio(Product(_Rational.literalLike(-1, c0), c0), c1))
      Some(_Comparison(v, if s < 0 then flip(op) else op, bound.eval(env).toExpression))
    case _ => None

/** The reversed relation, which is what multiplying through by a negative produces. */
private def flip(op: CompareOp): CompareOp = op match
  case CompareOp.Lt => CompareOp.Gt
  case CompareOp.Gt => CompareOp.Lt
  case CompareOp.Le => CompareOp.Ge
  case CompareOp.Ge => CompareOp.Le
  case CompareOp.Ne => CompareOp.Ne

/** Evaluates `e` to a finite real number, or `None`. */
private def foldReal(e: _Expression, env: Environment): Option[Double] = e.eval(env) match
  case Right(_Number(d)) if d.isFinite => Some(d)
  case _                                              => None


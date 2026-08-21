package it.grypho.scala.leonardo
package scalar

import core.*


/** Highest Taylor order [[taylorSeries]] will expand.
 *
 *  Matches `Expand`'s integer-power cap of 20 and exists for the same reason: the work
 *  grows with the order (each term needs one more differentiation), and a runaway order
 *  should give up rather than hang.  Beyond the cap the `_Taylor` node stays symbolic.
 */
val MaxTaylorOrder: Int = 20


/** Returns `true` when an unevaluated `_Derivative` survives anywhere in `e`.
 *
 *  The Taylor coefficients are built from repeated differentiation, and `derive` returns
 *  the `_Derivative` node itself for anything outside its rule table (`Gamma`, `fact`, an
 *  integral in an unrelated variable, ...).  A series whose coefficients contain such a
 *  node is not a polynomial and is worse than no answer, so its presence is the signal to
 *  stay symbolic -- the same guard `SolveODESymbolic` applies to a surviving `_Integral`.
 */
private def hasDerivative(e: _Expression): Boolean = e match
  case _: _Derivative => true
  case other          => other.children.exists(hasDerivative)


/** Expands `e` as a Taylor polynomial in `v` about `point`, to and including order `order`.
 *
 *  `Σ(k = 0 to order) f⁽ᵏ⁾(point) / k! · (v − point)ᵏ`, built by folding
 *  [[deriveN]] (which is memoised, so the k-th derivative reuses the work of the
 *  (k−1)-th) and instantiating each coefficient at `point` with [[substitute]].  The
 *  result is run through [[simplifyFully]], which collapses the `k = 0` term's
 *  `(v − point)⁰` to `1` and folds the constant coefficients.
 *
 *  `point` need not be numeric: expanding about a symbolic centre `a` is meaningful and
 *  yields a polynomial in `(v − a)` with `f⁽ᵏ⁾(a)` coefficients.
 *
 *  @param e     the expression to expand
 *  @param v     the expansion variable; it appears **free** in the result, so it is not a
 *               binder in the `_Derivative` sense
 *  @param point the centre of the expansion
 *  @param order the highest power retained; must be in `0 .. MaxTaylorOrder`
 *  @return the truncated series, or `None` when the order is out of range or some
 *          coefficient could not be differentiated
 */
def taylorSeries(
    e: _Expression, v: _Variable, point: _Expression, order: Int
): Option[_Expression] =
  if order < 0 || order > MaxTaylorOrder then None
  else
    // foldLeft over an Option accumulator: the first underivable coefficient aborts the
    // whole series rather than yielding a polynomial with a _Derivative inside it.
    val terms = (0 to order).foldLeft(Option(Vector.empty[_Expression])) { (acc, k) =>
      acc.flatMap { built =>
        val dk = deriveN(e, v, k)
        Option.when(!hasDerivative(dk)) {
          val atPoint = substitute(dk, Map(v.variable -> point))
          val shifted = Sum(v, Product(_Number(-1), point))
          built :+ Product(
            Ratio(atPoint, _Number(factorialOf(k.toDouble).getOrElse(Double.NaN))),
            Power(shifted, _Number(k)))
        }
      }
    }
    terms.map(ts => simplifyFully(ts.reduceLeft(Sum.apply)))


/** Expands `e` as a Maclaurin polynomial — the Taylor series about zero.
 *
 *  Sugar over [[taylorSeries]] with `point = 0`, not a separate algorithm; the grammar's
 *  `maclaurin(e, v, n)` desugars to `taylor(e, v, 0, n)` in the same way `log(x)`
 *  desugars to `LogBase(x, 10)`.
 *
 *  @param e     the expression to expand
 *  @param v     the expansion variable
 *  @param order the highest power retained
 *  @return the truncated series, or `None` under the same conditions as [[taylorSeries]]
 */
def maclaurinSeries(e: _Expression, v: _Variable, order: Int): Option[_Expression] =
  taylorSeries(e, v, _Number(0), order)

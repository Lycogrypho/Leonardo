package it.grypho.scala.leonardo
package scalar

import core.*


/** Locating and classifying singularities.
 *
 *  This is the half of the domain analysis that the **Laurent series** is built on: a
 *  Laurent expansion is taken *about* a singularity, so the point must first be found and
 *  its order known before the principal part has a length.
 *
 *  **Rational tier only, deliberately.**  For `N(v)/D(v)` with numeric coefficients the
 *  singularities are exactly the roots of `D`, and `polyRoots` finds them — the answer is
 *  complete and checkable.  For a general expression the critical points cannot be
 *  enumerated at all, so anything non-rational reports `None` rather than a partial list
 *  that would read as an exhaustive one.
 *
 *  The order is the multiplicity of the root in `D`, reduced by its multiplicity in `N`:
 *  `(x-1)/(x-1)` has a *removable* singularity at 1, not a pole, and saying "pole of order
 *  1" there would send the Laurent expansion looking for a principal part that does not exist.
 */

/** How an isolated singularity behaves. */
enum SingularityKind:
  /** The numerator cancels it entirely — the function extends continuously. */
  case Removable
  /** A genuine pole of the given order. */
  case Pole(order: Int)

/** One located singularity. */
case class Singularity(at: Double, kind: SingularityKind)

/** The singularities of `e` in `v`, or `None` when `e` is not a numeric rational function.
 *
 *  @param e   the expression to analyse
 *  @param v   the variable
 *  @param env bindings, so a bound coefficient participates
 *  @return the singularities in ascending order, or `None` if they cannot be enumerated
 */
private[leonardo] def singularitiesOf(e: _Expression, v: _Variable,
                                      env: Environment): Option[Vector[Singularity]] =
  // Read as written factors (F_0351): each factor's roots are found alone and a written power is a
  // multiplicity, so `1/((x - 1)*(x - 1.0001))` is two simple poles; expanded first, its
  // denominator's gcd read them as one double pole.  A numerator root is the same point as a
  // denominator one when the two computed locations are within their own rounding, not within a
  // fixed fraction of the roots' scale.
  factoredIn(e, v, env).map(_.cuts.filter(_.poleOrder > 0).map { c =>
    if c.zeroOrder >= c.poleOrder then Singularity(c.at, SingularityKind.Removable)
    else Singularity(c.at, SingularityKind.Pole(c.poleOrder - c.zeroOrder))
  })

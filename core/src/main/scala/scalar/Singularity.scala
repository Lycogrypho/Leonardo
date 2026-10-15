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
  rationalCoeffsIn(e, v, env).map { (num, den) =>
    val numRoots = realRootsWithMultiplicity(num)
    val denRoots = realRootsWithMultiplicity(den)
    // The roots' own scale (F_0125): a match is measured against it, not against 1.
    val scale = (numRoots ++ denRoots).map((r, _) => math.abs(r)).maxOption.getOrElse(0.0)
    denRoots.map { (centre, denMult) =>
      val numMult = numRoots.collect { case (r, m) if sameRoot(r, centre, scale) => m }.sum
      if numMult >= denMult then Singularity(centre, SingularityKind.Removable)
      else Singularity(centre, SingularityKind.Pole(denMult - numMult))
    }
  }

/** How far apart two computed roots may be and still count as the same point.
 *
 *  Only ever compares a numerator root against a denominator one now — the *multiplicity*
 *  of each no longer depends on a tolerance at all (see [[realRootsWithMultiplicity]]).
 */
private val RootClusterTolerance = 1e-6

/** Whether two computed roots are the same point, relative to the scale of all the roots.
 *  It was relative to `max(1, |a|, |b|)`, an absolute `10⁻⁶` below unit scale, so the
 *  numerator root at `10⁻⁷` matched the pole at `3·10⁻⁷` too and that pole was reported
 *  with order 0 (F_0125). */
private def sameRoot(a: Double, b: Double, scale: Double): Boolean =
  math.abs(a - b) <= RootClusterTolerance * scale

/** The distinct real roots of `cs` with their multiplicities, in ascending order.
 *
 *  **Multiplicity comes from square-free factorisation, not from counting scattered
 *  roots.**  The original reading — that a repeated root merely *scatters* under
 *  `polyRoots`, so nearby roots could be clustered and counted — understated the problem:
 *  it turned out that the QR iteration can fail to converge on a repeated root
 *  **entirely**, and `(x-2)^5` then produced no roots at all, silently losing the
 *  singularity rather than misreporting its order.
 *
 *  `squareFreeFactors` splits the polynomial by multiplicity arithmetically, so each part
 *  carries only *simple* roots — well conditioned, and located exactly.  The cluster
 *  tolerance disappears from this computation, taking its failure mode with it.
 *
 *  A complex root whose imaginary part is negligible still counts as real: within a
 *  square-free part that is ordinary rounding, not the old repeated-root perturbation.
 */
private def realRootsWithMultiplicity(cs: Vector[Double]): Vector[(Double, Int)] =
  if degreeOf(cs) < 1 then Vector.empty
  else
    squareFreeFactors(cs).flatMap { (part, mult) =>
      realParts(rootsOfSquareFree(part).getOrElse(Vector.empty), RootClusterTolerance).map((_, mult))
    }.sortBy(_._1)


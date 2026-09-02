package it.grypho.scala.leonardo
package scalar

import core.*


/** Locating and classifying singularities — issue 3.3 slice D.
 *
 *  This is the half of the domain analysis that **6.14 (Laurent series)** is blocked on: a
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
 *  1" there would send 6.14 looking for a principal part that does not exist.
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
  asRatio(e, v, env).map { (num, den) =>
    val numRoots = realRootsWithMultiplicity(num)
    realRootsWithMultiplicity(den).map { (centre, denMult) =>
      val numMult = numRoots.collect { case (r, m) if sameRoot(r, centre) => m }.sum
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

/** Whether two computed roots are the same point, relative to magnitude. */
private def sameRoot(a: Double, b: Double): Boolean =
  math.abs(a - b) <= RootClusterTolerance * math.max(1.0, math.max(math.abs(a), math.abs(b)))

/** The distinct real roots of `cs` with their multiplicities, in ascending order.
 *
 *  **Multiplicity comes from square-free factorisation, not from counting scattered roots**
 *  (issue 2.5).  The original reading — that a repeated root merely *scatters* under
 *  `polyRoots`, so nearby roots could be clustered and counted — understated the problem:
 *  issue 3.12 found that the QR iteration can fail to converge on a repeated root
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
  if polyDegree(cs) < 1 then Vector.empty
  else
    squareFreeFactors(cs).flatMap { (part, mult) =>
      rootsOfSquareFree(part).getOrElse(Vector.empty).collect {
        case (re, im) if math.abs(im) <= RootClusterTolerance * math.max(1.0, math.abs(re)) =>
          (re, mult)
      }
    }.sortBy(_._1)

/** Reads `e` as a ratio of two numeric-coefficient polynomials in `v`. */
private def asRatio(e: _Expression, v: _Variable,
                    env: Environment): Option[(Vector[Double], Vector[Double])] = e match
  case Ratio(n, d) =>
    for (nn, nd) <- asRatio(n, v, env); (dn, dd) <- asRatio(d, v, env)
    yield (mulPoly(nn, dd), mulPoly(nd, dn))
  case Product(a, b) =>
    for (an, ad) <- asRatio(a, v, env); (bn, bd) <- asRatio(b, v, env)
    yield (mulPoly(an, bn), mulPoly(ad, bd))
  case Sum(a, b) =>
    for (an, ad) <- asRatio(a, v, env); (bn, bd) <- asRatio(b, v, env)
    yield (addPoly(mulPoly(an, bd), mulPoly(bn, ad)), mulPoly(ad, bd))
  case _ =>
    plainCoeffs(e, v, env).map(cs => (cs, Vector(1.0)))

/** `collect` plus a fold of every coefficient to a number. */
private def plainCoeffs(e: _Expression, v: _Variable, env: Environment): Option[Vector[Double]] =
  collect(e, v).flatMap { cs =>
    cs.foldRight(Option(Vector.empty[Double])) { (c, acc) =>
      for tail <- acc; d <- (c.eval(env) match
                               case Right(_Number(x)) if !x.isNaN && !x.isInfinite => Some(x)
                               case _                                              => None)
      yield d +: tail
    }
  }

private def addPoly(a: Vector[Double], b: Vector[Double]): Vector[Double] =
  if a.isEmpty then b else if b.isEmpty then a
  else Vector.tabulate(math.max(a.size, b.size)) { i =>
    a.applyOrElse(i, (_: Int) => 0.0) + b.applyOrElse(i, (_: Int) => 0.0)
  }

private def mulPoly(a: Vector[Double], b: Vector[Double]): Vector[Double] =
  if a.isEmpty || b.isEmpty then Vector.empty
  else
    val out = Array.fill(a.size + b.size - 1)(0.0)
    for i <- a.indices; j <- b.indices do out(i + j) += a(i) * b(j)
    out.toVector

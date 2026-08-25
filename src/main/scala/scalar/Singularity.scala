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
    val denClusters = clusterRoots(nearRealRoots(den))
    val numRoots    = nearRealRoots(num)
    denClusters.map { (centre, denMult) =>
      val numMult = numRoots.count(r => sameRoot(r, centre))
      if numMult >= denMult then Singularity(centre, SingularityKind.Removable)
      else Singularity(centre, SingularityKind.Pole(denMult - numMult))
    }
  }

/** How far apart two computed roots may be and still count as one repeated root.
 *
 *  **This is loose on purpose.**  A root of multiplicity `m` is ill-conditioned: the
 *  companion-matrix eigenvalues scatter it by roughly `eps^(1/m)`, which is already ~6e-6
 *  for a triple root, and typically pushes all but one copy slightly off the real axis.  A
 *  tolerance tight enough to be "safe" would therefore report `(x-1)^3` as a *simple* pole,
 *  which is worse than merging two genuinely distinct roots that sit this close together.
 */
private val RootClusterTolerance = 1e-4

/** Whether two computed roots are the same point, relative to magnitude. */
private def sameRoot(a: Double, b: Double): Boolean =
  math.abs(a - b) <= RootClusterTolerance * math.max(1.0, math.max(math.abs(a), math.abs(b)))

/** Real roots of `cs`, **including complex ones whose imaginary part is negligible**.
 *
 *  Discarding those would undercount every repeated real root, since that is precisely the
 *  form the perturbation takes.
 */
private def nearRealRoots(cs: Vector[Double]): Vector[Double] =
  if polyDegree(cs) < 1 then Vector.empty
  else polyRoots(cs).fold(Vector.empty[Double]) { roots =>
    roots.flatMap {
      case _Number(d) if !d.isNaN => Some(d)
      case c: _Complex if math.abs(c.im) <= RootClusterTolerance * math.max(1.0, math.abs(c.re)) =>
        Some(c.re)
      case _ => None
    }.sorted
  }

/** Groups roots that are the same point, returning each centre with its multiplicity. */
private def clusterRoots(roots: Vector[Double]): Vector[(Double, Int)] =
  roots.foldLeft(Vector.empty[Vector[Double]]) { (acc, r) =>
    acc.lastOption match
      case Some(g) if sameRoot(g.head, r) => acc.init :+ (g :+ r)
      case _                              => acc :+ Vector(r)
  }.map(g => (g.sum / g.size, g.size))

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

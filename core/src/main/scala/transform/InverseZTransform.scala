package it.grypho.scala.leonardo
package transform

import core.*
import scalar.*


/** Inverse one-sided z-transform — issue 6.33 slice 2.
 *
 *  [[inverseZTransformOf]] recovers `x[n]` from a rational `X(z)`, returning
 *  [[_InverseZTransform]]`(f, z, n)` unchanged when no rule applies.
 *
 *  **The decomposition is of `X(z)/z`, not of `X(z)`, and that division is the whole
 *  technique.**  Every entry in the forward table carries a factor of `z` in its numerator
 *  (`z/(z-a)`, `z/(z-1)^2`, …), so decomposing `X(z)` directly yields terms `A/(z-r)` whose
 *  inverse is the awkward `r^(n-1)` shifted family.  Dividing by `z` first, decomposing, then
 *  multiplying each term back by `z` restores the `z/(z-r)^j` shape the table is written in,
 *  whose inverse is the clean binomial family below.
 *
 *  The basis is `z/(z-r)^j`, inverting to `C(n, j-1) * r^(n-j+1)`:
 *  - `j = 1` gives `r^n`,
 *  - `j = 2` gives `n * r^(n-1)`,
 *  - `j = 3` gives `n*(n-1)/2 * r^(n-2)`, and so on.
 *
 *  The binomial coefficient is emitted as the `binom` node from issue 6.28 rather than
 *  expanded into a polynomial in `n`, so a repeated pole of any order needs no special case.
 *
 *  **Complex poles are declined, not approximated.**  A conjugate pair inverts to a damped
 *  oscillation `r^n * cos(w*n + phi)`, which this tier does not build; such an `X(z)` stays
 *  symbolic rather than being returned in a complex-exponential form the rest of the library
 *  would not simplify.  That is the same boundary `inverseLaplaceOf` draws at a repeated
 *  irreducible quadratic, and it is recorded as the natural next slice.
 */

/** Recovers `x[n]` from `f = X(z)`.
 *
 *  @param f the z-domain expression
 *  @param z the z-domain variable
 *  @param n the discrete index variable
 *  @return the sequence in terms of `n`, or [[_InverseZTransform]]`(f, z, n)` if no rule applies
 */
def inverseZTransformOf(f: _Expression, z: _Variable, n: _Variable): _Expression =
  invZImpl(f, z, n)

private def invZImpl(f: _Expression, z: _Variable, n: _Variable): _Expression =
  // Linearity first, then the general rational path.  The split is for the *output*: a sum
  // inverted term by term reads as a sum of named sequences, where folding it into one
  // rational first and decomposing that would give the same values through a longer route.
  // It falls through rather than failing, because a term that is not invertible alone may
  // still be part of a whole that is.
  val byParts = f match
    case Sum(a, b) =>
      val (ia, ib) = (invZImpl(a, z, n), invZImpl(b, z, n))
      Option.when(!ia.isInstanceOf[_InverseZTransform] && !ib.isInstanceOf[_InverseZTransform])(Sum(ia, ib))
    case Product(c, g) if !dependsOn(c, z) =>
      val ig = invZImpl(g, z, n)
      Option.when(!ig.isInstanceOf[_InverseZTransform])(Product(c, ig))
    case Product(g, c) if !dependsOn(c, z) =>
      val ig = invZImpl(g, z, n)
      Option.when(!ig.isInstanceOf[_InverseZTransform])(Product(c, ig))
    case _ => None

  byParts.orElse(invRationalZ(f, z, n)).getOrElse(_InverseZTransform(f, z, n))

/** Inverts a rational `X(z)` by decomposing `X(z)/z` over real poles.
 *
 *  **The input is normalised into coefficient vectors rather than pattern-matched as a
 *  `Ratio`**, because the forward transform does not produce one: `Z{n}` comes back as
 *  `(-1*z) * ((…)/(z-1)^2)` from the multiply-by-`n` rule, a `Product` whose left factor
 *  depends on `z`.  Matching on `Ratio` inverted the table's own output for some entries and
 *  not others, which is exactly the kind of gap a round-trip test exists to catch.
 *
 *  The normalisation itself is `scalar.rationalCoeffs`; it began here and moved when 6.29
 *  became its third caller, the point at which issues 3.1 and 2.5 each promoted a helper
 *  rather than let a copy diverge.
 */
private def invRationalZ(f: _Expression, z: _Variable, n: _Variable): Option[_Expression] =
  for
    (ns, ds) <- rationalCoeffs(f, z)
    // Decompose X(z)/z: the denominator gains a factor of z, i.e. a root at the origin.
    shifted = Vector(0.0) ++ polyTrim(ds)
    if polyTrim(ns).size <= shifted.size - 1     // strictly proper after the division by z
    terms <- decomposeReal(polyTrim(ns), shifted)
    result <- combineTerms(terms, z, n)
  yield simplifyFully(result)

/** A partial-fraction term `coeff / (v - root)^power`. */
private case class ZTerm(coeff: Double, root: Double, power: Int)

/** Decomposes `ns/ds` into real partial fractions, or `None` if any pole is complex.
 *
 *  Multiplicities come from [[squareFreeFactors]] rather than from counting roots, for the
 *  reason issue 2.5 records: the QR iteration does not merely scatter a repeated root, it can
 *  fail to converge on one outright, so the polynomial is split by multiplicity arithmetically
 *  and only the square-free parts — simple, well-conditioned roots — reach a root finder.
 */
private def decomposeReal(ns: Vector[Double], ds: Vector[Double]): Option[Vector[ZTerm]] =
  val degree = polyDegree(ds)

  // (root, multiplicity) for every pole, refusing the moment one is not real.
  def polesOf: Option[Vector[(Double, Int)]] =
    squareFreeFactors(ds).foldLeft(Option(Vector.empty[(Double, Int)])) {
      case (None, _) => None
      case (Some(acc), (factor, mult)) =>
        rootsOfSquareFree(factor) match
          case Some(rs) if rs.forall((_, im) => math.abs(im) < 1e-9) =>
            Some(acc ++ rs.map((re, _) => (re, mult)))
          case _ => None
    }

  if degree < 1 then None
  else polesOf.flatMap { ps =>
    // One basis function per (pole, power): 1/(v - r)^j for j = 1..multiplicity. Over the
    // common denominator each contributes the polynomial ds / (v - r)^j, so the columns of
    // the system are those quotients and the right-hand side is ns.
    val basis = ps.flatMap((r, m) => (1 to m).map(j => (r, j)))
    if basis.size != degree then None
    else
      val columns = basis.map { (r, j) =>
        val factor = (1 to j).foldLeft(Vector(1.0))((acc, _) => polyMul(acc, Vector(-r, 1.0)))
        val (q, rem) = polyDivide(ds, factor)
        if polyTrim(rem).exists(math.abs(_) > 1e-9) then Vector.empty[Double] else q
      }
      if columns.exists(_.isEmpty) then None
      else
        // Row i is the coefficient of v^i across every column; solve for the numerators.
        val a = Array.tabulate(degree, degree)((i, j) => columns(j).lift(i).getOrElse(0.0))
        val b = Vector.tabulate(degree)(i => ns.lift(i).getOrElse(0.0))
        solveDense(a, b).map(xs => basis.zip(xs).map((rj, c) => ZTerm(c, rj._1, rj._2)))
  }

/** Solves the dense system `a * x = b` through the `core` inverse kernel; `None` if singular. */
private def solveDense(a: Array[Array[Double]], b: Vector[Double]): Option[Vector[Double]] =
  val size = b.size
  val m    = _MatrixValue(size, size, a.flatten)
  m.inverse.map { inv =>
    Vector.tabulate(size)(i => (0 until size).map(k => inv(i, k) * b(k)).sum)
  }

/** Rebuilds the sequence from its partial-fraction terms.
 *
 *  Each `coeff/(z-r)^j`, multiplied back by the `z` divided out at the start, inverts to
 *  `coeff * C(n, j-1) * r^(n-j+1)`.  A pole at the origin (`r = 0`) is the impulse family and
 *  is declined: it would need the Kronecker delta the library has no node for.
 */
private def combineTerms(terms: Vector[ZTerm], z: _Variable, n: _Variable): Option[_Expression] =
  val nonZero = terms.filter(t => math.abs(t.coeff) > 1e-12)
  if nonZero.exists(t => math.abs(t.root) < 1e-12) then None
  else if nonZero.isEmpty then Some(_Number(0))
  else Some(nonZero.map(termToSequence(_, n)).reduce(Sum.apply))

/** `coeff * C(n, j-1) * r^(n-j+1)` for one term, with the trivial factors folded away. */
private def termToSequence(t: ZTerm, n: _Variable): _Expression =
  val exponent =
    if t.power == 1 then n
    else Sum(n, _Number(-(t.power - 1)))
  val geometric = Power(_Number(t.root), exponent)
  val withBinom =
    if t.power == 1 then geometric
    else Product(Binom(n, _Number(t.power - 1)), geometric)
  if math.abs(t.coeff - 1.0) < 1e-12 then withBinom
  else Product(_Number(t.coeff), withBinom)

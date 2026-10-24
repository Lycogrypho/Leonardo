package it.grypho.scala.leonardo
package matrix

import core.*
import scalar.{QPoly, qSquareFree, qRationalRoots}
import spire.math.Rational as Q


/** The exact Jordan form of a rational matrix whose eigenvalues are all rational (F_0176).
 *
 *  In floating point the Jordan form is not computable: a defective eigenvalue leaves the QR
 *  iteration split by about `√ε`, and an arbitrarily small perturbation turns a block into
 *  distinct eigenvalues, so `J` would be a discontinuous function of rounded input.  Over the
 *  rationals every step is a decision without a tolerance:
 *
 *  1. the characteristic polynomial by Faddeev–LeVerrier;
 *  2. its eigenvalues with their multiplicities, through F_0171's square-free factorisation and
 *     verified rational roots, **declining unless they account for the whole degree**, so an
 *     irrational or complex eigenvalue leaves the caller's numeric path in charge;
 *  3. for each eigenvalue `λ`, the null spaces of `Nᵏ = (A - λI)ᵏ`: their dimensions give how many
 *     blocks have size at least `k`, and the chains are chosen largest first, each new head
 *     independent of `ker Nˢ⁻¹` and of the chains already taken at that level;
 *  4. a **certificate**: `AP = PJ` exactly and `P` invertible, or no answer.
 *
 *  **Convention, stated because the literature varies**: eigenvalues ascending, the blocks of one
 *  eigenvalue by decreasing size, the `1`s above the diagonal, so a block's columns are
 *  `Nˢ⁻¹v, …, Nv, v`.
 */

// QMat, qIdentity and qMatMul are the shared kernels of RationalMatrix.scala (F_0213).

private def qMatVec(a: QMat, v: Vector[Q]): Vector[Q] =
  a.map(row => row.indices.foldLeft(Q.zero)((s, k) => s + row(k) * v(k)))

private def qShift(a: QMat, l: Q): QMat =
  Vector.tabulate(a.size, a.size)((i, j) => if i == j then a(i)(j) - l else a(i)(j))

/** The characteristic polynomial `det(λI - A)`, monic, by Faddeev–LeVerrier: `M₁ = I`,
 *  `cₙ₋ₖ = -tr(A·Mₖ)/k`, `Mₖ₊₁ = A·Mₖ + cₙ₋ₖ·I`.  Exact, so the division by `k` costs nothing. */
private def charPoly(a: QMat): QPoly =
  val n = a.size
  val coeffs = Array.fill(n + 1)(Q.zero)
  coeffs(n) = Q.one
  var mk = qIdentity(n)
  for k <- 1 to n do
    val am = qMatMul(a, mk)
    val c  = -(am.indices.foldLeft(Q.zero)((s, i) => s + am(i)(i))) / Q(k)
    coeffs(n - k) = c
    mk = Vector.tabulate(n, n)((i, j) => if i == j then am(i)(j) + c else am(i)(j))
  coeffs.toVector

/** The rank of `m` and a basis of its null space, by reduction to row echelon form. */
private def rankAndNull(m: QMat): (Int, List[Vector[Q]]) =
  val rows = m.size
  val cols = if rows == 0 then 0 else m.head.size
  val a    = m.map(_.toArray).toArray
  val pivots = scala.collection.mutable.ArrayBuffer[Int]()
  var r = 0
  for c <- 0 until cols if r < rows do
    (r until rows).find(i => !a(i)(c).isZero).foreach { p =>
      val t = a(r); a(r) = a(p); a(p) = t
      val inv = Q.one / a(r)(c)
      for k <- 0 until cols do a(r)(k) = a(r)(k) * inv
      for i <- 0 until rows if i != r && !a(i)(c).isZero do
        val f = a(i)(c)
        for k <- 0 until cols do a(i)(k) = a(i)(k) - f * a(r)(k)
      pivots += c
      r += 1
    }
  val free = (0 until cols).filterNot(pivots.contains)
  val basis = free.toList.map { f =>
    val v = Array.fill(cols)(Q.zero)
    v(f) = Q.one
    pivots.zipWithIndex.foreach((pc, row) => v(pc) = -a(row)(f))
    v.toVector
  }
  (pivots.size, basis)

/** The rank of a set of vectors, as the rank of the matrix having them as rows. */
private def qRank(vs: List[Vector[Q]]): Int = if vs.isEmpty then 0 else rankAndNull(vs.toVector)._1

/** The rational eigenvalues of `a` with their algebraic multiplicities, ascending, or `None` when
 *  they do not account for the whole degree (an irrational or complex eigenvalue). */
private def rationalSpectrum(a: QMat): Option[List[(Q, Int)]] =
  val found = qSquareFree(charPoly(a)).flatMap((factor, mult) => qRationalRoots(factor).map(r => (r, mult)))
  Option.when(found.map(_._2).sum == a.size)(found.sortBy(_._1))

/** The Jordan chains of one eigenvalue, largest block first, each chain's columns `Nˢ⁻¹v, …, v`. */
private def chainsOf(a: QMat, l: Q, mult: Int): Option[List[List[Vector[Q]]]] =
  val n   = a.size
  val nl  = qShift(a, l)
  // kernels(k) = a basis of ker Nᵏ, for k = 0 … s, s the first power whose kernel has dimension mult
  val kernels = Iterator.iterate(qIdentity(n))(p => qMatMul(nl, p)).map(p => rankAndNull(p)._2)
    .take(mult + 1).toVector
  val top = kernels.indexWhere(_.size == mult)
  if top < 0 then None
  else
    val d = kernels.take(top + 1).map(_.size)                // d(k) = dim ker Nᵏ
    def atLeast(k: Int): Int = if k > top then 0 else d(k) - d(k - 1)   // blocks of size ≥ k
    var chains = List.empty[(Vector[Q], Int)]               // (head, size), largest first
    var ok = true
    for s <- top to 1 by -1 if ok do
      val needed = atLeast(s) - atLeast(s + 1)
      def image(v: Vector[Q], times: Int): Vector[Q] = (1 to times).foldLeft(v)((w, _) => qMatVec(nl, w))
      var base = kernels(s - 1) ++ chains.map((w, t) => image(w, t - s))
      var taken = 0
      for v <- kernels(s) if taken < needed do
        if qRank(v :: base) > qRank(base) then
          base = v :: base
          chains = chains :+ ((v, s))
          taken += 1
      if taken < needed then ok = false
    Option.when(ok)(chains.map((v, s) => (s - 1 to 0 by -1).toList.map(k =>
      (1 to k).foldLeft(v)((w, _) => qMatVec(nl, w)))))

/** `(P, J)` with `A = P·J·P⁻¹` for a rational matrix whose eigenvalues are rational, certified;
 *  `None` otherwise or past `maxOrder` rows.
 *
 *  @param cells    the matrix, row-major, every cell exact
 *  @param n        its order
 *  @param maxOrder the largest order attempted, `Limit.MaxJordanOrder`
 *  @return the two matrices, row-major, or `None`
 */
private[leonardo] def exactJordan(cells: Vector[_Rational], n: Int, maxOrder: Int): Option[(Vector[_Rational], Vector[_Rational])] =
  if n < 1 || n > maxOrder || cells.size != n * n then None
  else
    val a: QMat = Vector.tabulate(n, n)((i, j) => Q(cells(i * n + j).num, cells(i * n + j).den))
    for
      spectrum <- rationalSpectrum(a)
      blocks   <- spectrum.foldLeft(Option(List.empty[(Q, List[Vector[Q]])])) { (acc, lm) =>
                    acc.flatMap(sofar => chainsOf(a, lm._1, lm._2).map(cs => sofar ++ cs.map(c => (lm._1, c)))) }
      columns   = blocks.flatMap(_._2)
      if columns.size == n
      p         = Vector.tabulate(n, n)((i, j) => columns(j)(i))
      starts    = blocks.scanLeft(0)((s, b) => s + b._2.size)
      j         = Vector.tabulate(n, n) { (r, c) =>
                    if r == c then blocks(blockOf(starts, r))._1
                    else if c == r + 1 && blockOf(starts, r) == blockOf(starts, c) then Q.one
                    else Q.zero
                  }
      if qMatMul(a, p) == qMatMul(p, j) && rankAndNull(p)._1 == n
    yield (p.flatten.map(toRational), j.flatten.map(toRational))

/** The index of the block row `r` lies in. */
private def blockOf(starts: List[Int], r: Int): Int = starts.lastIndexWhere(_ <= r)

private def toRational(q: Q): _Rational =
  _Rational.of(q.numerator.toBigInt, q.denominator.toBigInt).getOrElse(_Rational(0))

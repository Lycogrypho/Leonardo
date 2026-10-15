package it.grypho.scala.leonardo
package optimize

import core.*
import matrix.{numericalRank, rankTolerance}


/** The tight stopping target on the certificate's cosine, far below any precision's (F_0063):
 *  the iteration aims well past the contract, and the certificate then decides. */
private val LsqTarget: Double = 1e-10

/** How many rounding units of the terms a residual cancels an exact fit may leave.  At an exact
 *  fit the residual is rounding noise and its cosine with a column is noise too, so the cosine
 *  cannot certify it; a residual this small relative to its own terms is a fit to rounding. */
private val ExactFitUlps: Double = 100.0

/** The damping past which a step no longer moves the point: the iteration has stalled.
 *  Dimensionless, since the columns are scaled to unit norm. */
private val MaxDamping: Double = 1e16

private val Eps: Double = math.ulp(1.0)

private def sumSq(a: Array[Double]): Double =
  var s = 0.0
  var i = 0
  while i < a.length do
    s += a(i) * a(i)
    i += 1
  s

/** `Jᵀr` for a row-major `m×n` `J`. */
private def jtr(j: Array[Double], r: Array[Double], m: Int, n: Int): Array[Double] =
  val g = new Array[Double](n)
  var k = 0
  while k < m do
    var i = 0
    while i < n do
      g(i) += j(k * n + i) * r(k)
      i += 1
    k += 1
  g

/** `J·s`. */
private def jtimes(j: Array[Double], s: Array[Double], m: Int, n: Int): Array[Double] =
  Array.tabulate(m) { k =>
    var acc = 0.0
    var i = 0
    while i < n do
      acc += j(k * n + i) * s(i)
      i += 1
    acc
  }

/** The Euclidean norm of each column of `J`. */
private def columnNorms(j: Array[Double], m: Int, n: Int): Array[Double] =
  val c = new Array[Double](n)
  var k = 0
  while k < m do
    var i = 0
    while i < n do
      c(i) += j(k * n + i) * j(k * n + i)
      i += 1
    k += 1
  c.map(math.sqrt)


/** Levenberg–Marquardt with Moré's scaling, Nielsen's damping and a projected active set, then
 *  the certificate and the identifiability check (see [[lsqfit]]). */
private def levenbergMarquardt(res: Residual, p0: Vector[Double], box: (Vector[Double], Vector[Double]),
                               env: Environment): Either[LsqFailure, LsqResult] =
  val (m, n)   = (res.m, res.n)
  val (lo, hi) = (box._1.toArray, box._2.toArray)
  val tol      = 0.5 * math.pow(10, -env.precision)

  def clamp(p: Array[Double]): Array[Double] = Array.tabulate(n)(i => math.min(hi(i), math.max(lo(i), p(i))))
  def isActive(p: Array[Double], g: Array[Double], i: Int): Boolean =
    (p(i) <= lo(i) && g(i) > 0) || (p(i) >= hi(i) && g(i) < 0)
  def freeOf(p: Array[Double], g: Array[Double]): Array[Int] = (0 until n).filterNot(isActive(p, g, _)).toArray

  /** The certificate's measure: the largest cosine between a free column and the residual. */
  def cosine(j: Array[Double], r: Array[Double], g: Array[Double], free: Array[Int]): Double =
    val rn = math.sqrt(sumSq(r))
    val cn = columnNorms(j, m, n)
    free.foldLeft(0.0) { (acc, i) =>
      val c = if g(i) == 0.0 then 0.0
              else if cn(i) == 0.0 || rn == 0.0 then Double.PositiveInfinity
              else math.abs(g(i)) / (cn(i) * rn)
      math.max(acc, c)
    }

  def exactFit(p: Array[Double], r: Array[Double]): Boolean =
    res.scale(p).exists(s => math.sqrt(sumSq(r)) <= ExactFitUlps * Eps * math.sqrt(sumSq(s)))

  /** `σ²·(RᵀR)⁻¹` from the QR of `J` — never `(JᵀJ)⁻¹`. */
  def covariance(j: Array[Double], r: Array[Double]): Option[_MatrixValue] =
    if m <= n then None
    else _MatrixValue(m, n, j).qrDecompose.map { (_, rr) =>
      val rv  = rr.toVector
      val inv = new Array[Double](n * n)
      for col <- 0 until n; i <- (0 until n).reverse do
        val s = (i + 1 until n).foldLeft(if i == col then 1.0 else 0.0)((acc, k) => acc - rv(i * n + k) * inv(k * n + col))
        inv(i * n + col) = s / rv(i * n + i)
      val sigma2 = sumSq(r) / (m - n)
      _MatrixValue(n, n, Array.tabulate(n * n) { idx =>
        val (a, b) = (idx / n, idx % n)
        sigma2 * (0 until n).map(k => inv(a * n + k) * inv(b * n + k)).sum
      })
    }

  /** Identifiability, then the certificate (Decisions A and B). */
  def finish(p: Array[Double], r: Array[Double], j: Array[Double], k: Int,
             atCap: Boolean): Either[LsqFailure, LsqResult] =
    val g    = jtr(j, r, m, n)
    val free = freeOf(p, g)
    val c    = cosine(j, r, g, free)
    val nf   = free.length
    // The rank of the column-NORMALISED Jacobian: identifiability does not depend on the units of
    // a parameter, and the raw J's singular values spread with them -- `a` in units of 1e6 put
    // twelve orders between the two and declined a perfectly determined fit (the units test).
    val cn   = columnNorms(j, m, n)
    val rank =
      if nf == 0 then Some(0)
      else _MatrixValue(m, nf, Array.tabulate(m * nf) { i =>
             val c = free(i % nf)
             if cn(c) == 0.0 then 0.0 else j((i / nf) * n + c) / cn(c)
           }).svd
             .map((_, sigma, _) => numericalRank(sigma, rankTolerance(env)))
    rank match
      case Some(q) if q < nf => Left(LsqFailure.NotIdentifiable(q, nf))
      case Some(_) if c <= tol || exactFit(p, r) =>
        val active = (0 until n).filterNot(free.contains).toVector
        Right(LsqResult(p.toVector, math.sqrt(sumSq(r)), k,
                        if active.isEmpty then covariance(j, r) else None, active))
      case _ => Left(if atCap then LsqFailure.MaxIterations(env.limits.count(Limit.MaxLsqIterations), c) else LsqFailure.Stalled(c))

  /** The scaled, damped step over the free parameters: QR of `[J_F·D⁻¹; √λ·I]` against
   *  `[−r; 0]`, never the normal equations. */
  def step(j: Array[Double], r: Array[Double], d: Array[Double], free: Array[Int],
           lambda: Double): Option[Array[Double]] =
    val nf = free.length
    val a  = new Array[Double]((m + nf) * nf)
    for k <- 0 until m; c <- 0 until nf do a(k * nf + c) = j(k * n + free(c)) / d(free(c))
    for c <- 0 until nf do a((m + c) * nf + c) = math.sqrt(lambda)
    _MatrixValue(m + nf, nf, a).qrDecompose.map { (q, rr) =>
      val (qv, rv) = (q.toVector, rr.toVector)
      val qtb = Array.tabulate(nf)(c => (0 until m).map(k => -qv(k * nf + c) * r(k)).sum)
      val e   = new Array[Double](nf)
      for i <- (0 until nf).reverse do
        e(i) = (qtb(i) - (i + 1 until nf).map(c => rv(i * nf + c) * e(c)).sum) / rv(i * nf + i)
      val delta = new Array[Double](n)
      for c <- 0 until nf do delta(free(c)) = e(c) / d(free(c))
      delta
    }

  @annotation.tailrec
  def loop(p: Array[Double], r: Array[Double], j: Array[Double], d: Array[Double],
           lambda: Double, nu: Double, k: Int): Either[LsqFailure, LsqResult] =
    val g    = jtr(j, r, m, n)
    val free = freeOf(p, g)
    if free.isEmpty || cosine(j, r, g, free) <= LsqTarget || exactFit(p, r) then finish(p, r, j, k, atCap = false)
    else if k >= env.limits.count(Limit.MaxLsqIterations) then finish(p, r, j, k, atCap = true)
    else if lambda > MaxDamping then finish(p, r, j, k, atCap = false)
    else
      val f = 0.5 * sumSq(r)
      val trial = step(j, r, d, free, lambda).flatMap { delta =>
        val pn   = clamp(Array.tabulate(n)(i => p(i) + delta(i)))
        val s    = Array.tabulate(n)(i => pn(i) - p(i))
        val pred = -(0 until n).map(i => g(i) * s(i)).sum - 0.5 * sumSq(jtimes(j, s, m, n))
        for
          rn <- res.values(pn)
          actual = f - 0.5 * sumSq(rn)
          if pred > 0 && actual > 0
          jn <- res.jacobian(pn)
        yield (pn, rn, jn, actual / pred, actual)
      }
      trial match
        case None => loop(p, r, j, d, lambda * nu, nu * 2, k + 1)
        case Some((pn, rn, jn, rho, actual)) =>
          if actual <= 4 * Eps * f then finish(pn, rn, jn, k + 1, atCap = false)
          else
            val cn = columnNorms(jn, m, n)
            val dn = Array.tabulate(n)(i => math.max(d(i), cn(i)))
            loop(pn, rn, jn, dn, lambda * math.max(1.0 / 3, 1 - math.pow(2 * rho - 1, 3)), 2.0, k + 1)

  if m.toLong * n * n > env.limits(Limit.MaxLsqWork) then Left(LsqFailure.TooLarge(m, n))
  else
    val p = clamp(p0.toArray)
    (for r <- res.values(p); j <- res.jacobian(p) yield (r, j)) match
      case None         => Left(LsqFailure.NotEvaluable)
      case Some((r, j)) =>
        // Moré: a column that starts at zero norm is scaled by 1 until it grows.
        val d = columnNorms(j, m, n).map(c => if c > 0 then c else 1.0)
        loop(p, r, j, d, 1e-3, 2.0, 0)

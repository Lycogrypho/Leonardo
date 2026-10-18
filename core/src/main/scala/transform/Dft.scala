package it.grypho.scala.leonardo
package transform

import core.*
import matrix._Matrix


/** Discrete-time Fourier analysis (F_0137): the discrete Fourier transform by the direct sum, its
 *  inverse, and discrete linear convolution.
 *
 *  **This file is why `transform` imports `matrix`** (the edge is new with F_0137 and acyclic:
 *  `matrix` imports nothing from here).  A transform of complex values has complex cells, which
 *  only the symbolic `_Matrix` carrier holds.
 *
 *  **The direct sum, not an FFT.**  The transforms a course or a lesson computes are short, the
 *  sum is its own definition — nothing to get subtly wrong — and its cost is bounded by
 *  `Limit.MaxDftLength`.  The twiddle factors are tabulated once per length from the angle
 *  `2πj/N`, and a term reads entry `k·n mod N`, so no cosine is ever taken of a huge argument.
 */

/** A vector operand: its shape and its cells as `(re, im)`, or `None` when it is not a row or
 *  column of numbers.  An exact cell is read as the number it holds. */
private def vectorOf(r: Either[_Expression, _Value]): Option[((Int, Int), Vector[_Value])] = r match
  case Right(m: _MatrixValue) if m.rows == 1 || m.cols == 1 => Some(((m.rows, m.cols), m.toVector.map(_Number(_))))
  case Left(m: _Matrix) if m.rows == 1 || m.cols == 1 =>
    val vs = m.elems.collect { case v: _Value if _Complex.parts(v).isDefined => v }
    Option.when(vs.size == m.elems.size)(((m.rows, m.cols), vs))
  case _ => None

/** The discrete Fourier transform `Xₖ = Σₙ xₙ·e^(−2πikn/N)`, or with `inverse` the inverse
 *  `xₙ = (1/N)·Σₖ Xₖ·e^(2πikn/N)`.
 *
 *  @param xs      the samples, as `(re, im)`
 *  @param inverse whether to invert
 *  @return the transform, the same length
 */
def dft(xs: Vector[(Double, Double)], inverse: Boolean): Vector[(Double, Double)] =
  val n    = xs.size
  val sign = if inverse then 1.0 else -1.0
  val cosT = Array.tabulate(n)(j => math.cos(2 * math.Pi * j / n))
  val sinT = Array.tabulate(n)(j => sign * math.sin(2 * math.Pi * j / n))
  val (re, im) = (xs.map(_._1).toArray, xs.map(_._2).toArray)
  Vector.tabulate(n) { k =>
    var (sr, si) = (0.0, 0.0)
    var t = 0
    while t < n do
      val j = ((k.toLong * t) % n).toInt
      sr += re(t) * cosT(j) - im(t) * sinT(j)
      si += re(t) * sinT(j) + im(t) * cosT(j)
      t += 1
    if inverse then (sr / n, si / n) else (sr, si)
  }

/** `dft(x)` / `idft(X)` (F_0137): the transform of a row or column of real or complex values,
 *  the shape kept.  An exact operand is demoted — the twiddle factors are irrational — and a
 *  length past the environment's `Limit.MaxDftLength` stays unevaluated.
 *
 *  @param x       the samples
 *  @param inverse whether this is `idft`
 */
case class _Dft(x: _Expression, inverse: Boolean) extends _Expression:
  override def toString: String = s"${if inverse then "idft" else "dft"}($x)"
  override def children: List[_Expression] = List(x)
  override def rebuild(c: List[_Expression]): _Expression = _Dft(c.head, inverse)

  override def eval(env: Environment): Either[_Expression, _Value] =
    val r = x.eval(env)
    vectorOf(r) match
      case Some(((rows, cols), vs)) if vs.size <= env.limits(Limit.MaxDftLength) =>
        val out = dft(vs.flatMap(_Complex.parts), inverse)
        _Matrix(rows, cols, out.map((a, b) => _Complex.of(a, b): _Expression)).eval(env)
      // A refused numeric vector echoes the call as written: spelling out a vector past the
      // budget cell by cell would bury the note that says why (the svd precedent, F_0144).
      case Some(_) => Left(this)
      case None    => Left(_Dft(r.toExpression, inverse))

  /** Why this transform stays unevaluated, when its operand is a vector of numbers (F_0144).
   *  @param env the evaluation environment
   *  @return the reason, or `None`
   */
  def declineReason(env: Environment): Option[String] = dftDecline(x.eval(env), env)


/** Discrete linear convolution `(x ∗ h)ₖ = Σⱼ xⱼ·hₖ₋ⱼ`, length `n + m − 1` (F_0137).
 *
 *  **Exact on exact input**, since it is only products and sums: two all-rational operands give
 *  rational cells; anything else is computed on `(re, im)` pairs.
 *
 *  @param xs  the first sequence
 *  @param hs  the second
 *  @param env supplies the reduction policy for the exact path
 *  @return the convolution's cells
 */
def convolve(xs: Vector[_Value], hs: Vector[_Value], env: Environment): Vector[_Value] =
  val len = xs.size + hs.size - 1
  if (xs ++ hs).forall(_.isInstanceOf[_Rational]) then
    val (rx, rh, p) = (xs.collect { case r: _Rational => r }, hs.collect { case r: _Rational => r }, env.rationalPolicy)
    Vector.tabulate(len) { k =>
      (math.max(0, k - rh.size + 1) to math.min(k, rx.size - 1)).map(j => rx(j).multiply(rh(k - j), p))
        .reduceLeft((a, b) => a.add(b, p))
    }
  else
    // A while loop over arrays: the boxed pair per term this replaced cost several times the
    // arithmetic (the QP measurement, F_0053), and the work here is n·m (F_0144).
    val (cx, ch) = (xs.flatMap(_Complex.parts), hs.flatMap(_Complex.parts))
    val (xr, xi) = (cx.map(_._1).toArray, cx.map(_._2).toArray)
    val (hr, hi) = (ch.map(_._1).toArray, ch.map(_._2).toArray)
    Vector.tabulate(len) { k =>
      var (sr, si) = (0.0, 0.0)
      var j = math.max(0, k - hr.length + 1)
      val last = math.min(k, xr.length - 1)
      while j <= last do
        val i = k - j
        sr += xr(j) * hr(i) - xi(j) * hi(i)
        si += xr(j) * hi(i) + xi(j) * hr(i)
        j += 1
      _Complex.of(sr, si)
    }

/** Why a `dft`/`idft` of `r` declines, when `r` is a vector at all (F_0144).
 *
 *  @param r   the evaluated operand
 *  @param env supplies the length budget
 *  @return the reason, or `None` when it answers or is not a vector of numbers
 */
private def dftDecline(r: Either[_Expression, _Value], env: Environment): Option[String] =
  vectorOf(r).map(_._2.size).filter(_ > env.limits(Limit.MaxDftLength)).map(n =>
    s"a length of $n is past the direct sum's budget of ${env.limits(Limit.MaxDftLength)} (Limit.MaxDftLength)")

/** `conv(x, h)` (F_0137): [[convolve]] at the prompt, a row when `x` is a row, else a column.
 *
 *  **Its budget is the work, not only the output** (F_0144): the output's length is `n + m − 1`
 *  but the products are `n·m`, so two columns of `5·10⁵` passed an output cap and asked for
 *  `2.5·10¹¹` products — an uninterruptible browser tab.  A floating-point convolution is held to
 *  `Limit.MaxConvWork` products and an exact one to the far smaller `Limit.MaxConvExactWork`.
 *
 *  @param x the first sequence
 *  @param h the second
 */
case class _Conv(x: _Expression, h: _Expression) extends _Expression:
  override def toString: String = s"conv($x, $h)"
  override def children: List[_Expression] = List(x, h)
  override def rebuild(c: List[_Expression]): _Expression = _Conv(c.head, c(1))

  override def eval(env: Environment): Either[_Expression, _Value] =
    val (rx, rh) = (x.eval(env), h.eval(env))
    (vectorOf(rx), vectorOf(rh)) match
      case (Some(((rows, _), xs)), Some((_, hs))) if convDecline(xs, hs, env).isEmpty =>
        val out = convolve(xs, hs, env)
        (if rows == 1 then _Matrix(1, out.size, out) else _Matrix(out.size, 1, out)).eval(env)
      // Refused on its budget: the call as written, not two vectors spelt out (the svd precedent).
      case (Some(_), Some(_)) => Left(this)
      case _ => Left(_Conv(rx.toExpression, rh.toExpression))

  /** Why this convolution stays unevaluated, when both operands are vectors of numbers.
   *  @param env the evaluation environment
   *  @return the reason, or `None`
   */
  def declineReason(env: Environment): Option[String] =
    (vectorOf(x.eval(env)), vectorOf(h.eval(env))) match
      case (Some((_, xs)), Some((_, hs))) => convDecline(xs, hs, env)
      case _                              => None

/** Why a convolution of `xs` and `hs` declines: an empty operand, an output past
 *  `MaxMatrixCells`, or more products than the floating-point or exact budget allows. */
private def convDecline(xs: Vector[_Value], hs: Vector[_Value], env: Environment): Option[String] =
  val exact = (xs ++ hs).forall(_.isInstanceOf[_Rational])
  val (budget, name) = if exact then (Limit.MaxConvExactWork, "MaxConvExactWork") else (Limit.MaxConvWork, "MaxConvWork")
  val work = xs.size.toLong * hs.size
  if xs.isEmpty || hs.isEmpty then Some("an operand is empty")
  else if xs.size + hs.size - 1 > env.limits(Limit.MaxMatrixCells) then
    Some(s"the result's ${xs.size + hs.size - 1} cells are past Limit.MaxMatrixCells")
  else if work > env.limits(budget) then
    Some(s"${xs.size} × ${hs.size} = $work products are past the ${if exact then "exact " else ""}budget of ${env.limits(budget)} (Limit.$name)")
  else None

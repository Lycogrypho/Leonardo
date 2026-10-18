package it.grypho.scala.leonardo
package transform

import core.*
import scalar.*


/** AST node for the Laplace transform: `laplace(e, t, s)` in the grammar.
 *
 *  Computes `L{e(t)}` with Laplace variable `s` via the rule table in
 *  [[laplaceOf]].  The integration binder `t` is excluded from `children` per the
 *  `_Functional` convention; `s` names the output variable and appears free in the result.
 *
 *  `eval` applies [[laplaceOf]] and guards against the fixpoint (stays symbolic when
 *  the rule table cannot reduce the expression).
 *
 *  @param e the time-domain expression to transform
 *  @param t the time variable (binder -- excluded from `children`)
 *  @param s the complex frequency variable (free in the result)
 */
case class _Laplace(e: _Expression, t: _Variable, s: _Variable) extends _Functional:
  override def toString: String = s"laplace($e, $t, $s)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = _Laplace(c.head, t, s)
  override def eval(env: Environment): Either[_Expression, _Value] =
    val result = laplaceOf(e, t, s)
    if result == this then Left(this)
    else result.eval(env)


/** AST node for the two-sided Fourier transform: `fourier(e, t, w)` in the grammar.
 *
 *  Computes `F{e(t)} = ∫ e(t)·e^(−iwt) dt` over the whole line (F_0141; it was the one-sided
 *  transform, unchecked, until then) as the Laplace transforms of the causal and the reflected
 *  anti-causal part at `s = ±i·w`, and stays unevaluated unless both converge on the imaginary
 *  axis — see [[fourierOf]].  The binder `t` is excluded from `children`; `w` appears free in the
 *  result.
 *
 *  `eval` applies [[fourierOf]] and guards against the fixpoint.
 *
 *  @param e the time-domain expression to transform
 *  @param t the time variable (binder -- excluded from `children`)
 *  @param w the angular frequency variable (free in the result)
 */
case class _Fourier(e: _Expression, t: _Variable, w: _Variable) extends _Functional:
  override def toString: String = s"fourier($e, $t, $w)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = _Fourier(c.head, t, w)
  override def eval(env: Environment): Either[_Expression, _Value] =
    val result = fourierOf(e, t, w)
    if result == this then Left(this)
    else result.eval(env)

  /** Why this transform stays unevaluated, or `None` when it answers; see [[fourierDecline]].
   *  @param env the evaluation environment (unused: the transform reads no bindings)
   *  @return the reason, or `None`
   */
  def declineReason(env: Environment): Option[String] = fourierDecline(e, t, w, causal = false)


/** AST node for the causal Fourier transform: `fourierCausal(e, t, w)` in the grammar (F_0141).
 *
 *  The transform of `e(t)·u(t)`, `L{e}(i·w)`, which is what `fourier` answered before it became
 *  two-sided — now only when every pole of `L{e}` lies strictly left of the imaginary axis; see
 *  [[causalFourierOf]].  The binder `t` is excluded from `children`; `w` appears free in the
 *  result.
 *
 *  @param e the time-domain expression to transform
 *  @param t the time variable (binder -- excluded from `children`)
 *  @param w the angular frequency variable (free in the result)
 */
case class _FourierCausal(e: _Expression, t: _Variable, w: _Variable) extends _Functional:
  override def toString: String = s"fourierCausal($e, $t, $w)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = _FourierCausal(c.head, t, w)
  override def eval(env: Environment): Either[_Expression, _Value] =
    val result = causalFourierOf(e, t, w)
    if result == this then Left(this)
    else result.eval(env)

  /** Why this transform stays unevaluated, or `None` when it answers; see [[fourierDecline]].
   *  @param env the evaluation environment (unused: the transform reads no bindings)
   *  @return the reason, or `None`
   */
  def declineReason(env: Environment): Option[String] = fourierDecline(e, t, w, causal = true)


/** AST node for the z-transform: `ztrans(x, n, z)` in the grammar.
 *
 *  Computes the **one-sided** (unilateral) transform `X(z) = sum(k >= 0) x[k]*z^-k` via the
 *  rule table in [[zTransformOf]].  The summation binder `n` is excluded from `children` per
 *  the `_Functional` convention; `z` names the output variable and appears free in the result.
 *
 *  One-sided rather than bilateral by design, matching `laplace`, which is
 *  itself unilateral.  A bilateral transform would have to carry a **region of convergence**
 *  on every result — without one the same `X(z)` inverts to a causal or an anti-causal signal,
 *  so the inverse would have to guess — and that is deferred as future work.
 *
 *  `eval` applies [[zTransformOf]] and guards against the fixpoint (stays symbolic when the
 *  rule table cannot reduce the expression), the convention shared with [[_Laplace]].
 *
 *  @param x the sequence expression to transform, in terms of the index `n`
 *  @param n the discrete index variable (binder -- excluded from `children`)
 *  @param z the complex frequency variable (free in the result)
 */
case class _ZTransform(x: _Expression, n: _Variable, z: _Variable) extends _Functional:
  override def toString: String = s"ztrans($x, $n, $z)"
  override def children: List[_Expression] = List(x)
  override def rebuild(c: List[_Expression]): _Expression = _ZTransform(c.head, n, z)
  override def eval(env: Environment): Either[_Expression, _Value] =
    val result = zTransformOf(x, n, z)
    if result == this then Left(this)
    else result.eval(env)


/** AST node for the inverse z-transform: `invztrans(X, z, n)` in the grammar.
 *
 *  Recovers `x[n]` from a rational `X(z)` via the rule table in [[inverseZTransformOf]].
 *  The frequency binder `z` is excluded from `children` (dual of [[_ZTransform]]); `n` names
 *  the output index and appears free in the result.
 *
 *  `eval` applies [[inverseZTransformOf]] and guards against the fixpoint.
 *
 *  @param f the frequency-domain expression to invert
 *  @param z the z-domain variable (binder -- excluded from `children`)
 *  @param n the discrete index variable (free in the result)
 */
case class _InverseZTransform(f: _Expression, z: _Variable, n: _Variable) extends _Functional:
  override def toString: String = s"invztrans($f, $z, $n)"
  override def children: List[_Expression] = List(f)
  override def rebuild(c: List[_Expression]): _Expression = _InverseZTransform(c.head, z, n)
  override def eval(env: Environment): Either[_Expression, _Value] =
    val result = inverseZTransformOf(f, z, n)
    if result == this then Left(this)
    else result.eval(env)


/** AST node for the inverse Laplace transform: `invlaplace(f, s, t)` in the grammar.
 *
 *  Computes `L^-1{f(s)}` as a function of `t` via the rule table in
 *  [[inverseLaplaceOf]].  The frequency binder `s` is excluded from `children` (dual
 *  of [[_Laplace]]); `t` names the output variable and appears free in the result.
 *
 *  `eval` applies [[inverseLaplaceOf]] and guards against the fixpoint.
 *
 *  @param f the frequency-domain expression to invert
 *  @param s the Laplace frequency variable (binder -- excluded from `children`)
 *  @param t the time variable (free in the result)
 */
case class _InverseLaplace(f: _Expression, s: _Variable, t: _Variable) extends _Functional:
  override def toString: String = s"invlaplace($f, $s, $t)"
  override def children: List[_Expression] = List(f)
  override def rebuild(c: List[_Expression]): _Expression = _InverseLaplace(c.head, s, t)
  override def eval(env: Environment): Either[_Expression, _Value] =
    val result = inverseLaplaceOf(f, s, t)
    if result == this then Left(this)
    else result.eval(env)

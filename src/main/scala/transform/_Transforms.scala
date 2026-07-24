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


/** AST node for the Fourier transform: `fourier(e, t, w)` in the grammar.
 *
 *  Computes the unilateral Fourier transform `F{e(t)}` with frequency variable `w`
 *  via the Laplace-to-Fourier substitution `s -> i*w` (see [[fourierOf]]).
 *  Results are generally complex-valued.  The binder `t` is excluded from `children`;
 *  `w` appears free in the result.
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

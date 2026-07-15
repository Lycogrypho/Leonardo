package it.grypho.scala.leonardo
package transform

import core.*
import scalar.*


// L{e(t)} with Laplace variable s, computed by the rule table in LaplaceTransform.scala.
// children = List(e) only — t is the integration binder (excluded from children per the
// _Functional convention), s names the output variable and appears free in the result.
case class _Laplace(e: _Expression, t: _Variable, s: _Variable) extends _Functional:
  override def toString: String = s"laplace($e, $t, $s)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = _Laplace(c.head, t, s)
  override def eval(env: Environment): Either[_Expression, _Value] =
    val result = laplaceOf(e, t, s)
    if result == this then Left(this)
    else result.eval(env)


// F{e(t)} (unilateral Fourier transform) with frequency variable w, computed in
// FourierTransform.scala via the Laplace-to-Fourier substitution s → i·w.
// Result is generally complex-valued.
case class _Fourier(e: _Expression, t: _Variable, w: _Variable) extends _Functional:
  override def toString: String = s"fourier($e, $t, $w)"
  override def children: List[_Expression] = List(e)
  override def rebuild(c: List[_Expression]): _Expression = _Fourier(c.head, t, w)
  override def eval(env: Environment): Either[_Expression, _Value] =
    val result = fourierOf(e, t, w)
    if result == this then Left(this)
    else result.eval(env)

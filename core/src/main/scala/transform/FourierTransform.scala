package it.grypho.scala.leonardo
package transform

import core.*
import scalar.*


/** Unilateral Fourier transform via the Laplace-to-Fourier substitution.
 *
 *  The unilateral Fourier transform `F{f(t)} = integral_0^inf f(t) * exp(-i*w*t) dt`
 *  equals `L{f(t)}` evaluated at `s = i*w`.  [[fourierOf]] computes the Laplace
 *  transform first and then substitutes `s -> i*w`.
 *
 *  Returns [[_Fourier]]`(e, t, w)` unchanged when the Laplace transform cannot be
 *  computed.  Results are generally complex-valued (contain the imaginary unit `i`).
 */

/** Computes `F{e}` with time variable `t` and frequency variable `w`.
 *
 *  Picks a fresh internal name for the Laplace frequency variable that cannot collide
 *  with any free variable already present in `e` (or with the binders `t` and `w`).
 *  Returns [[_Fourier]]`(e, t, w)` when [[laplaceOf]] cannot reduce the expression.
 *
 *  @param e the time-domain expression to transform
 *  @param t the time variable
 *  @param w the angular frequency variable
 *  @return the Fourier transform of `e`, or [[_Fourier]]`(e, t, w)` if the Laplace
 *          transform is not computable
 */
def fourierOf(e: _Expression, t: _Variable, w: _Variable): _Expression =
  // Pick an internal name for the Laplace frequency variable that cannot collide with
  // any free variable already present in e (or with the user's t and w binders); the
  // shared capture-safe generator (scalar.freshVar) does the pigeonhole search.
  val reserved = e.freeVars + t.variable + w.variable
  val s        = freshVar(reserved, "__lts")
  val sName    = s.variable
  val L        = laplaceOf(e, t, s)
  L match
    case _: _Laplace => _Fourier(e, t, w)   // Laplace unknown -> stay symbolic
    case _ =>
      val iw = Product(_Complex.of(0, 1), w)
      substitute(L, Map(sName -> iw))

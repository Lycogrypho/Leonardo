package it.grypho.scala.leonardo
package transform

import core.*
import scalar.*


// Unilateral (one-sided) Fourier transform: F{f(t)} = integral_0^inf f(t) e^{-iwt} dt.
// This equals L{f(t)} evaluated at s = i*w, so we compute the Laplace transform first
// and substitute the imaginary frequency i*w for the internal Laplace variable.
//
// Returns _Fourier(e, t, w) unchanged when the Laplace transform cannot be computed.
// Results are generally complex-valued (contain the imaginary unit i).

private val LaplaceInternalVar = "__lt_s__"

def fourierOf(e: _Expression, t: _Variable, w: _Variable): _Expression =
  val s = _Variable(LaplaceInternalVar)
  val L = laplaceOf(e, t, s)
  L match
    case _: _Laplace => _Fourier(e, t, w)   // Laplace unknown → stay symbolic
    case _ =>
      val iw = Product(_Complex.of(0, 1), w)
      substitute(L, Map(LaplaceInternalVar -> iw))

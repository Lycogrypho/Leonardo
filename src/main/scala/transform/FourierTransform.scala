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

def fourierOf(e: _Expression, t: _Variable, w: _Variable): _Expression =
  // Pick an internal name for the Laplace frequency variable that cannot collide with
  // any free variable already present in e (or with the user's t and w binders).
  // Pigeonhole: among the |reserved|+1 candidates __lts0 … __lts|reserved| at least one
  // is free, so find always succeeds and the getOrElse default is unreachable.
  val reserved = e.freeVars + t.variable + w.variable
  val sName    = (0 to reserved.size).iterator.map(i => s"__lts$i")
    .find(!reserved.contains(_))
    .getOrElse(s"__lts${reserved.size + 1}")
  val s        = _Variable(sName)
  val L        = laplaceOf(e, t, s)
  L match
    case _: _Laplace => _Fourier(e, t, w)   // Laplace unknown → stay symbolic
    case _ =>
      val iw = Product(_Complex.of(0, 1), w)
      substitute(L, Map(sName -> iw))

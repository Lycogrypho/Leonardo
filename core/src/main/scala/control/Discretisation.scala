package it.grypho.scala.leonardo
package control

import core.*
import scalar.*


/** Continuous-to-discrete conversion (issue 6.29 slice 6, Decision C).
 *
 *  **The method is always a visible argument, never a hidden default.**  The same plant
 *  discretised by zero-order hold and by Tustin has *different* discrete poles, so a silent
 *  choice would make two correct-looking answers disagree with no way to see why.
 */
enum DiscretisationMethod:
  /** Zero-order hold — what a sampled-data control system physically does: the input is held
   *  constant between samples.  The default choice for a plant.
   */
  case Zoh

  /** Tustin, the bilinear transform `s = (2/Ts)*(z-1)/(z+1)` — what a filter designer wants.
   *  Maps the left half-plane onto the unit disc exactly, so stability is preserved.
   */
  case Tustin

export DiscretisationMethod.{Zoh, Tustin}

/** Discretises a continuous transfer function over sample period `ts`.
 *
 *  Tustin is a **substitution**: replace `s` by `(2/Ts)*(z-1)/(z+1)` and renormalise.  Zero-
 *  order hold is a **hold equivalence**, `G_d(z) = (1 - z^-1) * Z{L^-1[G(s)/s]}`, assembled
 *  from the inverse Laplace transform of 3.17 and the z-transform of 6.33 — which is the
 *  reason 6.33 had to be built first.
 *
 *  @param g      the continuous transfer function
 *  @param s      the continuous frequency variable
 *  @param z      the discrete variable to express the result in
 *  @param ts     the sample period; must be positive
 *  @param method [[Zoh]] or [[Tustin]]
 *  @return the discrete transfer function, or `None` when the conversion does not close
 */
def c2d(g: _Expression, s: _Variable, z: _Variable, ts: Double,
        method: DiscretisationMethod): Option[_Expression] =
  if ts <= 0 then None
  else method match
    case Tustin =>
      val sub = Ratio(Product(_Number(2 / ts), Sum(z, _Number(-1))), Sum(z, _Number(1)))
      Some(normaliseTf(substitute(g, Map(s.variable -> sub)), z))

    case Zoh =>
      // Step response in the time domain, sampled at n*Ts, transformed back, then the
      // (1 - z^-1) factor that turns the step's accumulation into the impulse response.
      val response = stepResponse(g, s, _Variable("__t"))
      if response.isInstanceOf[_StepResponse] then None
      else
        val sampled = substitute(response, Map("__t" -> Product(_Number(ts), _Variable("__n"))))
        val xz      = transform.zTransformOf(simplifyFully(sampled), _Variable("__n"), z)
        if xz.isInstanceOf[transform._ZTransform] then None
        else Some(normaliseTf(Product(Sum(_Number(1), Ratio(_Number(-1), z)), xz), z))

/** Inverse of [[c2d]]: recovers a continuous transfer function from a discrete one.
 *
 *  Only [[Tustin]] is invertible in closed form — the substitution `z = (2 + Ts*s)/(2 - Ts*s)`
 *  is its exact inverse, which is what makes `d2c(c2d(G))` return to `G`.  A zero-order-hold
 *  inverse would need a matrix logarithm and is declined rather than approximated.
 *
 *  @param g      the discrete transfer function
 *  @param z      the discrete variable
 *  @param s      the continuous variable to express the result in
 *  @param ts     the sample period
 *  @param method must be [[Tustin]]
 *  @return the continuous transfer function, or `None`
 */
def d2c(g: _Expression, z: _Variable, s: _Variable, ts: Double,
        method: DiscretisationMethod): Option[_Expression] =
  if ts <= 0 then None
  else method match
    case Tustin =>
      val sub = Ratio(Sum(_Number(2), Product(_Number(ts), s)),
                      Sum(_Number(2), Product(_Number(-ts), s)))
      Some(normaliseTf(substitute(g, Map(z.variable -> sub)), s))
    case Zoh => None

// The fold-back-into-one-rational helper lives in TransferFunction.scala as `normaliseTf`,
// shared by every slice: a substituted tree and an interconnected one need exactly the same
// treatment, and a second copy here would only be a way for the two to disagree about
// pole-zero cancellation.

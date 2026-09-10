package it.grypho.scala.leonardo
package transform

import core.*
import scalar.*


/** One-sided (unilateral) z-transform rule table — issue 6.33.
 *
 *  [[zTransformOf]] computes `X(z) = sum(k >= 0) x[k]*z^-k`, treating `n` as the discrete
 *  index and `z` as the complex frequency variable.  Returns [[_ZTransform]]`(x, n, z)`
 *  unchanged when no rule applies — the fixpoint / stay-symbolic convention shared with
 *  `laplaceOf`, `integrate` and `derive`.
 *
 *  Rules implemented:
 *  - **Constant**            `Z{c}         = c*z/(z-1)`   (any expr free of `n`)
 *  - **Linearity**           `Z{a + b}     = Z{a} + Z{b}`
 *                            `Z{c * x}     = c * Z{x}`    (`c` free of `n`)
 *  - **Unit step**           `Z{u[n]}      = z/(z-1)`
 *  - **Geometric**           `Z{a^n}       = z/(z-a)`     (`a` free of `n`)
 *  - **Exponential**         `Z{exp(c*n)}  = z/(z-e^c)`   (the geometric rule with `a = e^c`)
 *  - **Sine**                `Z{sin(w*n)}  = z*sin(w) / (z^2 - 2*z*cos(w) + 1)`
 *  - **Cosine**              `Z{cos(w*n)}  = z*(z-cos(w)) / (z^2 - 2*z*cos(w) + 1)`
 *  - **Multiply-by-n**       `Z{n * x[n]}  = -z * dX/dz`  (`X = Z{x}`; recursive)
 *
 *  **`Z{c} = c*z/(z-1)` is the entry most easily got wrong**, because the Laplace analogue
 *  `L{c} = c/s` does not carry over: a constant *sequence* is `c` at every index, so its
 *  transform is `c` times the transform of the unit step, not `c` divided by the variable.
 *
 *  **The multiply-by-`n` rule is why the table is short.**  `Z{n}`, `Z{n^2}` and `Z{n*a^n}`
 *  are not entries — they fall out of `Z{n*x[n]} = -z*dX/dz` applied to the unit step and the
 *  geometric rule, so there is one definition of that family rather than four that could
 *  disagree.  It is the exact counterpart of `laplaceOf`'s derivative-of-transform rule.
 *
 *  **The Kronecker delta is absent because the library has no node for it.**  `Z{delta[n]} = 1`
 *  is the most basic entry in any table, and it is missing rather than approximated: there is
 *  no discrete impulse in `core`, and inventing one is a wider decision than this issue.
 *  `_Heaviside` covers the step, which is what the control tier (6.29) actually needs.
 */

/** Maximum recursion depth for the multiply-by-`n` rule, mirroring `MaxLaplacePowerN`. */
private val MaxZPowerN = 20

/** Computes `Z{x}` with index variable `n` and frequency variable `z`.
 *
 *  @param x the sequence expression, in terms of `n`
 *  @param n the discrete index variable
 *  @param z the z-domain variable
 *  @return the z-transform of `x`, or [[_ZTransform]]`(x, n, z)` if no rule applies
 */
def zTransformOf(x: _Expression, n: _Variable, z: _Variable): _Expression =
  zImpl(x, n, z, n.variable)

/** Recursive implementation of the rule table; carries `nv = n.variable` to avoid re-boxing. */
private def zImpl(x: _Expression, n: _Variable, z: _Variable, nv: String): _Expression =

  /** `z / (z - a)`, the geometric transform shared by the `a^n` and `exp(c*n)` rules. */
  def geometric(a: _Expression): _Expression =
    Ratio(z, Sum(z, Product(_Number(-1), a)))

  /** `z^2 - 2*z*cos(w) + 1`, the denominator both trigonometric rules share. */
  def trigDen(w: _Expression): _Expression =
    Sum(Sum(Power(z, _Number(2)), Product(_Number(-2), Product(z, Cos(w)))), _Number(1))

  x match

    // Z{c} = c*z/(z-1) for a sequence free of n -- NOT c/z; see the class comment.
    case _ if !dependsOn(x, n) =>
      Product(x, Ratio(z, Sum(z, _Number(-1))))

    // Linearity: Z{a + b} = Z{a} + Z{b}
    case Sum(a, b) =>
      Sum(zImpl(a, n, z, nv), zImpl(b, n, z, nv))

    // Linearity: Z{c * x} = c * Z{x} when c is free of n (both Product orderings)
    case Product(c, f) if !dependsOn(c, n) => Product(c, zImpl(f, n, z, nv))
    case Product(f, c) if !dependsOn(c, n) => Product(c, zImpl(f, n, z, nv))

    // Z{u[n]} = z/(z-1)
    case _Heaviside(vv: _Variable) if vv.variable == nv =>
      Ratio(z, Sum(z, _Number(-1)))

    // Z{a^n} = z/(z-a) for a base free of n
    case Power(a, vv: _Variable) if vv.variable == nv && !dependsOn(a, n) =>
      geometric(a)

    // Z{exp(c*n)} = z/(z - e^c) -- the geometric rule with a = e^c
    case Exp(inner) => coeffOfT(inner, nv) match
      case Some(c) => geometric(Exp(c))
      case None    => _ZTransform(x, n, z)

    // Z{sin(w*n)} = z*sin(w) / (z^2 - 2*z*cos(w) + 1)
    case Sin(inner) => coeffOfT(inner, nv) match
      case Some(w) => Ratio(Product(z, Sin(w)), trigDen(w))
      case None    => _ZTransform(x, n, z)

    // Z{cos(w*n)} = z*(z - cos(w)) / (z^2 - 2*z*cos(w) + 1)
    case Cos(inner) => coeffOfT(inner, nv) match
      case Some(w) => Ratio(Product(z, Sum(z, Product(_Number(-1), Cos(w)))), trigDen(w))
      case None    => _ZTransform(x, n, z)

    // Multiply-by-n: Z{n^k * g[n]} = (-z d/dz)^k applied to G(z). Covers `n`, `n^2` and
    // `n*a^n` from one rule; stays symbolic when G itself does.
    case other =>
      nPowerFactor(other, nv) match
        case Some((k, g)) =>
          val G = zImpl(g, n, z, nv)
          if G.isInstanceOf[_ZTransform] then _ZTransform(x, n, z)
          else simplifyFully((1 to k).foldLeft(G)((acc, _) =>
            Product(Product(_Number(-1), z), derive(acc, z))))
        case None => _ZTransform(x, n, z)

/** Returns `Some((k, g))` when `x = n^k * g` (either ordering), or `x = n^k` alone with
 *  `g = u[n]`.
 *
 *  The bare-power case is what makes `Z{n}` and `Z{n^2}` reachable: `n^k` *is* `n^k * u[n]`
 *  for a one-sided sequence, so it feeds the multiply-by-`n` rule with the unit step as its
 *  base rather than needing table entries of its own.
 */
private def nPowerFactor(x: _Expression, nv: String): Option[(Int, _Expression)] =
  def power(e: _Expression): Option[Int] = e match
    case vv: _Variable if vv.variable == nv => Some(1)
    case Power(vv: _Variable, _Number(k))
        if vv.variable == nv && k.toInt.toDouble == k && k >= 1 && k <= MaxZPowerN => Some(k.toInt)
    case _ => None

  x match
    case Product(a, g) if power(a).isDefined => power(a).map((_, g))
    case Product(g, a) if power(a).isDefined => power(a).map((_, g))
    case bare                                => power(bare).map((_, _Heaviside(_Variable(nv))))

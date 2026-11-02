package it.grypho.scala.leonardo
package transform

import core.*
import scalar.*


/** One-sided (unilateral) [[https://en.wikipedia.org/wiki/Z-transform z-transform]] rule table.
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
 *  no discrete impulse in `core`, and inventing one is a wider decision than this tier's.
 *  `_Heaviside` covers the step, which is what the control tier actually needs.
 */

/** Maximum recursion depth for the multiply-by-`n` rule, mirroring `MaxLaplacePowerN`, and
 *  fixed for the same reason (F_0124 phase C). */
private val MaxZPowerN = 20

/** Computes `Z{x}` with index variable `n` and frequency variable `z`.
 *
 *  @param x the sequence expression, in terms of `n`
 *  @param n the discrete index variable
 *  @param z the z-domain variable
 *  @return the z-transform of `x`, or [[_ZTransform]]`(x, n, z)` if no rule applies
 */
def zTransformOf(x: _Expression, n: _Variable, z: _Variable): _Expression =
  zImpl(x, n, z, n.variable)(using tierOf(x))

/** Recursive implementation of the rule table; carries `nv = n.variable` to avoid re-boxing, and
 *  the whole input's tier for every constant a rule invents (F_0210). */
private def zImpl(x: _Expression, n: _Variable, z: _Variable, nv: String)(using lit: Int => _Value): _Expression =

  /** `z / (z - a)`, the geometric transform shared by the `a^n` and `exp(c*n)` rules. */
  def geometric(a: _Expression): _Expression =
    Ratio(z, Sum(z, Product(lit(-1), a)))

  /** `z^2 - 2*z*cos(w) + 1`, the denominator both trigonometric rules share. */
  def trigDen(w: _Expression): _Expression =
    Sum(Sum(Power(z, lit(2)), Product(lit(-2), Product(z, Cos(w)))), lit(1))

  x match

    // Z{c} = c*z/(z-1) for a sequence free of n -- NOT c/z; see the class comment.
    // A unit constant is the step itself, not `1 * step` (F_0275).
    case _ if !dependsOn(x, n) =>
      val step = Ratio(z, Sum(z, lit(-1)))
      if isOneLiteral(x) then step else Product(x, step)

    // Linearity: Z{a + b} = Z{a} + Z{b}
    case Sum(a, b) =>
      Sum(zImpl(a, n, z, nv), zImpl(b, n, z, nv))

    // Linearity: Z{c * x} = c * Z{x} when c is free of n (both Product orderings)
    case Product(c, f) if !dependsOn(c, n) => Product(c, zImpl(f, n, z, nv))
    case Product(f, c) if !dependsOn(c, n) => Product(c, zImpl(f, n, z, nv))
    // and over a constant divisor (F_0210): `ztrans(n/3, n, z)` stayed unevaluated
    case Ratio(f, c) if !dependsOn(c, n) =>
      zImpl(f, n, z, nv) match
        case _: _ZTransform => _ZTransform(x, n, z)
        case transformed    => Ratio(transformed, c)

    // Z{u[n]} = z/(z-1)
    case _Heaviside(vv: _Variable) if vv.variable == nv =>
      Ratio(z, Sum(z, lit(-1)))

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
      case Some(w) => Ratio(Product(z, Sum(z, Product(lit(-1), Cos(w)))), trigDen(w))
      case None    => _ZTransform(x, n, z)

    // Multiply-by-n: Z{n^k * g[n]} = (-z d/dz)^k applied to G(z). Covers `n`, `n^2` and
    // `n*a^n` from one rule; stays symbolic when G itself does.
    case other =>
      nPowerFactor(other, nv) match
        case Some((k, g)) =>
          val G = zImpl(g, n, z, nv)
          if G.isInstanceOf[_ZTransform] then _ZTransform(x, n, z)
          else (1 to k).foldLeft(G)((acc, _) => minusZDerivative(acc, z))
        case None => _ZTransform(x, n, z)

/** `-z · dG/dz`, the multiply-by-`n` operator, in the form a reader would write (F_0275).
 *
 *  It returned `derive`'s raw quotient, which `simplify` does not collect: `Z{n}` printed
 *  `-z * (((z - 1) - z)/(z - 1)^2)`.  The operator is linear, so it is applied through sums and
 *  constant factors, and a quotient `N/B^m` is differentiated as `(N'B - m·N·B')/B^(m+1)` rather
 *  than by the general quotient rule, whose `B^(2m)` would leave a common factor of `B^(m-1)` that
 *  nothing here cancels; the numerator is expanded, so `Z{n}` is `z/(z - 1)^2` and `Z{n^2}`
 *  `(z^2 + z)/(z - 1)^3`.  Every table entry is such a quotient; anything else takes the general
 *  path it always took.  The constants are the input's tier (F_0210), the derivative too.
 */
private def minusZDerivative(g: _Expression, z: _Variable)(using lit: Int => _Value): _Expression =
  val exact = lit(1).isInstanceOf[_Rational]
  def d(e: _Expression): _Expression = deriveInTier(e, z, exact)
  def quotient(num: _Expression, base: _Expression, m: Int): _Expression =
    val raw = Product(Product(lit(-1), z),
      Sum(Product(d(num), base), Product(lit(-m), Product(num, d(base)))))
    Ratio(expand(simplifyFully(raw)), Power(simplifyFully(base), lit(m + 1)))
  g match
    case Sum(a, b)                         => Sum(minusZDerivative(a, z), minusZDerivative(b, z))
    case Product(c, f) if !dependsOn(c, z) => Product(c, minusZDerivative(f, z))
    case Product(f, c) if !dependsOn(c, z) => Product(c, minusZDerivative(f, z))
    case Ratio(f, c) if !dependsOn(c, z)   => Ratio(minusZDerivative(f, z), c)
    // The rule never builds a power above `MaxZPowerN + 1`; a larger one takes the general path.
    case Ratio(num, Power(base, _Number(k)))
        if dependsOn(base, z) && k == math.floor(k) && k >= 1 && k <= MaxZPowerN + 1 =>
      quotient(num, base, k.toInt)
    case Ratio(num, base) if dependsOn(base, z) => quotient(num, base, 1)
    case other => simplifyFully(Product(Product(lit(-1), z), d(other)))

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

package it.grypho.scala.leonardo
package control

import core.*
import scalar.*
import transform.inverseLaplaceOf


/** Continuous-to-discrete conversion.
 *
 *  **The method is always a visible argument, never a hidden default.**  The same plant
 *  discretised by zero-order hold and by Tustin has *different* discrete poles, so a silent
 *  choice would make two correct-looking answers disagree with no way to see why.
 */
enum DiscretisationMethod:
  /** [[https://en.wikipedia.org/wiki/Zero-order_hold Zero-order hold]] — what a sampled-data control system physically does: the input is held
   *  constant between samples.  The default choice for a plant.
   */
  case Zoh

  /** Tustin, the [[https://en.wikipedia.org/wiki/Bilinear_transform bilinear transform]] `s = (2/Ts)*(z-1)/(z+1)` — what a filter designer wants.
   *  Maps the left half-plane onto the unit disc exactly, so stability is preserved.  Its
   *  prewarped form is [[c2dPrewarped]], a parameter on this method rather than a method of
   *  its own (F_0009).
   */
  case Tustin

  /** [[https://en.wikipedia.org/wiki/First-order_hold First-order hold]], the non-causal
   *  triangle hold: `G_d(z) = (z − 1)²/(Ts·z) · Z{L⁻¹[G(s)/s²]}`, the ramp-invariant equivalent.
   *  Keeps the DC gain (F_0009).
   */
  case Foh

  /** [[https://en.wikipedia.org/wiki/Impulse_invariance Impulse invariance]]:
   *  `G_d(z) = Ts · Z{g(n·Ts)}`, `g = L⁻¹[G]` (MATLAB's scaling, with no `g(0)/2` correction).
   *  **Aliases**: the frequency response is the continuous one folded at the Nyquist frequency,
   *  so it is faithful only for a plant that is band-limited well below `π/Ts`.  Declines a
   *  plant with direct feedthrough, whose impulse response has an impulse at `t = 0` (F_0009).
   */
  case ImpulseInvariant

  /** [[https://en.wikipedia.org/wiki/Matched_Z-transform_method Matched pole-zero]]: every
   *  finite pole and zero maps by `z = e^(s·Ts)`, all but one zero at infinity to `z = −1`
   *  (the one kept is a one-step delay), and the gain is matched at low frequency — at DC, or,
   *  with `k` net integrators or differentiators, so that `G_d(z)·(Ts/(z − 1))^k → G(s)/s^k`.
   *  Needs numeric poles and zeros (F_0009).
   */
  case MatchedPoleZero

  /** Forward Euler, `s = (z − 1)/Ts`.  **Can map a stable plant to an unstable one**: a pole at
   *  `s = p` goes to `z = 1 + p·Ts`, outside the unit circle once `|1 + p·Ts| > 1` (F_0009).
   */
  case ForwardEuler

  /** Backward Euler, `s = (z − 1)/(Ts·z)`: maps the left half-plane into the unit disc, so a
   *  stable plant stays stable (F_0009).
   */
  case BackwardEuler

export DiscretisationMethod.{Zoh, Tustin, Foh, ImpulseInvariant, MatchedPoleZero, ForwardEuler, BackwardEuler}

object DiscretisationMethod:
  /** The slot word a method is written with at the prompt (none is reserved). */
  def keyword(m: DiscretisationMethod): String = m match
    case Zoh              => "zoh"
    case Tustin           => "tustin"
    case Foh              => "foh"
    case ImpulseInvariant => "impulse"
    case MatchedPoleZero  => "matched"
    case ForwardEuler     => "forward"
    case BackwardEuler    => "backward"

  /** Looks a method up by its slot word. */
  def fromKeyword(s: String): Option[DiscretisationMethod] = values.find(m => keyword(m) == s)

/** Discretises a continuous transfer function over sample period `ts`.
 *
 *  Tustin and both Euler methods are **substitutions** for `s`, renormalised.  Zero-order
 *  hold, first-order hold and impulse invariance are **response equivalences**: the step,
 *  ramp or impulse response, sampled at `n·Ts`, transformed by the z-transform and scaled —
 *  assembled from the inverse Laplace transform and the z-transform, which is why the
 *  z-transform had to exist first.  Matched pole-zero maps the roots themselves.
 *
 *  @param g      the continuous transfer function
 *  @param s      the continuous frequency variable
 *  @param z      the discrete variable to express the result in
 *  @param ts     the sample period; must be positive
 *  @param method the method, always named
 *  @return the discrete transfer function, or `None` when the conversion does not close
 */
def c2d(g: _Expression, s: _Variable, z: _Variable, ts: Double,
        method: DiscretisationMethod): Option[_Expression] =
  if ts <= 0 then None
  else method match
    case Tustin =>
      val sub = Ratio(Product(_Number(2 / ts), Sum(z, _Number(-1))), Sum(z, _Number(1)))
      Some(normaliseTf(substitute(g, Map(s.variable -> sub)), z))

    case ForwardEuler =>
      Some(normaliseTf(substitute(g, Map(s.variable -> Ratio(Sum(z, _Number(-1)), _Number(ts)))), z))

    case BackwardEuler =>
      Some(normaliseTf(substitute(g, Map(s.variable -> Ratio(Sum(z, _Number(-1)), Product(_Number(ts), z)))), z))

    case Zoh =>
      // Step response in the time domain, sampled at n*Ts, transformed back, then the
      // (1 - z^-1) factor that turns the step's accumulation into the impulse response.
      //
      // Both internal variables are GENERATED by `scalar.freshVar` rather than named by hand.
      // Naming them `__t`/`__n` literally would let a plant carrying a
      // parameter of the same name have it substituted along with the time variable --
      // turning a constant gain into a time-varying one, silently.
      //
      // That capture is **unreachable today**, and the reason is worth stating because it is
      // not a property of this function: `stepResponse` routes through the inverse-Laplace
      // tier, which requires numeric coefficients, so a plant with any free parameter is
      // declined before the substitution is reached.  The safety therefore rests on a
      // NEIGHBOURING tier's current limitation -- one its own documentation calls a boundary
      // rather than a law -- and would lapse the day symbolic inversion lands.  Generating
      // the names costs two lines and removes the coupling: the house rule that a latent
      // capture is worth closing whether or not today's callers can reach it.
      sampledEquivalent(g, s, z, ts, stepResponse(_, _, _), _.isInstanceOf[_StepResponse],
                        Sum(_Number(1), Ratio(_Number(-1), z)))

    case Foh =>
      // The ramp response, sampled and transformed, times (z - 1)^2/(Ts z): ramp invariance.
      sampledEquivalent(g, s, z, ts, rampResponse(_, _, _), _.isInstanceOf[transform._InverseLaplace],
                        Ratio(Power(Sum(z, _Number(-1)), _Number(2)), Product(_Number(ts), z)))

    case ImpulseInvariant =>
      if !strictlyProper(g, s) then None
      else sampledEquivalent(g, s, z, ts, impulseResponse(_, _, _), _.isInstanceOf[_ImpulseResponse], _Number(ts))

    case MatchedPoleZero => matchedPoleZero(g, s, z, ts)

/** Prewarped Tustin (F_0009): the bilinear transform scaled so the discrete frequency response
 *  equals the continuous one **exactly at `w`**, `s = (w / tan(w·Ts/2))·(z − 1)/(z + 1)`.  Plain
 *  Tustin compresses the whole frequency axis into the Nyquist band, so a filter's corner
 *  lands somewhere else; prewarping puts the one frequency that matters back.
 *
 *  @param g  the continuous transfer function
 *  @param s  the continuous frequency variable
 *  @param z  the discrete variable to express the result in
 *  @param ts the sample period; must be positive
 *  @param w  the frequency to match, in rad/s, with `0 < w·Ts/2 < π/2`
 *  @return the discrete transfer function, or `None`
 */
def c2dPrewarped(g: _Expression, s: _Variable, z: _Variable, ts: Double, w: Double): Option[_Expression] =
  prewarpScale(ts, w).map { c =>
    val sub = Ratio(Product(_Number(c), Sum(z, _Number(-1))), Sum(z, _Number(1)))
    normaliseTf(substitute(g, Map(s.variable -> sub)), z)
  }

/** The scale `w / tan(w·Ts/2)` replacing Tustin's `2/Ts`; `None` outside `0 < w·Ts/2 < π/2`. */
private def prewarpScale(ts: Double, w: Double): Option[Double] =
  val half = w * ts / 2
  Option.when(ts > 0 && w > 0 && half < math.Pi / 2)(w / math.tan(half)).filter(c => c > 0 && !c.isInfinite)

/** A response equivalence: `response` in a fresh time variable, sampled at `n·Ts`, z-transformed
 *  and multiplied by `factor`; `None` when the response or the transform does not close. */
private def sampledEquivalent(g: _Expression, s: _Variable, z: _Variable, ts: Double,
                              response: (_Expression, _Variable, _Variable) => _Expression,
                              declined: _Expression => Boolean, factor: _Expression): Option[_Expression] =
  val tv = freshVar(g.freeVars ++ Set(s.variable, z.variable), "__t")
  val r  = response(g, s, tv)
  if declined(r) then None
  else
    val nv      = freshVar(r.freeVars ++ Set(z.variable), "__n")
    val sampled = substitute(r, Map(tv.variable -> Product(_Number(ts), nv)))
    val xz      = transform.zTransformOf(simplifyFully(sampled), nv, z)
    if containsZTransform(xz) then None
    else Some(normaliseTf(Product(factor, xz), z))

/** Whether `e` still holds a [[transform._ZTransform]] anywhere.  `zTransformOf` applies
 *  linearity term by term, so a term it cannot close stays as a node inside a `Sum` — a test of
 *  the top node alone answered such a half-converted expression, carrying the fresh index
 *  variable, for every plant with a complex pole pair (F_0103: the table has no
 *  `e^(an)·cos(bn)` entry).  The `containsIntegral` rule. */
private def containsZTransform(e: _Expression): Boolean = e match
  case _: transform._ZTransform => true
  case _                        => e.children.exists(containsZTransform)

/** The ramp response `L⁻¹{G(s)/s²}`, for first-order hold; the inverse transform unchanged
 *  when it does not close, or when `G` is improper. */
private def rampResponse(g: _Expression, s: _Variable, t: _Variable): _Expression =
  val ramp = normaliseTf(Ratio(g, Power(s, _Number(2))), s)
  if !properOrStrict(g, s, strict = false) then transform._InverseLaplace(ramp, s, t)
  else inverseLaplaceOf(ramp, s, t)

/** Whether the numerator's degree is below (`strict`) or at most the denominator's. */
private def properOrStrict(g: _Expression, v: _Variable, strict: Boolean): Boolean =
  rationalCoeffs(g, v).exists { (num, den) =>
    val (n, d) = (polyDegree(polyTrim(num)), polyDegree(polyTrim(den)))
    if strict then n < d else n <= d
  }

private def strictlyProper(g: _Expression, v: _Variable): Boolean = properOrStrict(g, v, strict = true)

/** Matched pole-zero: the roots mapped by `e^(s·Ts)`, all but one zero at infinity placed at
 *  `z = −1`, and the gain matched at low frequency (see [[MatchedPoleZero]]).
 *
 *  The roots at the origin are counted from the coefficients — trailing zero coefficients are
 *  exact, where a computed root is only near `0` — and the low-frequency gain is the ratio of the
 *  remaining polynomials' constant terms, `lim G(s)/s^k`.  The discrete polynomials are built
 *  over complex roots and must come out real; a residue that does not vanishes declines.
 */
private def matchedPoleZero(g: _Expression, s: _Variable, z: _Variable, ts: Double): Option[_Expression] =
  coeffsOf(g, s).flatMap { (num0, den0) =>
    val (num, den) = (polyTrim(num0), polyTrim(den0))
    val excess = polyDegree(den) - polyDegree(num)
    def origin(cs: Vector[Double]): Int = cs.indexWhere(_ != 0.0) match { case -1 => 0; case k => k }
    val (m0, p0) = (origin(num), origin(den))
    val (ns, ds) = (num.drop(m0), den.drop(p0))
    def rootsOf(cs: Vector[Double]): Option[Vector[(Double, Double)]] =
      if polyDegree(cs) < 1 then Some(Vector.empty)
      else polyRoots(cs).flatMap(rs => sequence(rs.toList.map(_Complex.parts))).map(_.toVector)
    if excess < 0 || ns.isEmpty || ds.isEmpty then None
    else
      for
        zs <- rootsOf(ns)
        ps <- rootsOf(ds)
        mapped = (r: (Double, Double)) => (math.exp(r._1 * ts) * math.cos(r._2 * ts), math.exp(r._1 * ts) * math.sin(r._2 * ts))
        zd = zs.map(mapped) ++ Vector.fill(m0)((1.0, 0.0)) ++ Vector.fill(math.max(excess - 1, 0))((-1.0, 0.0))
        pd = ps.map(mapped) ++ Vector.fill(p0)((1.0, 0.0))
        nz <- realPolynomial(zd)
        dz <- realPolynomial(pd)
        // The low-frequency gain: lim G(s)/s^k against lim G_d(z)/((z - 1)/Ts)^k, k = m0 - p0.
        target = ns.head / ds.head
        a = (zs.map(mapped).map(w => (1 - w._1, -w._2)) ++ Vector.fill(math.max(excess - 1, 0))((2.0, 0.0)))
              .foldLeft((1.0, 0.0))(complexTimes)
        b = ps.map(mapped).map(w => (1 - w._1, -w._2)).foldLeft((1.0, 0.0))(complexTimes)
        if math.abs(a._2) <= 1e-9 * math.max(1.0, math.abs(a._1)) && math.abs(b._2) <= 1e-9 * math.max(1.0, math.abs(b._1))
        ratio = a._1 / b._1
        if ratio != 0.0 && !ratio.isNaN && !ratio.isInfinite
        gain = target / (math.pow(ts, m0 - p0) * ratio)
        if !gain.isNaN && !gain.isInfinite
      yield normaliseTf(Ratio(Product(_Number(gain), polyToExpr(nz, z)), polyToExpr(dz, z)), z)
  }

private def complexTimes(a: (Double, Double), b: (Double, Double)): (Double, Double) =
  (a._1 * b._1 - a._2 * b._2, a._1 * b._2 + a._2 * b._1)

/** `Π (z − rᵢ)` as real ascending coefficients, or `None` when an imaginary part survives (a root
 *  without its conjugate). */
private def realPolynomial(roots: Vector[(Double, Double)]): Option[Vector[Double]] =
  val cs = roots.foldLeft(Vector((1.0, 0.0))) { (acc, r) =>
    // multiply by (z - r): shift up, subtract r times acc
    val shifted = (0.0, 0.0) +: acc
    val scaled  = acc.map(c => complexTimes(c, (-r._1, -r._2))) :+ ((0.0, 0.0))
    shifted.zip(scaled).map((x, y) => (x._1 + y._1, x._2 + y._2))
  }
  val scale = math.max(1.0, cs.map(c => math.abs(c._1)).max)
  Option.when(cs.forall(c => math.abs(c._2) <= 1e-9 * scale))(cs.map(_._1))

/** Inverse of [[c2d]]: recovers a continuous transfer function from a discrete one.
 *
 *  The substitution methods invert in closed form: [[Tustin]] by `z = (2 + Ts*s)/(2 - Ts*s)`,
 *  which is what makes `d2c(c2d(G))` return to `G`, and the Euler methods by `z = 1 + Ts·s` and
 *  `z = 1/(1 − Ts·s)` (F_0009).  The response equivalences would need a matrix logarithm, and
 *  matched pole-zero a logarithm of each root with its branch; all four are declined rather
 *  than approximated.
 *
 *  @param g      the discrete transfer function
 *  @param z      the discrete variable
 *  @param s      the continuous variable to express the result in
 *  @param ts     the sample period
 *  @param method the method `g` was discretised with
 *  @return the continuous transfer function, or `None`
 */
def d2c(g: _Expression, z: _Variable, s: _Variable, ts: Double,
        method: DiscretisationMethod): Option[_Expression] =
  if ts <= 0 then None
  else
    val sub: Option[_Expression] = method match
      case Tustin        => Some(Ratio(Sum(_Number(2), Product(_Number(ts), s)), Sum(_Number(2), Product(_Number(-ts), s))))
      case ForwardEuler  => Some(Sum(_Number(1), Product(_Number(ts), s)))
      case BackwardEuler => Some(Ratio(_Number(1), Sum(_Number(1), Product(_Number(-ts), s))))
      case _             => None
    sub.map(e => normaliseTf(substitute(g, Map(z.variable -> e)), s))

/** Inverse of [[c2dPrewarped]]: `z = (c + s)/(c − s)`, `c = w / tan(w·Ts/2)`.
 *
 *  @param g  the discrete transfer function
 *  @param z  the discrete variable
 *  @param s  the continuous variable to express the result in
 *  @param ts the sample period
 *  @param w  the frequency the discretisation was prewarped at
 *  @return the continuous transfer function, or `None`
 */
def d2cPrewarped(g: _Expression, z: _Variable, s: _Variable, ts: Double, w: Double): Option[_Expression] =
  prewarpScale(ts, w).map { c =>
    normaliseTf(substitute(g, Map(z.variable -> Ratio(Sum(_Number(c), s), Sum(_Number(c), Product(_Number(-1), s))))), s)
  }

// The fold-back-into-one-rational helper lives in TransferFunction.scala as `normaliseTf`,
// shared by every slice: a substituted tree and an interconnected one need exactly the same
// treatment, and a second copy here would only be a way for the two to disagree about
// pole-zero cancellation.

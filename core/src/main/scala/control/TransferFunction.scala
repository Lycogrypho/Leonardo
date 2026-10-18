package it.grypho.scala.leonardo
package control

import core.*
import scalar.*


/** [[https://en.wikipedia.org/wiki/Transfer_function Transfer-function]] algebra, poles and zeros, stability, and frequency response.
 *
 *  Every function takes the **frequency variable explicitly** — `s` for continuous time, `z`
 *  for discrete — because a transfer function is an ordinary `Ratio` and carries no type to
 *  dispatch on.
 */

/** Series (cascade) connection: `G * H`.
 *
 *  @param g first block
 *  @param h second block
 *  @param v the frequency variable
 *  @return the combined transfer function, normalised to a single rational
 */
def series(g: _Expression, h: _Expression, v: _Variable): _Expression =
  normaliseTf(Product(g, h), v)

/** Parallel connection: `G + H`.
 *
 *  @param g first block
 *  @param h second block
 *  @param v the frequency variable
 *  @return the combined transfer function, normalised to a single rational
 */
def parallel(g: _Expression, h: _Expression, v: _Variable): _Expression =
  normaliseTf(Sum(g, h), v)

/** Closed loop with **negative** feedback: `G / (1 + G*H)`.
 *
 *  **The sign convention is stated because it cannot be inferred.**  Negative feedback is the
 *  control convention and what a unity-feedback loop means by default; a substantial part of
 *  the literature writes the positive form `G/(1 - G*H)`, and choosing silently would make
 *  every closed-loop result wrong for half its readers — the reasoning that also fixes the
 *  spherical polar-angle convention in the vector package.  For positive feedback, negate `h`.
 *
 *  @param g the forward path
 *  @param h the feedback path (use `_Number(1)` for unity feedback)
 *  @param v the frequency variable
 *  @return the closed-loop transfer function, normalised to a single rational
 */
def feedback(g: _Expression, h: _Expression, v: _Variable): _Expression =
  normaliseTf(Ratio(g, Sum(_Number(1), Product(g, h))), v)

/** The coefficient view of a transfer function: `(numerator, denominator)` in **lowest terms**.
 *
 *  **The cancellation is not tidiness, it is the difference between right and wrong poles.**
 *  `rationalCoeffs` folds a tree into one fraction by polynomial arithmetic and performs no
 *  cancellation, so `feedback(G, 1)` with `G = 1/(s(s+2))` arrives as
 *  `s(s+2) / (s(s+2)(s+1)²)` — whose roots include a spurious `0` and `−2` beside the true
 *  double pole at `−1`. Reporting those would answer a different question with confidence,
 *  the failure mode this project refuses everywhere else. Every consumer of the coefficients
 *  goes through here so no two of them can disagree about what the poles are.
 */
private[control] def coeffsOf(e: _Expression, v: _Variable): Option[(Vector[Double], Vector[Double])] =
  // trimExact, not polyTrim: rationalCoeffs has already trimmed by magnitude, and the absolute
  // RationalEps dropped a genuine tiny leading coefficient and its pole (F_0121).
  rationalCoeffs(e, v).map((num, den) => cancelCommon(trimExact(num), trimExact(den)))

/** Divides a common polynomial factor out of `num`/`den`.
 *
 *  The gcd is computed in floating point, so the quotients are **verified**: a division is
 *  accepted only when both remainders are negligible against the operands. An ill-conditioned
 *  gcd therefore leaves the fraction unreduced — which loses nothing, since the uncancelled
 *  form is what the caller had — rather than silently deleting a genuine factor.
 */
private def cancelCommon(num: Vector[Double], den: Vector[Double]): (Vector[Double], Vector[Double]) =
  // At the joint root scale (F_0118): `x = ρ·y` maps both polynomials alike, so their ratio is
  // unchanged, and the gcd's absolute trim then meets coefficients of order one rather than the
  // tiny ones a plant with poles near 1e-6 rad/s has. The remainder test is relative to the
  // scaled coefficients, with no `max(1, …)` floor.
  // Each is also divided by its OWN size, so the gcd's trim meets coefficients of order one rather
  // than the 1e-12 a plant with poles near 1e-6 has even at its root scale (F_0121): the common
  // factor of feedback(1e-6/(s + 1e-6), 1) went uncancelled and was reported as a pole. One
  // common size is not enough: it leaves the smaller polynomial below the trim, and the gcd then
  // swallowed a constant numerator whole. A common factor of `num` is one of `num/k`, and each
  // quotient is restored with its own factor, so the ratio is unchanged.
  val rho = math.max(rootScale(num), rootScale(den))
  def scaledRaw(p: Vector[Double]): Vector[Double] =
    if rho == 0.0 then p else Vector.tabulate(p.size)(i => p(i) * math.pow(rho, i.toDouble))
  def sizeOf(p: Vector[Double]): Double =
    val s = p.map(math.abs).max
    if s > 0.0 && !s.isInfinite then s else 1.0
  val (rawN, rawD)   = (scaledRaw(num), scaledRaw(den))
  val (normN, normD) = (sizeOf(rawN), sizeOf(rawD))
  def unscaled(p: Vector[Double], norm: Double): Vector[Double] =
    if rho == 0.0 then p.map(_ * norm) else Vector.tabulate(p.size)(i => p(i) * norm / math.pow(rho, i.toDouble))
  val (ny, dy) = (rawN.map(_ / normN), rawD.map(_ / normD))
  if polyDegree(ny) < 0 || polyDegree(dy) < 0 then (num, den)
  else
    val g = polyGcd(ny, dy)
    if polyDegree(g) <= 0 then (num, den)
    else
      val (qn, rn) = polyDivide(ny, g)
      val (qd, rd) = polyDivide(dy, g)
      // Both are at unit size now, so the remainder test is against one.
      val exact    = (rn ++ rd).forall(r => math.abs(r) < 1e-6)
      if !exact then (num, den)
      else
        val (rn0, rd0) = (trimExact(unscaled(polyTrim(qn), normN)), trimExact(unscaled(polyTrim(qd), normD)))
        // The scaled gcd is monic in `y`, so the quotients carry a stray `ρ^deg g`: rescale so the
        // denominator keeps its own leading coefficient, the form the unscaled division gave
        // before F_0118 — and so feedback(1e-6/(s + 1e-6), 1) reads 1e-6/(s + 2e-6), not
        // 3e-12/(3e-6·s + 6e-12).
        val k = den(degreeOf(den)) / rd0.last
        if k == 0.0 || k.isNaN || k.isInfinite then (rn0, rd0) else (rn0.map(_ * k), rd0.map(_ * k))

/** Rebuilds an expression as one rational `N(v)/D(v)`, or simplifies it if it is not rational.
 *
 *  Keeping the result a *single* fraction in lowest terms is what stops [[feedback]] returning
 *  the nested `G/(1 + G*H)` a reader would have to unpick by hand — the one real argument that
 *  was made for giving transfer functions a carrier, and it is served here instead.
 */
private[control] def normaliseTf(e: _Expression, v: _Variable): _Expression =
  coeffsOf(e, v) match
    case Some((num, den)) => simplifyFully(Ratio(polyToExpr(num, v), polyToExpr(den, v)))
    case None             => simplifyFully(e)

/** Rebuilds a dense coefficient vector into an expression in `v`. */
private[control] def polyToExpr(cs: Vector[Double], v: _Variable): _Expression =
  // trimExact, not polyTrim: the coefficients arrive trimmed for their provenance, and the absolute
  // RationalEps erased a numerator of 1e-10 (F_0121).
  val terms = trimExact(cs).zipWithIndex.collect {
    case (c, 0)                                  => _Number(c)
    case (c, 1) if math.abs(c - 1.0) < RationalEps => v
    case (c, 1)                                  => Product(_Number(c), v)
    case (c, i) if math.abs(c - 1.0) < RationalEps => Power(v, _Number(i))
    case (c, i)                                  => Product(_Number(c), Power(v, _Number(i)))
  }
  if terms.isEmpty then _Number(0) else terms.reduce(Sum.apply)

/** Poles of `G`: the roots of its denominator.
 *
 *  Returns `_Value`s rather than `Double`s because **a complex pole is the interesting case**,
 *  not an error — an oscillatory mode is a conjugate pair, and reducing the result to reals
 *  would silently drop exactly the systems a control engineer cares about.
 *
 *  @param g the transfer function
 *  @param v the frequency variable
 *  @return the poles, or `None` when `G` is not rational in `v` or the roots do not resolve
 */
def poles(g: _Expression, v: _Variable): Option[Vector[_Value]] =
  coeffsOf(g, v).flatMap((_, den) => rootsAtScale(den))

/** Zeros of `G`: the roots of its numerator.  See [[poles]] for the `_Value` result type.
 *
 *  @param g the transfer function
 *  @param v the frequency variable
 *  @return the zeros, or `None` when `G` is not rational in `v`
 */
def zeros(g: _Expression, v: _Variable): Option[Vector[_Value]] =
  coeffsOf(g, v).flatMap((num, _) => rootsAtScale(num))

/** Steady-state gain: `G` evaluated at `v = 0`.
 *
 *  @param g the transfer function
 *  @param v the frequency variable
 *  @return `G(0)`, or `None` when it is not finite (a pole at the origin) or not rational
 */
def dcgain(g: _Expression, v: _Variable): Option[Double] =
  g.eval(new Environment().withBinding(v.variable, _Number(0))) match
    case Right(_Number(d)) if !d.isNaN && !d.isInfinite => Some(d)
    case _                                              => None

/** Real part of a pole, for the stability tests. */
private def realPart(p: _Value): Option[Double] = p match
  case c: _Complex => _Complex.parts(c).map(_._1)
  case _Number(d)  => Some(d)
  case _           => None

/** Modulus of a pole, for the discrete stability test. */
private def modulus(p: _Value): Option[Double] = p match
  case c: _Complex => _Complex.parts(c).map((re, im) => math.hypot(re, im))
  case _Number(d)  => Some(math.abs(d))
  case _           => None

/** Continuous-time stability: every pole strictly in the left half-plane.
 *
 *  **Decided from the poles rather than from a [[https://en.wikipedia.org/wiki/Routh%E2%80%93Hurwitz_stability_criterion Routh table]]**, a deliberate departure from the
 *  original plan.  Routh's advantage is symbolic coefficients, but it carries well-known
 *  degenerate cases — a zero in the first column, or an identically zero row — each needing
 *  its own repair, and a mis-handled one yields a confident *wrong* verdict rather than a
 *  refusal.  Root location has neither failure mode.  Symbolic coefficients are refused here
 *  instead, which is the same answer Routh would have to give once `scalar.sign` could not
 *  decide, and [[routhTable]] remains available for the table itself.
 *
 *  **Strict, so marginal stability is not stability**: poles on the imaginary axis give
 *  `false`.  An oscillator that never decays is not a stable system, and reporting it as one
 *  would be the confident wrong answer.
 *
 *  @param g the transfer function
 *  @param v the frequency variable
 *  @return `Some(true)`/`Some(false)`, or `None` when the poles cannot be determined
 */
def isStable(g: _Expression, v: _Variable): Option[Boolean] =
  // A pole is in the open left half-plane when its real part is negative beyond the rounding
  // of the pole itself: Re(p) < -RationalEps·|p|. The test was Re(p) < -1e-9, absolute, so a
  // stable plant with a pole at -1e-10 was reported unstable — a verdict that moved with the time
  // units (F_0121). An imaginary-axis pole, computed with a rounding-size real part, still fails.
  poles(g, v).flatMap { ps =>
    val parts = ps.map(p => realPart(p).zip(modulus(p)))
    Option.when(parts.forall(_.isDefined))(parts.flatten.forall((re, mod) => re < -RationalEps * mod))
  }

/** Discrete-time stability: every pole strictly **inside the unit circle**.
 *
 *  The continuous test does not transfer.  `z = 0.5` is a stable discrete pole while the same
 *  value read as a continuous one is unstable, so applying the left-half-plane rule to a
 *  z-domain transfer function inverts the answer rather than degrading it.
 *
 *  @param g the discrete transfer function
 *  @param v the z-domain variable
 *  @return `Some(true)`/`Some(false)`, or `None` when the poles cannot be determined
 */
def isStableDiscrete(g: _Expression, v: _Variable): Option[Boolean] =
  poles(g, v).flatMap { ps =>
    val mods = ps.map(modulus)
    Option.when(mods.forall(_.isDefined))(mods.flatten.forall(_ < 1.0 - RationalEps))
  }

/** The Routh array of `G`'s denominator, for inspecting a stability margin by hand.
 *
 *  Numeric coefficients only.  [[isStable]] does not consult it — see the note there.
 *
 *  @param g the transfer function
 *  @param v the frequency variable
 *  @return the array, row by row from the highest power down, or `None`
 */
def routhTable(g: _Expression, v: _Variable): Option[Vector[Vector[Double]]] =
  // Each entry carries its magnitude, the size of the terms that formed it, and reads as zero
  // only at RationalEps of that (F_0125). An absolute 1e-9 cut the table short for poles near
  // 1e-6, where every entry below the first row is that small: three rows of four.
  type Entry = (Double, Double)
  def isZero(e: Entry): Boolean = math.abs(e._1) <= RationalEps * e._2

  /** The next row from the previous two; empty when the pivot vanishes (a degenerate case
   *  this tier reports rather than repairs — see [[isStable]] for why it is not consulted). */
  def nextRow(a: Vector[Entry], b: Vector[Entry]): Vector[Entry] =
    val pivot = b.headOption.getOrElse((0.0, 0.0))
    if isZero(pivot) then Vector.empty
    else Vector.tabulate(math.max(0, a.size - 1)) { i =>
      val (ai, bi) = (a.lift(i + 1).getOrElse((0.0, 0.0)), b.lift(i + 1).getOrElse((0.0, 0.0)))
      ((pivot._1 * ai._1 - a.head._1 * bi._1) / pivot._1,
       (math.abs(pivot._1) * ai._2 + a.head._2 * bi._2) / math.abs(pivot._1))
    }

  def exhausted(r: Vector[Entry]): Boolean = r.forall(isZero)

  coeffsOf(g, v).map { (_, den) =>
    val cs = den.reverse.map(c => (c, math.abs(c)))   // descending powers, each its own magnitude
    val (first, second) = cs.zipWithIndex.partition(_._2 % 2 == 0)
    // The array is a two-term recurrence, so it unfolds from the pair of seed rows: the
    // even- and odd-indexed coefficients. At most `cs.size` rows, and it stops at the first
    // exhausted row — which is *kept*, since a zero row is itself the diagnostic.
    val rows = LazyList
      .iterate((first.map(_._1), second.map(_._1)))((a, b) => (b, nextRow(a, b)))
      .map(_._1).take(cs.size).toVector
    // Scanning from index 1: the leading coefficient is non-zero by construction (the
    // denominator arrives trimmed), so the seed row never terminates the array.
    val kept = rows.indexWhere(exhausted, 1) match
      case -1   => rows
      case stop => rows.take(stop + 1)
    kept.map(_.map(_._1))
  }

/** [[https://en.wikipedia.org/wiki/Frequency_response Frequency response]] of `G` at angular frequency `w`: substitute `v -> i*w`.
 *
 *  Rides the existing `_Complex` closure — no new arithmetic — which is why the whole of
 *  Bode and Nyquist is one substitution.
 *
 *  @param g the transfer function
 *  @param v the frequency variable
 *  @param w the angular frequency, in radians per unit time
 *  @return `(magnitude, phase-in-radians)`, or `None` where the response is not finite
 *          (evaluating an integrator at `w = 0`, say) — never a fabricated infinity
 */
def bode(g: _Expression, v: _Variable, w: Double): Option[(Double, Double)] =
  responseAt(g, v, w, new Environment())

/** [[bode]] with an environment, so a plant carrying a bound parameter can fold.
 *
 *  `bode` is defined as this at an empty environment rather than as a second copy of the
 *  substitution — the arrangement `observable` has with `controllable`.  It exists because a
 *  sweep is normally driven from a REPL session, where `K := 10` is an ordinary thing to have
 *  written first, and `bode`'s own empty environment would leave such a plant symbolic.
 */
private def responseAt(g: _Expression, v: _Variable, w: Double,
                       env: Environment): Option[(Double, Double)] =
  substitute(g, Map(v.variable -> _Complex.of(0, w))).eval(env) match
    case Right(c: _Complex) =>
      _Complex.parts(c).filter((re, im) => re.isFinite && im.isFinite)
        .map((re, im) => (math.hypot(re, im), math.atan2(im, re)))
    case Right(_Number(d)) if d.isFinite => Some((math.abs(d), if d < 0 then math.Pi else 0.0))
    case _                               => None

/** [[https://en.wikipedia.org/wiki/Nyquist_stability_criterion Nyquist]] point: the real and imaginary parts of `G(i*w)`.
 *
 *  The same substitution as [[bode]], reported in rectangular rather than polar form — the two
 *  are one computation presented two ways, not two computations.
 *
 *  @param g the transfer function
 *  @param v the frequency variable
 *  @param w the angular frequency
 *  @return `(real, imaginary)`, or `None` where the response is not finite
 */
def nyquist(g: _Expression, v: _Variable, w: Double): Option[(Double, Double)] =
  bode(g, v, w).map((mag, phase) => (mag * math.cos(phase), mag * math.sin(phase)))

/** The geometrically spaced grid both sweeps are read on (issue F_0004).
 *
 *  **Geometric rather than linear, and that is the whole point of the entry.**  A frequency
 *  response is read on a logarithmic axis spanning decades, so a linear grid of 200 points
 *  over `0.01 .. 100` puts 199 of them in the final decade and none near a corner at `0.1` —
 *  the interesting part of the curve is exactly the part it fails to resolve.  `scalar.sample`
 *  is the vector producer everywhere else in the library, but its grid is linear, which is why
 *  this needs its own helper rather than a call to it.
 *
 *  Empty for any interval a geometric grid cannot span: `log(0)` is not a number, so the lower
 *  bound must be strictly positive, the bounds strictly ordered, and there must be at least
 *  two points.  **Refused rather than repaired** — a caller sweeping from zero has made a units
 *  mistake, and quietly nudging the bound would hide it.
 *
 *  @param wMin   the lowest angular frequency, strictly positive
 *  @param wMax   the highest angular frequency, strictly greater than `wMin`
 *  @param points how many samples, at least 2
 *  @return the grid, ascending, hitting both endpoints exactly; empty when it cannot be built
 */
private def logGrid(wMin: Double, wMax: Double, points: Int): Vector[Double] =
  if !(wMin > 0.0) || !(wMax > wMin) || points < 2 || !wMax.isFinite then Vector.empty
  else
    // Interpolating the LOGARITHMS and exponentiating keeps the ratio between neighbours
    // constant.  The endpoints are written back verbatim, since `exp(log(w))` drifts in the
    // last bits and a sweep that misses the decade it was asked for reads as a bug.
    val (lo, hi) = (math.log(wMin), math.log(wMax))
    val step     = (hi - lo) / (points - 1)
    Vector.tabulate(points) {
      case 0                    => wMin
      case k if k == points - 1 => wMax
      case k                    => math.exp(lo + k * step)
    }

/** The root locus of `G` over `gains` (F_0140): the roots of `den + K·num` at each gain, as
 *  **continuous branches**, one per closed-loop pole.
 *
 *  **The matching is the substance.**  A root finder returns its roots in no particular order,
 *  and the order changes wherever two roots meet — at every breakaway point — so connecting the
 *  `i`-th root of one gain to the `i`-th of the next cross-connects the branches.  Each gain's
 *  roots are therefore matched to the previous gain's by nearest continuation, the globally
 *  closest pair first.  A gain where the degree drops (`K = −denₙ/numₙ` for a biproper plant) or
 *  the roots do not resolve is dropped from **every** branch at once — the point, never the
 *  axis (the F_0022 rule), or later points would pair with the wrong partners.
 *
 *  @param g     the open-loop transfer function, proper and rational with numeric coefficients
 *  @param v     the frequency variable
 *  @param gains the gains, in the order the branches should run
 *  @return the branches, each a sequence of `(re, im)` points, or `None` for an improper or
 *          non-rational `G`, or when no gain resolved
 */
def rootLocus(g: _Expression, v: _Variable, gains: Vector[Double]): Option[Vector[Vector[(Double, Double)]]] =
  coeffsOf(g, v).filter((num, den) => degreeOf(den) >= 1 && degreeOf(num) <= degreeOf(den)).flatMap { (num, den) =>
    val n = degreeOf(den)
    val samples = gains.flatMap { k =>
      val c = Vector.tabulate(n + 1)(i => den.lift(i).getOrElse(0.0) + k * num.lift(i).getOrElse(0.0))
      if degreeOf(c) != n then None
      else rootsAtScale(c).map(_.flatMap(_Complex.parts)).filter(_.size == n)
    }
    Option.when(samples.nonEmpty) {
      samples.tail.scanLeft(samples.head)(continueBranches).transpose
    }
  }

/** `next` reordered so that its `i`-th root continues `prev`'s `i`-th: the globally closest
 *  remaining pair is matched first. */
private def continueBranches(prev: Vector[(Double, Double)], next: Vector[(Double, Double)]): Vector[(Double, Double)] =
  val pairs = (for i <- prev.indices; j <- next.indices
               yield (math.hypot(prev(i)._1 - next(j)._1, prev(i)._2 - next(j)._2), i, j)).sortBy(_._1)
  val chosen = pairs.foldLeft(Map.empty[Int, Int]) { case (m, (_, i, j)) =>
    if m.contains(i) || m.values.exists(_ == j) then m else m + (i -> j)
  }
  prev.indices.toVector.map(i => next(chosen(i)))

/** The root-locus sweep behind the browser's `rlocus` command (F_0140): `G` with the
 *  environment's parameters substituted (never `v`), swept over `points` gains from `kMin` to
 *  `kMax` — geometric when `kMin > 0`, since a locus is read across decades of gain as a Bode plot
 *  is across decades of frequency, linear otherwise.
 *
 *  @param g      the open-loop transfer function
 *  @param v      the frequency variable
 *  @param kMin   the lowest gain
 *  @param kMax   the highest gain, above `kMin`
 *  @param points how many gains, at least 2
 *  @param env    the bindings for `G`'s parameters
 *  @return the branches with the open-loop poles and zeros as `(re, im)`, or `None`
 */
def rootLocusSweep(g: _Expression, v: _Variable, kMin: Double, kMax: Double, points: Int,
                   env: Environment): Option[(Vector[Vector[(Double, Double)]], Vector[(Double, Double)], Vector[(Double, Double)])] =
  val plant = resolved(g, v, env)
  val gains =
    if kMin > 0.0 then logGrid(kMin, kMax, points)
    else if points >= 2 && kMax > kMin then Vector.tabulate(points)(i => kMin + i * (kMax - kMin) / (points - 1))
    else Vector.empty
  def parts(vs: Option[Vector[_Value]]): Vector[(Double, Double)] = vs.getOrElse(Vector.empty).flatMap(_Complex.parts)
  rootLocus(plant, v, gains).map(bs => (bs, parts(poles(plant, v)), parts(zeros(plant, v))))

/** The Bode sweep: magnitude in decibels and **unwrapped** phase in degrees, over a
 *  logarithmically spaced grid (issue F_0004).
 *
 *  **Unwrapping is the substance here, not the grid.**  `bode`'s phase comes from `atan2`,
 *  whose principal value is `(-pi, pi]`, so a swept curve jumps by a full turn wherever it
 *  crosses the branch cut — `1/(s+1)^3` tends to `-270` degrees but a raw sweep reports `+90`.
 *  That jump is an artefact of the arctangent and not of the plant, and **every reader who
 *  plots a raw sweep inherits it**.  This accumulates a turn whenever consecutive samples
 *  differ by more than half a turn, which restores the continuous curve.
 *
 *  **dB and degrees rather than the raw pair**, because that is what a Bode plot *is*; the raw
 *  magnitude and radian phase stay available from [[bode]] itself, so nothing is lost.
 *
 *  **The one assumption worth stating**: unwrapping cannot distinguish a genuine half-turn
 *  step from a grid too coarse to resolve a fast one, so a sparse sweep across a lightly
 *  damped resonance can unwrap the wrong way.  That is inherent to unwrapping rather than
 *  particular to this implementation, and the remedy is points, not cleverness.
 *
 *  A frequency where the response is not finite — a pole on the imaginary axis — is dropped,
 *  mirroring `scalar.sample`; the unwrapping state carries across the gap, so the curve
 *  resumes rather than restarting.
 *
 *  @param g      the transfer function
 *  @param v      the frequency variable
 *  @param wMin   the lowest angular frequency, strictly positive
 *  @param wMax   the highest angular frequency
 *  @param points how many samples, at least 2
 *  @param env    bindings for any parameter the plant carries
 *  @return `(omega, magnitude-in-dB, unwrapped-phase-in-degrees)`, ascending in omega
 */
def frequencyResponse(g: _Expression, v: _Variable, wMin: Double, wMax: Double,
                      points: Int, env: Environment): Vector[(Double, Double, Double)] =
  // `turns` accumulates the whole turns atan2 removed, and `previous` holds the last RAW
  // phase -- comparing against the unwrapped one would compound the correction rather than
  // continue it.  A fold rather than a var, so the carried state is visible in the type.
  val (out, _, _) =
    logGrid(wMin, wMax, points)
      .foldLeft((Vector.empty[(Double, Double, Double)], 0.0, Double.NaN)) {
        case ((acc, turns, previous), w) =>
          responseAt(g, v, w, env) match
            case Some((mag, phase)) if mag > 0.0 && mag.isFinite =>
              val shifted =
                if previous.isNaN                   then turns
                else if phase - previous > math.Pi  then turns - 2 * math.Pi
                else if phase - previous < -math.Pi then turns + 2 * math.Pi
                else turns
              (acc :+ ((w, 20.0 * math.log10(mag), (phase + shifted) * 180.0 / math.Pi)),
               shifted, phase)
            // A zero magnitude is minus infinity decibels and a non-finite one has no decibel
            // value at all; either would be unplottable, so the point is dropped.
            case _ => (acc, turns, previous)
      }
  out

/** The Nyquist sweep: the same grid as [[frequencyResponse]], in rectangular coordinates.
 *
 *  One sweep presented two ways — the relationship [[bode]] and [[nyquist]] already have, and
 *  the reason both read the same [[logGrid]] rather than each building one.  Unwrapping does
 *  not arise here: a point in the plane is unchanged by adding a turn to its argument.
 *
 *  @param g      the transfer function
 *  @param v      the frequency variable
 *  @param wMin   the lowest angular frequency, strictly positive
 *  @param wMax   the highest angular frequency
 *  @param points how many samples, at least 2
 *  @param env    bindings for any parameter the plant carries
 *  @return `(real, imaginary)` for each grid frequency, non-finite points dropped
 */
def nyquistSweep(g: _Expression, v: _Variable, wMin: Double, wMax: Double,
                 points: Int, env: Environment): Vector[(Double, Double)] =
  logGrid(wMin, wMax, points).flatMap { w =>
    responseAt(g, v, w, env)
      .map((mag, phase) => (mag * math.cos(phase), mag * math.sin(phase)))
      .filter((re, im) => re.isFinite && im.isFinite)
  }

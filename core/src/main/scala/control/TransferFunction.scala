package it.grypho.scala.leonardo
package control

import core.*
import scalar.*


/** Transfer-function algebra, poles and zeros, stability, and frequency response.
 *
 *  Every function takes the **frequency variable explicitly** — `s` for continuous time, `z`
 *  for discrete — because a transfer function is an ordinary `Ratio` and carries no type to
 *  dispatch on (6.29 Decision A).
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
 *  every closed-loop result wrong for half its readers.  This is the lesson issue 6.26
 *  recorded for the spherical polar angle.  For positive feedback, negate `h`.
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
private def coeffsOf(e: _Expression, v: _Variable): Option[(Vector[Double], Vector[Double])] =
  rationalCoeffs(e, v).map((num, den) => cancelCommon(polyTrim(num), polyTrim(den)))

/** Divides a common polynomial factor out of `num`/`den`.
 *
 *  The gcd is computed in floating point, so the quotients are **verified**: a division is
 *  accepted only when both remainders are negligible against the operands. An ill-conditioned
 *  gcd therefore leaves the fraction unreduced — which loses nothing, since the uncancelled
 *  form is what the caller had — rather than silently deleting a genuine factor.
 */
private def cancelCommon(num: Vector[Double], den: Vector[Double]): (Vector[Double], Vector[Double]) =
  val g = polyGcd(num, den)
  if polyDegree(g) <= 0 then (num, den)
  else
    val (qn, rn)  = polyDivide(num, g)
    val (qd, rd)  = polyDivide(den, g)
    val scale     = math.max(1.0, (num ++ den).map(math.abs).max)
    val exact     = (rn ++ rd).forall(r => math.abs(r) < 1e-6 * scale)
    if exact then (polyTrim(qn), polyTrim(qd)) else (num, den)

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
  val terms = polyTrim(cs).zipWithIndex.collect {
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
  coeffsOf(g, v).flatMap((_, den) => polyRoots(den))

/** Zeros of `G`: the roots of its numerator.  See [[poles]] for the `_Value` result type.
 *
 *  @param g the transfer function
 *  @param v the frequency variable
 *  @return the zeros, or `None` when `G` is not rational in `v`
 */
def zeros(g: _Expression, v: _Variable): Option[Vector[_Value]] =
  coeffsOf(g, v).flatMap((num, _) => polyRoots(num))

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
 *  **Decided from the poles rather than from a Routh table**, a deliberate departure from the
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
  poles(g, v).flatMap { ps =>
    val parts = ps.map(realPart)
    Option.when(parts.forall(_.isDefined))(parts.flatten.forall(_ < -RationalEps))
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
  coeffsOf(g, v).map { (_, den) =>
    val cs = den.reverse                      // descending powers
    val (first, second) = cs.zipWithIndex.partition(_._2 % 2 == 0)
    val rows = scala.collection.mutable.ListBuffer(first.map(_._1), second.map(_._1))
    while rows.last.count(math.abs(_) > RationalEps) > 0 && rows.size < cs.size do
      val (a, b) = (rows(rows.size - 2), rows.last)
      val pivot  = b.headOption.getOrElse(0.0)
      rows += (if math.abs(pivot) < RationalEps then Vector.empty
               else Vector.tabulate(math.max(0, a.size - 1)) { i =>
                 (pivot * a.lift(i + 1).getOrElse(0.0) - a.head * b.lift(i + 1).getOrElse(0.0)) / pivot
               })
    rows.toVector
  }

/** Frequency response of `G` at angular frequency `w`: substitute `v -> i*w`.
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
  substitute(g, Map(v.variable -> _Complex.of(0, w))).eval(new Environment()) match
    case Right(c: _Complex) =>
      _Complex.parts(c).filter((re, im) => re.isFinite && im.isFinite)
        .map((re, im) => (math.hypot(re, im), math.atan2(im, re)))
    case Right(_Number(d)) if d.isFinite => Some((math.abs(d), if d < 0 then math.Pi else 0.0))
    case _                               => None

/** Nyquist point: the real and imaginary parts of `G(i*w)`.
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

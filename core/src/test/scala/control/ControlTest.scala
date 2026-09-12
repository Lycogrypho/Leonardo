package it.grypho.scala.leonardo
package control

import core.*
import scalar.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** Issue 6.29 — the control-theory domain.
 *
 *  Leans on **identities** wherever one exists, because they hold for every input and so catch
 *  a wrong construction rather than a wrong transcription: `feedback(G, 0) = G`, `series`
 *  associativity, `poles(feedback(G, H))` being the roots of `1 + G·H`, and the discretisation
 *  round-trip `d2c(c2d(G))`.  Closed-form comparisons are numeric at sample points, never
 *  through `toString`.
 */
class ControlTest extends AnyFlatSpec:

  val s: _Variable = _Variable("s")
  val z: _Variable = _Variable("z")
  val t: _Variable = _Variable("t")

  def parse(input: String): _Expression =
    val r = Parser.parse(input)
    assert(r.successful, s"parse failed for \"$input\": $r")
    r.get

  def evalAt(e: _Expression, v: _Variable, at: Double): Double =
    e.eval(new Environment().withBinding(v.variable, _Number(at))) match
      case Right(_Number(d)) => d
      case other             => fail(s"expected a number for $e at ${v.variable}=$at but got: $other")

  def assertAgrees(actual: _Expression, expected: _Expression, v: _Variable,
                   points: Seq[Double], tol: Double = 1e-7): Unit =
    for p <- points do
      val (a, b) = (evalAt(actual, v, p), evalAt(expected, v, p))
      assert(math.abs(a - b) < tol * math.max(1.0, math.abs(b)),
        s"at ${v.variable}=$p: expected $b but got $a  (from $actual)")

  val pts: Seq[Double] = Seq(0.7, 1.3, 2.5, 4.0)

  // --- slice 1: interconnection algebra ---

  "series(G, H)" should "multiply the transfer functions" in
  {
    assertAgrees(series(parse("1/(s+1)"), parse("2/(s+3)"), s), parse("2/((s+1)*(s+3))"), s, pts)
  }

  "parallel(G, H)" should "add them" in
  {
    assertAgrees(parallel(parse("1/(s+1)"), parse("1/(s+2)"), s), parse("1/(s+1) + 1/(s+2)"), s, pts)
  }

  "feedback(G, H)" should "be G/(1 + G*H), negative feedback by default" in
  {
    assertAgrees(feedback(parse("1/(s+1)"), _Number(1), s), parse("1/(s+2)"), s, pts)
  }

  "feedback(G, 0)" should "be G" in
  {
    // The identity: with no feedback path the loop is open. Holds for every G.
    val g = parse("(2*s+1)/(s^2+3*s+5)")
    assertAgrees(feedback(g, _Number(0), s), g, s, pts)
  }

  "series" should "be associative" in
  {
    val (g, h, k) = (parse("1/(s+1)"), parse("2/(s+3)"), parse("s/(s+4)"))
    assertAgrees(series(series(g, h, s), k, s), series(g, series(h, k, s), s), s, pts)
  }

  /** Real roots only. `_Number` is a WIDENING extractor but `_Complex` is a *sibling*, so a
   *  complex pole simply does not match here — which is the behaviour wanted: `poles` returns
   *  `_Value`s precisely so an oscillatory conjugate pair is not silently dropped.
   */
  def reals(vs: Vector[_Value]): Vector[Double] = vs.collect { case _Number(d) => d }

  "poles and zeros" should "be the denominator and numerator roots" in
  {
    val g = parse("(s+2)/((s+1)*(s+3))")
    assert(poles(g, s).map(p => reals(p).sorted).contains(Vector(-3.0, -1.0)))
    assert(zeros(g, s).map(z0 => reals(z0).sorted).contains(Vector(-2.0)))
  }

  "poles of a closed loop" should "be the roots of 1 + G*H" in
  {
    // The identity that ties feedback to the characteristic equation.
    val (g, h) = (parse("1/(s*(s+2))"), _Number(1))
    val closed = reals(poles(feedback(g, h, s), s).getOrElse(fail("expected poles")))
    // 1 + 1/(s(s+2)) = 0  ->  s^2 + 2s + 1 = 0  ->  double root at -1
    assert(closed.size == 2 && closed.forall(p => math.abs(p + 1.0) < 1e-6), s"got $closed")
  }

  "a complex conjugate pair" should "be returned, not dropped" in
  {
    // s^2 + 1 has poles at +-i. Reducing the result to Doubles would lose exactly the
    // oscillatory systems control theory is about, so the carrier is _Value.
    val ps = poles(parse("1/(s^2 + 1)"), s).getOrElse(fail("expected poles"))
    assert(ps.size == 2 && ps.forall(_.isInstanceOf[_Complex]), s"got $ps")
  }

  "dcgain" should "be G evaluated at s = 0" in
  {
    assert(dcgain(parse("5/(s+2)"), s).exists(g => math.abs(g - 2.5) < 1e-9))
  }

  "a non-rational G" should "be refused rather than approximated" in
  {
    // A dead-time term. Pade-approximating it is the USER's choice; `pade` exists for that.
    assert(poles(parse("exp(-2*s)/(s+1)"), s).isEmpty)
  }

  // --- slice 2: time response ---

  "step response of 1/(s+1)" should "be 1 - exp(-t)" in
  {
    assertAgrees(stepResponse(parse("1/(s+1)"), s, t), parse("1 - exp(-t)"), t, Seq(0.0, 0.5, 1.0, 2.0))
  }

  "impulse response of 1/(s+1)" should "be exp(-t)" in
  {
    assertAgrees(impulseResponse(parse("1/(s+1)"), s, t), parse("exp(-t)"), t, Seq(0.0, 0.5, 1.0, 2.0))
  }

  "an improper G" should "decline rather than drop the impulsive term" in
  {
    // deg num > deg den: the response carries a delta at t = 0 that the inverse-transform
    // tier does not represent, so it must not silently return only the smooth part.
    val e = _StepResponse(parse("s^2/(s+1)"), s, t)
    assert(e.eval(new Environment()) == Left(e))
  }

  // --- slice 3: stability ---

  "a Hurwitz denominator" should "be stable" in
  {
    // s^2 + 3s + 2 = (s+1)(s+2): both roots in the left half-plane.
    assert(isStable(parse("1/(s^2 + 3*s + 2)"), s).contains(true))
  }

  "a right-half-plane pole" should "be unstable" in
  {
    assert(isStable(parse("1/(s - 1)"), s).contains(false))
  }

  "a marginally stable system" should "not be reported stable" in
  {
    // Poles on the imaginary axis: s^2 + 1. Strict stability requires Re < 0.
    assert(isStable(parse("1/(s^2 + 1)"), s).contains(false))
  }

  "an undeterminable coefficient sign" should "stay undecided" in
  {
    // `a` is free, so the Routh test cannot decide -- the 3.2 boundary, not a false answer.
    assert(isStable(parse("1/(s^2 + a*s + 1)"), s).isEmpty)
  }

  // --- slice 4: frequency response ---

  "bode magnitude at w = 0" should "be the DC gain" in
  {
    val g = parse("5/(s+2)")
    val (mag, _) = bode(g, s, 0.0).getOrElse(fail("expected a response"))
    assert(math.abs(mag - 2.5) < 1e-9)
  }

  "bode of 1/(s+1) at the corner" should "be -3 dB and -45 degrees" in
  {
    val (mag, phase) = bode(parse("1/(s+1)"), s, 1.0).getOrElse(fail("expected a response"))
    assert(math.abs(mag - 1.0 / math.sqrt(2)) < 1e-9, s"magnitude $mag")
    assert(math.abs(phase + math.Pi / 4) < 1e-9, s"phase $phase")
  }

  "an integrator" should "have no finite response at w = 0" in
  {
    assert(bode(parse("1/s"), s, 0.0).isEmpty)
  }

  // --- slice 5: state-space ---

  "ss(A, B, C, D)" should "bundle the four matrices as a 1x4 row" in
  {
    val m = stateSpace(parse("[[0, 1], [-2, -3]]"), parse("[[0], [1]]"),
                       parse("[[1, 0]]"), parse("[[0]]"))
    m.eval(new Environment()) match
      case Left(r: matrix._Matrix) => assert(r.rows == 1 && r.cols == 4)
      case other                   => fail(s"unexpected: $other")
  }

  "ctrb of a controllable pair" should "have full rank" in
  {
    // A = [[0,1],[-2,-3]], B = [[0],[1]] -> [B AB] = [[0,1],[1,-3]], rank 2.
    assert(controllable(parse("[[0, 1], [-2, -3]]"), parse("[[0], [1]]")).contains(true))
  }

  "ctrb of an uncontrollable pair" should "be rank deficient" in
  {
    // B in an invariant subspace of a diagonal A: the second mode is unreachable.
    assert(controllable(parse("[[1, 0], [0, 2]]"), parse("[[1], [0]]")).contains(false))
  }

  "controllability" should "not depend on the units B is expressed in" in
  {
    // Scaling B by k != 0 spans the same reachable subspace, so the verdict must not move:
    // millivolts and volts describe one system. The det(M*M^T) test fails this outright --
    // a determinant scales like ||M||^(2n), so 1e-3 on a 2-state system lands the value at
    // 1e-12, under any fixed threshold, and reports a perfectly controllable plant as not.
    val a = parse("[[0, 1], [-2, -3]]")
    for scale <- Seq("1", "0.001", "1000") do
      assert(controllable(a, parse(s"[[0], [$scale]]")).contains(true), s"B scaled by $scale")
  }

  it should "not depend on the units A is expressed in either" in
  {
    // The same argument on the other operand: A*k changes the time scale, not reachability.
    for scale <- Seq("0.001", "1000") do
      assert(controllable(parse(s"[[0, $scale], [0, 0]]"), parse("[[0], [1]]")).contains(true),
             s"A scaled by $scale")
  }

  "observability" should "be unit-invariant by the same duality" in
  {
    // observable(A, C) is controllable(A', C'), so it inherits the property rather than
    // needing its own fix -- which is the point of defining it that way.
    val a = parse("[[0, 1], [-2, -3]]")
    for scale <- Seq("1", "0.001", "1000") do
      assert(observable(a, parse(s"[[$scale, 0]]")).contains(true), s"C scaled by $scale")
    assert(observable(parse("[[1, 0], [0, 2]]"), parse("[[0.001, 0]]")).contains(false))
  }

  // --- slice 6: discrete time (needs 6.33 and 6.39) ---

  "c2d by Tustin" should "substitute s = (2/Ts)*(z-1)/(z+1)" in
  {
    val gd = c2d(parse("1/(s+1)"), s, z, 0.1, Tustin).getOrElse(fail("expected a discretisation"))
    // At z = 1 the substitution gives s = 0, so the discrete DC gain matches the continuous one.
    assert(math.abs(evalAt(gd, z, 1.0) - 1.0) < 1e-9)
  }

  "d2c(c2d(G))" should "return to G" in
  {
    // The round-trip identity: Tustin is its own inverse under the reverse substitution.
    val g  = parse("2/(s+3)")
    val gd = c2d(g, s, z, 0.05, Tustin).getOrElse(fail("expected a discretisation"))
    val gc = d2c(gd, z, s, 0.05, Tustin).getOrElse(fail("expected a continuous form"))
    assertAgrees(gc, g, s, pts, 1e-6)
  }

  "discrete stability" should "use the unit circle, not the left half-plane" in
  {
    // z = 0.5 is stable in discrete time; the same value as a CONTINUOUS pole would not be.
    assert(isStableDiscrete(parse("1/(z - 0.5)"), z).contains(true))
    assert(isStableDiscrete(parse("1/(z - 1.5)"), z).contains(false))
  }

  "exact state-space discretisation" should "use expm and handle a singular A" in
  {
    // A = [[0,1],[0,0]] is singular -- an integrator, entirely ordinary -- so the naive
    // B_d = A^-1 (A_d - I) B is undefined and the block-matrix form is required.
    val (ad, bd) = c2dExact(parse("[[0, 1], [0, 0]]"), parse("[[0], [1]]"), 0.5)
      .getOrElse(fail("expected an exact discretisation"))
    // A_d = I + A*Ts (A is nilpotent), B_d = [Ts^2/2, Ts]^T
    assert(math.abs(ad(0, 1) - 0.5) < 1e-9, s"A_d = $ad")
    assert(math.abs(bd(0, 0) - 0.125) < 1e-9 && math.abs(bd(1, 0) - 0.5) < 1e-9, s"B_d = $bd")
  }

  // --- parser and round-trip ---

  "the control functions" should "parse and round-trip" in
  {
    for src <- List("series(a, b, s)", "parallel(a, b, s)", "feedback(a, b, s)",
                    "step(a, s, t)", "impulse(a, s, t)") do
      val first = parse(src)
      assert(parse(first.toString) == first, s"round-trip failed for $src: ${first.toString}")
  }

  "bare control keywords" should "be reserved words" in
  {
    for w <- List("series", "parallel", "feedback", "impulse", "routh") do
      assert(!Parser.parse(w).successful, s"'$w' should be reserved")
  }

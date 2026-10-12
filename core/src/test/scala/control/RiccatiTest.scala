package it.grypho.scala.leonardo
package control

import core.*
import matrix._Matrix
import parser.Parser
import probability.{drawSamples, _Distribution}
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

/** F_0065 (MPC T3-01): the discrete algebraic Riccati equation, LQR, the Stein equation and the
 *  Kalman filter.
 *
 *  Every answer is checked against what defines it, computed here independently of the solver: the
 *  Riccati residual, the closed loop's spectral radius, the Stein residual, and for the filter the
 *  fixed point of its own covariance recursion. The acceptance values are the requirement's.
 */
class RiccatiTest extends AnyFlatSpec:

  private val env = new Environment()

  private def m(rows: Int, cols: Int, xs: Double*) = _MatrixValue(rows, cols, xs.toArray)
  private def eye(n: Int) = _MatrixValue.identity(n)
  private def frob(a: _MatrixValue) = math.sqrt(a.toVector.map(x => x * x).sum)
  private def minus(a: _MatrixValue, b: _MatrixValue) = a.add(b.scale(-1))

  private def random(rows: Int, cols: Int, seed: Long): _MatrixValue =
    val rng = new Random(seed)
    _MatrixValue(rows, cols, Array.fill(rows * cols)(rng.nextDouble() * 2 - 1))

  private def parse(src: String): _Expression =
    val r = Parser.parse(src)
    assert(r.successful, s"parse failed for \"$src\": $r")
    r.get

  /** The Riccati residual AᵀPA − P − AᵀPB(R + BᵀPB)⁻¹BᵀPA + Q, written out here. */
  private def residual(a: _MatrixValue, b: _MatrixValue, q: _MatrixValue, r: _MatrixValue, p: _MatrixValue): _MatrixValue =
    val at  = a.transpose
    val s   = r.add(b.transpose.multiply(p).multiply(b)).inverse.getOrElse(fail("R + BᵀPB singular"))
    val apb = at.multiply(p).multiply(b)
    minus(at.multiply(p).multiply(a).add(q), p.add(apb.multiply(s).multiply(apb.transpose)))

  /** Stable when a high power decays: ‖M^(2^20)‖ ≪ 1 forces ρ(M) < 1 (ρ ≤ ‖Mᵏ‖^(1/k)). Written
   *  here rather than through eigenDecompose so the check shares no code with an eigensolver. */
  private def decays(a: _MatrixValue): Boolean =
    frob((1 to 20).foldLeft(a)((x, _) => x.multiply(x))) < 1e-6

  private def solved[A](e: Either[RiccatiFailure, A]): A = e.fold(f => fail(s"declined: $f"), identity)

  // ── dare / dlqr ──

  "dare" should "give T3-01's acceptance P = (1 + √5)/2 for A = B = Q = R = 1" in
  {
    val p = solved(dare(m(1, 1, 1), m(1, 1, 1), m(1, 1, 1), m(1, 1, 1), env))
    assert(math.abs(p(0, 0) - (1 + math.sqrt(5)) / 2) < 1e-12)
  }

  it should "solve random stabilisable systems: residual small, P symmetric, A − BK stable" in
  {
    for (n, k, seed) <- List((2, 1, 1L), (3, 1, 2L), (4, 2, 3L), (6, 2, 4L), (8, 3, 5L)) do
      val a = random(n, n, seed).scale(1.5)          // typically unstable
      val b = random(n, k, seed + 50)
      val q = eye(n)
      val r = eye(k)
      val (gain, p) = solved(dlqr(a, b, q, r, env))
      assert(frob(residual(a, b, q, r, p)) <= 1e-9 * math.max(1.0, frob(p)), s"n = $n residual")
      assert(frob(minus(p, p.transpose)) == 0.0, s"n = $n: P not symmetric")
      assert(decays(minus(a, b.multiply(gain))), s"n = $n: A − BK not stable")
  }

  "dlqr" should "give the gain of the law u = −K·x, K = (R + BᵀPB)⁻¹BᵀPA" in
  {
    val (gain, p) = solved(dlqr(m(1, 1, 1), m(1, 1, 1), m(1, 1, 1), m(1, 1, 1), env))
    assert(math.abs(gain(0, 0) - p(0, 0) / (1 + p(0, 0))) < 1e-12)       // 0.618…
  }

  "dare" should "decline when (A, B) is not stabilisable" in
  {
    // The mode at 2 is unstable and B cannot reach it.
    val f = dare(m(2, 2, 2, 0, 0, 0.5), m(2, 1, 0, 1), eye(2), eye(1), env)
    assert(f == Left(RiccatiFailure.NoStabilisingSolution), s"$f")
  }

  it should "decline a solution that solves the equation but does not stabilise (the closed-loop check)" in
  {
    // The mode at 2 is neither reachable nor penalised: the iteration converges to P = diag(0, p),
    // which satisfies the equation exactly and leaves A − BK with an eigenvalue at 2.
    val f = dare(m(2, 2, 2, 0, 0, 0.5), m(2, 1, 0, 1), m(2, 2, 0, 0, 0, 1), eye(1), env)
    assert(f == Left(RiccatiFailure.NoStabilisingSolution), s"$f")
  }

  it should "decline when (A, Q^½) is not detectable, as T3-01 asks" in
  {
    // a = 2, b = 1, q = 0, r = 1: the equation p = 4p − 4p²/(1 + p) has p = 0 and p = 3, and only
    // p = 3 stabilises. The doubling starts from H₀ = Q = 0 and never leaves it, so it finds the
    // non-stabilising p = 0, which the closed-loop check refuses: an unpenalised unstable mode
    // is declined, which is the requirement's own rule.
    assert(dare(m(1, 1, 2), m(1, 1, 1), m(1, 1, 0), m(1, 1, 1), env) == Left(RiccatiFailure.NoStabilisingSolution))
  }

  it should "refuse an R that is not positive definite and a Q that is not symmetric or not semidefinite" in
  {
    val (a, b) = (m(1, 1, 1), m(1, 1, 1))
    assert(dare(a, b, m(1, 1, 1), m(1, 1, 0), env) == Left(RiccatiFailure.NotDefinite("R")))
    assert(dare(a, b, m(1, 1, -1), m(1, 1, 1), env) == Left(RiccatiFailure.NotDefinite("Q")))
    assert(dare(eye(2), eye(2), m(2, 2, 1, 2, 0, 1), eye(2), env) == Left(RiccatiFailure.NotSymmetric("Q")))
  }

  it should "refuse shapes that do not conform" in
  {
    assert(dare(eye(2), eye(3), eye(2), eye(2), env).isLeft)
    assert(dare(eye(2), m(2, 1, 1, 1), eye(3), eye(1), env).isLeft)
  }

  // ── dlyap ──

  "dlyap" should "solve the Stein equation A·X·Aᵀ − X + Q = 0, and agree with solve" in
  {
    val a = m(2, 2, 0.5, 0, 0, 0.2)
    val x = solved(dlyap(a, eye(2), env))
    assert(math.abs(x(0, 0) - 1 / (1 - 0.25)) < 1e-12 && math.abs(x(1, 1) - 1 / (1 - 0.04)) < 1e-12)
    for seed <- 1L to 4L do
      val raw = random(4, 4, seed)
      val b   = raw.scale(0.9 / frob(raw))          // ρ ≤ ‖·‖_F = 0.9: stable by construction
      val q = { val c = random(4, 4, seed + 9); c.multiply(c.transpose) }
      val y = solved(dlyap(b, q, env))
      val res = minus(b.multiply(y).multiply(b.transpose).add(q), y)
      assert(frob(res) <= 1e-10 * frob(y), s"seed $seed")
  }

  it should "decline an A that is not stable" in
  {
    assert(dlyap(m(1, 1, 1.5), m(1, 1, 1), env) == Left(RiccatiFailure.NoStabilisingSolution))
  }

  // ── the Kalman filter ──

  "kalmanStep" should "converge to a constant state under noise (T3-01's acceptance)" in
  {
    // x = 3 forever; y = x + v, v ~ N(0, 0.5²), drawn reproducibly (F_0067).
    val noise = drawSamples(_Distribution.of(probability.DistKind.Normal, Vector(0.0, 0.5)).getOrElse(fail("normal")),
                            400, 2026L).fold(f => fail(s"$f"), identity)
    val (a, b, c, d) = (m(1, 1, 1), m(1, 1, 0), m(1, 1, 1), m(1, 1, 0))
    val (qw, rv)     = (m(1, 1, 0), m(1, 1, 0.25))
    val (xFinal, pFinal) = noise.foldLeft((m(1, 1, 0), m(1, 1, 100))) { case ((x, p), v) =>
      solved(kalmanStep(x, p, m(1, 1, 0), m(1, 1, 3 + v), a, b, c, d, qw, rv))
    }
    assert(math.abs(xFinal(0, 0) - 3) <= 5 * 0.5 / math.sqrt(noise.size), s"x̂ = ${xFinal(0, 0)}")
    assert(pFinal(0, 0) > 0 && pFinal(0, 0) < 0.25 / noise.size * 1.01, s"P = ${pFinal(0, 0)}")
  }

  "kalmanGain" should "be the fixed point of kalmanStep's covariance recursion" in
  {
    val a  = m(2, 2, 0.9, 0.2, -0.1, 0.8)
    val c  = m(1, 2, 1, 0)
    val qw = m(2, 2, 0.1, 0, 0, 0.05)
    val rv = m(1, 1, 0.3)
    val (gain, pSteady) = solved(kalmanGain(a, c, qw, rv, env))
    // Iterate the covariance alone (it does not depend on the data) from P = I.
    val pIter = (1 to 400).foldLeft(eye(2)) { (p, _) =>
      solved(kalmanStep(m(2, 1, 0, 0), p, m(1, 1, 0), m(1, 1, 0), a, m(2, 1, 0, 0), c, m(1, 1, 0), qw, rv))._2
    }
    assert(frob(minus(pIter, pSteady)) <= 1e-9 * frob(pSteady))
    val s = c.multiply(pSteady).multiply(c.transpose).add(rv).inverse.getOrElse(fail("S"))
    assert(frob(minus(gain, pSteady.multiply(c.transpose).multiply(s))) <= 1e-12)
  }

  // ── the nodes ──

  "the prompt forms" should "answer dare as a matrix and dlqr / kalmanGain / kalmanStep as rows" in
  {
    parse("dare(1, 1, 1, 1)").eval(env) match
      case Right(p: _MatrixValue) => assert(math.abs(p(0, 0) - 1.6180339887) < 1e-9)
      case other                  => fail(s"$other")
    parse("dlqr(1, 1, 1, 1)").eval(env) match
      case Left(_Matrix(1, 2, Vector(k: _MatrixValue, _: _MatrixValue))) => assert(math.abs(k(0, 0) - 0.6180339887) < 1e-9)
      case other => fail(s"$other")
    assert(parse("dlyap([[0.5, 0], [0, 0.2]], eye(2))").eval(env).isRight)
    parse("kalmanGain(1, 1, 0.1, 1)").eval(env) match
      case Left(_Matrix(1, 2, _)) => ()
      case other                  => fail(s"$other")
    parse("kalmanStep(0, 1, 0, 2, stateSpace(1, 0, 1, 0), 0, 1)").eval(env) match
      case Left(_Matrix(1, 2, Vector(x: _MatrixValue, p: _MatrixValue))) =>
        assert(math.abs(x(0, 0) - 1.0) < 1e-12 && math.abs(p(0, 0) - 0.5) < 1e-12)  // gain 1/2
      case other => fail(s"$other")
  }

  it should "stay as written when declined, with the reason, and round-trip" in
  {
    val e = parse("dare([[2, 0], [0, 0.5]], [[0], [1]], eye(2), 1)")
    assert(e.eval(env) == Left(e))
    e match
      case n: _Riccati => assert(n.detailed(env).left.toOption.contains(RiccatiFailure.NoStabilisingSolution))
      case other       => fail(s"parsed as $other")
    for src <- List("dare(A, B, Q, R)", "dlqr(A, B, Q, R)", "dlyap(A, Q)", "kalmanGain(A, C, W, V)",
                    "kalmanStep(x, P, u, y, sys, W, V)") do
      val p = parse(src)
      assert(parse(p.toString) == p, src)
    for w <- List("dare", "dlqr", "dlyap", "kalmanGain", "kalmanStep") do
      assert(Parser.ReservedWords.contains(w), w)
  }

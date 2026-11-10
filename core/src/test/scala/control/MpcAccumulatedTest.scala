package it.grypho.scala.leonardo
package control

import core.*
import org.scalatest.flatspec.AnyFlatSpec

/** F_0328 (APAC L_0012) -- a prediction whose input is already an increment of an accumulated input:
 *  `MpcBounds.u` bounds the position, `MpcBounds.du` the moves.
 *  F_0329 (APAC L_0013) -- a weight and a target on that position in the increment forms. */
class MpcAccumulatedTest extends AnyFlatSpec:

  private val env = new Environment(Environment.DefaultPrecision, Map.empty)
  private val lim = env.limits
  private def m(rows: Int, cols: Int, xs: Double*) = _MatrixValue(rows, cols, xs.toArray)
  private def mul(a: _MatrixValue, v: Vector[Double]) =
    Vector.tabulate(a.rows)(i => (0 until a.cols).map(j => a(i, j) * v(j)).sum)
  private def add(a: Vector[Double], b: Vector[Double]) = a.zip(b).map(_ + _)
  private def quad(w: _MatrixValue, v: Vector[Double]) = v.zip(mul(w, v)).map(_ * _).sum
  private def ok[A](r: Either[MpcFailure, A]): A = r.fold(e => fail(e.toString), identity)
  private def close(a: Seq[Double], b: Seq[Double], tol: Double) =
    a.size == b.size && a.zip(b).forall((p, q) => math.abs(p - q) <= tol)

  // a stable plant with DC gain 0.5, and the same plant with u(k-1) in the state and Δu as its input
  private val a  = m(2, 2, 0.8, 0.1, 0.0, 0.9)
  private val b  = m(2, 1, 0.0, 0.1)
  private val c  = m(1, 2, 1.0, 0.0)
  private val aa = m(3, 3, 0.8, 0.1, 0.0,  0.0, 0.9, 0.1,  0.0, 0.0, 1.0)
  private val ba = m(3, 1, 0.0, 0.1, 1.0)
  private val ca = m(1, 3, 1.0, 0.0, 0.0)
  private val (q, r) = (m(1, 1, 5.0), m(1, 1, 0.05))

  // ── F_0328: the form ────────────────────────────────────────────────────────────────────────────

  "mpcAccumulatedPrediction" should "predict as the model does, the increments zero past Nc" in {
    val (np, nc) = (6, 3)
    val pr = ok(mpcAccumulatedPrediction(aa, ba, ca, np, nc, lim))
    val x0 = Vector(0.3, -0.2, 0.4)
    val du = Vector(Vector(0.5), Vector(-0.25), Vector(0.1))
    val moves = (0 until np).map(k => if k < nc then du(k) else Vector(0.0))
    val ys = moves.scanLeft(x0)((x, d) => add(mul(aa, x), mul(ba, d))).tail.map(x => mul(ca, x).head)
    val pred = add(mul(pr.phi, x0), mul(pr.gamma, du.flatten))
    assert(close(ys, pred, 1e-12))
  }

  it should "differ from the absolute prediction of the same model, which holds its last move, unless Nc = Np" in {
    val held = ok(mpcPrediction(aa, ba, ca, 6, 3, lim))
    val zero = ok(mpcAccumulatedPrediction(aa, ba, ca, 6, 3, lim))
    assert(!close(held.gamma.toVector, zero.gamma.toVector, 1e-9))     // a held increment is a ramp
    val full = ok(mpcPrediction(aa, ba, ca, 4, 4, lim))
    assert(close(full.gamma.toVector, ok(mpcAccumulatedPrediction(aa, ba, ca, 4, 4, lim)).gamma.toVector, 0.0))
  }

  "mpcQp on an accumulated prediction" should "be the incremental form's programme of the plant, bounds included" in {
    val (np, nc) = (6, 3)
    val acc = ok(mpcAccumulatedPrediction(aa, ba, ca, np, nc, lim))
    val inc = ok(mpcIncrementalPrediction(a, b, c, np, nc, lim))
    val (x0, uPrev) = (Vector(0.2, -0.1), Vector(0.15))
    val bounds = MpcBounds(u = Some((Vector(-0.3), Vector(0.4))), du = Some((Vector(-0.2), Vector(0.2))),
                           y = Some((Vector(-1.0), Vector(0.3))))
    val w = MpcWeights(Vector(q), Vector(r))
    val qa = ok(mpcQp(acc, w, Vector(1.0), x0 ++ uPrev, uPrev, bounds, lim))
    val qi = ok(mpcQp(inc, w, Vector(1.0), x0, uPrev, bounds, lim))
    assert(qa.incremental)
    assert(close(qa.h.toVector, qi.h.toVector, 1e-12) && close(qa.f, qi.f, 1e-12))
    assert(qa.lb == qi.lb && qa.ub == qi.ub)
    assert(qa.lb.take(nc) == Vector.fill(nc)(-0.2) && qa.ub.take(nc) == Vector.fill(nc)(0.2))  // du is the direct bound
    (qa.ineq, qi.ineq) match
      case (Some((ma, ra)), Some((mi, ri))) =>
        assert(ma.rows == mi.rows && close(ma.toVector, mi.toVector, 1e-12) && close(ra, ri, 1e-12))
        assert(ma.rows == 2 * np + 2 * nc)                               // the outputs, then the positions
      case other => fail(s"rows expected: $other")
    val (sa, si) = (ok(qa.solve(Set.empty, env)), ok(qi.solve(Set.empty, env)))
    assert(close(sa.moves.flatten, si.moves.flatten, 1e-7))
    assert(close(sa.input, si.input, 1e-7))
    assert(close(sa.input, add(uPrev, sa.moves.head), 0.0))              // u(k) = u(k-1) + Δu(k)
  }

  "MpcBounds.u" should "hold the actuator position in a closed loop on an increment-input model" in {
    // the reference needs u = 2; the actuator stops at 0.5, where the plant settles at 0.25
    val (np, nc) = (8, 3)
    val bounds = MpcBounds(u = Some((Vector(-0.5), Vector(0.5))))
    val w = MpcWeights(Vector(q), Vector(r))
    def run(prediction: MpcPredictionMatrices, position: MpcSolution => Vector[Double] => Vector[Double]) =
      (0 until 150).foldLeft((Vector(0.0, 0.0), Vector(0.0), Vector.empty[Double])) { case ((x, uPrev, seen), _) =>
        val sol = ok(ok(mpcQp(prediction, w, Vector(1.0), x ++ uPrev, uPrev, bounds, lim)).solve(Set.empty, env))
        val u = position(sol)(uPrev)
        (add(mul(a, x), mul(b, u)), u, seen :+ u.head)
      }
    val (xEnd, _, positions) = run(ok(mpcAccumulatedPrediction(aa, ba, ca, np, nc, lim)), sol => _ => sol.input)
    assert(positions.forall(u => u >= -0.5 - 1e-9 && u <= 0.5 + 1e-9), positions.max.toString)
    assert(math.abs(positions.last - 0.5) < 1e-6 && math.abs(xEnd.head - 0.25) < 1e-4)
    // read as an absolute prediction, the same bounds hold the increments and nothing holds the position
    val (_, _, loose) = run(ok(mpcPrediction(aa, ba, ca, np, nc, lim)), sol => uPrev => add(uPrev, sol.input))
    assert(loose.max > 0.6)
  }

  it should "hold it on arx2nmssIncremental's model, whose state is measured" in {
    // y(k) = 0.8 y(k-1) + 0.3 u(k-1) + 0.2 u(k-2): DC gain 2.5, state [Δy(k), Δu(k-1), y(k)]
    val model = ArxMimoModel(Vector(m(1, 1, 0.8)), Vector(m(1, 1, 0.3), m(1, 1, 0.2)))
    val (an, bn, cn, _) = arx2nmssIncremental(model, 1).fold(e => fail(e.toString), identity)
    assert(an.rows == 3)
    val pr = ok(mpcAccumulatedPrediction(an, bn, cn, 10, 3, lim))
    val bounds = MpcBounds(u = Some((Vector(0.0), Vector(1.0))), du = Some((Vector(-0.4), Vector(0.4))))
    val w = MpcWeights(Vector(m(1, 1, 1.0)), Vector(m(1, 1, 0.05)))
    // the histories newest first: y(k), y(k-1) and u(k-1), u(k-2)
    val (ys, us) = (0 until 80).foldLeft((Vector(0.0, 0.0), Vector(0.0, 0.0))) { case ((y, u), _) =>
      val state = Vector(y(0) - y(1), u(0) - u(1), y(0))
      val sol = ok(ok(mpcQp(pr, w, Vector(5.0), state, Vector(u(0)), bounds, lim)).solve(Set.empty, env))
      val now = sol.input.head
      assert(now >= -1e-9 && now <= 1.0 + 1e-9 && math.abs(now - u(0)) <= 0.4 + 1e-9, s"u = $now after ${u(0)}")
      ((0.8 * y(0) + 0.3 * now + 0.2 * u(0)) +: y.take(1), now +: u.take(1))
    }
    assert(math.abs(us.head - 1.0) < 1e-6 && math.abs(ys.head - 2.5) < 1e-4)
  }

  "a terminal cost" should "need a prediction in the same form" in {
    val (np, nc) = (5, 2)
    val acc  = ok(mpcAccumulatedPrediction(aa, ba, ca, np, nc, lim))
    val term = ok(mpcAccumulatedTerminalPrediction(aa, ba, np, nc, lim))
    val pw   = _MatrixValue.identity(3)
    val x0   = Vector(0.2, -0.1, 0.3)
    def build(t: MpcPredictionMatrices) =
      mpcQp(acc, MpcWeights(Vector(q), Vector(r), Some(MpcTerminal(pw, t))), Vector(1.0), x0, Vector(0.3), MpcBounds(), lim)
    val qp = ok(build(term))
    // x(k+Np) as the model gives it, the increments zero past Nc
    val du = Vector(0.4, -0.3)
    val xN = (0 until np).foldLeft(x0)((x, k) => add(mul(aa, x), mul(ba, Vector(if k < nc then du(k) else 0.0))))
    assert(close(add(mul(term.phi, x0), mul(term.gamma, du)), xN, 1e-12))
    def cost(z: Vector[Double]): Double =
      val traj = (0 until np).scanLeft(x0)((x, k) => add(mul(aa, x), mul(ba, Vector(if k < nc then z(k) else 0.0)))).tail
      traj.map(x => 5.0 * math.pow(mul(ca, x).head - 1.0, 2)).sum + 0.05 * z.map(d => d * d).sum + quad(pw, traj.last)
    def modelCost(z: Vector[Double]) = 0.5 * quad(qp.h, z) + qp.f.zip(z).map(_ * _).sum
    val (z1, z2) = (Vector(0.3, -0.4), Vector(-1.1, 0.8))
    assert(math.abs((cost(z1) - cost(z2)) - (modelCost(z1) - modelCost(z2))) < 1e-10)
    for other <- Seq(ok(mpcTerminalPrediction(aa, ba, np, nc, incremental = false, lim)),
                     ok(mpcTerminalPrediction(aa, ba, np, nc, incremental = true, lim))) do
      assert(build(other).left.exists(_.isInstanceOf[MpcFailure.InvalidInput]))
  }

  "an input target" should "be refused for an accumulated prediction, whose moves are increments" in {
    val acc = ok(mpcAccumulatedPrediction(aa, ba, ca, 4, 2, lim))
    val w = MpcWeights(Vector(q), Vector(r), None, Some(Vector(0.5)))
    assert(mpcQp(acc, w, Vector(1.0), Vector(0.0, 0.0, 0.0), Vector(0.0), MpcBounds(), lim)
      .left.exists(_.isInstanceOf[MpcFailure.InvalidInput]))
  }

  // ── F_0329: the position target ─────────────────────────────────────────────────────────────────

  // two inputs, one output
  private val a2 = m(2, 2, 0.7, 0.1, 0.0, 0.5)
  private val b2 = m(2, 2, 0.2, 0.0, 0.1, 0.3)
  private val c2 = m(1, 2, 1.0, 0.5)

  "a position target" should "add the cost it states to the incremental programme" in {
    val (np, nc) = (4, 3)
    val inc = ok(mpcIncrementalPrediction(a2, b2, c2, np, nc, lim))
    val ss  = Vector(m(2, 2, 1.0, 0.2, 0.2, 0.5), m(2, 2, 0.3, 0.0, 0.0, 0.0), m(2, 2, 2.0, -0.5, -0.5, 1.0))
    val us  = Vector(0.4, -0.2, 0.1, 0.3, -0.6, 0.2)
    val rr  = m(2, 2, 0.1, 0.0, 0.0, 0.2)
    val (x0, uPrev, ref) = (Vector(0.2, -0.3), Vector(0.25, -0.1), Vector(0.8))
    val w = MpcWeights(Vector(m(1, 1, 2.0)), Vector(rr), None, None, Some(MpcPositionTarget(ss, us)))
    val qp = ok(mpcQp(inc, w, ref, x0, uPrev, MpcBounds(), lim))
    def cost(z: Vector[Double]): Double =
      val dus = z.grouped(2).toVector
      val positions = dus.scanLeft(uPrev)(add).tail
      val applied = (0 until np).map(k => positions(math.min(k, nc - 1)))
      val traj = applied.scanLeft(x0)((x, u) => add(mul(a2, x), mul(b2, u))).tail
      traj.map(x => 2.0 * math.pow(mul(c2, x).head - ref.head, 2)).sum + dus.map(quad(rr, _)).sum +
        positions.indices.map(j => quad(ss(j), positions(j).zip(us.slice(2 * j, 2 * j + 2)).map(_ - _))).sum
    def modelCost(z: Vector[Double]) = 0.5 * quad(qp.h, z) + qp.f.zip(z).map(_ * _).sum
    val (z1, z2) = (Vector(0.3, -0.4, 0.1, 0.2, -0.5, 0.6), Vector(-1.1, 0.8, 0.4, -0.3, 0.2, 0.1))
    assert(math.abs((cost(z1) - cost(z2)) - (modelCost(z1) - modelCost(z2))) < 1e-10)
    // one weight and a held target are the same cost at every step
    val heldW = MpcWeights(Vector(m(1, 1, 2.0)), Vector(rr), None, None, Some(MpcPositionTarget(Vector(ss.head), us.take(2))))
    val each  = MpcWeights(Vector(m(1, 1, 2.0)), Vector(rr), None, None,
                           Some(MpcPositionTarget(Vector.fill(nc)(ss.head), Vector.fill(nc)(us.take(2)).flatten)))
    val (qh, qe) = (ok(mpcQp(inc, heldW, ref, x0, uPrev, MpcBounds(), lim)), ok(mpcQp(inc, each, ref, x0, uPrev, MpcBounds(), lim)))
    assert(qh.h.toVector == qe.h.toVector && qh.f == qe.f)
  }

  it should "be the absolute form's input target in the other parameterisation" in {
    // ‖u − u_s‖²_S over the positions is one cost, decided as inputs or as increments
    val (np, nc) = (5, 3)
    val s  = m(2, 2, 0.4, 0.1, 0.1, 0.3)
    val us = Vector(0.3, -0.2)
    val (x0, uPrev, ref) = (Vector(0.1, 0.2), Vector(0.05, 0.1), Vector(0.6))
    val qq = Vector(m(1, 1, 3.0))
    val abs = ok(mpcPrediction(a2, b2, c2, np, nc, lim))
    val inc = ok(mpcIncrementalPrediction(a2, b2, c2, np, nc, lim))
    val ua = ok(ok(mpcQp(abs, MpcWeights(qq, Vector(s), None, Some(us)), ref, x0, uPrev, MpcBounds(), lim)).solve(Set.empty, env))
    val wi = MpcWeights(qq, Vector(m(2, 2, 0.0, 0.0, 0.0, 0.0)), None, None, Some(MpcPositionTarget(Vector(s), us)))
    val di = ok(ok(mpcQp(inc, wi, ref, x0, uPrev, MpcBounds(), lim)).solve(Set.empty, env))
    val positions = di.moves.scanLeft(uPrev)(add).tail
    assert(close(positions.flatten, ua.moves.flatten, 1e-6), s"$positions vs ${ua.moves}")
  }

  it should "be the same cost on an accumulated prediction" in {
    val (np, nc) = (5, 2)
    val acc = ok(mpcAccumulatedPrediction(aa, ba, ca, np, nc, lim))
    val inc = ok(mpcIncrementalPrediction(a, b, c, np, nc, lim))
    val w = MpcWeights(Vector(q), Vector(r), None, None, Some(MpcPositionTarget(Vector(m(1, 1, 0.7)), Vector(0.2))))
    val (x0, uPrev) = (Vector(0.2, -0.1), Vector(0.15))
    val qa = ok(mpcQp(acc, w, Vector(1.0), x0 ++ uPrev, uPrev, MpcBounds(), lim))
    val qi = ok(mpcQp(inc, w, Vector(1.0), x0, uPrev, MpcBounds(), lim))
    assert(close(qa.h.toVector, qi.h.toVector, 1e-12) && close(qa.f, qi.f, 1e-12))
    val plain = ok(mpcQp(inc, MpcWeights(Vector(q), Vector(r)), Vector(1.0), x0, uPrev, MpcBounds(), lim))
    assert(!close(qi.h.toVector, plain.h.toVector, 1e-3))
  }

  it should "settle the spare degree of freedom of a plant with more inputs than setpoints" in {
    // x⁺ = 0.5x + u1 + u2: any u1 + u2 = 0.5 holds y = 1
    val (ap, bp, cp) = (m(1, 1, 0.5), m(1, 2, 1.0, 1.0), m(1, 1, 1.0))
    val inc = ok(mpcIncrementalPrediction(ap, bp, cp, 6, 2, lim))
    val rr  = m(2, 2, 0.1, 0.0, 0.0, 0.1)
    def run(target: Option[MpcPositionTarget]) =
      (0 until 300).foldLeft((0.0, Vector(0.0, 0.0))) { case ((x, uPrev), _) =>
        val w = MpcWeights(Vector(m(1, 1, 1.0)), Vector(rr), None, None, target)
        val u = ok(ok(mpcQp(inc, w, Vector(1.0), Vector(x), uPrev, MpcBounds(), lim)).solve(Set.empty, env)).input
        (0.5 * x + u.sum, u)
      }
    // nothing tells the inputs apart: the split stays where symmetry puts it
    val (xFree, uFree) = run(None)
    assert(math.abs(xFree - 1.0) < 1e-6 && close(uFree, Vector(0.25, 0.25), 1e-6))
    // the first input as low as possible, the second left free: no offset, since the target costs no tracking
    val low = MpcPositionTarget(Vector(m(2, 2, 1.0, 0.0, 0.0, 0.0)), Vector(0.0, 0.0))
    val (xLow, uLow) = run(Some(low))
    assert(math.abs(xLow - 1.0) < 1e-5 && close(uLow, Vector(0.0, 0.5), 1e-4), s"$xLow, $uLow")
  }

  it should "be refused where it cannot be read" in {
    val abs = ok(mpcPrediction(a2, b2, c2, 4, 2, lim))
    val inc = ok(mpcIncrementalPrediction(a2, b2, c2, 4, 2, lim))
    val s   = m(2, 2, 1.0, 0.0, 0.0, 1.0)
    def invalid(p: MpcPredictionMatrices, t: MpcPositionTarget) =
      val w = MpcWeights(Vector(m(1, 1, 1.0)), Vector(s), None, None, Some(t))
      assert(mpcQp(p, w, Vector(1.0), Vector(0.0, 0.0), Vector(0.0, 0.0), MpcBounds(), lim)
        .left.exists(_.isInstanceOf[MpcFailure.InvalidInput]), t.toString)
    invalid(abs, MpcPositionTarget(Vector(s), Vector(0.1, 0.2)))                  // the absolute form has inputTarget
    invalid(inc, MpcPositionTarget(Vector(s, s, s), Vector(0.1, 0.2)))            // 3 weights for 2 moves
    invalid(inc, MpcPositionTarget(Vector.empty, Vector(0.1, 0.2)))
    invalid(inc, MpcPositionTarget(Vector(m(1, 1, 1.0)), Vector(0.1, 0.2)))       // 1x1 for 2 inputs
    invalid(inc, MpcPositionTarget(Vector(s), Vector(0.1, 0.2, 0.3)))             // 3 entries for 2 or 4
    invalid(inc, MpcPositionTarget(Vector(s), Vector(0.1, Double.NaN)))
    invalid(inc, MpcPositionTarget(Vector(m(2, 2, 1.0, 0.0, 0.0, Double.PositiveInfinity)), Vector(0.1, 0.2)))
    val fine = MpcWeights(Vector(m(1, 1, 1.0)), Vector(s), None, None, Some(MpcPositionTarget(Vector(s, s), Vector(0.1, 0.2, 0.3, 0.4))))
    assert(mpcQp(inc, fine, Vector(1.0), Vector(0.0, 0.0), Vector(0.0, 0.0), MpcBounds(), lim).isRight)
  }

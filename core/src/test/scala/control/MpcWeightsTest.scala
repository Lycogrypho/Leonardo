package it.grypho.scala.leonardo
package control

import core.*
import org.scalatest.flatspec.AnyFlatSpec

class MpcWeightsTest extends AnyFlatSpec:

  private val env = new Environment(Environment.DefaultPrecision, Map.empty)
  private val lim = env.limits
  private def m(rows: Int, cols: Int, xs: Double*) = _MatrixValue(rows, cols, xs.toArray)
  private def mul(a: _MatrixValue, v: Vector[Double]) =
    Vector.tabulate(a.rows)(i => (0 until a.cols).map(j => a(i, j) * v(j)).sum)
  private def quad(w: _MatrixValue, v: Vector[Double]) = v.zip(mul(w, v)).map(_ * _).sum
  private def ok[A](r: Either[MpcFailure, A]): A = r.fold(e => fail(e.toString), identity)

  private val a = m(2, 2, 1.0, 0.1, 0.0, 0.9)
  private val b = m(2, 1, 0.0, 0.1)
  private val c = m(1, 2, 1.0, 0.0)

  "mpcTerminalPrediction" should "predict x(k+Np) as the plant does, the last move held" in {
    val (np, nc) = (5, 2)
    val x0 = Vector(0.3, -0.2)
    val us = Vector(Vector(1.0), Vector(-0.5))
    val t = ok(mpcTerminalPrediction(a, b, np, nc, incremental = false, lim)).asInstanceOf[MpcPrediction]
    val held = (0 until np).map(k => us(math.min(k, nc - 1)))
    val xN = held.foldLeft(x0)((x, u) => mul(a, x).zip(mul(b, u)).map(_ + _))
    val pred = mul(t.phi, x0).zip(mul(t.gamma, us.flatten)).map(_ + _)
    xN.zip(pred).foreach((p, q) => assert(math.abs(p - q) < 1e-12))
  }

  "mpcQp with weights" should "reproduce the single-weight programme when every step has the same weight" in {
    val pr = ok(mpcPrediction(a, b, c, 4, 2, lim))
    val (q, r) = (m(1, 1, 2.0), m(1, 1, 0.3))
    val one = ok(mpcQp(pr, q, r, Vector(1.0), Vector(0.1, 0.0), Vector(0.0), MpcBounds(), lim))
    val many = ok(mpcQp(pr, MpcWeights(Vector.fill(4)(q), Vector.fill(2)(r)), Vector(1.0), Vector(0.1, 0.0),
                        Vector(0.0), MpcBounds(), lim))
    assert(one.h.toVector.zip(many.h.toVector).forall(_ == _))
    assert(one.f.zip(many.f).forall(_ == _))
  }

  it should "build the cost it states: per-step weights, a terminal weight and an input target" in {
    val (np, nc) = (3, 2)
    val pr = ok(mpcPrediction(a, b, c, np, nc, lim))
    val term = ok(mpcTerminalPrediction(a, b, np, nc, incremental = false, lim))
    val qs = Vector(m(1, 1, 1.0), m(1, 1, 2.0), m(1, 1, 3.0))
    val rs = Vector(m(1, 1, 0.5), m(1, 1, 0.7))
    val pw = m(2, 2, 4.0, 0.5, 0.5, 2.0)
    val (x0, ref, xs, us) = (Vector(0.2, -0.1), Vector(1.0), Vector(0.4, 0.1), Vector(0.25, -0.1))
    val weights = MpcWeights(qs, rs, Some(MpcTerminal(pw, term, Some(xs))), Some(us))
    val qp = ok(mpcQp(pr, weights, ref, x0, Vector(0.0), MpcBounds(), lim))
    def cost(z: Vector[Double]): Double =
      val held = (0 until np).map(k => Vector(z(math.min(k, nc - 1))))
      val traj = held.scanLeft(x0)((x, u) => mul(a, x).zip(mul(b, u)).map(_ + _)).tail
      val stage = traj.zipWithIndex.map((x, i) => quad(qs(i), Vector(mul(c, x).head - ref.head))).sum
      val moves = (0 until nc).map(j => quad(rs(j), Vector(z(j) - us(j)))).sum
      stage + moves + quad(pw, traj.last.zip(xs).map(_ - _))
    def model(z: Vector[Double]) = 0.5 * quad(qp.h, z) + qp.f.zip(z).map(_ * _).sum
    val (z1, z2) = (Vector(0.3, -0.4), Vector(-1.1, 0.8))
    assert(math.abs((cost(z1) - cost(z2)) - (model(z1) - model(z2))) < 1e-10)
  }

  it should "equal the LQR law with the terminal weight P - C'QC" in {
    // the stage cost already weighs y(k+Np), so P - C'QC completes the infinite-horizon cost-to-go
    val (q, r) = (m(2, 2, 1.0, 0.0, 0.0, 0.5), m(1, 1, 0.2))
    val eye = _MatrixValue.identity(2)
    val (k, p) = dlqr(a, b, q, r, env).fold(e => fail(e.toString), identity)
    val (np, nc) = (3, 3)
    val pr = ok(mpcPrediction(a, b, eye, np, nc, lim))
    val term = ok(mpcTerminalPrediction(a, b, np, nc, incremental = false, lim))
    val weights = MpcWeights(Vector(q), Vector(r), Some(MpcTerminal(p.add(q.scale(-1.0)), term)))
    val x0 = Vector(1.0, -0.5)
    val u = ok(ok(mpcQp(pr, weights, Vector(0.0, 0.0), x0, Vector(0.0), MpcBounds(), lim)).solve(Set.empty, env)).input
    assert(math.abs(u.head + mul(k, x0).head) < 1e-6, s"$u vs ${mul(k, x0)}")
  }

  it should "make a one-step MPC of an unstable plant stable" in {
    val (aa, bb, cc, q, r) = (m(1, 1, 2.0), m(1, 1, 1.0), m(1, 1, 1.0), m(1, 1, 1.0), m(1, 1, 10.0))
    val p = dare(aa, bb, q, r, env).fold(e => fail(e.toString), identity)
    val pr = ok(mpcPrediction(aa, bb, cc, 1, 1, lim))
    val term = ok(mpcTerminalPrediction(aa, bb, 1, 1, incremental = false, lim))
    def run(weights: MpcWeights): Double =
      (0 until 40).foldLeft(1.0) { (x, _) =>
        val u = ok(ok(mpcQp(pr, weights, Vector(0.0), Vector(x), Vector(0.0), MpcBounds(), lim)).solve(Set.empty, env)).input.head
        2.0 * x + u
      }
    assert(math.abs(run(MpcWeights(Vector(q), Vector(r)))) > 1e6)              // 2 - 2/11 per step
    assert(math.abs(run(MpcWeights(Vector(q), Vector(r), Some(MpcTerminal(p, term))))) < 1e-6)
  }

  "an input target" should "remove the absolute form's offset towards u = 0" in {
    val (aa, bb, cc) = (m(1, 1, 0.5), m(1, 1, 1.0), m(1, 1, 1.0))
    val pr = ok(mpcPrediction(aa, bb, cc, 5, 2, lim))
    def run(target: Option[Vector[Double]]): Double =
      (0 until 60).foldLeft(0.0) { (x, _) =>
        val w = MpcWeights(Vector(m(1, 1, 1.0)), Vector(m(1, 1, 1.0)), None, target)
        val u = ok(ok(mpcQp(pr, w, Vector(1.0), Vector(x), Vector(0.0), MpcBounds(), lim)).solve(Set.empty, env)).input.head
        0.5 * x + u
      }
    val us = ok(steadyStateTarget(aa, bb, cc, m(1, 1, 0.0), m(1, 1, 0.0), Vector(1.0), Vector(0.0), None, env)).u
    assert(math.abs(run(None) - 1.0) > 0.05)
    assert(math.abs(run(Some(us)) - 1.0) < 1e-6)
  }

  "mpcQp with weights" should "decline what it cannot read" in {
    val pr  = ok(mpcPrediction(a, b, c, 3, 2, lim))
    val inc = ok(mpcIncrementalPrediction(a, b, c, 3, 2, lim))
    val (q, r) = (m(1, 1, 1.0), m(1, 1, 1.0))
    def invalid(p: MpcPredictionMatrices, w: MpcWeights) =
      assert(mpcQp(p, w, Vector(1.0), Vector(0.0, 0.0), Vector(0.0), MpcBounds(), lim)
        .left.exists(_.isInstanceOf[MpcFailure.InvalidInput]), w.toString)
    invalid(pr, MpcWeights(Vector(q, q), Vector(r)))                           // 2 weights for 3 steps
    invalid(pr, MpcWeights(Vector(q), Vector(r, r, r)))                        // 3 for 2 moves
    invalid(inc, MpcWeights(Vector(q), Vector(r), None, Some(Vector(0.5))))    // no target in increments
    invalid(pr, MpcWeights(Vector(q), Vector(r), None, Some(Vector(0.5, 0.5, 0.5))))
    val incTerm = ok(mpcTerminalPrediction(a, b, 3, 2, incremental = true, lim))
    invalid(pr, MpcWeights(Vector(q), Vector(r), Some(MpcTerminal(_MatrixValue.identity(2), incTerm))))
    val absTerm = ok(mpcTerminalPrediction(a, b, 3, 2, incremental = false, lim))
    invalid(pr, MpcWeights(Vector(q), Vector(r), Some(MpcTerminal(_MatrixValue.identity(3), absTerm))))
  }

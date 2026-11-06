package it.grypho.scala.leonardo
package control

import core.*
import org.scalatest.flatspec.AnyFlatSpec

class MinrealTest extends AnyFlatSpec:

  private val env = new Environment(Environment.DefaultPrecision, Map.empty)
  private def m(rows: Int, cols: Int, xs: Double*) = _MatrixValue(rows, cols, xs.toArray)
  private def ok(r: Either[MinrealFailure, MinimalRealisation]) = r.fold(e => fail(e.toString), identity)

  /** C·A^k·B for k = 0 … count − 1. */
  private def markov(a: _MatrixValue, b: _MatrixValue, c: _MatrixValue, count: Int) =
    (0 until count).scanLeft(b)((ab, _) => a.multiply(ab)).take(count).map(c.multiply(_).toVector)
  private def sameResponse(big: (_MatrixValue, _MatrixValue, _MatrixValue), r: MinimalRealisation) =
    markov(big._1, big._2, big._3, 12).zip(markov(r.a, r.b, r.c, 12)).foreach((x, y) =>
      x.zip(y).foreach((p, q) => assert(math.abs(p - q) <= 1e-9 * math.max(1.0, math.abs(p)))))

  // a minimal 2-state plant
  private val a2 = m(2, 2, 0.8, 0.3, -0.2, 0.5)
  private val b2 = m(2, 1, 1.0, 0.5)
  private val c2 = m(1, 2, 1.0, -1.0)

  "minreal" should "remove an uncontrollable and an unobservable mode, hidden by a change of basis" in {
    // x = [x_min (2), x_uc, x_uo]: x_uc is driven by nothing; x_uo is seen by nothing
    val a = m(4, 4,
      0.8, 0.3, 0.4, 0.0,
      -0.2, 0.5, 0.1, 0.0,
      0.0, 0.0, 0.7, 0.0,
      0.3, 0.2, 0.0, 0.9)
    val b = m(4, 1, 1.0, 0.5, 0.0, 0.6)
    val c = m(1, 4, 1.0, -1.0, 0.5, 0.0)
    val t = m(4, 4, 1.0, 0.2, -0.3, 0.1, 0.0, 1.0, 0.4, -0.2, 0.3, 0.0, 1.0, 0.5, -0.1, 0.2, 0.0, 1.0)
    val ti = t.inverse.getOrElse(fail("singular"))
    val (at, bt, ct) = (ti.multiply(a).multiply(t), ti.multiply(b), c.multiply(t))
    val r = ok(minreal(at, bt, ct, m(1, 1, 0.0), env))
    assert(r.order == 2)
    sameResponse((at, bt, ct), r)
  }

  it should "leave a minimal realisation's order unchanged" in {
    val r = ok(minreal(a2, b2, c2, m(1, 1, 0.3), env))
    assert(r.order == 2 && r.d == m(1, 1, 0.3))
    sameResponse((a2, b2, c2), r)
  }

  it should "reduce arx2ss of a model with a pole-zero cancellation to its McMillan degree" in {
    // y[k] = 0.5 y[k-1] + u[k-1] - 0.5 u[k-2]: (z - 0.5)/(z(z - 0.5)) = 1/z
    val model = ArxMimoModel(Vector(m(1, 1, 0.5)), Vector(m(1, 1, 1.0), m(1, 1, -0.5)))
    val (a, b, c, d) = arx2ss(model, 1).fold(e => fail(e.toString), identity)
    assert(a.rows == 2)
    val r = ok(minreal(a, b, c, d, env))
    assert(r.order == 1)
    sameResponse((a, b, c), r)
  }

  it should "reduce the NMSS of a MIMO model without changing its response" in {
    val model = ArxMimoModel(Vector(m(2, 2, 0.5, 0.1, -0.2, 0.3)), Vector(m(2, 1, 1.0, 0.0), m(2, 1, 0.2, 0.4)))
    val (a, b, c, d) = arx2nmss(model, 1).fold(e => fail(e.toString), identity)
    val r = ok(minreal(a, b, c, d, env))
    assert(r.order <= a.rows)
    sameResponse((a, b, c), r)
  }

  it should "decline a realisation whose transfer function is D alone, and malformed input" in {
    val r = minreal(a2, m(2, 1, 0.0, 0.0), c2, m(1, 1, 1.0), env)
    assert(r.left.exists(_ == MinrealFailure.NoState))
    assert(minreal(a2, m(3, 1, 1.0, 1.0, 1.0), c2, m(1, 1, 0.0), env).left.exists(_.isInstanceOf[MinrealFailure.InvalidInput]))
    assert(minreal(a2, b2, c2, m(2, 2, 0.0, 0.0, 0.0, 0.0), env).left.exists(_.isInstanceOf[MinrealFailure.InvalidInput]))
  }

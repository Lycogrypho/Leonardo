package it.grypho.scala.leonardo
package control

import core.*
import org.scalatest.flatspec.AnyFlatSpec

class ArxNmssTest extends AnyFlatSpec:

  private def m(rows: Int, cols: Int, xs: Double*) = _MatrixValue(rows, cols, xs.toArray)
  private def mul(a: _MatrixValue, v: Vector[Double]) =
    Vector.tabulate(a.rows)(i => (0 until a.cols).map(j => a(i, j) * v(j)).sum)
  private def add(a: Vector[Double], b: Vector[Double]) = a.zip(b).map(_ + _)

  // two outputs, two inputs, na = 2, nb = 2
  private val model = ArxMimoModel(
    Vector(m(2, 2, 0.5, 0.1, -0.2, 0.3), m(2, 2, -0.1, 0.0, 0.05, -0.2)),
    Vector(m(2, 2, 1.0, 0.2, 0.0, 0.5), m(2, 2, -0.3, 0.1, 0.4, 0.0)))
  private val inputs = Vector.tabulate(30)(k => Vector(math.sin(0.7 * k), if k % 3 == 0 then 1.0 else -0.5))

  /** The ARX recursion from a zero past. */
  private def arxOutputs(nk: Int): Vector[Vector[Double]] =
    def u(t: Int) = if t >= 0 then inputs(t) else Vector(0.0, 0.0)
    inputs.indices.foldLeft(Vector.empty[Vector[Double]]) { (ys, t) =>
      def y(s: Int) = if s >= 0 then ys(s) else Vector(0.0, 0.0)
      val ar = model.a.indices.map(i => mul(model.a(i), y(t - 1 - i))).foldLeft(Vector(0.0, 0.0))(add)
      val ex = model.b.indices.map(l => mul(model.b(l), u(t - nk - l))).foldLeft(Vector(0.0, 0.0))(add)
      ys :+ add(ar, ex)
    }

  private def simulate(sys: (_MatrixValue, _MatrixValue, _MatrixValue, _MatrixValue), us: Vector[Vector[Double]]) =
    val (a, b, c, _) = sys
    us.scanLeft(Vector.fill(a.rows)(0.0))((x, u) => add(mul(a, x), mul(b, u))).init.map(x => (x, mul(c, x)))

  "arx2nmss" should "reproduce the ARX recursion with the measured past as its state" in {
    for nk <- Seq(1, 2, 3) do
      val sys = arx2nmss(model, nk).fold(e => fail(e.toString), identity)
      val ys  = arxOutputs(nk)
      assert(sys._1.rows == 2 * 2 + 2 * (nk + 2 - 2), s"nk = $nk")
      simulate(sys, inputs).zipWithIndex.foreach { case ((x, y), k) =>
        y.zip(ys(k)).foreach((p, q) => assert(math.abs(p - q) < 1e-12, s"nk = $nk, k = $k"))
        def yAt(s: Int) = if s >= 0 then ys(s) else Vector(0.0, 0.0)
        def uAt(s: Int) = if s >= 0 then inputs(s) else Vector(0.0, 0.0)
        val past = yAt(k) ++ yAt(k - 1) ++ (1 to nk + 2 - 2).flatMap(j => uAt(k - j))
        x.zip(past).foreach((p, q) => assert(math.abs(p - q) < 1e-12, s"state, nk = $nk, k = $k"))
      }
  }

  "arx2nmssIncremental" should "reproduce the ARX recursion driven by increments" in {
    for nk <- Seq(1, 2) do
      val sys = arx2nmssIncremental(model, nk).fold(e => fail(e.toString), identity)
      val ys  = arxOutputs(nk)
      val du  = inputs.indices.map(k => if k == 0 then inputs(0) else inputs(k).zip(inputs(k - 1)).map(_ - _)).toVector
      simulate(sys, du).zipWithIndex.foreach { case ((_, y), k) =>
        y.zip(ys(k)).foreach((p, q) => assert(math.abs(p - q) < 1e-12, s"nk = $nk, k = $k"))
      }
  }

  "both realisations" should "decline direct feedthrough" in {
    assert(arx2nmss(model, 0).left.exists(_.isInstanceOf[ArxFailure.InvalidOrders]))
    assert(arx2nmssIncremental(model, 0).left.exists(_.isInstanceOf[ArxFailure.InvalidOrders]))
  }

  it should "realise a model with no autoregressive part" in {
    val fir = ArxMimoModel(Vector.empty, Vector(m(1, 1, 2.0), m(1, 1, -1.0)))
    val (a, b, c, _) = arx2nmss(fir, 1).fold(e => fail(e.toString), identity)
    // y(k+1) = 2u(k) - u(k-1)
    val ys = simulate((a, b, c, m(1, 1, 0.0)), Vector(Vector(1.0), Vector(0.0), Vector(0.0))).map(_._2.head)
    assert(ys == Vector(0.0, 2.0, -1.0))
  }

package it.grypho.scala.leonardo
package control

import core.*
import scalar.*
import matrix._Matrix
import org.scalatest.flatspec.AnyFlatSpec

/** F_0093: true MIMO ARX, `y[k] = Σ Aᵢ·y[k−i] + Σ Bₗ·u[k−nk−l+1]` with `Aᵢ` `p×p` coupling the
 *  outputs. Every expectation is checked against an independent simulation of that recursion,
 *  never against a transcribed number. */
class ArxMimoTest extends AnyFlatSpec:

  private val env = new Environment()
  private val z   = _Variable("z")

  private def mat(rows: Vector[Vector[Double]]): _MatrixValue =
    _MatrixValue(rows.size, rows.head.size, rows.flatten.toArray)

  /** A deterministic, persistently exciting input: no seed, the same on every platform. */
  private def input(n: Int, phase: Double): Vector[Double] =
    Vector.tabulate(n)(k => math.sin(0.7 * k + phase) + 0.5 * math.cos(1.9 * k + 2 * phase) + ((k * 7919 + phase.toInt) % 11 - 5) / 5.0)

  /** The MIMO recursion from rest; returns one series per output. */
  private def simulate(as: Vector[_MatrixValue], bs: Vector[_MatrixValue], nk: Int, us: Vector[Vector[Double]]): Vector[Vector[Double]] =
    val p = bs.head.rows
    val n = us.head.size
    val ys = (0 until n).foldLeft(Vector.empty[Vector[Double]]) { (y, k) =>
      y :+ Vector.tabulate(p) { r =>
        val ar = for i <- as.indices; t = k - 1 - i if t >= 0; s <- 0 until p yield as(i)(r, s) * y(t)(s)
        val ex = for l <- bs.indices; t = k - nk - l if t >= 0; j <- us.indices yield bs(l)(r, j) * us(j)(t)
        ar.sum + ex.sum
      }
    }
    Vector.tabulate(p)(r => ys.map(_(r)))

  private val a1 = mat(Vector(Vector(0.5, 0.2), Vector(-0.1, 0.4)))
  private val a2 = mat(Vector(Vector(-0.1, 0.05), Vector(0.0, -0.2)))
  private val b1 = mat(Vector(Vector(1.0, 0.3), Vector(0.0, 0.7)))
  private val b2 = mat(Vector(Vector(0.2, 0.0), Vector(-0.4, 0.1)))
  private val us = Vector(input(150, 0.0), input(150, 2.3))
  private val ys = simulate(Vector(a1, a2), Vector(b1, b2), 1, us)

  private def close(a: Double, b: Double, tol: Double = 1e-9) = math.abs(a - b) <= tol
  private def closeM(a: _MatrixValue, b: _MatrixValue, tol: Double = 1e-9) =
    a.rows == b.rows && a.cols == b.cols && a.toVector.zip(b.toVector).forall((x, y) => close(x, y, tol))

  private def fit(na: Int, nb: Int, nk: Int, y: _MatrixValue = mat(ys)): ArxMimoModel =
    arxMimo(mat(us), y, na, nb, nk, env).fold(f => fail(s"declined: $f"), identity)

  private def valueAt(g: _Expression, at: Double): Double =
    g.eval(env.withBinding("z", _Number(at))) match
      case Right(_Number(d)) => d
      case other             => fail(s"$g at $at: $other")

  // --- identification ---

  "arxMimo" should "recover a cross-coupled two-output, two-input plant from noise-free data" in
  {
    val m = fit(2, 2, 1)
    assert(m.a.size == 2 && m.b.size == 2, m)
    Seq(m.a(0) -> a1, m.a(1) -> a2, m.b(0) -> b1, m.b(1) -> b2).foreach((got, want) => assert(closeM(got, want), s"$got vs $want"))
  }

  it should "read the output either way up" in
  {
    assert(fit(2, 2, 1) == fit(2, 2, 1, mat(ys).transpose))
  }

  it should "agree with arx bit for bit on a single output" in
  {
    val y1 = mat(Vector(ys(0)))
    val single = arx(mat(us), y1, 2, 2, 1, env).fold(f => fail(s"$f"), identity)
    val multi  = fit(2, 2, 1, y1)
    assert(single.a == multi.a.map(_(0, 0)))
    assert(single.b == Vector.tabulate(2)(j => multi.b.map(_(0, j))))
  }

  it should "fit a finite impulse response when na = 0" in
  {
    val fir = simulate(Vector.empty, Vector(b1, b2), 0, us)
    val m   = fit(0, 2, 0, mat(fir))
    assert(m.a.isEmpty && closeM(m.b(0), b1) && closeM(m.b(1), b2), m)
  }

  "arx" should "still decline several outputs, naming arxMimo" in
  {
    assert(arx(mat(us), mat(ys), 1, 1, 1, env).left.exists {
      case ArxFailure.ShapeMismatch(d) => d.contains("arxMimo")
      case _                           => false
    })
  }

  "arxRegressors" should "lay out each row as every output's past, lag by lag, then the inputs" in
  {
    val u = mat(Vector(Vector(1.0, 2.0, 3.0, 4.0, 5.0)))
    val y = mat(Vector(Vector(10.0, 20.0, 30.0, 40.0, 50.0), Vector(100.0, 200.0, 300.0, 400.0, 500.0)))
    arxRegressors(u, y, 2, 1, 1) match
      case Left(f) => fail(s"declined: $f")
      case Right((x, t)) =>
        // k0 = max(na, nk + nb - 1) = 2: rows k = 2, 3, 4; columns y1[k-1] y2[k-1] y1[k-2] y2[k-2] u[k-1].
        assert(x.rows == 3 && x.cols == 5 && t.rows == 3 && t.cols == 2)
        assert(x.toVector.take(5) == Vector(20.0, 200.0, 10.0, 100.0, 2.0))
        assert(t.toVector.take(2) == Vector(30.0, 300.0))
  }

  // --- conversions ---

  "arx2ss" should "realise the MIMO model: its recursion reproduces the outputs sample for sample" in
  {
    val cases = Seq((Vector(a1, a2), Vector(b1, b2), 1),
                    (Vector(a1), Vector(b1, b2), 0),         // direct feedthrough
                    (Vector.empty[_MatrixValue], Vector(b1, b2), 1), // finite impulse response
                    (Vector(a1, a2), Vector(b1), 3))          // a delay past the orders
    for (as, bs, nk) <- cases do
      val (am, bm, cm, dm) = arx2ss(ArxMimoModel(as, bs), nk).fold(f => fail(s"declined: $f"), identity)
      val y = simulate(as, bs, nk, us)
      assert(am.rows == 2 * math.max(as.size, nk + bs.size - 1), s"states for nk=$nk")
      (0 until 60).foldLeft(_MatrixValue(am.rows, 1, Array.fill(am.rows)(0.0))) { (x, k) =>
        val uk = _MatrixValue(2, 1, us.map(_(k)).toArray)
        val yk = cm.multiply(x).add(dm.multiply(uk))
        (0 until 2).foreach(r => assert(close(yk(r, 0), y(r)(k)), s"nk=$nk: y$r[$k] ${yk(r, 0)} vs ${y(r)(k)}"))
        am.multiply(x).add(bm.multiply(uk))
      }
  }

  it should "decline a static gain, which has no state, and blocks of different shapes" in
  {
    assert(arx2ss(ArxMimoModel(Vector.empty, Vector(b1)), 0).left.exists(_ == ArxFailure.NoState))
    val bad = ArxMimoModel(Vector(mat(Vector(Vector(0.5)))), Vector(b1))
    assert(arx2ss(bad, 1).left.exists(_.isInstanceOf[ArxFailure.ShapeMismatch]))
  }

  "arx2tf" should "give the p x m transfer matrix G with A(z) G(z) = B(z)" in
  {
    val g = arx2tf(ArxMimoModel(Vector(a1, a2), Vector(b1, b2)), 1, z).fold(f => fail(s"declined: $f"), identity)
    assert(g.size == 2 && g.forall(_.size == 2))
    for at <- Seq(2.0, -1.3, 0.7) do
      val gz = mat(g.map(_.map(valueAt(_, at))))
      val az = _MatrixValue.identity(2).add(a1.scale(-1 / at)).add(a2.scale(-1 / (at * at)))
      val bz = b1.scale(1 / at).add(b2.scale(1 / (at * at)))
      assert(closeM(az.multiply(gz), bz), s"at z = $at")
  }

  it should "agree with the single-output arx2tf at p = 1" in
  {
    val one = ArxMimoModel(Vector(mat(Vector(Vector(1.5))), mat(Vector(Vector(-0.7)))), Vector(mat(Vector(Vector(0.3, -0.4)))))
    val g  = arx2tf(one, 2, z).fold(f => fail(s"$f"), identity)
    val gs = arx2tf(Vector(1.5, -0.7), Vector(Vector(0.3), Vector(-0.4)), 2, z).fold(f => fail(s"$f"), identity)
    for j <- 0 until 2; at <- Seq(2.0, -1.3) do assert(close(valueAt(g(0)(j), at), valueAt(gs(j), at)), s"G_$j($at)")
  }

  it should "decline more outputs than the cofactor expansion is held to" in
  {
    val p   = 7
    val big = ArxMimoModel(Vector(_MatrixValue.identity(p).scale(0.5)), Vector(_MatrixValue.identity(p)))
    assert(arx2tf(big, 1, z).left.exists(_.isInstanceOf[ArxFailure.ShapeMismatch]))
  }

  // --- the nodes ---

  "the arx node" should "answer the stacked blocks [[a, b]] for several outputs" in
  {
    _Arx(mat(us), mat(ys), _Number(2), _Number(2), _Number(1)).eval(env) match
      case Left(m: _Matrix) =>
        (m.elems(0), m.elems(1)) match
          case (a: _MatrixValue, b: _MatrixValue) =>
            assert(a.rows == 4 && a.cols == 2 && b.rows == 4 && b.cols == 2)
            assert(closeM(a, mat(Vector(a1, a2).flatMap(blk => Vector.tabulate(2)(r => Vector.tabulate(2)(blk(r, _)))))), a)
            assert(closeM(b, mat(Vector(b1, b2).flatMap(blk => Vector.tabulate(2)(r => Vector.tabulate(2)(blk(r, _)))))), b)
          case other => fail(s"unexpected: $other")
      case other => fail(s"unexpected: $other")
  }

  it should "write a as zeros(p, p) when na = 0, which the converters read back unchanged" in
  {
    val fir = simulate(Vector.empty, Vector(b1), 1, us)
    _Arx(mat(us), mat(fir), _Number(0), _Number(1), _Number(1)).eval(env) match
      case Left(m: _Matrix) =>
        m.elems(0) match
          case a: _MatrixValue => assert(a.rows == 2 && a.cols == 2 && a.toVector.forall(_ == 0.0), a)
          case other           => fail(s"a: $other")
        _Arx2Tf(m.elems(0), m.elems(1), _Number(1), z).eval(env) match
          case Left(g: _Matrix) =>
            assert(g.rows == 2 && g.cols == 2)
            for r <- 0 until 2; j <- 0 until 2 do assert(close(valueAt(g.elems(r * 2 + j), 2.0), b1(r, j) / 2.0), s"G($r, $j)")
          case other => fail(s"arx2tf: $other")
      case other => fail(s"unexpected: $other")
  }

  it should "say why it declined several outputs" in
  {
    val short = _Arx(mat(us.map(_.take(4))), mat(ys.map(_.take(4))), _Number(2), _Number(2), _Number(1))
    assert(short.eval(env) == Left(short))
    assert(short.declineReason(env).exists(_.isInstanceOf[ArxFailure.TooShort]))
  }

  "the converter nodes" should "read stacked blocks and round-trip arx's answer" in
  {
    _Arx(mat(us), mat(ys), _Number(2), _Number(2), _Number(1)).eval(env) match
      case Left(m: _Matrix) =>
        _Arx2Ss(m.elems(0), m.elems(1), _Number(1)).eval(env) match
          case Left(row: _Matrix) => assert(row.rows == 1 && row.cols == 4)
          case other              => fail(s"arx2ss: $other")
        val tf = _Arx2Tf(m.elems(0), m.elems(1), _Number(1), z)
        assert(tf.eval(env).left.exists { case g: _Matrix => g.rows == 2 && g.cols == 2; case _ => false })
        assert(tf.declineReason(env).isEmpty)
        // A b whose rows are not whole blocks of p is refused.
        val badB = _Arx2Ss(m.elems(0), _MatrixValue(3, 2, Array.fill(6)(1.0)), _Number(1))
        assert(badB.eval(env) == Left(badB) && badB.detailed(env).left.exists(_.isInstanceOf[ArxFailure.ShapeMismatch]))
      case other => fail(s"unexpected: $other")
  }

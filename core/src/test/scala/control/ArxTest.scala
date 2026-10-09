package it.grypho.scala.leonardo
package control

import core.*
import scalar.*
import matrix._Matrix
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** F_0060: ARX identification, MISO, in the predictor form
 *  `y[k] = Σ aᵢ·y[k−i] + Σⱼ Σₗ bₗⱼ·uⱼ[k−nk−l+1]`, with the delay an explicit argument of the
 *  converters. Every expectation is checked against an independent simulation of that
 *  recursion rather than against a transcribed number. */
class ArxTest extends AnyFlatSpec:

  private val env = new Environment()
  private val z   = _Variable("z")

  /** A deterministic, persistently exciting input: no seed, the same on every platform. */
  private def input(n: Int, phase: Double): Vector[Double] =
    Vector.tabulate(n)(k => math.sin(0.7 * k + phase) + 0.5 * math.cos(1.9 * k) + ((k * 7919 + phase.toInt) % 11 - 5) / 5.0)

  /** The ARX recursion from rest: `a` the `na` coefficients, `b(j)` input `j`'s `nb`. */
  private def simulate(a: Vector[Double], b: Vector[Vector[Double]], nk: Int, us: Vector[Vector[Double]]): Vector[Double] =
    val n = us.head.size
    (0 until n).foldLeft(Vector.empty[Double]) { (y, k) =>
      val ar = a.indices.map(i => if k - 1 - i >= 0 then a(i) * y(k - 1 - i) else 0.0).sum
      val ex = for j <- b.indices; l <- b(j).indices; t = k - nk - l if t >= 0 yield b(j)(l) * us(j)(t)
      y :+ (ar + ex.sum)
    }

  private def row(v: Vector[Double]): _MatrixValue = _MatrixValue(1, v.size, v.toArray)
  private def rows(vs: Vector[Vector[Double]]): _MatrixValue = _MatrixValue(vs.size, vs.head.size, vs.flatten.toArray)

  private def fit(us: Vector[Vector[Double]], y: Vector[Double], na: Int, nb: Int, nk: Int): ArxModel =
    arx(rows(us), row(y), na, nb, nk, env).fold(f => fail(s"declined: $f"), identity)

  private def close(a: Double, b: Double, tol: Double = 1e-9) = math.abs(a - b) <= tol

  private def tfs(a: Vector[Double], b: Vector[Vector[Double]], nk: Int): Vector[_Expression] =
    arx2tf(a, b, nk, z).fold(f => fail(s"declined: $f"), identity)

  private def valueAt(g: _Expression, at: Double): Double =
    g.eval(env.withBinding("z", _Number(at))) match
      case Right(_Number(d)) => d
      case other             => fail(s"$g at $at: $other")

  // --- identification ---

  "arx" should "recover y[k] = 0.8 y[k-1] + 0.5 u[k-1] from noise-free data (the acceptance)" in
  {
    val u = input(60, 0.0)
    val y = simulate(Vector(0.8), Vector(Vector(0.5)), 1, Vector(u))
    val m = fit(Vector(u), y, 1, 1, 1)
    assert(close(m.a(0), 0.8, 1e-12), m)
    assert(close(m.b(0)(0), 0.5, 1e-12), m)
  }

  it should "recover a second-order, two-input model with a delay of two" in
  {
    val a  = Vector(1.5, -0.7)
    val b  = Vector(Vector(0.3, 0.1), Vector(-0.4, 0.2))
    val us = Vector(input(120, 0.0), input(120, 2.3))
    val m  = fit(us, simulate(a, b, 2, us), 2, 2, 2)
    a.indices.foreach(i => assert(close(m.a(i), a(i)), m))
    for j <- b.indices; l <- b(j).indices do assert(close(m.b(j)(l), b(j)(l)), s"b($l, $j): $m")
  }

  it should "read the inputs either way up and the output either way up" in
  {
    val us = Vector(input(80, 0.0), input(80, 1.1))
    val y  = simulate(Vector(0.6), Vector(Vector(1.0), Vector(-2.0)), 1, us)
    val byRows = arx(rows(us), row(y), 1, 1, 1, env)
    val byCols = arx(rows(us).transpose, row(y).transpose, 1, 1, 1, env)
    assert(byRows.isRight && byRows == byCols, s"$byRows vs $byCols")
  }

  it should "fit a finite impulse response when na = 0" in
  {
    val u = input(40, 0.5)
    val m = fit(Vector(u), simulate(Vector.empty, Vector(Vector(2.0, -1.0)), 0, Vector(u)), 0, 2, 0)
    assert(m.a.isEmpty)
    assert(close(m.b(0)(0), 2.0) && close(m.b(0)(1), -1.0), m)
  }

  "arxRegressors" should "lay out each row as the past outputs, then each input's lags" in
  {
    val u = Vector(1.0, 2.0, 3.0, 4.0, 5.0)
    val y = Vector(10.0, 20.0, 30.0, 40.0, 50.0)
    arxRegressors(row(u), row(y), 2, 2, 1) match
      case Left(f) => fail(s"declined: $f")
      case Right((x, t)) =>
        // k0 = max(na, nk + nb - 1) = 2, so rows k = 2, 3, 4.
        assert(x.rows == 3 && x.cols == 4 && t.rows == 3 && t.cols == 1)
        assert(x.toVector.take(4) == Vector(20.0, 10.0, 2.0, 1.0))   // y[1], y[0], u[1], u[0]
        assert(t.toVector == Vector(30.0, 40.0, 50.0))
  }

  // --- refusals ---

  "arx" should "decline a series too short for the orders" in
  {
    assert(arx(row(Vector(1.0, 2.0, 3.0)), row(Vector(0.0, 1.0, 2.0)), 2, 2, 1, env).left.exists {
      case ArxFailure.TooShort(_, _, _) => true
      case _                            => false
    })
  }

  it should "decline data that does not excite the model, rather than fit noise" in
  {
    // A constant input makes u[k-1] and u[k-2] the same column: QR could still return numbers.
    val u = Vector.fill(30)(1.0)
    val y = simulate(Vector(0.5), Vector(Vector(1.0, 0.0)), 1, Vector(u))
    assert(arx(row(u), row(y), 1, 2, 1, env).left.exists {
      case ArxFailure.NotExcited(rank, cols) => rank < cols
      case _                                 => false
    })
  }

  it should "decline a barely varying input, which QR alone would fit and answer with noise" in
  {
    // u[k-1] and u[k-2] differ by ~1e-8: QR does not see them as dependent, but at the session
    // precision the regressor matrix is rank-deficient -- the case the svd guard exists for.
    val u = Vector.tabulate(40)(k => 1.0 + 1e-8 * math.sin(1.3 * k))
    val y = simulate(Vector(0.5), Vector(Vector(1.0, 0.0)), 1, Vector(u))
    val (x, t) = arxRegressors(row(u), row(y), 1, 2, 1).fold(f => fail(s"$f"), identity)
    assert(statistics.leastSquares(x, t).isDefined, "premise: QR alone answers this design")
    assert(arx(row(u), row(y), 1, 2, 1, env).left.exists(_.isInstanceOf[ArxFailure.NotExcited]))
  }

  it should "decline mismatched lengths and invalid orders" in
  {
    val u = row(input(20, 0.0))
    val y = row(input(20, 1.0))
    assert(arx(u, row(input(19, 0.0)), 1, 1, 1, env).left.exists(_.isInstanceOf[ArxFailure.ShapeMismatch]))
    for (na, nb, nk) <- Seq((-1, 1, 1), (1, 0, 1), (1, 1, -1)) do
      assert(arx(u, y, na, nb, nk, env).left.exists(_.isInstanceOf[ArxFailure.InvalidOrders]), (na, nb, nk))
  }

  "the arx node" should "stay as written for a fractional order or symbolic data, and say why" in
  {
    val u = row(input(20, 0.0))
    val y = row(input(20, 1.0))
    val frac = _Arx(u, y, _Number(1.5), _Number(1), _Number(1))
    assert(frac.eval(env) == Left(frac))
    assert(frac.detailed(env).left.exists(_.isInstanceOf[ArxFailure.InvalidOrders]))
    val sym = _Arx(_Variable("u"), y, _Number(1), _Number(1), _Number(1))
    assert(sym.eval(env) == Left(sym))
    assert(sym.detailed(env).left.exists(_.isInstanceOf[ArxFailure.NotNumeric]))
  }

  it should "return the row [[a, b]], with a written 0 when na = 0" in
  {
    val u = input(40, 0.0)
    val y = simulate(Vector(0.8), Vector(Vector(0.5)), 1, Vector(u))
    _Arx(row(u), row(y), _Number(1), _Number(1), _Number(1)).eval(env) match
      case Left(m: _Matrix) =>
        assert(m.rows == 1 && m.cols == 2)
        m.elems(0) match
          case a: _MatrixValue => assert(a.rows == 1 && a.cols == 1 && close(a(0, 0), 0.8))
          case other           => fail(s"a: $other")
      case other => fail(s"unexpected: $other")
    val fir = simulate(Vector.empty, Vector(Vector(2.0)), 1, Vector(u))
    _Arx(row(u), row(fir), _Number(0), _Number(1), _Number(1)).eval(env) match
      case Left(m: _Matrix) => assert(m.elems(0) match { case _Number(0.0) => true; case _ => false }, m)
      case other            => fail(s"unexpected: $other")
  }

  // --- conversions ---

  "arx2tf" should "have the pole at z = 0.8 and the gain 0.5/(z - 0.8) (the acceptance)" in
  {
    val g = tfs(Vector(0.8), Vector(Vector(0.5)), 1).head
    assert(poles(g, z).exists(_.exists { case _Number(p) => close(p, 0.8); case _ => false }), g)
    assert(isStableDiscrete(g, z).contains(true))
    for at <- Seq(2.0, -1.5, 0.3) do assert(close(valueAt(g, at), 0.5 / (at - 0.8)), s"G($at)")
  }

  it should "give one transfer function per input, B_j(z)/A(z) with the delay" in
  {
    val a  = Vector(1.5, -0.7)
    val b  = Vector(Vector(0.3, 0.1), Vector(-0.4, 0.2))
    val gs = tfs(a, b, 2)
    assert(gs.size == 2)
    for (g, j) <- gs.zipWithIndex; at <- Seq(2.0, -1.3) do
      val den = 1 - a(0) / at - a(1) / (at * at)
      val num = b(j).indices.map(l => b(j)(l) * math.pow(at, -(2 + l))).sum
      assert(close(valueAt(g, at), num / den), s"G_$j($at)")
  }

  "arx2ss" should "realise the same model: its recursion reproduces the ARX output sample for sample" in
  {
    val cases = Seq((Vector(0.8), Vector(Vector(0.5)), 1),
                    (Vector(1.5, -0.7), Vector(Vector(0.3, 0.1), Vector(-0.4, 0.2)), 2),
                    (Vector(0.5), Vector(Vector(1.0, 0.25)), 0),           // direct feedthrough
                    (Vector.empty[Double], Vector(Vector(2.0, -1.0)), 1))  // finite impulse response
    for (a, b, nk) <- cases do
      val us = b.indices.map(j => input(40, j * 1.7)).toVector
      val (am, bm, cm, dm) = arx2ss(a, b, nk).fold(f => fail(s"declined: $f"), identity)
      val y = simulate(a, b, nk, us)
      (0 until 40).foldLeft(_MatrixValue(am.rows, 1, Array.fill(am.rows)(0.0))) { (x, k) =>
        val uk = _MatrixValue(us.size, 1, us.map(_(k)).toArray)
        val yk = cm.multiply(x).add(dm.multiply(uk))(0, 0)
        assert(close(yk, y(k)), s"a=$a b=$b nk=$nk: y[$k] $yk vs ${y(k)}")
        am.multiply(x).add(bm.multiply(uk))
      }
  }

  it should "have the poles of arx2tf as the eigenvalues of A" in
  {
    val (am, _, _, _) = arx2ss(Vector(1.5, -0.7), Vector(Vector(0.3)), 1).fold(f => fail(s"$f"), identity)
    def key(v: _Value) = v match
      case c: _Complex => _Complex.parts(c).getOrElse((Double.NaN, Double.NaN))
      case _Number(d)  => (d, 0.0)
      case _           => (Double.NaN, Double.NaN)
    val eig = am.eigenDecompose.getOrElse(fail("no eigenvalues")).map(key).sorted
    val ps  = poles(tfs(Vector(1.5, -0.7), Vector(Vector(0.3)), 1).head, z).getOrElse(fail("no poles")).map(key).sorted
    assert(eig.size == ps.size)
    eig.zip(ps).foreach((e, p) => assert(close(e._1, p._1) && close(math.abs(e._2), math.abs(p._2)), s"$e vs $p"))
  }

  it should "decline a static gain, which has no state" in
  {
    assert(arx2ss(Vector.empty, Vector(Vector(3.0)), 0).left.exists(_ == ArxFailure.NoState))
  }

  "the converter nodes" should "read a and b as arx returns them, and decline a b of the wrong shape" in
  {
    val aV = _MatrixValue(1, 1, Array(0.8))
    val bV = _MatrixValue(1, 1, Array(0.5))
    _Arx2Tf(aV, bV, _Number(1), z).eval(env) match
      case Left(g) => assert(close(valueAt(g, 2.0), 0.5 / 1.2), g)
      case other   => fail(s"unexpected: $other")
    _Arx2Ss(aV, bV, _Number(1)).eval(env) match
      case Left(m: _Matrix) => assert(m.rows == 1 && m.cols == 4)
      case other            => fail(s"unexpected: $other")
    // 0 means "no a" (na = 0), never "no b": b must be the nb x m matrix arx returns.
    val noB = _Arx2Ss(_Number(0), _Number(0), _Number(0))
    assert(noB.detailed(env).isLeft && noB.eval(env) == Left(noB))
  }

  "the ARX productions" should "round-trip and be reserved" in
  {
    for s <- Seq("arx(u, y, 2, 1, 1)", "arxRegressors(u, y, 2, 1, 1)", "arx2tf(a, b, 1, z)", "arx2ss(a, b, 1)") do
      val e = Parser.parse(s)
      assert(e.successful, s)
      assert(Parser.parse(e.get.toString).get == e.get, s)
    Seq("arx", "arxRegressors", "arx2tf", "arx2ss").foreach(w => assert(Parser.ReservedWords.contains(w), w))
  }

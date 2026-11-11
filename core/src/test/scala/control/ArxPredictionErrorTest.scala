package it.grypho.scala.leonardo
package control

import core.*
import org.scalatest.flatspec.AnyFlatSpec

/** F_0330 (AIACE L_0014) -- the one-step and the free-run error of an ARX model on a window of data. */
class ArxPredictionErrorTest extends AnyFlatSpec:

  private val env = new Environment(Environment.DefaultPrecision, Map.empty)

  /** A deterministic generator in `[-1, 1)`, so the data is the same on both platforms. */
  private def lcg(seed: Long, n: Int): Vector[Double] =
    Iterator.iterate(seed)(s => s * 6364136223846793005L + 1442695040888963407L)
      .drop(1).take(n).map(s => ((s >>> 11).toDouble / (1L << 53).toDouble) * 2.0 - 1.0).toVector

  /** `y[k] = 1.5 y[k-1] - 0.7 y[k-2] + u[k-1] + 0.5 u[k-2] + e[k]`: na = 2, nb = 2, nk = 1. */
  private def simulate(u: Vector[Double], noise: Vector[Double]): Vector[Double] =
    u.indices.foldLeft(Vector.empty[Double]) { (y, k) =>
      def yy(i: Int) = if i >= 0 then y(i) else 0.0
      def uu(i: Int) = if i >= 0 then u(i) else 0.0
      y :+ (1.5 * yy(k - 1) - 0.7 * yy(k - 2) + uu(k - 1) + 0.5 * uu(k - 2) + noise(k))
    }

  private def m(rows: Int, cols: Int, xs: Double*) = _MatrixValue(rows, cols, xs.toArray)
  private def row(xs: Vector[Double]) = _MatrixValue(1, xs.size, xs.toArray)
  private def col(xs: Vector[Double]) = _MatrixValue(xs.size, 1, xs.toArray)
  private def ok[A](r: Either[ArxFailure, A]): A = r.fold(e => fail(e.toString), identity)
  private def rmsOf(xs: Seq[Double]) = math.sqrt(xs.map(x => x * x).sum / xs.size)

  private val n     = 300
  private val u     = lcg(7, n).map(x => if x >= 0 then 1.0 else -1.0)
  private val noise = lcg(11, n).map(_ * 0.05)
  private val y     = simulate(u, noise)
  private val truth = ArxMimoModel(Vector(m(1, 1, 1.5), m(1, 1, -0.7)), Vector(m(1, 1, 1.0), m(1, 1, 0.5)))

  "arxPredictionError" should "score the true model on noise-free data as zero" in {
    val y0 = simulate(u, Vector.fill(n)(0.0))
    val e  = ok(arxPredictionError(truth, 1, row(u), row(y0), env))
    assert(e.from == 2 && e.residuals.rows == 1 && e.residuals.cols == n - 2)
    assert(e.rms.head <= 1e-12 * rmsOf(y0))
  }

  it should "leave exactly the noise as the true model's one-step residual" in {
    val e = ok(arxPredictionError(truth, 1, row(u), row(y), env))
    (2 until n).foreach(k => assert(math.abs(e.residuals(0, k - 2) - noise(k)) < 1e-12, s"k = $k"))
    assert(math.abs(e.rms.head - rmsOf(noise.drop(2))) < 1e-12)
  }

  it should "be the loss arxOrderSelection ranks by" in {
    // one candidate, so the selection scores on the model's own reach: k = 1 .. N-1
    val sel = ok(arxOrderSelection(row(u), row(y), Seq(1), Seq(1), Seq(1), ArxCriterion.AIC, None, env))
    val e   = ok(arxPredictionError(sel.model, 1, row(u), row(y), env))
    val nn  = e.residuals.cols
    assert(e.from == 1 && nn == n - 1)
    val aic = nn * math.log(e.rms.head * e.rms.head) + 2 * 2
    assert(math.abs(aic - sel.scores.head.score) <= 1e-9 * math.abs(aic))
  }

  it should "score a model on a window it was not fitted on" in {
    val model = ok(arxMimo(row(u), row(y), 2, 2, 1, env))
    val uv = lcg(13, 200).map(x => if x >= 0 then 1.0 else -1.0)
    val ev = lcg(17, 200).map(_ * 0.05)
    val yv = simulate(uv, ev)
    val e  = ok(arxPredictionError(model, 1, row(uv), row(yv), env))
    // the fitted model is close to the true one, so its residual on new data is close to the new noise
    assert(math.abs(e.rms.head - rmsOf(ev.drop(2))) < 0.1 * rmsOf(ev.drop(2)))
  }

  it should "read the samples along either side" in {
    val a = ok(arxPredictionError(truth, 1, row(u), row(y), env))
    val b = ok(arxPredictionError(truth, 1, col(u), col(y), env))
    assert(a.rms == b.rms && a.residuals.toVector == b.residuals.toVector)
  }

  it should "score every output of a MIMO model" in {
    val model = ArxMimoModel(
      Vector(m(2, 2, 0.5, 0.1, -0.2, 0.3), m(2, 2, -0.1, 0.0, 0.05, -0.2)),
      Vector(m(2, 2, 1.0, 0.2, 0.0, 0.5), m(2, 2, -0.3, 0.1, 0.4, 0.0)))
    val nk = 2
    val us = Vector(lcg(3, 60), lcg(5, 60))
    val es = Vector(lcg(19, 60).map(_ * 0.01), lcg(23, 60).map(_ * 0.02))
    def at(s: Vector[Double], t: Int) = if t >= 0 then s(t) else 0.0
    val ys = (0 until 60).foldLeft(Vector(Vector.empty[Double], Vector.empty[Double])) { (acc, k) =>
      val next = Vector.tabulate(2) { r =>
        val ar = model.a.indices.map(i => (0 until 2).map(c => model.a(i)(r, c) * at(acc(c), k - 1 - i)).sum).sum
        val br = model.b.indices.map(l => (0 until 2).map(j => model.b(l)(r, j) * at(us(j), k - nk - l)).sum).sum
        ar + br + es(r)(k)
      }
      Vector(acc(0) :+ next(0), acc(1) :+ next(1))
    }
    def rows2(s: Vector[Vector[Double]]) = _MatrixValue(2, 60, (s(0) ++ s(1)).toArray)
    val e = ok(arxPredictionError(model, nk, rows2(us), rows2(ys), env))
    assert(e.from == 3 && e.residuals.rows == 2 && e.residuals.cols == 57)
    for r <- 0 until 2 do
      (3 until 60).foreach(k => assert(math.abs(e.residuals(r, k - 3) - es(r)(k)) < 1e-12, s"output $r, k = $k"))
      assert(math.abs(e.rms(r) - rmsOf(es(r).drop(3))) < 1e-12)
  }

  "arxSimulationError" should "be zero for the true model on noise-free data" in {
    val y0 = simulate(u, Vector.fill(n)(0.0))
    val e  = ok(arxSimulationError(truth, 1, row(u), row(y0), env))
    assert(e.from == 2 && e.residuals.cols == n - 2)
    assert(e.rms.head <= 1e-10 * rmsOf(y0))
  }

  it should "feed the model's own outputs back, from the window's first measured samples" in {
    // by hand: the first two samples measured, every later one the model's own
    val sim = (2 until n).foldLeft(y.take(2))((s, k) => s :+ (1.5 * s(k - 1) - 0.7 * s(k - 2) + u(k - 1) + 0.5 * u(k - 2)))
    val e = ok(arxSimulationError(truth, 1, row(u), row(y), env))
    (2 until n).foreach(k => assert(math.abs(e.residuals(0, k - 2) - (y(k) - sim(k))) < 1e-10, s"k = $k"))
    // the free run accumulates what the one-step prediction forgets at every sample
    val one = ok(arxPredictionError(truth, 1, row(u), row(y), env))
    assert(e.rms.head > 1.5 * one.rms.head)
  }

  it should "decline a free run that leaves the finite numbers" in {
    val wild = ArxMimoModel(Vector(m(1, 1, 1e40)), Vector(m(1, 1, 1.0)))
    assert(arxSimulationError(wild, 1, row(u), row(y), env).left.exists(_.isInstanceOf[ArxFailure.NotNumeric]))
    assert(arxPredictionError(wild, 1, row(u), row(y), env).isRight)
  }

  "both errors" should "decline data that does not fit the model" in {
    for score <- Seq(arxPredictionError, arxSimulationError) do
      // a window no longer than the model's reach
      assert(score(truth, 1, row(u.take(2)), row(y.take(2)), env) == Left(ArxFailure.TooShort(2, 0, 4)))
      assert(score(truth, 1, row(u.take(3)), row(y.take(3)), env).isRight)
      // two input series for a one-input model
      val two = _MatrixValue(2, n, (u ++ u).toArray)
      assert(score(truth, 1, two, row(y), env).left.exists(_.isInstanceOf[ArxFailure.ShapeMismatch]))
      assert(score(truth, 1, row(u.take(50)), row(y), env).left.exists(_.isInstanceOf[ArxFailure.ShapeMismatch]))
      assert(score(truth, 1, row(u), row(y.updated(5, Double.NaN)), env).left.exists(_.isInstanceOf[ArxFailure.NotNumeric]))
      assert(score(truth, -1, row(u), row(y), env).left.exists(_.isInstanceOf[ArxFailure.InvalidOrders]))
      // the residual series is held to the matrix budget
      val small = env.withLimit(Limit.MaxMatrixCells, 100)
      assert(score(truth, 1, row(u), row(y), small).left.exists(_.isInstanceOf[ArxFailure.TooLarge]))
  }

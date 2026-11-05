package it.grypho.scala.leonardo
package control

import core.*
import org.scalatest.flatspec.AnyFlatSpec

class ArxOrderTest extends AnyFlatSpec:

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

  private def row(xs: Vector[Double]) = _MatrixValue(1, xs.size, xs.toArray)
  private def binary(xs: Vector[Double]) = xs.map(x => if x >= 0 then 1.0 else -1.0)

  private val n     = 500
  private val u     = binary(lcg(7, n))
  private val y     = simulate(u, lcg(11, n).map(_ * 0.05))

  "arxOrderSelection" should "recover the order of a known ARX model under BIC" in {
    arxOrderSelection(row(u), row(y), 0 to 4, 1 to 3, 0 to 2, ArxCriterion.BIC, None, env) match
      case Right(s) =>
        assert((s.na, s.nb, s.nk) == (2, 2, 1))
        assert(math.abs(s.model.a(0)(0, 0) - 1.5) < 0.05 && math.abs(s.model.a(1)(0, 0) + 0.7) < 0.05)
        assert(s.scores.size + s.skipped.size == 45)
      case Left(e) => fail(e.toString)
  }

  it should "recover it scored on a validation split" in {
    val uv = binary(lcg(13, 300))
    val yv = simulate(uv, lcg(17, 300).map(_ * 0.05))
    arxOrderSelection(row(u), row(y), 0 to 4, 1 to 3, 0 to 2, ArxCriterion.BIC, Some((row(uv), row(yv))), env) match
      case Right(s) => assert((s.na, s.nb, s.nk) == (2, 2, 1))
      case Left(e)  => fail(e.toString)
  }

  it should "choose the true order on noise-free data, where every larger model fits exactly" in {
    val y0 = simulate(u, Vector.fill(n)(0.0))
    for c <- ArxCriterion.values do
      arxOrderSelection(row(u), row(y0), 0 to 3, 1 to 3, 0 to 2, c, None, env) match
        case Right(s) => assert((s.na, s.nb, s.nk) == (2, 2, 1), c.toString)
        case Left(e)  => fail(e.toString)
  }

  it should "compute AIC, BIC and FPE by their textbook formulas" in {
    // one candidate, (1, 1, 1): scored on k = 1 .. N-1
    val model = arx(row(u), row(y), 1, 1, 1, env).fold(e => fail(e.toString), identity)
    val errs  = (1 until n).map(k => y(k) - model.a(0) * y(k - 1) - model.b(0)(0) * u(k - 1))
    val nn    = errs.size
    val v     = errs.map(e => e * e).sum / nn
    val d     = 2
    val expected = Map(
      ArxCriterion.AIC -> (nn * math.log(v) + 2 * d),
      ArxCriterion.BIC -> (nn * math.log(v) + d * math.log(nn.toDouble)),
      ArxCriterion.FPE -> v * (1 + d.toDouble / nn) / (1 - d.toDouble / nn))
    for (c, want) <- expected do
      arxOrderSelection(row(u), row(y), Seq(1), Seq(1), Seq(1), c, None, env) match
        case Right(s) => assert(math.abs(s.scores.head.score - want) <= 1e-9 * math.abs(want), c.toString)
        case Left(e)  => fail(e.toString)
  }

  it should "skip, and report, a candidate the data does not excite" in {
    // a constant input makes its two lags identical columns
    val uc = Vector.fill(n)(1.0)
    val yc = simulate(uc, lcg(11, n).map(_ * 0.05))
    arxOrderSelection(row(uc), row(yc), Seq(1), 1 to 2, Seq(1), ArxCriterion.AIC, None, env) match
      case Right(s) =>
        assert(s.nb == 1)
        assert(s.skipped.map(t => (t._1, t._2, t._3)) == Vector((1, 2, 1)))
        assert(s.skipped.head._4.isInstanceOf[ArxFailure.NotExcited])
      case Left(e) => fail(e.toString)
  }

  it should "decline a grid past the work budget before fitting anything" in {
    val r = arxOrderSelection(row(u), row(y), 0 to 60, 1 to 30, 0 to 2, ArxCriterion.BIC, None, env)
    assert(r.left.exists(_.isInstanceOf[ArxFailure.TooLarge]))
  }

  it should "decline an empty grid, a bad order and mismatched validation data" in {
    assert(arxOrderSelection(row(u), row(y), Seq.empty, Seq(1), Seq(1), ArxCriterion.AIC, None, env).isLeft)
    assert(arxOrderSelection(row(u), row(y), Seq(1), Seq(0), Seq(1), ArxCriterion.AIC, None, env)
      .left.exists(_.isInstanceOf[ArxFailure.InvalidOrders]))
    val two = _MatrixValue(2, 50, Array.fill(100)(1.0))
    assert(arxOrderSelection(row(u), row(y), Seq(1), Seq(1), Seq(1), ArxCriterion.AIC, Some((two, row(y.take(50)))), env)
      .left.exists(_.isInstanceOf[ArxFailure.ShapeMismatch]))
  }

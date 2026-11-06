package it.grypho.scala.leonardo
package statistics

import core.*
import org.scalatest.flatspec.AnyFlatSpec

class RlsNumericTest extends AnyFlatSpec:

  private val env = new Environment(Environment.DefaultPrecision, Map.empty)
  private def m(rows: Int, cols: Int, xs: Double*) = _MatrixValue(rows, cols, xs.toArray)

  private val theta = m(2, 1, 0.5, -0.2)
  private val p     = m(2, 2, 10.0, 1.0, 1.0, 5.0)
  private val phi   = m(2, 1, 1.5, -0.7)

  "the numeric rlsUpdate" should "agree bit for bit with the expression form" in {
    for (y, lambda) <- Seq((m(1, 1, 0.9), 1.0), (m(1, 1, -2.3), 0.95), (m(1, 1, 0.0), 0.5)) do
      val numeric = rlsUpdate(theta, p, phi, y, lambda).fold(e => fail(e.toString), identity)
      val viaExpr = rlsUpdate(theta, p, phi, y, _Number(lambda), env).getOrElse(fail("declined"))
      assert(viaExpr._1 == numeric._1 && viaExpr._2 == numeric._2)
  }

  it should "update several outputs at once" in {
    val th2 = m(2, 2, 0.5, 1.0, -0.2, 0.0)
    val y2  = m(2, 1, 0.9, 1.1)
    val numeric = rlsUpdate(th2, p, phi, y2, 0.99).fold(e => fail(e.toString), identity)
    val viaExpr = rlsUpdate(th2, p, phi, y2, _Number(0.99), env).getOrElse(fail("declined"))
    assert(viaExpr._1 == numeric._1 && viaExpr._2 == numeric._2)
  }

  it should "name each reason it declines" in {
    import RlsFailure.*
    val y = m(1, 1, 1.0)
    assert(rlsUpdate(theta, p, phi, y, 0.0).left.exists(_.isInstanceOf[LambdaOutOfRange]))
    assert(rlsUpdate(theta, p, phi, y, 1.5).left.exists(_.isInstanceOf[LambdaOutOfRange]))
    assert(rlsUpdate(theta, m(3, 3, Array.fill(9)(1.0)*), phi, y, 1.0).left.exists(_.isInstanceOf[ShapeMismatch]))
    assert(rlsUpdate(theta, p, m(1, 2, 1.0, 1.0), y, 1.0).left.exists(_.isInstanceOf[ShapeMismatch]))
    assert(rlsUpdate(theta, p, phi, m(2, 1, 1.0, 1.0), 1.0).left.exists(_.isInstanceOf[ShapeMismatch]))
    assert(rlsUpdate(theta, p, phi, m(1, 1, Double.NaN), 1.0).left.exists(_ == NotFinite))
    val negative = m(2, 2, -10.0, 0.0, 0.0, -10.0)
    assert(rlsUpdate(theta, negative, phi, y, 1.0).left.exists(_.isInstanceOf[NonPositiveDenominator]))
  }

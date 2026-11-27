package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** F_0387: outside exact mode `log(x, b)` is `ln(x)/ln(b)`, which lands an ulp beside an integer `k` even when
 *  `b^k == x` exactly, so `floor(log(1000))` was `2.0`.  The answer is `k` when the power is exact. */
class LogPowerTest extends AnyFlatSpec:

  private val env = new Environment()
  private def value(s: String): Either[_Expression, _Value] = Parser.parse(s).get.eval(env)

  "log of an exact power" should "be the exponent (F_0387)" in {
    assert(value("log(1000)") == Right(_Number(3.0)))
    assert(value("floor(log(1000))") == Right(_Number(3.0)))
    assert(value("1 + floor(log(10^15))") == Right(_Number(16.0)))
    assert(value("log(8, 2)") == Right(_Number(3.0)))
    assert(value("log(243, 3)") == Right(_Number(5.0)))
    assert(value("log(1/8, 2)") == Right(_Number(-3.0)))
    for k <- 0 to 22 do assert(value(s"log(10^$k)") == Right(_Number(k.toDouble)), s"10^$k")
    for k <- 1 to 60 do assert(value(s"log(2^$k, 2)") == Right(_Number(k.toDouble)), s"2^$k")
  }

  it should "stay the quotient where the power is not exact" in {
    // 1000.0000000000001 is the Double after 1000, no power of 10: its quotient is 2.9999999999999996 as well, and
    // must not be read as 3
    value("log(1000.0000000000001)") match
      case Right(_Number(d)) => assert(d != 3.0 && math.abs(d - 3.0) < 1e-15)
      case other             => fail(s"$other")
    value("log(2)") match
      case Right(_Number(d)) => assert(math.abs(d - 0.30102999566398120) < 1e-15)
      case other             => fail(s"$other")
    // element-wise over a matrix, the same rule
    assert(value("log([[1000, 100]])") == Right(_MatrixValue(1, 2, Array(3.0, 2.0))))
  }

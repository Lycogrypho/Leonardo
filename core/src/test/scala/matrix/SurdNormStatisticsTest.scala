package it.grypho.scala.leonardo
package matrix

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0250 -- norms and statistics of surd cells stay in the exact tier.  They read every surd as
 *  its own working-precision rational, so `norm([[√2], [1]], 1)` came back `2.41421`, and the
 *  2-norm took the root of a ~110-digit rational: the giant radicand behind F_0244. */
class SurdNormStatisticsTest extends AnyFlatSpec:

  private val env = new Environment()
  private def exact(s: String): Either[_Expression, _Value] = Parser.parse(s, Some(30)).get.eval(env)
  private def value(s: String): _Value = exact(s).fold(e => fail(s"$s stayed symbolic: $e"), identity)

  "a vector norm of surd cells" should "be exact over the radical" in {
    assert(value("norm([[2^(1/2)], [1]], 1)") == value("1 + 2^(1/2)"))
    assert(value("norm([[-2^(1/2)], [3*2^(1/2)]], inf)") == value("3*2^(1/2)"))
    // The squares are rational, so the 2-norm is exact: 2, and sqrt(2 + 18) = 2 sqrt 5.
    assert(value("norm([[2^(1/2)], [2^(1/2)]])") == _Rational(2))
    assert(value("norm([[2^(1/2)], [3*2^(1/2)]])") == value("20^(1/2)"))
  }

  it should "still answer, at the working precision, when the squares are not rational" in {
    // (1 + sqrt 2)^2 + 1 = 4 + 2 sqrt 2: its root is no surd, so it is a working-precision rational.
    value("norm([[1 + 2^(1/2)], [1]])") match
      case _Number(d) => assert(math.abs(d - math.sqrt(4 + 2 * math.sqrt(2))) < 1e-12, d)
      case other      => fail(s"$other")
  }

  "the mean and variance of a surd sample" should "be exact over the radical" in {
    assert(value("mean([[2^(1/2), 3*2^(1/2)]])") == value("2*2^(1/2)"))
    // ((sqrt 2 - 2 sqrt 2)^2 + (3 sqrt 2 - 2 sqrt 2)^2) / 1 = 4.
    assert(value("variance([[2^(1/2), 3*2^(1/2)]])") == _Rational(4))
    assert(value("covariance([[2^(1/2), 3*2^(1/2)]], [[1, 3]])") == value("2*2^(1/2)"))
  }

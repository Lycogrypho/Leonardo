package it.grypho.scala.leonardo
package equation

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0177 — in exact mode two values are equal when they agree to the working precision.
 *
 *  An irrational in the exact tier is a rational approximation carried to the working precision,
 *  so comparing two rationals exactly made a true identity through an irrational answer `false`
 *  (`(2^(1/2))^2 == 2`, off by `1.2e-41`).  Two exact values are now equal when
 *  `|a - b| <= 10^-wp * max(|a|, |b|, 1)`; the ordering relations agree, so exactly one of `<`,
 *  `==`, `>` still holds.
 */
class ExactEqualityTest extends AnyFlatSpec:

  private val env = new Environment()

  private def eval(s: String, e: Environment = env): Either[_Expression, _Value] =
    Parser.parse(s, Some(e.workingPrecision)) match
      case Parser.Success(x, _) => x.eval(e)
      case other                => fail(s"parse failed for '$s': $other")

  private def truth(s: String, e: Environment = env): Boolean = eval(s, e) match
    case Right(_Bool(b)) => b
    case other           => fail(s"'$s' gave no truth value: $other")

  "an identity through an irrational" should "hold in exact mode" in {
    assert(truth("(2^(1/2))^2 == 2"))
    assert(truth("(2^(1/2))^2 = 2"))
    assert(!truth("(2^(1/2))^2 != 2"))
  }

  it should "hold for a Cholesky factor with an irrational entry" in {
    assert(truth("chol([[4, 2], [2, 7]]) * transpose(chol([[4, 2], [2, 7]])) == [[4, 2], [2, 7]]"))
  }

  "the ordering relations" should "agree with equality, keeping trichotomy" in {
    assert(!truth("(2^(1/2))^2 < 2"))
    assert(!truth("(2^(1/2))^2 > 2"))
    assert(truth("(2^(1/2))^2 <= 2"))
    assert(truth("(2^(1/2))^2 >= 2"))
  }

  "two rationals" should "differ when they differ above the working precision" in {
    assert(!truth("1/3 == 0.33333"))
    assert(!truth("1 + 10^(-25) == 1"))
    assert(truth("1 + 10^(-25) > 1"))
    assert(truth("1/2 == 2/4"))
  }

  they should "agree below the working precision, relative to their size" in {
    assert(truth("1 + 10^(-35) == 1"))
    assert(truth("10^40 + 10^5 == 10^40"))
    assert(truth("10^(-40) == 0"))      // near zero the floor of 1 makes the tolerance absolute
  }

  "a larger working precision" should "tell apart what a smaller one cannot" in {
    val fine = new Environment(workingPrecision = 50)
    assert(!truth("1 + 10^(-35) == 1", fine))
    assert(truth("(2^(1/2))^2 == 2", fine))
  }

  "equalOnlyToWorkingPrecision" should "report the comparisons the tolerance decided" in {
    def check(l: String, r: String): Boolean =
      (Parser.parse(l, Some(30)), Parser.parse(r, Some(30))) match
        case (Parser.Success(a, _), Parser.Success(b, _)) => equalOnlyToWorkingPrecision(a, b, env)
        case other                                        => fail(s"$other")
    // A cube root is still a working-precision rational, so the tolerance decides its identity.
    assert(check("(2^(1/3))^3", "2"))
    // A square root is an exact surd since F_0231: (√2)² is exactly 2, and the note stays silent.
    assert(!check("(2^(1/2))^2", "2"))
    assert(!check("1/2", "2/4"))
    assert(!check("1/3", "0.33333"))
  }

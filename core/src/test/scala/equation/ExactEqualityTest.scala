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

  // F_0279 (LT L_0066): the hyperbolic and reciprocal functions had no exact kernel, so in exact mode
  // they were read at 15 digits beside a 30-digit cos, ln or exp, and the 30-digit comparison told
  // two equal values apart.
  "an identity through sec, cosh or acosh" should "hold in exact mode (F_0279)" in {
    val atOne = env.withBinding("x", _Rational(1))
    assert(truth("sec(x)^2 == 1/cos(x)^2", atOne))
    assert(truth("derive(tan(x), x) == sec(x)^2", atOne))
    assert(truth("acosh(2) == ln(2 + 3^(1/2))"))
    assert(truth("cosh(1) == (exp(1) + exp(-1))/2"))
    assert(truth("tanh(1/3) == sinh(1/3)/cosh(1/3)"))
    assert(truth("csc(2) == 1/sin(2)"))
    assert(truth("cot(2) == cos(2)/sin(2)"))
    assert(truth("asinh(1/2) == ln(1/2 + (5/4)^(1/2))"))
    assert(truth("atanh(1/2) == ln(3)/2"))
    assert(truth("sech(1) == 1/cosh(1)"))
    assert(truth("csch(1) == 1/sinh(1)"))
    assert(truth("coth(1) == cosh(1)/sinh(1)"))
    // ... and a value that differs in the 20th digit still differs.
    assert(!truth("cosh(1) == cosh(1) + 10^(-20)"))
  }

  it should "keep the exact zeros and refuse the poles" in {
    for s <- Seq("sinh(0)", "tanh(0)", "asinh(0)", "atanh(0)", "acosh(1)") do
      eval(s) match
        case Right(r: _Rational) => assert(r.isZero, s"$s = $r")
        case other               => fail(s"$s: $other")
    for s <- Seq("csch(0)", "coth(0)", "csc(0)", "cot(0)", "atanh(1)", "acosh(1/2)") do
      assert(eval(s).isLeft, s)
  }

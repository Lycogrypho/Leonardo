package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** F_0332 -- `limit` at the jump of `floor`, `ceil` and `round`.  Before the fix the limit algebra
 *  assumed every one-argument function continuous, so `limit(floor(x), x, 2)` answered `2.0` for a
 *  two-sided limit that does not exist. */
class LimitJumpTest extends AnyFlatSpec:

  private val env = new Environment()
  private val exactEnv = new Environment().withExactMode(true)

  private def eval(s: String, e: Environment = env, exact: Boolean = false): Either[_Expression, _Value] =
    (if exact then Parser.parse(s, Some(30)) else Parser.parse(s)) match
      case Parser.Success(x, _) => x.eval(e)
      case other                => fail(s"parse failed for \"$s\": $other")

  private def number(r: Either[_Expression, _Value]): Double = r match
    case Right(_Number(d)) => d
    case other             => fail(s"no number: $other")

  "a two-sided limit at a jump" should "decline" in {
    for src <- Seq("limit(floor(x), x, 2)", "limit(floor(x), x, 0)", "limit(ceil(x), x, 0)",
                   "limit(round(x), x, 0.5)", "limit(round(x), x, -1.5)", "limit(floor(x) - x, x, 3)") do
      assert(eval(src).isLeft, src)
  }

  "a one-sided limit at a jump" should "be the value from that side" in {
    assert(number(eval("limit(floor(x), x, 1, +)")) == 1.0)
    assert(number(eval("limit(floor(x), x, 1, -)")) == 0.0)
    assert(number(eval("limit(ceil(x), x, 1, +)")) == 2.0)
    assert(number(eval("limit(ceil(x), x, 1, -)")) == 1.0)
    assert(number(eval("limit(round(x), x, 0.5, +)")) == 1.0)
    assert(number(eval("limit(round(x), x, 0.5, -)")) == 0.0)
    // read by the epsilon tier's samples, to its usual accuracy
    assert(math.abs(number(eval("limit(floor(x) - x, x, 3, -)")) + 1.0) < 1e-5)
  }

  it should "come from the limit algebra too, with the side of the argument" in {
    // floor(-1/x) as x -> +inf: -1/x -> 0 from below, so floor is -1, not floor(0) = 0
    eval("limit(floor(-1/x), x, inf)") match
      case Right(_Number(d)) => assert(d == -1.0)
      case Left(_)           => succeed       // declining is honest; answering 0 is not
      case other             => fail(other.toString)
    assert(number(eval("limit(floor(1/x), x, inf)")) == 0.0)
  }

  "a limit where the integer part does not jump" should "still answer" in {
    assert(number(eval("limit(floor(x), x, 2.5)")) == 2.0)
    assert(number(eval("limit(floor(x^2), x, 0)")) == 0.0)
    assert(number(eval("limit(round(x), x, 0.3)")) == 0.0)
  }

  "the exact tier" should "answer exactly where the value at the point is the one-sided limit" in {
    // from the right the value at the point is the limit, so substitution answers in the input's tier;
    // from the left it is not, and the numeric tier answers (with the REPL's decimal note)
    assert(eval("limit(floor(x), x, 1, +)", exactEnv, exact = true) == Right(_Rational(1)))
    assert(number(eval("limit(floor(x), x, 1, -)", exactEnv, exact = true)) == 0.0)
    assert(eval("limit(floor(x), x, 1)", exactEnv, exact = true).isLeft)
  }

  "the functionals" should "not be read as continuous functions of their operand" in {
    // a limit of a transform or an unevaluated derivative is not the transform of the limit
    assert(eval("limit(digamma(x) + floor(x), x, 1)").isLeft)
  }

  "a jump reached within the rounding of the argument" should "still be a jump (F_0342)" in {
    // 0.3/0.1 is 2.9999999999999996: read exactly, it was no jump and floor of it answered 2.0
    assert(eval("limit(floor(x/0.1), x, 0.3)").isLeft)
    assert(eval("limit(ceil(x/0.1), x, 0.3)").isLeft)
    assert(number(eval("limit(floor(x/0.1), x, 0.3, +)")) == 3.0)
    assert(number(eval("limit(floor(x/0.1), x, 0.3, -)")) == 2.0)
    assert(number(eval("limit(ceil(x/0.1), x, 0.3, +)")) == 4.0)
    assert(number(eval("limit(ceil(x/0.1), x, 0.3, -)")) == 3.0)
    assert(eval("limit(round(x/0.1), x, 0.25)").isLeft)
    // beside the jump, nothing changes
    assert(number(eval("limit(floor(x/0.1), x, 0.35)")) == 3.0)
    assert(number(eval("limit(floor(x/0.1), x, 0.3 + 1e-9)")) == 3.0)
  }

  it should "be found by the limit algebra itself" in {
    // the arm's own jump test, where the other tiers are not consulted
    val alg = LimitAlgebra(_Variable("x"), env, exact = false)
    val e   = Parser.parse("floor(x/0.1)").get
    assert(alg.lim(e, alg.Pt.At(_Number(0.3), 1)).collect { case alg.LVal.Fin(_Number(d), _) => d }.contains(3.0))
    assert(alg.lim(e, alg.Pt.At(_Number(0.3), -1)).collect { case alg.LVal.Fin(_Number(d), _) => d }.contains(2.0))
    assert(limitAlgebra(e, _Variable("x"), _Number(0.3), LimitDir.Both, env).isEmpty)
  }

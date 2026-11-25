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

  "a jump at an irrational point in the exact tier" should "be seen through the point's reading (F_0364)" in {
    def exactValue(src: String): _Value = eval(src, exactEnv, exact = true) match
      case Right(r) if _Surd.isExact(r) => r
      case other                       => fail(s"$src: not exact: $other")
    // (3^(1/3))^3 is 10^-30 from 3 at the working precision: read exactly, no jump, and both sides gave 2
    for src <- Seq("limit(floor(x^3), x, 3^(1/3))", "limit(ceil(x^3), x, 2^(1/3))",
                   "limit(round(x^3), x, (5/2)^(1/3))") do
      assert(eval(src, exactEnv, exact = true).isLeft, src)
    // the one-sided limits are the values from each side, in the exact tier
    assert(exactValue("limit(floor(x^3), x, 3^(1/3), +)") == _Rational(3))
    assert(exactValue("limit(floor(x^3), x, 3^(1/3), -)") == _Rational(2))
    assert(exactValue("limit(ceil(x^3), x, 2^(1/3), +)") == _Rational(3))
    assert(exactValue("limit(ceil(x^3), x, 2^(1/3), -)") == _Rational(2))
    assert(exactValue("limit(round(x^3), x, (5/2)^(1/3), +)") == _Rational(3))
    assert(exactValue("limit(round(x^3), x, (5/2)^(1/3), -)") == _Rational(2))
    // a true exact point next to an integer is no jump, however close
    assert(exactValue("limit(floor(x), x, 1 + 1/10^40)") == _Rational(1))
    assert(exactValue("limit(floor(x^3), x, 3/2)") == _Rational(3))
  }

  it should "not read a point beside a jump as the jump because it holds an irrational (F_0376)" in {
    def exactValue(src: String): _Value = eval(src, exactEnv, exact = true) match
      case Right(r) if _Surd.isExact(r) => r
      case other                       => fail(s"$src: not exact: $other")
    // the point is 1 + 1.44e-20, above the jump: its reading moves when read more finely, but not towards 1
    assert(exactValue("limit(floor(x), x, 1 + 3^(1/3)/10^20, -)") == _Rational(1))
    assert(exactValue("limit(floor(x), x, 1 + 3^(1/3)/10^20)") == _Rational(1))
    assert(exactValue("limit(ceil(x), x, 1 + 3^(1/3)/10^20, -)") == _Rational(2))
    assert(exactValue("limit(floor(x), x, 1 - 3^(1/3)/10^20, +)") == _Rational(0))
    assert(exactValue("limit(round(x), x, 1/2 + 3^(1/3)/10^20, -)") == _Rational(1))
    assert(exactValue("limit(floor(x^3), x, 3^(1/3) + 1/10^25, -)") == _Rational(3))
    assert(exactValue("limit(floor(x^3), x, 3^(1/3) + 1/10^25)") == _Rational(3))
  }

  it should "be found by the limit algebra itself" in {
    // the arm's own jump test, where the other tiers are not consulted
    val alg = LimitAlgebra(_Variable("x"), env, exact = false)
    val e   = Parser.parse("floor(x/0.1)").get
    assert(alg.lim(e, alg.Pt.At(_Number(0.3), 1)).collect { case alg.LVal.Fin(_Number(d), _) => d }.contains(3.0))
    assert(alg.lim(e, alg.Pt.At(_Number(0.3), -1)).collect { case alg.LVal.Fin(_Number(d), _) => d }.contains(2.0))
    assert(limitAlgebra(e, _Variable("x"), _Number(0.3), LimitDir.Both, env).isEmpty)
  }

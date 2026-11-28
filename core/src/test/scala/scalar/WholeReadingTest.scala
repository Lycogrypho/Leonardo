package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** F_0384 (the owner's decision: the integer): in exact mode a product or quotient of working-precision readings lands
 *  ~10^-30 beside the integer it stands for.  An integer part of such a value is that integer's, as `==` already says;
 *  a true exact value beside an integer keeps its side. */
class WholeReadingTest extends AnyFlatSpec:

  private val exactEnv = new Environment(workingPrecision = 30, exactMode = true)
  private val env      = new Environment()

  private def exact(s: String): Either[_Expression, _Value] = Parser.parse(s, Some(30)).get.eval(exactEnv)
  private def plain(s: String): Either[_Expression, _Value] = Parser.parse(s).get.eval(env)

  "an integer part of a value whole only as a reading" should "be the integer's (F_0384)" in {
    assert(exact("floor(log(10^15))") == Right(_Rational(15)))
    assert(exact("1 + floor(log(10^15))") == Right(_Rational(16)))
    assert(exact("ceil(atan(1)*4/pi)") == Right(_Rational(1)))
    assert(exact("floor((3^(1/3))^3)") == Right(_Rational(3)))
    assert(exact("ceil((2^(1/3))^3)") == Right(_Rational(2)))
    // round jumps at the half-integers: (5/2)^(1/3) cubed is a reading of 5/2, which rounds away from zero
    assert(exact("round(((5/2)^(1/3))^3)") == Right(_Rational(3)))
    assert(exact("round(-((5/2)^(1/3))^3)") == Right(_Rational(-3)))
    // a dividend that is a reading of a multiple of the divisor leaves no remainder
    assert(exact("mod((3^(1/3))^3, 3)") == Right(_Rational(0)))
    // a reading of zero is zero
    assert(exact("step((3^(1/3))^3 - 3)") == Right(_Rational(1)))
  }

  it should "leave a true exact value beside an integer on its own side" in {
    assert(exact("floor(3 - 1/10^40)") == Right(_Rational(2)))
    assert(exact("ceil(2 + 1/10^40)") == Right(_Rational(3)))
    assert(exact("round(5/2 - 1/10^40)") == Right(_Rational(2)))
    assert(exact("mod(3 - 1/10^40, 3)") == exact("3 - 1/10^40"))
    assert(exact("step(-1/10^40)") == Right(_Rational(0)))
    // a point merely beside an integer that holds an irrational is not a reading of it either (F_0376's case)
    assert(exact("floor(1 + 3^(1/3)/10^20)") == Right(_Rational(1)))
    assert(exact("floor(1 - 3^(1/3)/10^20)") == Right(_Rational(0)))
  }

  "a sum's bound" should "be whole when it is an integer or a reading of one, and not otherwise (F_0384)" in {
    assert(exact("sum(1, k, 1, (3^(1/3))^3)") == Right(_Rational(3)))
    // 4 + 1/10^20 is no integer: it was read through a Double as 4
    assert(exact("sum(1, k, 1, 4 + 1/10^20)").isLeft)
    assert(exact("sum(1, k, 1, 4)") == Right(_Rational(4)))
  }

  "log of exact operands" should "be exact when rational and carry the working precision otherwise (F_0384)" in {
    assert(exact("log(100)") == Right(_Rational(2)))
    assert(exact("log(8, 4)") == exact("3/2"))
    assert(exact("log(1/1000)") == Right(_Rational(-3)))
    // it was read through Double: 15 digits where ln(2)/ln(10) keeps thirty
    exact("log(2) - ln(2)/ln(10)") match
      case Right(_Number(d)) => assert(math.abs(d) < 1e-28)
      case other             => fail(s"unexpected $other")
    // outside the real domain the complex fallback is unchanged
    assert(exact("log(-1)").isRight)
    assert(exact("log(2, 1)").isLeft)
  }

  "an argument that must be whole" should "be an integer or a reading of one, and stay unevaluated otherwise (F_0394)" in {
    // a reading of an integer is the integer (F_0384's decision)
    assert(exact("binom(4, (2^(1/3))^3)") == Right(_Rational(6)))
    assert(exact("at([[1, 2, 3]], 1, (3^(1/3))^3)") == Right(_Rational(3)))
    assert(exact("det(eye((2^(1/3))^3))") == Right(_Rational(1)))
    // a true exact non-integer is no index, dimension or lower index: it was read through Double as whole
    assert(exact("binom(4, 2 + 1/10^20)").isLeft)
    assert(exact("at([[1, 2, 3]], 1, 3 - 1/10^20)").isLeft)
    assert(exact("eye(2 + 1/10^20)").isLeft)
    assert(exact("zeros(2 + 1/10^20, 2)").isLeft)
    assert(exact("repmat([[1]], 2 + 1/10^20, 1)").isLeft)
    assert(exact("ones(2, 2 + 1/10^20)").isLeft)
    assert(exact("size([[1, 2]], 2 + 1/10^20)").isLeft)
    // an exact matrix stays on the symbolic carrier: the reading of 2 repeats it twice
    exact("repmat([[1]], (2^(1/3))^3, 1)") match
      case Left(m: matrix._Matrix) => assert(m.rows == 2 && m.cols == 1, m)
      case other                   => fail(s"$other")
    // the upper index of binom may be real, as before
    assert(exact("binom(5/2, 2)").isRight)
    // and a plain integer is as before
    assert(exact("binom(4, 2)") == Right(_Rational(6)))
    assert(exact("at([[1, 2, 3]], 1, 3)") == Right(_Rational(3)))
  }

  "the Double tier" should "be unchanged" in {
    assert(plain("floor((3^(1/3))^3)") == Right(_Number(2.0)))
    assert(plain("floor(2.5)") == Right(_Number(2.0)))
    assert(plain("mod(7, 3)") == Right(_Number(1.0)))
  }

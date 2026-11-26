package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** F_0340 -- the definite integral of a staircase, split at the jumps of its integer parts.  Before the
 *  fix `integral(floor(x), x, 0, 3)` stayed unevaluated. */
class StaircaseIntegralTest extends AnyFlatSpec:

  private val env      = new Environment()
  private val exactEnv = new Environment(workingPrecision = 30, exactMode = true)

  private def plain(s: String): Either[_Expression, _Value] = Parser.parse(s) match
    case Parser.Success(e, _) => e.eval(env)
    case other                => fail(s"parse failed for \"$s\": $other")

  private def exact(s: String): Either[_Expression, _Value] = Parser.parse(s, Some(30)) match
    case Parser.Success(e, _) => e.eval(exactEnv)
    case other                => fail(s"parse failed for \"$s\": $other")

  private def number(r: Either[_Expression, _Value]): Double = r match
    case Right(_Number(d)) => d
    case other             => fail(s"no number: $other")

  private def close(s: String, want: Double) =
    val got = number(plain(s))
    assert(math.abs(got - want) <= 1e-9 * math.max(1.0, math.abs(want)), s"$s = $got, want $want")

  "a staircase" should "integrate piece by piece" in {
    close("integral(floor(x), x, 0, 3)", 3.0)              // 0 + 1 + 2
    close("integral(floor(x), x, 0.5, 1.5)", 0.5)          // 0 over half a unit, then 1 over half
    close("integral(ceil(x), x, 0, 2)", 3.0)               // 1 + 2
    close("integral(round(x), x, 0, 2)", 2.0)              // 0 on [0, 1/2), 1 on (1/2, 3/2), 2 on (3/2, 2]
    close("integral(mod(x, 1), x, 0, 10)", 5.0)            // ten sawteeth of area 1/2
    close("integral(floor(2*x + 1), x, 0, 1)", 1.5)        // 1 on [0, 1/2), 2 on [1/2, 1)
    close("integral(x*floor(x), x, 0, 2)", 1.5)            // 0 + ∫₁² x dx
    close("integral(floor(x), x, 3, 0)", -3.0)             // reversed bounds
    close("integral(floor(-x), x, 0, 2)", -3.0)            // -1 + -2: a negative slope
  }

  it should "be exact on exact input" in {
    assert(exact("integral(floor(x), x, 0, 3)") == Right(_Rational(3)))
    assert(exact("integral(floor(x), x, 1/2, 3/2)") == Right(_Rational.of(BigInt(1), BigInt(2)).getOrElse(fail("1/2"))))
    assert(exact("integral(mod(x, 1), x, 0, 10)") == Right(_Rational(5)))
  }

  it should "keep the jump beside a bound within 10^-16 of it, exactly (F_0365)" in {
    // the jump range was read off a Double reading of the bounds, so the piece past the jump was lost
    def value(s: String): _Value = exact(s).fold(l => fail(s"no value: $l"), identity)
    def rational(s: String): _Value = value(s)
    assert(value("integral(floor(x), x, 0, 4 + 1/10^20)") == rational("6 + 4/10^20"))
    assert(value("integral(floor(x), x, 3 - 1/10^20, 4)") == rational("3 + 2/10^20"))
    assert(value("integral(ceil(x), x, 0, 4 + 1/10^20)") == rational("10 + 5/10^20"))
    assert(value("integral(round(x), x, 0, 1/2 + 1/10^20)") == rational("1/10^20"))
  }

  it should "count its jumps past 2^53 exactly, and not guess them in Double (F_0383)" in {
    def value(s: String): _Value = exact(s).fold(l => fail(s"no value: $l"), identity)
    // 10^20 on [10^20, 10^20 + 1], 10^20 + 1 on the next unit, 10^20 + 2 on the last half
    assert(value("integral(floor(x), x, 10^20, 10^20 + 5/2)") == value("250000000000000000002"))
    assert(value("integral(floor(x), x, 2^53 + 1/2, 2^53 + 5/2) - 2*2^53 - 2") == _Rational(0))
    assert(value("integral(floor(x), x, 2^60 + 1/2, 2^60 + 5/2) - 2*2^60 - 2") == _Rational(0))
    // in Double the jumps past 2^53 are no longer distinct numbers: an answer must be right, or there is none
    plain("integral(floor(x), x, 1e17, 1e17 + 64)") match
      case Right(_Number(d)) => assert(math.abs(d - 6.400000000000002016e18) <= 1e-12 * 6.4e18, s"$d")
      case Left(_)           => succeed
      case other             => fail(other.toString)
  }

  it should "decline what it cannot split, and past the piece budget" in {
    val x = _Variable("x")
    def split(s: String) = Staircase.integral(Parser.parse(s).get, x, _Number(0.5), _Number(3.0), env)
    // a non-linear argument: the jumps of floor(sin(x)) are not enumerated (the quadrature may still answer)
    assert(split("floor(sin(x))").isEmpty)
    // a divisor that depends on the variable
    assert(split("mod(1, x)").isEmpty)
    // nested integer parts
    assert(split("floor(floor(x))").isEmpty)
    assert(split("floor(x)").isDefined)
    val small = env.withLimit(Limit.MaxStaircasePieces, 10)
    assert(Parser.parse("integral(floor(x), x, 0, 20)").get.eval(small).isLeft)
    assert(Parser.parse("integral(floor(x), x, 0, 5)").get.eval(small).isRight)
  }

  "an integrand with no integer part" should "be unchanged" in {
    close("integral(x^2, x, 0, 3)", 9.0)
    assert(exact("integral(x^2, x, 0, 1)") == Right(_Rational.of(BigInt(1), BigInt(3)).getOrElse(fail("1/3"))))
  }

  "a staircase that is not split" should "say why (F_0348)" in {
    def reason(s: String, lo: Double, hi: Double): Option[String] = Parser.parse(s) match
      case Parser.Success(e, _) => Staircase.declineReason(e, _Variable("x"), _Number(lo), _Number(hi), env)
      case other                => fail(other.toString)
    assert(reason("floor(1000*x)", 0, 2).exists(_.contains("2000 pieces")), reason("floor(1000*x)", 0, 2))
    assert(reason("floor(x^2)", 0, 2).exists(_.contains("linear")))
    // a staircase that was split, or none at all, has no reason to give
    assert(reason("floor(x)", 0, 3).isEmpty && reason("sin(x)", 0, 3).isEmpty)
  }

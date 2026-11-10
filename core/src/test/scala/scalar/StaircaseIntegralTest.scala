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

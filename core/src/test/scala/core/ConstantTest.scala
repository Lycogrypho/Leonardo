package it.grypho.scala.leonardo
package core

import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0289 (LT L_0063): the named constants are symbols.  Outside exact mode they evaluate to their
 *  `Double`; in exact mode they stay symbolic, the functions answer exactly where a rule knows the
 *  value, and a number is otherwise read at the working precision. */
class ConstantTest extends AnyFlatSpec:

  private val exactEnv = new Environment(workingPrecision = 30).withExactMode(true)
  private val plainEnv = new Environment()

  private def exact(s: String): Either[_Expression, _Value] = Parser.parse(s, Some(30)).get.eval(exactEnv)
  private def plain(s: String): Either[_Expression, _Value] = Parser.parse(s).get.eval(plainEnv)

  private def q(n: Int, d: Int): _Rational = _Rational.of(BigInt(n), BigInt(d)).getOrElse(fail("zero"))
  private def surd(n: Int, d: Int, r: Int): _Value =
    _Surd.of(_Rational.Zero, q(n, d), BigInt(r)).getOrElse(fail("surd"))

  "a trigonometric function at a rational multiple of pi" should "answer exactly (F_0289)" in {
    assert(exact("sin(pi/6)") == Right(q(1, 2)))
    assert(exact("cos(pi/3)") == Right(q(1, 2)))
    assert(exact("sin(pi)") == Right(q(0, 1)))
    assert(exact("cos(pi/2)") == Right(q(0, 1)))
    assert(exact("cos(2*pi)") == Right(q(1, 1)))
    assert(exact("sin(-pi/2)") == Right(q(-1, 1)))
    assert(exact("sin(pi/4)") == Right(surd(1, 2, 2)))
    assert(exact("cos(5*pi/6)") == Right(surd(-1, 2, 3)))
    assert(exact("tan(pi/3)") == Right(surd(1, 1, 3)))
    assert(exact("tan(3*pi/4)") == Right(q(-1, 1)))
  }

  it should "stay undefined at a pole" in {
    assert(exact("tan(pi/2)").isLeft)
  }

  it should "answer each term of a tabulation exactly" in {
    exact("tabulate(sin(k*pi), k, 1, 4)") match
      case Left(m: _MatrixShaped) => assert(m.children.forall(_ == q(0, 1)), s"$m")
      case other                  => fail(s"$other")
  }

  "a function with no exact rule" should "read the constant at the working precision" in {
    exact("cos(pi/7)") match
      case Right(r: _Rational) =>
        assert((r.toBigDecimal(30) - BigDecimal("0.900968867902419126236102319507")).abs < BigDecimal("1e-25"))
      case other => fail(s"$other")
  }

  "ln" should "invert e exactly" in {
    assert(exact("ln(e)") == Right(q(1, 1)))
    assert(exact("ln(e^3)") == Right(q(3, 1)))
  }

  "arithmetic on a constant in exact mode" should "stay symbolic" in {
    exact("pi/2") match
      case Left(e) => assert(e.toString == "(pi / 2)")
      case other   => fail(s"$other")
  }

  "a relation" should "read the constants where it needs a number" in {
    assert(exact("pi > 3") == Right(_Bool(true)))
    assert(exact("pi < 22/7") == Right(_Bool(true)))
    assert(exact("e == pi") == Right(_Bool(false)))
  }

  "goldenratio" should "be the exact surd in exact mode, and satisfy its equation" in {
    assert(exact("goldenratio") == Right(_Surd.of(q(1, 2), q(1, 2), BigInt(5)).getOrElse(fail("surd"))))
    assert(exact("goldenratio^2 - goldenratio - 1") == Right(q(0, 1)))
  }

  "outside exact mode" should "evaluate every constant to its Double" in {
    assert(plain("pi/2") == Right(_Number(math.Pi / 2)))
    assert(plain("eulergamma") == Right(_Number(0.5772156649015329)))
    assert(plain("goldenratio") == Right(_Number(1.618033988749895)))
    assert(plain("γ + φ") == Right(_Number(0.5772156649015329 + 1.618033988749895)))
  }

  "a formula" should "print the constant's name" in {
    assert(Parser.parse("sin(pi*y/2)").get.toString == "sin(((pi * y) / 2.0))")
    assert(Parser.parse("φ").get.toString == "goldenratio")
  }

  "derive" should "treat a constant as a constant, and e^u as exp(u)" in {
    assert(scalar.derive(Parser.parse("pi*x").get, _Variable("x")) == _Constant(ConstantKind.Pi))
    val d = scalar.derive(Parser.parse("e^(2*x)").get, _Variable("x"))
    assert(d.eval(plainEnv.withBinding("x", _Number(0))) == Right(_Number(2)))
  }

  "a definite integral to pi" should "answer exactly in exact mode" in {
    assert(exact("integral(sin(x), x, 0, pi)") == Right(q(2, 1)))
  }

package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0324 (LT L_0080) -- `floor`, `ceil`, `round` and `mod`.  Before the fix the grammar had none of
 *  the four: `floor(x)` parsed as the product of a free variable `floor` with `x`. */
class RoundingFunctionsTest extends AnyFlatSpec:

  private val plain = new Environment()
  private val exactEnv = new Environment(workingPrecision = 30, exactMode = true)

  private def parse(src: String, exact: Boolean = false): _Expression =
    Parser.parse(src, if exact then Some(30) else None) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for '$src': $other")

  private def value(src: String): Either[_Expression, _Value] = parse(src).eval(plain)
  private def exact(src: String): Either[_Expression, _Value] = parse(src, exact = true).eval(exactEnv)

  private def num(src: String): Double = value(src) match
    case Right(_Number(d)) => d
    case other             => fail(s"'$src' did not reduce to a real number: $other")

  private def rat(n: Int, d: Int = 1): _Rational = _Rational.of(BigInt(n), BigInt(d)).getOrElse(fail("rational"))

  "floor and ceil" should "round towards minus and plus infinity" in {
    assert(num("floor(2.5)") == 2.0 && num("floor(-2.5)") == -3.0 && num("floor(3)") == 3.0)
    assert(num("ceil(2.1)") == 3.0 && num("ceil(-2.1)") == -2.0 && num("ceil(3)") == 3.0)
  }

  "round" should "go half away from zero" in {
    assert(num("round(2.4)") == 2.0 && num("round(2.5)") == 3.0 && num("round(2.6)") == 3.0)
    assert(num("round(-2.4)") == -2.0 && num("round(-2.5)") == -3.0 && num("round(-2.6)") == -3.0)
    assert(num("round(0.5)") == 1.0 && num("round(-0.5)") == -1.0)
  }

  it should "not step over the largest double below one half" in {
    // floor(x + 0.5) is 1 there, because x + 0.5 rounds up to 1.0; the fractional part is exact
    assert(num("round(0.49999999999999994)") == 0.0)
  }

  "the results" should "never be a negative zero" in {
    for src <- Seq("ceil(-0.5)", "round(-0.4)", "floor(0)")
    do assert(1.0 / num(src) == Double.PositiveInfinity, src)
  }

  "in exact mode" should "answer an exact integer" in {
    assert(exact("floor(7/2)") == Right(rat(3)) && exact("floor(-7/2)") == Right(rat(-4)))
    assert(exact("ceil(7/2)") == Right(rat(4)) && exact("ceil(-7/2)") == Right(rat(-3)))
    assert(exact("round(5/2)") == Right(rat(3)) && exact("round(-5/2)") == Right(rat(-3)))
    assert(exact("round(7/3)") == Right(rat(2)) && exact("round(1/2)") == Right(rat(1)))
  }

  it should "decide a surd exactly, past the working precision" in {
    assert(exact("floor(2^(1/2))") == Right(rat(1)) && exact("ceil(2^(1/2))") == Right(rat(2)))
    // sqrt(10^80 - 1) is 10^40 - 5e-41: a 30-digit reading is 10^40 itself, the floor is one less
    val n = BigInt(10).pow(40)
    assert(exact("floor((10^80 - 1)^(1/2))") == Right(_Rational(n - 1)))
    assert(exact("ceil((10^80 - 1)^(1/2))") == Right(_Rational(n)))
  }

  "a named constant" should "be read at the working precision in exact mode" in {
    assert(exact("floor(pi)") == Right(rat(3)) && exact("ceil(pi)") == Right(rat(4)))
  }

  "the functions" should "act element-wise on a matrix" in {
    assert(value("floor([[1.5, -1.5], [2, 0.5]])") == value("[[1, -2], [2, 0]]"))
    assert(value("round([[1.5, -1.5]])") == value("[[2, -2]]"))
  }

  they should "stay symbolic on a free name, a complex value and an interval" in {
    assert(value("floor(x)") == Left(parse("floor(x)")))
    assert(value("floor(2 + 3i)").isLeft && value("round(i)").isLeft)
    assert(value("ceil([1, 2])").isLeft)
  }

  "mod" should "take the sign of the divisor" in {
    assert(num("mod(7, 3)") == 1.0 && num("mod(-7, 3)") == 2.0)
    assert(num("mod(7, -3)") == -2.0 && num("mod(-7, -3)") == -1.0)
    assert(num("mod(5.5, 2)") == 1.5 && num("mod(6, 3)") == 0.0)
  }

  it should "be exact on exact input" in {
    assert(exact("mod(1/2, 1/3)") == Right(rat(1, 6)))
    assert(exact("mod(-1/2, 1/3)") == Right(rat(1, 6)))
    assert(exact("mod(10^30 + 7, 12)") == Right(rat(((BigInt(10).pow(30) + 7) mod BigInt(12)).toInt)))
  }

  it should "decline a zero divisor and a non-finite operand" in {
    assert(value("mod(7, 0)").isLeft && exact("mod(7, 0)").isLeft)
    assert(value("mod(inf, 3)").isLeft)
  }

  it should "not form a quotient it cannot hold, in Double" in {
    // a - b*floor(a/b) loses everything once a/b passes 2^53; the remainder itself is exact
    val a = 1e15 + 0.5
    val b = 0.1
    // java.math.BigDecimal(double) is the exact binary value; Scala's BigDecimal(double) is the
    // shortest decimal, a different number
    val expected = new java.math.BigDecimal(a).remainder(new java.math.BigDecimal(b)).doubleValue
    val got = Parser.parse(s"mod($a, $b)") match
      case Parser.Success(e, _) => e.eval(plain)
      case other                => fail(other.toString)
    assert(got == Right(_Number(expected)), got.toString)
  }

  "derive" should "be zero away from the jumps, and mod's slope the identity's" in {
    def at(src: String, x: Double, y: Double): Double =
      parse(src).eval(plain.withBinding("x", _Number(x)).withBinding("y", _Number(y))) match
        case Right(_Number(d)) => d
        case other             => fail(s"'$src' did not reduce: $other")
    assert(at("derive(floor(x^2), x)", 1.5, 0) == 0.0)
    assert(at("derive(ceil(x), x)", 1.5, 0) == 0.0 && at("derive(round(x), x)", 1.5, 0) == 0.0)
    assert(at("derive(mod(x, 3), x)", 7.5, 0) == 1.0)
    assert(at("derive(mod(x, y), y)", 7.0, 3.0) == -2.0)    // -floor(x/y)
  }

  // a domain node answers as an expression in the language, so it is read as text
  private def shown(src: String): String = value(src).fold(_.toString, _.toString)

  "differentiable" should "exclude the jumps, which are infinitely many" in {
    // like tan: an infinite exclusion set is not written out, so the answer stays symbolic
    for src <- Seq("differentiable(floor(x), x)", "differentiable(ceil(x + 1), x)",
                   "differentiable(round(x), x)", "differentiable(mod(x, 3), x)")
    do assert(shown(src).startsWith("differentiable("), s"$src -> ${shown(src)}")
    // away from the argument's variable the function is a constant
    assert(shown("differentiable(floor(y), x)") == "true")
  }

  "domain" should "be the whole line, but mod needs a non-zero divisor" in {
    assert(shown("domain(floor(x), x)") == "true" && shown("domain(round(x), x)") == "true")
    assert(shown("domain(mod(x, y), y)") == "((y < 0.0) or (y > 0.0))", shown("domain(mod(x, y), y)"))
    assert(shown("domain(mod(x, 3), x)") == "true")
  }

  "the printed form" should "re-parse to the same expression" in {
    for src <- Seq("floor(x)", "ceil(x + 1)", "round(x/2)", "mod(x, 3)") do
      val e = parse(src)
      assert(!e.toString.startsWith("("), e.toString)
      assert(parse(e.toString) == e, src)
  }

  "the LaTeX" should "use the brackets of the floor and the ceiling, and bmod for the remainder" in {
    assert(latex.ToLatex(parse("floor(x)")) == "\\left\\lfloor x\\right\\rfloor")
    assert(latex.ToLatex(parse("ceil(x/2)")) == "\\left\\lceil \\frac{x}{2.0}\\right\\rceil")
    assert(latex.ToLatex(parse("mod(a + 1, b)")) == "\\left(a + 1.0\\right) \\bmod b")
    assert(latex.ToLatex(parse("round(x)")) == "\\mathrm{round}\\left(x\\right)")
  }

  "the four words" should "be reserved" in {
    for w <- Seq("floor", "ceil", "round", "mod") do assert(Parser.ReservedWords.contains(w), w)
  }

  "sample" should "agree with the tree, through the compiled closure" in {
    val e = parse("floor(x) + mod(x, 2) + round(x) + ceil(x)")
    val pts = sample(e, _Variable("x"), 0.25, 3.25, 4, plain)
    // at 0.25: 0 + 0.25 + 0 + 1; at 1.25: 1 + 1.25 + 1 + 2; at 2.25: 2 + 0.25 + 2 + 3; at 3.25: 3 + 1.25 + 3 + 4
    assert(pts.map(_._2) == Vector(1.25, 5.25, 7.25, 11.25))
  }

package it.grypho.scala.leonardo
package equation

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** F_0351, F_0353, F_0354 -- the decimal tier's root finding: close roots read from the written factors,
 *  a cut point's membership from its threshold, every real root of a polynomial equation.  Before:
 *  `solve((x - 1)*(x - 1.00001) > 0, x)` was `x != 1.000005`, `domain(1/(x^2 - 2), x)` was `true`, and
 *  `solve((x - 1)^2*(x + 5) = 0, x)` was `-5` alone. */
class CloseRootsTest extends AnyFlatSpec:

  private val env      = new Environment()
  private val exactEnv = new Environment(workingPrecision = 30, exactMode = true)
  private val x        = _Variable("x")

  private def parse(src: String, exact: Boolean = false): _Expression =
    Parser.parse(src, if exact then Some(30) else None) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for '$src': $other")

  private def ans(src: String): String = parse(src).eval(env).toExpression.toString

  private def roots(src: String, exact: Boolean = false): List[_Expression] = parse(src, exact) match
    case eq: _Equation => solve(eq, x, if exact then exactEnv else env).map(_.rhs)
    case other         => fail(s"not an equation: $other")

  private def numbers(src: String): List[Double] = roots(src).map {
    case _Number(d) => d
    case other      => fail(s"not a number: $other")
  }

  "an inequality over close roots" should "keep them apart when they are written as factors" in {
    assert(ans("solve((x - 1)*(x - 1.00001) > 0, x)") == "((x < 1.0) or (x > 1.00001))")
    assert(ans("solve((x - 1)*(x - 1.00001)*(x + 5) > 0, x)") == "(((x > -5.0) and (x < 1.0)) or (x > 1.00001))")
    assert(ans("solve((x + 5)/((x - 1)*(x - 1.00001)) > 0, x)") == "(((x > -5.0) and (x < 1.0)) or (x > 1.00001))")
    // at any scale: the same two bounds a million times smaller and larger
    for k <- Seq("1e-6", "1e6") do
      parse(s"solve((x - $k)*(x - 1.00001*$k) > 0, x)").eval(env).toExpression match
        case logic.Or(_Comparison(_, CompareOp.Lt, _Number(a)), _Comparison(_, CompareOp.Gt, _Number(b))) =>
          assert(b > a && math.abs(b / a - 1.00001) < 1e-12, s"$a, $b")
        case other => fail(other.toString)
  }

  it should "tell an expanded quadratic's close roots from a double root by the discriminant's rounding" in {
    // gap 1e-6: the discriminant is 1e-13 of its terms, far above their rounding; it read `x != 1.0`
    parse("solve(x^2 - 2.000001*x + 1.000001 > 0, x)").eval(env).toExpression match
      case logic.Or(_Comparison(_, CompareOp.Lt, _Number(a)), _Comparison(_, CompareOp.Gt, _Number(b))) =>
        assert(math.abs(a - 1) < 1e-8 && math.abs(b - 1.000001) < 1e-8 && a < b, s"$a, $b")
      case other => fail(other.toString)
  }

  "a multiple root typed in decimals" should "still be one root" in {
    assert(ans("solve(x^2 - 0.6*x + 0.09 > 0, x)") == "(x != 0.3)")
    assert(ans("solve(x^2 - 2.2*x + 1.21 > 0, x)") == "(x != 1.1)")
    assert(ans("solve((x - 0.3)^3 > 0, x)") == "(x > 0.3)")
    assert(ans("solve((x - 0.3)^4 > 0, x)") == "(x != 0.3)")
    assert(ans("solve(x^3 - 0.9*x^2 + 0.27*x - 0.027 > 0, x)") == "(x > 0.3)")
    assert(ans("solve(x^3 + 4.4*x^2 - 2.91*x + 0.45 > 0, x)") == "(((x > -5.0) and (x < 0.3)) or (x > 0.3))")
    assert(numbers("x^2 - 0.6*x + 0.09 = 0") == List(0.3))
  }

  "a rational inequality" should "be answered when it is not strict, and when nothing cuts the line" in {
    assert(ans("solve((x - 1)^2/(x - 2) >= 0, x)") == "(x = 1.0 or (x > 2.0))")
    assert(ans("solve(x/(x^2 - 1) <= 0, x)") == "((x < -1.0) or ((x >= 0.0) and (x < 1.0)))")
    assert(ans("solve(1/(x^2 + 1) > 0, x)") == "true")
    // a point the numerator cancels is still undefined
    assert(ans("solve((x^2 - 1)/(x - 1) > 0, x)") == "(((x > -1.0) and (x < 1.0)) or (x > 1.0))")
  }

  "the polynomial tiers" should "answer as before" in {
    assert(ans("solve(x^2 - 4 > 0, x)") == "((x < -2.0) or (x > 2.0))")
    assert(ans("solve(x^2 - 4 <= 0, x)") == "((x >= -2.0) and (x <= 2.0))")
    assert(ans("solve(x^2 + 1 > 0, x)") == "true" && ans("solve(x^2 + 1 < 0, x)") == "false")
    assert(ans("solve(x^2 <= 0, x)") == "x = 0.0" && ans("solve(x^2 >= 0, x)") == "true")
    assert(ans("solve(x^3 - x >= 0, x)") == "(((x >= -1.0) and (x <= 0.0)) or (x >= 1.0))")
    assert(ans("solve(1/x^2 > 0, x)") == "(x != 0.0)")
    // the stable form (issue 1.1): the small root of x^2 + 1e8*x + 1 keeps its digits
    parse("solve(x^2 + 1e8*x + 1 > 0, x)").eval(env).toExpression match
      case logic.Or(_Comparison(_, CompareOp.Lt, _Number(a)), _Comparison(_, CompareOp.Gt, _Number(b))) =>
        assert(a == -1e8 && b == -1e-8, s"$a, $b")
      case other => fail(other.toString)
  }

  "solve of a polynomial equation" should "list every real root (F_0354)" in {
    assert(numbers("(x - 1)^2*(x + 5) = 0") == List(-5.0, 1.0))
    assert(numbers("x^4 - 2*x^2 + 1 = 0") == List(-1.0, 1.0))
    assert(numbers("(x - 150)*(x - 1)*(x + 5) = 0") == List(-5.0, 1.0, 150.0))
    assert(numbers("(x - 1)*(x - 1.000001) = 0") == List(1.0, 1.000001))
    assert(numbers("x^3 = 1e9").map(r => math.round(r * 1e6) / 1e6) == List(1000.0))
    assert(numbers("x^4 + 1 = 0").isEmpty)
    assert(numbers("1/x + 1/(x - 1) = 2").zip(List(1 - math.sqrt(0.5), 1 + math.sqrt(0.5))).forall((a, b) => math.abs(a - b) < 1e-12))
    // the transcendental equations keep the scan
    assert(numbers("x*exp(x) = 1").exists(r => math.abs(r - 0.5671432904097838) < 1e-9))
    assert(partialRootsNote(parse("(x - 150)*(x - 1)*(x + 5) = 0").asInstanceOf[_Equation], x, env).isEmpty)
  }

  it should "be exact on exact input" in {
    assert(roots("x^3 - 6*x^2 + 11*x - 6 = 0", exact = true) == List(_Rational(1), _Rational(2), _Rational(3)))
    val rs = roots("(x - 1/3)*(x^2 - 2) = 0", exact = true)
    assert(rs.size == 3 && rs.forall(_Surd.isExact) && rs(1) == parse("1/3", exact = true).eval(exactEnv).toExpression, rs.toString)
  }

  "a domain" should "exclude a zero of its argument however the root rounds (F_0353)" in {
    assert(ans("domain(1/(x^2 - 2), x)") == "(((x < -1.41421) or ((x > -1.41421) and (x < 1.41421))) or (x > 1.41421))")
    assert(ans("domain(ln(x^2 - 2), x)") == "((x < -1.41421) or (x > 1.41421))")
    assert(ans("domain((x^2 - 2)^(1/2), x)") == "((x <= -1.41421) or (x >= 1.41421))")
    assert(ans("differentiable((x^2 - 2)^(1/2), x)") == "((x < -1.41421) or (x > 1.41421))")
    assert(ans("domain(asin(x^2 - 2), x)") == "(((x >= -1.73205) and (x <= -1.0)) or ((x >= 1.0) and (x <= 1.73205)))")
    assert(ans("domain(1/((x - 1)*(x - 1.0001)), x)") == "(((x < 1.0) or ((x > 1.0) and (x < 1.0001))) or (x > 1.0001))")
    assert(ans("domain(ln((x - 1)*(x - 1.0001)), x)") == "((x < 1.0) or (x > 1.0001))")
  }

  "singularities" should "count a written power and keep two close poles apart" in {
    assert(ans("singularities(1/((x - 1)*(x - 1.0001)), x)") == "[[1.0, 1.0001], [1.0, 1.0]]")
    assert(ans("singularities(1/((x - 1)^2*(x + 5)), x)") == "[[-5.0, 1.0], [1.0, 2.0]]")
    assert(ans("singularities((x^3 - 1)/(x - 1), x)") == "[[1.0], [0.0]]")
    assert(ans("singularities(1/(x^2 - 0.6*x + 0.09), x)") == "[[0.3], [2.0]]")
  }

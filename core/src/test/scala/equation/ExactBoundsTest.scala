package it.grypho.scala.leonardo
package equation

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** F_0337 -- the bounds of an inequality's solution set and of a domain in the input's tier.  Before the
 *  fix, under `exact on`, `solve(x^2 > 4, x)` answered `x < -2.0 or x > 2.0` and `domain(ln(x - 1/3), x)`
 *  `x > 0.33333`. */
class ExactBoundsTest extends AnyFlatSpec:

  private val exactEnv = new Environment(workingPrecision = 30, exactMode = true)

  private def exact(src: String): String = Parser.parse(src, Some(30)) match
    case Parser.Success(e, _) => e.eval(exactEnv).toExpression.toString
    case other                => fail(s"parse failed for '$src': $other")

  private def plain(src: String): String = Parser.parse(src) match
    case Parser.Success(e, _) => e.eval(new Environment()).toExpression.toString
    case other                => fail(s"parse failed for '$src': $other")

  /** The rendered answer holds no decimal leaf: every number in it is exact. */
  private def noDecimal(s: String): Boolean = !s.contains(".0") && !raw"\d\.\d".r.findFirstIn(s).isDefined

  "an inequality under exact on" should "have exact bounds" in {
    assert(exact("solve(x^2 > 4, x)") == "((x < -2) or (x > 2))")
    assert(exact("solve(x^2 - 1/9 < 0, x)") == "((x > -1/3) and (x < 1/3))")
    assert(exact("solve(x^2 - 2 > 0, x)").contains("√2"), exact("solve(x^2 - 2 > 0, x)"))
    assert(exact("solve(x^2*(2*x - 9) < 0, x)") == "((x < 0) or ((x > 0) and (x < 9/2)))")
    assert(noDecimal(exact("solve(1/(x - 1/3) > 0, x)")), exact("solve(1/(x - 1/3) > 0, x)"))
    assert(noDecimal(exact("solve(abs(x - 1/3) < 1/2, x)")), exact("solve(abs(x - 1/3) < 1/2, x)"))
  }

  it should "keep a decimal where the root has no exact form" in {
    // x^3 - 2x - 5 has one real root, irrational, not a surd of one square root
    assert(raw"\d\.\d".r.findFirstIn(exact("solve(x^3 - 2*x - 5 > 0, x)")).isDefined)
  }

  "a domain under exact on" should "have exact bounds and exact constants" in {
    assert(exact("domain(ln(x), x)") == "(x > 0)")
    assert(exact("domain(1/x, x)") == "((x < 0) or (x > 0))")
    assert(exact("domain(ln(x - 1/3), x)") == "(x > 1/3)")
    assert(exact("domain(asin(2*x), x)") == "((x >= -1/2) and (x <= 1/2))")
    assert(exact("domain(ln(x - a), x)") == "((x + (-1 * a)) > 0)")
    assert(noDecimal(exact("domain(1/(x^2 - 2), x)")), exact("domain(1/(x^2 - 2), x)"))
  }

  "singularities under exact on" should "be exact" in {
    assert(exact("singularities(1/(x - 1/3)^2, x)") == "[[1/3], [2]]")
  }

  "outside exact mode" should "be unchanged" in {
    assert(plain("solve(x^2 > 4, x)") == "((x < -2.0) or (x > 2.0))")
    assert(plain("domain(ln(x - 1/3), x)") == "(x > 0.33333)")
    assert(plain("domain(1/x, x)") == "((x < 0.0) or (x > 0.0))")
    assert(plain("singularities(1/(x - 1)^2, x)") == "[[1.0], [2.0]]")
  }

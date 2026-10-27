package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** F_0225: `expand` divides a sum by a numeric divisor term by term, and writes a polynomial in
 *  one variable by descending degree, the textbook form of a characteristic polynomial. */
class ExpandOrderTest extends AnyFlatSpec:

  private def exact(s: String): _Expression = Parser.parse(s, Some(30)).get
  private def plain(s: String): _Expression = Parser.parse(s).get

  "a sum over a numeric divisor" should "be divided term by term, exactly on exact input" in {
    assert(expand(exact("(4*x3 + 6*x4)/2")).toString == "((2 * x3) + (3 * x4))")
  }

  it should "be divided in the Double tier on inexact input" in {
    val r = expand(plain("(4*x3 + 6*x4)/2"))
    assert(r == Sum(Product(_Number(2), _Variable("x3")), Product(_Number(3), _Variable("x4"))))
  }

  it should "accept a negative divisor" in {
    assert(expand(exact("(4*x3 + 2*x4)/(-2)")).toString == "((-2 * x3) + (-1 * x4))")
  }

  "a sum over a symbolic divisor" should "stay a quotient" in {
    assert(expand(plain("(x + 1)/y")) == Ratio(Sum(_Variable("x"), _Number(1)), _Variable("y")))
  }

  "a division by zero" should "stay a quotient" in {
    assert(expand(plain("(x + 1)/0")).isInstanceOf[Ratio])
  }

  "a polynomial in one variable" should "be written by descending degree" in {
    val textbook = "(((lambda ^ 2) + (-6 * lambda)) + 8)"
    assert(expand(exact("(lambda - 2)*(lambda - 4)")).toString == textbook)
    assert(expand(exact("(4 - lambda)*(2 - lambda)")).toString == textbook)
  }

  it should "keep its value" in {
    val e   = plain("(1 + x)^3 / 4")
    val env = Environment().withBinding("x", _Number(1.7))
    (e.eval(env), expand(e).eval(env)) match
      case (Right(_Number(a)), Right(_Number(b))) => assert(math.abs(a - b) < 1e-12)
      case other                                  => fail(s"not numeric: $other")
  }

  "a polynomial in several variables" should "keep first-seen order" in {
    // No numeral, so the Double tier even in exact mode (a letters-only expression, F_0171).
    assert(expand(exact("(b + a)*(b + a)")).toString == "(((b ^ 2.0) + (2.0 * (b * a))) + (a ^ 2.0))")
  }

  "a single-variable polynomial inside a function" should "be ordered too" in {
    assert(expand(exact("sin((1 + x)^2)")).toString == "sin((((x ^ 2) + (2 * x)) + 1))")
  }

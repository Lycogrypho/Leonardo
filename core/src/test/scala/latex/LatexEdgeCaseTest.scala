package it.grypho.scala.leonardo
package latex

import core.*
import scalar.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** Regressions from the audit of 2026-10-23 in the LaTeX emitter and the based literals: each
 *  case failed before its fix. */
class LatexEdgeCaseTest extends AnyFlatSpec:

  private def tex(s: String, exact: Option[Int] = None): String =
    ToLatex(Parser.parse(s, exact).get.eval(new Environment()).toExpression)

  "a name with an underscore" should "render as a subscript, never a raw `_` (F_0206)" in {
    assert(ToLatex(_Variable("a_b_c")) == "a_{b\\_c}")
    assert(ToLatex(_Variable("theta_0")) == "\\theta_{0}")
    assert(ToLatex(_Variable("x")) == "x")
  }

  "a negative exact term" should "render as a subtraction (F_0206)" in {
    val t = tex("x - 1/3", Some(30))
    assert(t.contains("- \\frac{1}{3}") && !t.contains("+ -"), t)
  }

  "a number" should "typeset its exponent and infinity (F_0206)" in {
    assert(ToLatex(_Number(1e-4)) == "1.0 \\times 10^{-4}")
    assert(ToLatex(_Number(Double.PositiveInfinity)) == "\\infty")
    assert(ToLatex(_Number(2.5)) == "2.5")
  }

  "a pure-imaginary power base" should "keep its brackets (F_0206)" in {
    val t = ToLatex(Power(_Complex.of(0, 2), _Variable("x")))
    assert(t.startsWith("\\left("), t)
  }

  "a based literal" should "not wrap past a Long (F_0186)" in {
    Parser.parse("0x10000000000000000").get.eval(new Environment()) match
      case Right(_Number(d)) => assert(d == math.pow(2, 64))
      case other             => fail(s"got $other")
  }

  "a negative based value" should "print a form that re-parses to it (F_0186)" in {
    Parser.parse("tobase(-255, 16)").get.eval(new Environment()) match
      case Right(b: _Based) =>
        assert(b.toString == "tobase(-255, 16)")
        assert(Parser.parse(b.toString).get.eval(new Environment()) == Right(b))
      case other => fail(s"got $other")
  }

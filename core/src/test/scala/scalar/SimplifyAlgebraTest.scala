package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0292 (LT L_0053): like terms, the Pythagorean pair and common factors. */
class SimplifyAlgebraTest extends AnyFlatSpec:

  private def p(s: String, exact: Boolean = false): _Expression =
    val r = Parser.parse(s, if exact then Some(30) else None)
    assert(r.successful, s"parse failed: $s")
    r.get

  private val env = new Environment()

  private def valueAt(e: _Expression, bindings: (String, Double)*): Double =
    bindings.foldLeft(env)((en, b) => en.withBinding(b._1, _Number(b._2))) match
      case en => e.eval(en) match
        case Right(_Number(d)) => d
        case other             => fail(s"$e: $other")

  "like terms" should "collect, whatever the order of their factors (F_0292)" in {
    assert(valueAt(simplifyAlgebraic(p("2*sin(x)*cos(x) - 2*cos(x)*sin(x)"))) == 0.0)
    assert(simplifyAlgebraic(p("x*y + 3*y*x")).toString.count(_ == '+') == 0)
  }

  "the Pythagorean pair" should "read as 1" in {
    assert(valueAt(simplifyAlgebraic(p("cos(t)^2 + sin(t)^2"))) == 1.0)
    assert(simplifyAlgebraic(p("cos(t)*r*cos(t) + r*sin(t)*sin(t)")) == _Variable("r"))
  }

  "a common factor" should "cancel across a quotient" in {
    val q = simplifyAlgebraic(p("((x + h)^2 - x^2)/h"))
    assert(!q.toString.contains("/"), s"$q")
    assert(math.abs(valueAt(q, "x" -> 2, "h" -> 0.5) - 4.5) < 1e-12)
    assert(simplifyAlgebraic(p("(x*y)/x")) == _Variable("y"))
  }

  "an expression with nothing to combine" should "keep its form" in {
    val e = p("x*y + z")
    assert(simplifyAlgebraic(e) eq e)
  }

  "derive" should "answer in collected form" in {
    assert(p("derive(sin(x)^2 + cos(x)^2, x)").eval(env) == Right(_Number(0)))
    val d = p("derive(x^x, x)").eval(env).toExpression
    assert(!d.toString.contains("(x / x)"), s"$d")
    assert(math.abs(valueAt(d, "x" -> 2) - 4 * (math.log(2) + 1)) < 1e-12)
  }

  "an exact sum" should "collect exactly" in {
    val e = simplifyAlgebraic(p("x/3 + x/6", exact = true))
    assert(e.eval(env.withBinding("x", _Rational(2))) == Right(_Rational(1)))
  }

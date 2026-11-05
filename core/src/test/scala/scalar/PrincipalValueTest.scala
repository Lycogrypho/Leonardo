package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0297 (the owner's decision): `x^(p/q)` at a negative `x` is the principal complex value, in every
 *  tier — `limit` and the definite integral too, which read the real domain and declined. */
class PrincipalValueTest extends AnyFlatSpec:

  private val env = new Environment()
  private def value(s: String): (Double, Double) =
    Parser.parse(s).get.eval(env) match
      case Right(v: _Value) => _Complex.parts(v).getOrElse(fail(s"$s: $v"))
      case other            => fail(s"$s: $other")

  private def near(got: (Double, Double), want: (Double, Double)): Unit =
    assert(math.hypot(got._1 - want._1, got._2 - want._2) < 1e-6, s"got $got, want $want")

  private val w = (0.5, math.sqrt(3) / 2)   // e^(i*pi/3)

  "limit at a negative base" should "be the principal value (F_0297)" in {
    near(value("limit(x^(1/3), x, -8)"), (2 * w._1, 2 * w._2))
    near(value("limit(ln(x), x, -1)"), (0.0, math.Pi))
  }

  "a definite integral over negative x" should "integrate the principal value" in {
    near(value("integral(x^(1/3), x, -8, -1)"), (11.25 * w._1, 11.25 * w._2))
  }

  "a real limit and a real integral" should "be unchanged" in {
    near(value("limit(x^(1/3), x, 8)"), (2.0, 0.0))
    near(value("integral(x^(1/3), x, 1, 8)"), (11.25, 0.0))
  }

  "a value with no principal value in the library" should "still decline" in {
    assert(Parser.parse("limit(asin(x), x, 2)").get.eval(env).isLeft)
  }

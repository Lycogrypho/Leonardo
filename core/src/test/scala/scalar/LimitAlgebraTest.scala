package it.grypho.scala.leonardo
package scalar

import org.scalatest.flatspec.AnyFlatSpec
import core.*
import parser.Parser


/** F_0286 (LT L_0052, L_0055, L_0070): the limits the numeric tiers cannot reach, answered by the limit
 *  algebra — free parameters, growth at infinity, infinite one-sided limits. */
class LimitAlgebraTest extends AnyFlatSpec:

  private val env = new Environment()

  private def result(src: String): Either[_Expression, _Value] =
    val p = Parser.parse(src)
    assert(p.successful, s"parse failed: $src")
    p.get.eval(env)

  /** The limit as a number, failing when it declined. */
  private def number(src: String): Double = result(src) match
    case Right(_Number(d)) => d
    case other             => fail(s"$src: $other")

  private def is(src: String, want: Double): Unit =
    val d = number(src)
    if want.isInfinite then assert(d == want, s"$src: $d")
    else assert(math.abs(d - want) <= 1e-9 * math.max(1.0, math.abs(want)), s"$src: $d, want $want")

  "a limit with a free parameter" should "answer symbolically (F_0286)" in {
    for (src, bind, want) <- Seq(("limit(((x + h)^2 - x^2)/h, h, 0)", "x" -> 3.0, 6.0),
                                 ("limit(a/x, x, 2)", "a" -> 4.0, 2.0),
                                 ("limit(sin(a*x)/x, x, 0)", "a" -> 3.0, 3.0)) do
      result(src) match
        case Left(e) if !e.isInstanceOf[_Limit] =>
          e.eval(env.withBinding(bind._1, _Number(bind._2))) match
            case Right(_Number(d)) => assert(math.abs(d - want) <= 1e-9, s"$src: $d")
            case other             => fail(s"$src: $other")
        case other => fail(s"$src declined: $other")
  }

  "a limit at infinity" should "compare growth (F_0286)" in {
    is("limit(n/2^n, n, inf)", 0.0)
    is("limit((n + 1)^2/2^(n + 1)*2^n/n^2, n, inf)", 0.5)
    is("limit((1 + 2/x)^x, x, inf)", math.exp(2))
    is("limit(x^2 - 3*x + 4, x, inf)", Double.PositiveInfinity)
    is("limit(x^(7/5), x, inf)", Double.PositiveInfinity)
    is("limit(ln(x)/x, x, inf)", 0.0)
    is("limit(n*sin(5/n), n, inf)", 5.0)
    is("limit(x*exp(-x), x, inf)", 0.0)
  }

  "an infinite one-sided limit" should "answer its infinity (F_0286)" in {
    is("limit(-1/x^2, x, 0)", Double.NegativeInfinity)
    is("limit(-1/t, t, 0, +)", Double.NegativeInfinity)
    is("limit(ln(t), t, 0, +)", Double.NegativeInfinity)
    is("limit(h^(-2/3), h, 0, +)", Double.PositiveInfinity)
  }

  "the limits F_0276 turned into declines" should "now answer (F_0286)" in {
    is("limit(h^(1/3)/h, h, 0, +)", Double.PositiveInfinity)
    is("limit(1/sin(x), x, pi, -)", Double.PositiveInfinity)
    is("limit(1/sin(x), x, pi, +)", Double.NegativeInfinity)
    is("limit(tan(x), x, pi/2, -)", Double.PositiveInfinity)
    is("limit(tan(x), x, pi/2, +)", Double.NegativeInfinity)
    assert(result("limit(1/sin(x), x, pi)").isLeft, "the sides differ")
    is("limit(x^(-2) - (x^(-2) - 1), x, 0)", 1.0)
    is("limit(ln(sin(x)/x)/x^2, x, 0)", -1.0 / 6)
    is("limit((sin(x)/x)^(1/x^2), x, 0)", math.exp(-1.0 / 6))
    is("limit(cos(x)^(1/x^2), x, 0)", math.exp(-0.5))
  }

  "a generic answer" should "record what it assumed (F_0286, the owner's decision)" in {
    val e = Parser.parse("x/(x - a)").get
    val conditions = limitConditions(e, _Variable("x"), _Number(2), LimitDir.Both, env)
    assert(conditions.nonEmpty, "no condition recorded")
    assert(limitConditions(Parser.parse("a/x").get, _Variable("x"), _Number(2), LimitDir.Both, env).isEmpty)
  }

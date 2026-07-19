package it.grypho.scala.leonardo
package scalar

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.BeforeAndAfter
import core.*
import scalar.*
import parser.Parser


class LimitTest extends AnyFlatSpec with BeforeAndAfter:

  private val env = new Environment()

  // â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€ helpers â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

  private def parse(s: String): _Expression = Parser.parse(s).get
  private def eval(s: String): String =
    parse(s).eval(env).toExpression match
      case n: _Number  => n.display(5)
      case other       => other.toString

  private def approx(s: String, expected: Double, tol: Double = 1e-4): Unit =
    val result = parse(s).eval(env).toExpression
    result match
      case _Number(d) => assert(math.abs(d - expected) <= tol,
        s"$s: expected â‰ˆ $expected but got $d")
      case other => fail(s"$s: expected numeric result, got $other")

  // â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€ _Number infinity display â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

  "inf constant" should "parse to _Number(+Infinity)" in:
    assert(parse("inf") == _Number(Double.PositiveInfinity))

  it should "parse -inf to _Number(-Infinity) via unary minus" in:
    assert(parse("-inf") == _Number(Double.NegativeInfinity))

  it should "display +âˆž as inf" in:
    assert(_Number(Double.PositiveInfinity).toString == "inf")

  it should "display -âˆž as -inf" in:
    assert(_Number(Double.NegativeInfinity).toString == "-inf")

  it should "display +âˆž via display(p) as inf" in:
    assert(_Number(Double.PositiveInfinity).display(5) == "inf")

  it should "display -âˆž via display(p) as -inf" in:
    assert(_Number(Double.NegativeInfinity).display(5) == "-inf")

  // â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€ parser round-trips â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

  "limit parser" should "parse two-sided limit into _Limit(Both)" in:
    val e = parse("limit(x^2, x, 3)")
    assert(e.isInstanceOf[_Limit])
    val l = e.asInstanceOf[_Limit]
    assert(l.v.variable == "x")
    assert(l.dir == LimitDir.Both)

  it should "parse limit with + direction into FromRight" in:
    val e = parse("limit(1/x, x, 0, +)")
    val l = e.asInstanceOf[_Limit]
    assert(l.dir == LimitDir.FromRight)

  it should "parse limit with - direction into FromLeft" in:
    val e = parse("limit(1/x, x, 0, -)")
    val l = e.asInstanceOf[_Limit]
    assert(l.dir == LimitDir.FromLeft)

  it should "parse limit at inf" in:
    val e = parse("limit(1/x, x, inf)")
    val l = e.asInstanceOf[_Limit]
    assert(l.point == _Number(Double.PositiveInfinity))

  it should "parse limit at -inf" in:
    val e = parse("limit(atan(x), x, -inf)")
    val l = e.asInstanceOf[_Limit]
    assert(l.point == _Number(Double.NegativeInfinity))

  it should "round-trip through toString â€” two-sided" in:
    val e   = parse("limit(x^2, x, 3.0)")
    val str = e.toString
    val e2  = parse(str)
    assert(e == e2)

  it should "round-trip through toString â€” from right" in:
    val e   = parse("limit(1/x, x, 0.0, +)")
    val str = e.toString
    val e2  = parse(str)
    assert(e == e2)

  it should "round-trip through toString â€” from left" in:
    val e   = parse("limit(1/x, x, 0.0, -)")
    val str = e.toString
    val e2  = parse(str)
    assert(e == e2)

  // â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€ Tier 1: direct substitution â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

  "limit eval" should "evaluate x^2 at x=3 by direct substitution" in:
    approx("limit(x^2, x, 3)", 9.0)

  it should "evaluate x + 1 at x = 5" in:
    approx("limit(x + 1, x, 5)", 6.0)

  it should "evaluate sin(x) at x = 0" in:
    approx("limit(sin(x), x, 0)", 0.0)

  it should "evaluate cos(x) at x = 0" in:
    approx("limit(cos(x), x, 0)", 1.0)

  it should "evaluate a constant" in:
    approx("limit(7, x, 0)", 7.0)

  // â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€ Tier 2: L'HÃ´pital 0/0 â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

  it should "apply L'HÃ´pital for sin(x)/x at 0 (â†’ 1)" in:
    approx("limit(sin(x)/x, x, 0)", 1.0)

  it should "apply L'HÃ´pital for (x^2 - 1)/(x - 1) at 1 (â†’ 2)" in:
    approx("limit((x^2 - 1)/(x - 1), x, 1)", 2.0)

  it should "apply L'HÃ´pital for (x^3 - x)/(x - 1) at 1 (â†’ 2)" in:
    approx("limit((x^3 - x)/(x - 1), x, 1)", 2.0)

  // â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€ Tier 2: c/0 form â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

  it should "return +inf for 1/x at 0 from the right" in:
    assert(eval("limit(1/x, x, 0, +)") == "inf")

  it should "return -inf for 1/x at 0 from the left" in:
    assert(eval("limit(1/x, x, 0, -)") == "-inf")

  it should "return +inf for 1/x^2 at 0 (two-sided, denom always positive)" in:
    assert(eval("limit(1/x^2, x, 0)") == "inf")

  it should "stay symbolic for 1/x at 0 two-sided (limit does not exist)" in:
    val r = parse("limit(1/x, x, 0)").eval(env).toExpression
    assert(r.isInstanceOf[_Limit], s"expected symbolic _Limit, got $r")

  // â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€ Tier 3: limits at Â±âˆž â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

  it should "evaluate 1/x as xâ†’inf to 0" in:
    approx("limit(1/x, x, inf)", 0.0)

  it should "evaluate x as xâ†’inf to +inf" in:
    assert(eval("limit(x, x, inf)") == "inf")

  it should "evaluate x^2 as xâ†’inf to +inf" in:
    assert(eval("limit(x^2, x, inf)") == "inf")

  it should "evaluate x^2 as xâ†’-inf to +inf (even power)" in:
    assert(eval("limit(x^2, x, -inf)") == "inf")

  it should "evaluate exp(x) as xâ†’inf to +inf" in:
    assert(eval("limit(exp(x), x, inf)") == "inf")

  it should "evaluate exp(x) as xâ†’-inf to 0" in:
    approx("limit(exp(x), x, -inf)", 0.0)

  it should "evaluate atan(x) as xâ†’+inf to Ï€/2" in:
    approx("limit(atan(x), x, inf)", math.Pi / 2, 1e-10)

  it should "evaluate atan(x) as xâ†’-inf to -Ï€/2" in:
    approx("limit(atan(x), x, -inf)", -math.Pi / 2, 1e-10)

  it should "evaluate rational same-degree (2x^2+x)/(x^2+1) at +inf to 2" in:
    approx("limit((2*x^2 + x)/(x^2 + 1), x, inf)", 2.0)

  it should "evaluate rational lower-degree numerator 1/(x^2+1) at +inf to 0" in:
    approx("limit(1/(x^2 + 1), x, inf)", 0.0)

  it should "evaluate rational (3x + 1)/(x + 2) at +inf to 3" in:
    approx("limit((3*x + 1)/(x + 2), x, inf)", 3.0)

  // â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€ symbolic cases â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

  it should "stay symbolic when the limit point is an unbound variable" in:
    val r = parse("limit(sin(x)/x, x, a)").eval(env).toExpression
    assert(r.isInstanceOf[_Limit], s"expected symbolic _Limit, got $r")

  it should "stay symbolic for oscillating sin(x) at +inf" in:
    val r = parse("limit(sin(x), x, inf)").eval(env).toExpression
    assert(r.isInstanceOf[_Limit], s"expected symbolic _Limit, got $r")

  // â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€ simplify / expand pass-through â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

  it should "simplify body and point of a _Limit" in:
    val e = _Limit(Sum(_Number(1), _Number(0)), _Variable("x"), Sum(_Number(3), _Number(0)))
    val s = simplify(e)
    s match
      case _Limit(body, _, pt, _) =>
        assert(body == _Number(1))
        assert(pt   == _Number(3))
      case other => fail(s"expected _Limit, got $other")

  it should "expand body of a _Limit" in:
    val e = _Limit(Product(_Variable("x"), Sum(_Variable("x"), _Number(1))), _Variable("x"), _Number(0))
    val expanded = expand(e)
    expanded match
      case _Limit(body, _, _, _) =>
        assert(body.toString.contains("+"))  // (x*x + x*1)
      case other => fail(s"expected _Limit, got $other")

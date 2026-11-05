package it.grypho.scala.leonardo
package equation

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0295 (LT L_0072): an invertible function of the unknown is inverted, whatever the root's size;
 *  a polynomial inequality of any degree is answered by its sign chart. */
class SolveInversionTest extends AnyFlatSpec:

  private val env = new Environment()
  private def p(s: String): _Expression = Parser.parse(s).get

  private def roots(src: String): List[Double] =
    p(src) match
      case s: _Solve => solve(s.eq.asInstanceOf[_Equation], s.v, env).map(_.rhs.eval(env) match
        case Right(_Number(d)) => d
        case other             => fail(s"$src: $other"))
      case other => fail(s"$other")

  "an exponential equation" should "be inverted whatever the size of its root (F_0295)" in {
    val r = roots("solve(exp(-t/1000) = 1/2, t)")
    assert(r.size == 1 && math.abs(r.head - 1000 * math.log(2)) < 1e-9, s"$r")
    assert(math.abs(roots("solve(exp(-t/100) = 1/2, t)").head - 100 * math.log(2)) < 1e-9)
    assert(math.abs(roots("solve(3*exp(2*t) + 1 = 7, t)").head - math.log(2) / 2) < 1e-12)
    assert(roots("solve(exp(t) = -1, t)").isEmpty)
  }

  "a logarithm or a power of a constant base" should "be inverted" in {
    assert(math.abs(roots("solve(ln(x - 1) = 2, x)").head - (1 + math.exp(2))) < 1e-9)
    assert(math.abs(roots("solve(2^x = 1000000, x)").head - math.log(1e6) / math.log(2)) < 1e-9)
    assert(math.abs(roots("solve(log(x) = 3, x)").head - 1000) < 1e-9)
  }

  private def ineq(src: String): _Expression = p(src).eval(env).toExpression

  private def holds(rel: _Expression, x: Double): Boolean =
    rel.eval(env.withBinding("x", _Number(x))) match
      case Right(_Bool(b)) => b
      case other           => fail(s"$rel at $x: $other")

  "a cubic inequality" should "be answered by its sign chart" in {
    val r = ineq("solve(x^2*(2*x - 9) < 0, x)")
    assert(!r.isInstanceOf[_Solve], s"$r")
    for x <- Seq(-3.0, -0.1, 0.1, 2.0, 4.4) do assert(holds(r, x), s"$r at $x")
    for x <- Seq(0.0, 4.5, 5.0, 10.0) do assert(!holds(r, x), s"$r at $x")
  }

  it should "include the zeros of a non-strict relation, an isolated one too" in {
    val r = ineq("solve(x^2*(2*x - 9) <= 0, x)")
    for x <- Seq(-3.0, 0.0, 2.0, 4.5) do assert(holds(r, x), s"$r at $x")
    assert(!holds(r, 5.0))
    val s = ineq("solve((x - 1)^2*(x - 3) >= 0, x)")
    for x <- Seq(1.0, 3.0, 4.0) do assert(holds(s, x), s"$s at $x")
    for x <- Seq(0.0, 2.0) do assert(!holds(s, x), s"$s at $x")
  }

  "a quartic with no real root" should "be true or false" in {
    assert(ineq("solve(x^4 + 1 > 0, x)") == _Bool(true))
    assert(ineq("solve(x^4 + 1 < 0, x)") == _Bool(false))
  }

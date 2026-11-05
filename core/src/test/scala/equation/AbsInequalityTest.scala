package it.grypho.scala.leonardo
package equation

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0290 (LT L_0056): an inequality on an absolute value. */
class AbsInequalityTest extends AnyFlatSpec:

  private val env = new Environment()
  private def solved(s: String): _Expression = Parser.parse(s).get.eval(env).toExpression

  private def holds(rel: _Expression, x: Double): Boolean =
    rel.eval(env.withBinding("x", _Number(x))) match
      case Right(_Bool(b)) => b
      case other           => fail(s"$rel at $x: $other")

  private def check(src: String, in: Seq[Double], out: Seq[Double]): Unit =
    val r = solved(src)
    assert(!r.isInstanceOf[_Solve], s"$src stayed unevaluated")
    in.foreach(x => assert(holds(r, x), s"$src: $r should hold at $x"))
    out.foreach(x => assert(!holds(r, x), s"$src: $r should not hold at $x"))

  "abs(u) < c" should "be the band around the centre (F_0290)" in {
    check("solve(abs(x - 1) < 1/20, x)", Seq(0.96, 1.0, 1.04), Seq(0.94, 1.06))
    check("solve(abs(2*x) <= 4, x)", Seq(-2.0, 0.0, 2.0), Seq(-2.1, 2.1))
  }

  "abs(u) > c" should "be the two tails" in {
    check("solve(abs(x - 1) > 2, x)", Seq(-2.0, 3.5), Seq(-1.0, 1.0, 3.0))
    check("solve(3 < abs(x), x)", Seq(-4.0, 4.0), Seq(0.0, 3.0))
  }

  "a negative bound" should "decide the relation outright" in {
    assert(solved("solve(abs(x) < -1, x)") == _Bool(false))
    assert(solved("solve(abs(x) > -1, x)") == _Bool(true))
  }

  "two absolute values" should "compare by squaring" in {
    check("solve(abs(x - 1) < abs(x + 1), x)", Seq(0.5, 3.0), Seq(-0.5, -3.0))
  }

  "a symbolic bound of unknown sign" should "stay unevaluated" in {
    assert(solved("solve(abs(x) < a, x)").isInstanceOf[_Solve])
  }

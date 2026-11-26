package it.grypho.scala.leonardo
package equation

import core.*
import matrix._Matrix
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** F_0381, F_0382 (audit 2026-11-25): polynomial roots whose coefficients or roots span many decades.  Scaling an
 *  equation must not move its roots, and `solve` must never bind a value that is not a root. */
class ExtremeCoefficientsTest extends AnyFlatSpec:

  private val env = new Environment()

  private def eval(s: String): _Expression = Parser.parse(s).get.eval(env).fold(identity, identity)

  /** The roots `solve` answers, ascending; empty when it stays unevaluated. */
  private def roots(s: String): Vector[Double] =
    def rhs(x: _Expression): Double = x match
      case _Equation(_, _Number(d)) => d
      case other                    => fail(s"$s: not a numeric root: $other")
    eval(s) match
      case eq: _Equation                => Vector(rhs(eq))
      case m: _Matrix if m.rows == 1    => m.elems.map(rhs).sorted
      case _: _Solve                    => Vector.empty
      case other                        => fail(s"$s: $other")

  private def near(got: Vector[Double], want: Vector[Double], s: String) =
    assert(got.size == want.size && got.zip(want).forall((g, w) => math.abs(g - w) <= 1e-9 * math.abs(w)),
           s"$s: $got, want $want")

  "a quadratic" should "keep its roots whatever the size of its coefficients (F_0381)" in {
    near(roots("solve(x^2 + 1e200*x + 1 = 0, x)"), Vector(-1e200, -1e-200), "1e200")
    near(roots("solve(x^2 + 1e160*x + 1 = 0, x)"), Vector(-1e160, -1e-160), "1e160")
    near(roots("solve(1e-170*x^2 - 3e-170*x + 2e-170 = 0, x)"), Vector(1.0, 2.0), "1e-170")
    near(roots("solve(1e200*x^2 - 3e200*x + 2e200 = 0, x)"), Vector(1.0, 2.0), "1e200")
    // the sign chart reads the same two roots, not one double root
    val chart = eval("solve(x^2 + 1e200*x + 1 > 0, x)").toString
    assert(!chart.contains("!=") && chart.contains(" or "), chart)
  }

  "an expanded polynomial with roots far apart" should "keep its small roots and bind only roots (F_0382)" in {
    def close(got: Vector[Double], want: Vector[Double], s: String) =
      assert(got.size == want.size && got.zip(want).forall((g, w) => math.abs(g - w) <= 1e-6 * math.max(1.0, math.abs(w))),
             s"$s: $got, want $want")
    // the roots are about -1, 1 and 1/c; at the roots' scale 1/c the pair +-1 merged into a double root at 0
    close(roots("solve(1e-6*x^3 - x^2 + 1 = 0, x)"), Vector(-1.0, 1.0, 1e6), "1e-6")
    close(roots("solve(1e-12*x^3 - x^2 + 1 = 0, x)"), Vector(-1.0, 1.0, 1e12), "1e-12")
    // the sign chart: positive between -1 and 1, and past 1e12
    val chart = Parser.parse("solve(1e-12*x^3 - x^2 + 1 > 0, x)").get.eval(env).fold(identity, identity)
    def holds(x: Double) = chart.eval(env.withBinding("x", _Number(x))) match
      case Right(_Bool(b)) => b
      case other           => fail(s"$chart at $x: $other")
    assert(holds(0.0) && !holds(2.0) && !holds(-2.0) && holds(2e12), chart.toString)
    // three simple poles, not a double pole at 0
    eval("singularities(1/(1e-12*x^3 - x^2 + 1), x)") match
      case m: _Matrix => assert(m.cols == 3, m.toString)
      case other      => fail(other.toString)
  }

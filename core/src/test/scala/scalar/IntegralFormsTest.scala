package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0287: the standard forms LT L_0057 and L_0069 found the integrator missing.  Each answer is
 *  checked by differentiating it back numerically (a central difference), never through its
 *  printed form, since an antiderivative has many correct spellings; and each is checked in exact
 *  mode as well. */
class IntegralFormsTest extends AnyFlatSpec:

  private def valueAt(e: _Expression, v: String, x: Double): Double =
    e.eval(new Environment().withBinding(v, _Number(x))) match
      case Right(_Number(y)) => y
      case other             => fail(s"$e did not evaluate at $v = $x: $other")

  private def parse(source: String, exact: Boolean): _Expression =
    val p = Parser.parse(source, if exact then Some(30) else None)
    assert(p.successful, s"parse failed: $source")
    p.get

  /** The antiderivative of `source`, verified against the integrand at `points`. */
  private def closes(source: String, v: String = "x", points: Seq[Double] = Seq(0.3, 0.55, 0.8),
                     exact: Boolean = false): _Expression =
    val f = parse(source, exact)
    val F = integrate(f, _Variable(v))
    assert(!containsIntegral(F), s"$source stayed unevaluated: $F")
    val h = 1e-5
    points.foreach[Unit] { x =>
      val slope = (valueAt(F, v, x + h) - valueAt(F, v, x - h)) / (2 * h)
      val want  = valueAt(f, v, x)
      assert(math.abs(slope - want) <= 1e-6 * math.max(1.0, math.abs(want)),
        s"d/d$v of $F at $x is $slope, the integrand is $want")
      ()
    }
    F

  /** Whether any constant in `e` is a `Double`. */
  private def hasDouble(e: _Expression): Boolean = e match
    case _: _Number => true
    case other      => other.children.exists(hasDouble)

  private val forms = Seq(
    "1/(7*x^(1/2))", "2*x*x^(1/2)", "1/x^(3/2)", "1/x^(1/2)", "(x^2 - 2*x + 2)^2",
    "exp(-x)*sin(x)", "exp(x/2)*cos(2*x)", "3*exp(2*x + 1)*sin(3*x - 1)",
    "ln(x)/x^2", "1/(x*ln(x)^2)", "x*(1 - x)^(1/2)", "x^3*(1 + x^2)^(1/2)", "1/(x^2 + 4)^(3/2)",
    "1/(x^2*(9 + x^2)^(1/2))", "x^(1/2)/(x^2 + x)", "sin(x)^5*cos(x)^8", "x*sin(x)*cos(x)",
    "cos(ln(x))")

  forms.foreach { src =>
    "integrate" should s"integrate $src (F_0287)" in closes(src)
    it should s"integrate $src in exact mode" in closes(src, exact = true)
  }

  it should "integrate in another variable" in {
    closes("t^2*t*2*t", "t")
  }

  it should "keep an algebraic answer exact on exact input" in {
    Seq("1/(7*x^(1/2))", "2*x*x^(1/2)", "1/x^(3/2)", "(x^2 - 2*x + 2)^2", "x*(1 - x)^(1/2)",
        "sin(x)^5*cos(x)^8").foreach { src =>
      val F = closes(src, exact = true)
      assert(!hasDouble(F), s"$src: $F")
    }
  }

  it should "close the definite integral of an integrable singularity through the antiderivative" in {
    // Deferred from F_0288: the antiderivative now exists, so the endpoint is read as its limit.
    val r = parse("integral(1/x^(1/2), x, 0, 1)", exact = false).eval(new Environment())
    r match
      case Right(_Number(y)) => assert(math.abs(y - 2.0) <= 1e-9, s"$y")
      case other             => fail(s"$other")
  }

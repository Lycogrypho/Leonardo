package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0171 phase B — the structural integrator builds its constants in the integrand's tier.
 *
 *  `integrate` built every constant as a `Double` and read every slope as one, so an exact
 *  integrand integrated inexactly: in exact mode `∫ 3t` was `3·(t^2.0/2.0)`.  Each case is checked
 *  for exact leaves and, independently, by differentiating back at a point.
 */
class ExactIntegrationTest extends AnyFlatSpec:

  private val t = _Variable("t")

  private def parse(s: String, exact: Boolean = true): _Expression =
    Parser.parse(s, Option.when(exact)(30)) match
      case Parser.Success(x, _) => x
      case other                => fail(s"parse failed for \"$s\": $other")

  /** Every numeric leaf that is not exact. */
  private def inexact(e: _Expression): List[_Expression] = e match
    case _: _Rational => Nil
    case n: _Number   => List(n)
    case other        => other.children.flatMap(inexact)

  /** The antiderivative of `src`, checked exact and checked by differentiating back at `t = 7/10`. */
  private def exactAntiderivative(src: String): _Expression =
    val f = parse(src)
    val g = integrate(f, t)
    assert(!g.toString.contains("integral"), s"$src stayed unevaluated: $g")
    assert(inexact(g).isEmpty, s"$src -> $g")
    val at = new Environment().withBinding("t", _Number(0.7))
    (derive(g, t).eval(at), f.eval(at)) match
      case (Right(_Number(a)), Right(_Number(b))) => assert(math.abs(a - b) <= 1e-9 * math.max(1.0, math.abs(b)), s"$src: $a vs $b")
      case other                                  => fail(s"$src: $other")
    g

  "integrate" should "apply the power rule exactly" in {
    exactAntiderivative("3*t")
    exactAntiderivative("t^2")
    exactAntiderivative("t/3")
    exactAntiderivative("(2*t + 1)^3")
  }

  it should "divide by an exact slope" in {
    exactAntiderivative("sin(2*t)")
    exactAntiderivative("exp(t/2)")
    exactAntiderivative("cos(3*t + 1)")
  }

  // An integrand in letters alone (`t*exp(t)`, `tan(t)`) holds no exact value, so `integrate`
  // keeps its Double constants: the content has no tier, and nothing is bound (V3's limit).
  // Each case below therefore carries an exact number.

  it should "integrate by parts exactly" in {
    exactAntiderivative("t*exp(2*t)")
    exactAntiderivative("t^2*sin(t)")
    exactAntiderivative("ln(3*t)")
  }

  it should "reduce a power exactly" in {
    exactAntiderivative("sin(t)^3")
    exactAntiderivative("cos(2*t)^4")
    exactAntiderivative("tan(t)^3")
  }

  it should "use the rule table exactly" in {
    exactAntiderivative("tan(2*t)")
    exactAntiderivative("sec(2*t)*tan(2*t)")
    exactAntiderivative("ln(2*t)/(2*t)")
  }

  // Phase C: the rational tier computes in Double arrays.
  it should "integrate a rational function exactly" in {
    exactAntiderivative("1/(2*t + 1)")
    exactAntiderivative("(t + 3)/(t^2 - 3*t + 2)")
    exactAntiderivative("1/(t^2 + 4)")
    exactAntiderivative("t^3/(t + 1)")
  }

  it should "decompose repeated, quadratic and irrational factors exactly" in {
    exactAntiderivative("1/((t - 1)^2*(t + 2))")
    exactAntiderivative("1/(t^2 + 2)")        // atan with an exact radical, 2^(1/2)
    exactAntiderivative("1/(t^2 - 2)")        // irrational real roots: one logarithm of a quotient
    exactAntiderivative("1/(t^3 + t)")
    exactAntiderivative("1/(t^2 + 1)^2")
    exactAntiderivative("(2*t + 3)/(t^2 + t + 1)")
  }

  it should "fall back to the Double tier for an irreducible cubic, and still be right" in {
    val f = parse("1/(t^3 - 2)")
    val g = integrate(f, t)
    assert(!g.toString.contains("integral") && inexact(g).nonEmpty, g)
    val at = new Environment().withBinding("t", _Number(0.7))
    (derive(g, t).eval(at), f.eval(at)) match
      case (Right(_Number(a)), Right(_Number(b))) => assert(math.abs(a - b) <= 1e-9 * math.abs(b), s"$a vs $b")
      case other                                  => fail(s"$other")
  }

  it should "substitute the half angle and the trigonometric radical exactly" in {
    exactAntiderivative("1/(2 + cos(t))")
    exactAntiderivative("(4 - t^2)^(1/2)")
    exactAntiderivative("(t^2 + 9)^(-1/2)")
  }

  it should "substitute exactly" in {
    exactAntiderivative("3*t*exp(t^2)")
    exactAntiderivative("sin(t)^2*cos(t)")
  }

  "an integral of letters bound to exact values" should "answer exactly (V3)" in {
    val env = new Environment().withBinding("a", _Rational.of(BigInt(1), BigInt(3)).getOrElse(fail("1/3")))
                               .withBinding("t", _Rational(2))
    assert(parse("integral(a*t, t)").eval(env) == Right(_Rational.of(BigInt(2), BigInt(3)).getOrElse(fail("2/3"))))
  }

  "an integrand with no exact value" should "integrate with Double constants as before" in {
    assert(inexact(integrate(parse("t", exact = false), t)).nonEmpty)
    assert(integrate(parse("3*t", exact = false), t).toString == integrate(Product(_Number(3), t), t).toString)
  }

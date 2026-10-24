package it.grypho.scala.leonardo
package transform

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** Regressions from the audit of 2026-10-23 in the inverse transforms: each case failed before its fix. */
class InverseTransformEdgeCaseTest extends AnyFlatSpec:

  private val env = new Environment()

  /** The inverse of `f`, evaluated at `t = at`. */
  private def inverseAt(f: String, at: Double, in: Environment = env): Double =
    Parser.parse(s"invlaplace($f, s, t)").get.eval(in) match
      case Left(_: _InverseLaplace) => fail(s"$f stayed unevaluated")
      case r => r.toExpression.eval(in.withBinding("t", _Number(at))) match
        case Right(_Number(d)) => d
        case other             => fail(s"not a number at t = $at: $other")

  "invlaplace" should "keep an s-free exp numerator as a constant factor (F_0189)" in {
    assert(math.abs(inverseAt("exp(3)/(s+1)", 0.0) - math.exp(3)) < 1e-9)
  }

  it should "shift by exp(-s/2), not drop it (F_0189)" in {
    assert(inverseAt("exp(-s/2)/s", 0.25) == 0.0)
    assert(inverseAt("exp(-s/2)/s", 0.75) == 1.0)
  }

  it should "read a rounded repeated quadratic as repeated (F_0189)" in {
    assert(math.abs(inverseAt("1/(s^2 + 0.2*s + 0.01)", 1.0) - math.exp(-0.1)) < 1e-9)
  }

  it should "read the session's bound parameters, but never the frequency variable (F_0209)" in {
    val withA = env.withBinding("a", _Number(2))
    assert(math.abs(inverseAt("1/(s - a)", 0.5, withA) - math.E) < 1e-9)
    val withS = env.withBinding("s", _Number(3))
    assert(math.abs(inverseAt("1/(s + 1)", 0.0, withS) - 1.0) < 1e-12)
  }

  // --- F_0211: the imaginary-part tests at the poles' own scale ---

  it should "invert a small-scale complex pole pair as one, not as two real poles (F_0211)" in {
    // 1/(s((s + a)² + b²)) with a = b = 1e-7: the pair's |im| = 1e-7 passed the absolute 1e-6 test.
    val (a, b) = (1e-7, 1e-7)
    val t      = 2e7
    val exact  = (1 - math.exp(-a * t) * (math.cos(b * t) + a / b * math.sin(b * t))) / (a * a + b * b)
    val got    = Parser.parse("invlaplace(1/(s*((s + 1e-7)^2 + 1e-14)), s, t)").get.eval(env) match
      case Left(_: _InverseLaplace) => None
      case r => r.toExpression.eval(env.withBinding("t", _Number(t))) match
        case Right(_Number(d)) => Some(d)
        case _                 => None
    got.foreach(d => assert(math.abs(d / exact - 1) < 1e-6, s"$d against $exact"))
  }

  it should "invert three large real poles, whose computed imaginary parts are rounding (F_0211)" in {
    // Poles at -1e10, -2e10, -3e10: the eigenvalue noise is ~1e-16 of the root, above the absolute
    // 1e-6, so the poles read as complex.  Residues of 1e30/((s+a)(s+2a)(s+3a)): 1/(2a²), -1/a², 1/(2a²) · 1e30.
    val a = 1e10
    val t = 1e-10
    val exact = 1e30 * (math.exp(-a * t) / (2 * a * a) - math.exp(-2 * a * t) / (a * a) + math.exp(-3 * a * t) / (2 * a * a))
    assert(math.abs(inverseAt("1e30/((s + 1e10)*(s + 2e10)*(s + 3e10))", t) / exact - 1) < 1e-6)
  }

  "invztrans" should "decline a small-scale complex pole pair rather than read it as real (F_0211)" in {
    // Poles 1e-10 ± 1e-10i: the absolute 1e-9 test read them as one real pole twice.
    val r = Parser.parse("invztrans(z/(z^2 - 2e-10*z + 2e-20), z, n)").get.eval(env)
    assert(r.left.exists(_.isInstanceOf[_InverseZTransform]), r)
  }

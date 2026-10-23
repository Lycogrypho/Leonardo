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

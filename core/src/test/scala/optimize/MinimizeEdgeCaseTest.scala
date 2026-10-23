package it.grypho.scala.leonardo
package optimize

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** `minimize` answers a minimiser, not any stationary point (F_0199, Decision 1). */
class MinimizeEdgeCaseTest extends AnyFlatSpec:

  private val env = new Environment()
  private val x   = _Variable("x")
  private def run(f: String, x0: Double, m: MinimizeMethod): Either[MinimizeFailure, MinimizeResult] =
    minimize(Parser.parse(f).get, Vector(x), Vector(x0), None, m, env)

  "minimize" should "refuse a start at a maximum, by every method (F_0199)" in {
    for m <- Seq(MinimizeMethod.BFGS, MinimizeMethod.Newton, MinimizeMethod.GradientDescent, MinimizeMethod.ProjectedBFGS) do
      assert(run("x^4 - 2*x^2", 0.0, m) == Left(MinimizeFailure.NotAMinimum), m)
  }

  it should "refuse a degenerate saddle and answer a degenerate minimum (F_0199)" in {
    assert(run("x^3", 0.0, MinimizeMethod.BFGS) == Left(MinimizeFailure.NotAMinimum))
    assert(run("x^4", 0.0, MinimizeMethod.BFGS).map(_.x) == Right(Vector(0.0)))
  }

  it should "still find the minimum from elsewhere (F_0199)" in {
    run("x^4 - 2*x^2", 0.5, MinimizeMethod.BFGS) match
      case Right(r) => assert(math.abs(r.x(0) - 1.0) < 1e-4, r)
      case Left(why) => fail(s"$why")
  }

  "a bound" should "decide a point on it, not the unconstrained curvature (F_0199)" in {
    val f = Parser.parse("-(x^2)").get
    minimize(f, Vector(x), Vector(1.0), Some((Vector(-1.0), Vector(1.0))), MinimizeMethod.ProjectedBFGS, env) match
      case Right(r) => assert(math.abs(math.abs(r.x(0)) - 1.0) < 1e-9, r)
      case Left(why) => fail(s"$why")
  }

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

  /** `Σ(xᵢ − cᵢ)² + ¼Σxᵢxᵢ₊₁`, `cᵢ = i mod 7 + 1`, as a balanced sum: strictly convex,
   *  its Hessian `2I` plus `¼` off the diagonal. */
  private def coupled(n: Int): (_Expression, Vector[_Variable]) =
    import scalar.{Sum, Product, Power}
    def balanced(xs: Vector[_Expression]): _Expression =
      if xs.size == 1 then xs.head else { val (a, b) = xs.splitAt(xs.size / 2); Sum(balanced(a), balanced(b)) }
    val vs = Vector.tabulate(n)(i => _Variable(s"x$i"))
    val sq = vs.zipWithIndex.map((v, i) => Power(Sum(v, _Number(-(i % 7 + 1).toDouble)), _Number(2)): _Expression)
    val cp = vs.sliding(2).collect { case Vector(a, b) => Product(_Number(0.25), Product(a, b)): _Expression }.toVector
    (balanced(sq ++ cp), vs)

  "bfgs" should "answer a well-conditioned convex quadratic it used to decline (F_0234)" in {
    val (f, vs) = coupled(50)
    val x0 = Vector.fill(50)(0.0)
    (minimize(f, vs, x0, None, MinimizeMethod.BFGS, env),
     minimize(f, vs, x0, None, MinimizeMethod.ProjectedBFGS, env)) match
      case (Right(a), Right(b)) =>
        assert(a.x.lazyZip(b.x).forall((u, v) => math.abs(u - v) < 1e-4), s"${a.x} vs ${b.x}")
      case other => fail(s"$other")
  }

  "the variable count" should "be held to Limit.MaxMinimizeVariables, and raised by a caller (F_0216)" in {
    val vs = Vector.tabulate(101)(i => _Variable(s"v$i"))
    val f  = scalar.Power(vs.head, _Number(2))
    minimize(f, vs, Vector.fill(101)(1.0), None, MinimizeMethod.GradientDescent, env) match
      case Left(MinimizeFailure.InvalidInput(detail)) => assert(detail.contains("at most 100 variables"), detail)
      case other => fail(s"expected the cap, got $other")
    val raised = env.withLimit(Limit.MaxMinimizeVariables, 101)
    assert(!minimize(f, vs, Vector.fill(101)(1.0), None, MinimizeMethod.GradientDescent, raised)
               .left.exists(_.isInstanceOf[MinimizeFailure.InvalidInput]))
  }

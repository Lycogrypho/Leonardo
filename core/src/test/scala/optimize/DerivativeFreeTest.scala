package it.grypho.scala.leonardo
package optimize

import core.*
import org.scalatest.flatspec.AnyFlatSpec
import DerivativeFreeMethod.*

class DerivativeFreeTest extends AnyFlatSpec:

  private val env  = new Environment(Environment.DefaultPrecision, Map.empty)
  private val inf  = Double.PositiveInfinity
  private val rosenbrock: Array[Double] => Double = x => 100 * math.pow(x(1) - x(0) * x(0), 2) + math.pow(1 - x(0), 2)

  private def ok(r: Either[DerivativeFreeFailure, UncertifiedMinimum]): UncertifiedMinimum =
    r.fold(e => fail(e.toString), identity)

  "minimizeDerivativeFree" should "find Rosenbrock's minimum with Nelder-Mead" in {
    val r = ok(minimizeDerivativeFree(rosenbrock, Vector(-1.2, 1.0), Vector(-5.0, -5.0), Vector(5.0, 5.0), NelderMead, env))
    assert(math.abs(r.x(0) - 1) < 1e-3 && math.abs(r.x(1) - 1) < 1e-3, r.toString)
    assert(r.stop == DerivativeFreeStop.SimplexConverged)
  }

  it should "approach Rosenbrock's minimum with pattern search" in {
    val r = ok(minimizeDerivativeFree(rosenbrock, Vector(-1.2, 1.0), Vector(-5.0, -5.0), Vector(5.0, 5.0), PatternSearch, env))
    assert(math.abs(r.x(0) - 1) < 2e-2 && math.abs(r.x(1) - 1) < 4e-2, r.toString)
    assert(r.stop == DerivativeFreeStop.MeshConverged)
    assert(r.step.forall(_ <= 0.5e-5 * 10))
  }

  it should "stop on an active bound" in {
    // (x - 3)² + (y + 1)² with x ≤ 2: the optimum is (2, -1)
    val f: Array[Double] => Double = x => math.pow(x(0) - 3, 2) + math.pow(x(1) + 1, 2)
    for m <- DerivativeFreeMethod.values do
      val r = ok(minimizeDerivativeFree(f, Vector(0.0, 0.0), Vector(-4.0, -4.0), Vector(2.0, 4.0), m, env))
      assert(math.abs(r.x(0) - 2) < 1e-4 && math.abs(r.x(1) + 1) < 1e-4, s"$m: $r")
  }

  it should "minimise the non-smooth |x| + |y|" in {
    val f: Array[Double] => Double = x => math.abs(x(0) - 0.3) + math.abs(x(1) + 0.7)
    for m <- DerivativeFreeMethod.values do
      val r = ok(minimizeDerivativeFree(f, Vector(2.0, 2.0), Vector(-3.0, -3.0), Vector(3.0, 3.0), m, env))
      assert(math.abs(r.x(0) - 0.3) < 1e-4 && math.abs(r.x(1) + 0.7) < 1e-4, s"$m: $r")
  }

  it should "keep an integer coordinate whole" in {
    // (n - 3.4)² + (x - n/2)² over whole n: n = 3, x = 1.5
    val f: Array[Double] => Double = v => math.pow(v(0) - 3.4, 2) + math.pow(v(1) - v(0) / 2, 2)
    val r = ok(minimizeDerivativeFree(f, Vector(10.0, 0.0), Vector(1.0, -10.0), Vector(20.0, 10.0), Set(0), PatternSearch, env))
    assert(r.x(0) == 3.0 && math.abs(r.x(1) - 1.5) < 1e-4, r.toString)
    assert(r.step(0) == 1.0)
  }

  it should "report an exhausted budget as a Left carrying the best point" in {
    val tight = env.withLimit(Limit.MaxDerivativeFreeEvaluations, 50)
    minimizeDerivativeFree(rosenbrock, Vector(-1.2, 1.0), Vector(-5.0, -5.0), Vector(5.0, 5.0), PatternSearch, tight) match
      case Left(DerivativeFreeFailure.MaxEvaluations(50, x, v)) => assert(v <= rosenbrock(Array(-1.2, 1.0)) && x.size == 2)
      case other => fail(other.toString)
  }

  it should "take the same path when a variable is rescaled" in {
    // y = 1024·x₁: a power of two, so the rescaled iterates are exact
    val k = 1024.0
    val scaled: Array[Double] => Double = x => rosenbrock(Array(x(0), x(1) / k))
    for m <- DerivativeFreeMethod.values do
      val a = ok(minimizeDerivativeFree(rosenbrock, Vector(-1.2, 1.0), Vector(-5.0, -5.0), Vector(5.0, 5.0), m, env))
      val b = ok(minimizeDerivativeFree(scaled, Vector(-1.2, k), Vector(-5.0, -5 * k), Vector(5.0, 5 * k), m, env))
      assert(a.evaluations == b.evaluations, m.toString)
      assert(a.x(0) == b.x(0) && a.x(1) * k == b.x(1), m.toString)
  }

  it should "measure an unbounded variable by its start" in {
    val f: Array[Double] => Double = x => math.pow(x(0) - 5e6, 2)
    for m <- DerivativeFreeMethod.values do
      val r = ok(minimizeDerivativeFree(f, Vector(1e6), Vector(-inf), Vector(inf), m, env))
      assert(math.abs(r.x(0) - 5e6) < 1e2, s"$m: $r")
  }

  it should "take the same number of evaluations on every platform" in {
    // pinned on the JVM; the suite runs on Node too, so a divergence fails there
    val ps = ok(minimizeDerivativeFree(rosenbrock, Vector(-1.2, 1.0), Vector(-5.0, -5.0), Vector(5.0, 5.0), PatternSearch, env))
    val nm = ok(minimizeDerivativeFree(rosenbrock, Vector(-1.2, 1.0), Vector(-5.0, -5.0), Vector(5.0, 5.0), NelderMead, env))
    assert((ps.evaluations, nm.evaluations) == (PinnedPs, PinnedNm))
  }

  it should "decline malformed input" in {
    val f: Array[Double] => Double = x => x(0) * x(0)
    import DerivativeFreeFailure.*
    def invalid(r: Either[DerivativeFreeFailure, UncertifiedMinimum]) = assert(r.left.exists(_.isInstanceOf[InvalidInput]), r.toString)
    invalid(minimizeDerivativeFree(f, Vector(3.0), Vector(-1.0), Vector(1.0), PatternSearch, env))        // outside
    invalid(minimizeDerivativeFree(f, Vector(0.0), Vector(-inf), Vector(inf), PatternSearch, env))       // no scale
    invalid(minimizeDerivativeFree(f, Vector(0.5), Vector(-1.0), Vector(1.0), Set(0), PatternSearch, env)) // not whole
    invalid(minimizeDerivativeFree(f, Vector(0.0), Vector(-1.0), Vector(1.0), Set(0), NelderMead, env))
    invalid(minimizeDerivativeFree(f, Vector(0.0), Vector(-1.0, 0.0), Vector(1.0), PatternSearch, env))
    assert(minimizeDerivativeFree(_ => Double.NaN, Vector(0.0), Vector(-1.0), Vector(1.0), PatternSearch, env)
      == Left(NotEvaluable))
  }

  private val PinnedPs = 9614
  private val PinnedNm = 160

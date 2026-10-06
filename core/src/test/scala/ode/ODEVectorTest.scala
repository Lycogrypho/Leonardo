package it.grypho.scala.leonardo
package ode

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

import scala.math.{abs, exp, log, Pi}


/** F_0051 — vector ODE integration: `ode`'s certified vector tier, the column-of-names state,
 *  and the public `odeStep` / `odeSolve` with a named method.
 *
 *  The acceptance values are MPC requirement T1-04's; every comparison is numeric.
 */
class ODEVectorTest extends AnyFlatSpec:

  private val env = new Environment()

  private def parse(s: String): _Expression =
    Parser.parse(s) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for \"$s\": $other")

  private def column(s: String, e: Environment = env): Vector[Double] = parse(s).eval(e) match
    case Right(m: _MatrixValue) if m.cols == 1 => m.toVector
    case other                                => fail(s"\"$s\" gave no column: $other")

  private def rows(s: String): Vector[Vector[Double]] = parse(s).eval(env) match
    case Right(m: _MatrixValue) => m.toVector.grouped(m.cols).toVector
    case other                  => fail(s"\"$s\" gave no trajectory: $other")

  private def close(a: Vector[Double], b: Vector[Double], tol: Double): Boolean =
    a.size == b.size && a.zip(b).forall((x, y) => abs(x - y) <= tol)

  private val oscillator = "[[v], [-x]], [[x], [v]], t, 0, [[1], [0]]"

  // --- the acceptance problems ---

  "odeSolve" should "bring the harmonic oscillator back to its start after 2π (h = 0.01, rk4)" in
  {
    val path = rows(s"odeSolve($oscillator, 2*pi, 0.01, rk4)")
    assert(abs(path.last(0) - 2 * Pi) < 1e-12, s"the last row is t1 itself: ${path.last(0)}")
    assert(close(path.last.tail, Vector(1.0, 0.0), 1e-6), s"end state ${path.last.tail}")
  }

  it should "do the same adaptively with rk45, and visibly worse with euler" in
  {
    assert(close(rows(s"odeSolve($oscillator, 2*pi, 0.1, rk45)").last.tail, Vector(1.0, 0.0), 1e-6))
    // Euler's first-order error spirals outward: same problem, h = 0.01, about 0.37 off.
    val euler = rows(s"odeSolve($oscillator, 2*pi, 0.01, euler)").last.tail
    assert(!close(euler, Vector(1.0, 0.0), 1e-2), s"euler should not be this accurate: $euler")
  }

  it should "lay out one row per output time, t first, t1 included" in
  {
    val path = rows("odeSolve([[1]], [[x]], t, 0, [[0]], 1, 0.3, rk4)")
    assert(close(path.map(_(0)), Vector(0.0, 0.3, 0.6, 0.9, 1.0), 1e-12), s"times ${path.map(_(0))}")
    assert(close(path.last, Vector(1.0, 1.0), 1e-12))
  }

  "ode's vector tier" should "agree with F_0006's closed form on a constant-coefficient system" in
  {
    // ode(A*y, ...) takes F_0006's exact matrix exponential; the same problem stepped by RK4
    // must agree to the method's order (h^4 = 1e-8 at h = 0.01).
    val a      = "[[0, 1], [-2, -3]]"
    val closed = column(s"ode($a * y, y, t, 0, [[1], [0]], 1)")
    val rk4    = rows(s"odeSolve($a * y, y, t, 0, [[1], [0]], 1, 0.01, rk4)").last.tail
    assert(close(closed, rk4, 1e-8), s"closed form $closed, rk4 $rk4")
  }

  it should "integrate a time-varying A(t) through the single name" in
  {
    // y' = diag(t, -t) y  =>  y = (e^(t^2/2), 2 e^(-t^2/2)) at t = 1
    val y = column("ode([[t, 0], [0, -t]] * y, y, t, 0, [[1], [2]], 1)")
    assert(close(y, Vector(exp(0.5), 2 * exp(-0.5)), 1e-6), s"got $y")
  }

  it should "integrate a nonlinear system written through at(y, i, 1)" in
  {
    val y = column("ode([[at(y, 2, 1)], [-at(y, 1, 1)]], y, t, 0, [[1], [0]], pi)")
    assert(close(y, Vector(-1.0, 0.0), 1e-6), s"got $y")
  }

  "ode with a column of names" should "conserve Lotka–Volterra's first integral" in
  {
    // x' = x - xy, y' = xy - y conserves V = x - ln x + y - ln y.
    def v(s: Vector[Double]) = s(0) - log(s(0)) + s(1) - log(s(1))
    val end = column("ode([[x - x*y], [x*y - y]], [[x], [y]], t, 0, [[2], [1]], 5)")
    assert(abs(v(end) - v(Vector(2.0, 1.0))) < 1e-5, s"V drifted: ${v(end)} against ${v(Vector(2.0, 1.0))}")
  }

  "odeStep" should "take exactly one step of the named method" in
  {
    val h  = 0.1
    val y1 = column("odeStep([[y]], [[y]], t, 0, [[1]], 0.1, rk4)")
    assert(abs(y1(0) - (1 + h + h * h / 2 + h * h * h / 6 + h * h * h * h / 24)) < 1e-15)
    assert(column("odeStep([[y]], [[y]], t, 0, [[1]], 0.1, euler)") == Vector(1.1))
  }

  it should "hold an environment-bound control input constant during the step" in
  {
    val withU = env.withBinding("u", _Number(2))
    assert(close(column("odeStep([[u]], [[x]], t, 0, [[0]], 1, euler)", withU), Vector(2.0), 1e-15))
  }

  // --- the declines, with their reasons ---

  "the vector tier" should "decline, with the reason, rather than broadcast or guess" in
  {
    val names = OdeState.Components(Vector(_Variable("x"), _Variable("v")))
    val t     = _Variable("t")
    def step(rhs: String, y0: Vector[Double], h: Double = 0.1) =
      odeStep(parse(rhs), names, t, 0, y0, h, OdeMethod.RK4, env)
    assert(step("[[v]]", Vector(1, 0)).left.exists(_ == OdeFailure.ShapeMismatch(1, 1, 2)))
    assert(step("[[v], [x], [x]]", Vector(1, 0)).left.exists(_ == OdeFailure.ShapeMismatch(3, 1, 2)))
    assert(step("[[v], [k*x]]", Vector(1, 0)).left.exists(_ == OdeFailure.NotEvaluable(0)))
    assert(step("[[v], [x]]", Vector(1, 0, 0)).left.exists(_.isInstanceOf[OdeFailure.InvalidInput]))
    assert(step("[[v], [x]]", Vector(1, 0), h = 0).left.exists(_.isInstanceOf[OdeFailure.InvalidInput]))
    val backwards = odeSolve(parse("[[v], [-x]]"), names, t, 0, Vector(1, 0), 1, -0.1, OdeMethod.RK4, env)
    assert(backwards.left.exists(_.isInstanceOf[OdeFailure.InvalidInput]))
    val tooMany = odeSolve(parse("[[v], [-x]]"), names, t, 0, Vector(1, 0), 1, 1e-6, OdeMethod.RK4, env)
    assert(tooMany.left.exists(_.isInstanceOf[OdeFailure.TooManyRows]))
  }

  it should "refuse a stiff system rather than return a confident wrong number" in
  {
    // y' = -10^6 (y - cos t): RK4 at the automatic step is unstable here.  The certificate
    // (or the non-finite guard) must turn that into a refusal.
    val stiff = parse("ode([[-1000000 * (y - cos(t))]], [[y]], t, 0, [[0]], 1)")
    assert(stiff.eval(env).isLeft, "a stiff system must not be answered by explicit RK4")
  }

  "a scalar ode" should "be unchanged by the vector tier" in
  {
    parse("ode(y, y, t, 0, 1, 1)").eval(env) match
      case Right(_Number(d)) => assert(abs(d - math.E) < 1e-9)
      case other             => fail(s"expected e, got $other")
  }

  // --- the language ---

  "the new forms" should "round-trip through toString" in
  {
    for s <- List("ode([[v], [-x]], [[x], [v]], t, 0, [[1], [0]], 2)",
                  "odeStep([[v], [-x]], [[x], [v]], t, 0, [[1], [0]], 0.1, rk4)",
                  "odeStep(A*y, y, t, 0, [[1], [0]], 0.1, euler)",
                  "odeSolve([[v], [-x]], [[x], [v]], t, 0, [[1], [0]], 1, 0.1, rk45)") do
      val e = parse(s)
      assert(parse(e.toString) == e, s"round-trip failed for $s: ${e.toString}")
  }

  it should "reserve the two function names and none of the method words" in
  {
    assert(Parser.ReservedWords.contains("odeStep") && Parser.ReservedWords.contains("odeSolve"))
    for w <- List("euler", "rk4", "rk45") do
      assert(!Parser.ReservedWords.contains(w))
      assert(parse(s"$w + 1").freeVars == Set(w))
  }

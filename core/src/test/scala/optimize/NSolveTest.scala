package it.grypho.scala.leonardo
package optimize

import core.*
import matrix._Matrix
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

import scala.math.{abs, exp, sqrt}


/** F_0319 (APAC L_0011) -- a certified root of a square nonlinear system.
 *
 *  The certificate is recomputed here independently of the solver: each equation against the size of
 *  its own terms, at the precision's tolerance. */
class NSolveTest extends AnyFlatSpec:

  private val env = new Environment()
  private val tol = 0.5e-5

  private def parse(s: String): _Expression = Parser.parse(s) match
    case Parser.Success(e, _) => e
    case other                => fail(s"parse failed for \"$s\": $other")

  private def vars(names: String*): Vector[_Variable] = names.map(_Variable(_)).toVector

  private def solve(cells: Seq[String], vs: Vector[_Variable], x0: Vector[Double],
                    bounds: Option[(Vector[Double], Vector[Double])] = None): Either[RootFailure, RootResult] =
    nsolve(_Matrix(cells.size, 1, cells.map(parse).toVector), vs, x0, bounds, env)

  private def answer(r: Either[RootFailure, RootResult]): RootResult = r.fold(w => fail(s"declined: $w"), identity)

  // APAC Q15's hydraulics: a pump curve, a retentate valve, a Darcy membrane and a permeate valve,
  // unknowns the feed and permeate pressures P1, P2
  private val hydraulics = Seq(
    "(maximum(0, 4*0.64 - P1)/0.05)^(1/2) - 2*0.5*P1^(1/2) - (P1 - P2)",
    "(P1 - P2) - 3*0.5*P2^(1/2)")

  "nsolve" should "solve the hydraulics of a pump, two valves and a membrane" in {
    val r = answer(solve(hydraulics, vars("P1", "P2"), Vector(1.0, 0.5), Some((Vector(0.0, 0.0), Vector(10.0, 10.0)))))
    info(s"hydraulics: ${r.iterations} iterations")
    val (p1, p2) = (r.x(0), r.x(1))
    val f1 = sqrt(math.max(0, 4 * 0.64 - p1) / 0.05) - sqrt(p1) - (p1 - p2)
    val f2 = (p1 - p2) - 1.5 * sqrt(p2)
    assert(abs(f1) <= tol * (sqrt(math.max(0, 4 * 0.64 - p1) / 0.05) + sqrt(p1) + abs(p1 - p2)))
    assert(abs(f2) <= tol * (abs(p1 - p2) + 1.5 * sqrt(p2)))
    assert(abs(p1 - 2.16201478954794) < 1e-6 && abs(p2 - 0.81109914549161) < 1e-6, r.x.toString)
    assert(r.active.isEmpty)
  }

  it should "solve Broyden's tridiagonal system" in {
    val n = 6
    def xi(i: Int) = if i < 1 || i > n then "0" else s"x$i"
    val cells = (1 to n).map(i => s"(3 - 2*x$i)*x$i - ${xi(i - 1)} - 2*${xi(i + 1)} + 1")
    val r = answer(solve(cells, (1 to n).map(i => _Variable(s"x$i")).toVector, Vector.fill(n)(-1.0)))
    info(s"Broyden n = $n: ${r.iterations} iterations")
    val x = r.x
    for i <- 1 to n do
      def at(k: Int) = if k < 1 || k > n then 0.0 else x(k - 1)
      val fi = (3 - 2 * at(i)) * at(i) - at(i - 1) - 2 * at(i + 1) + 1
      assert(abs(fi) <= tol * (abs((3 - 2 * at(i)) * at(i)) + abs(at(i - 1)) + 2 * abs(at(i + 1)) + 1))
  }

  it should "solve Powell's badly scaled function" in {
    val r = answer(solve(Seq("10000*a*b - 1", "exp(-a) + exp(-b) - 1.0001"), vars("a", "b"), Vector(0.0, 1.0)))
    info(s"Powell: ${r.iterations} iterations")
    assert(abs(r.x(0) - 1.098159e-5) < 1e-9 && abs(r.x(1) - 9.106146) < 1e-5, r.x.toString)
  }

  "the verdict and the iteration count" should "not move when the equations or the unknowns are rescaled" in {
    val base   = answer(solve(Seq("x^2 + y^2 - 4", "x - y"), vars("x", "y"), Vector(1.0, 0.0)))
    val scaled = answer(solve(Seq("1e-9*(x^2 + y^2 - 4)", "1e-9*(x - y)"), vars("x", "y"), Vector(1.0, 0.0)))
    val units  = answer(solve(Seq("(x/1000)^2 + (y/1000)^2 - 4", "x/1000 - y/1000"), vars("x", "y"), Vector(1000.0, 0.0)))
    info(s"circle: ${base.iterations} iterations")
    // the iteration aims at RootTarget, far past the certificate, so the root carries its full digits
    assert(abs(base.x(0) - sqrt(2)) < 1e-9 && abs(base.x(1) - sqrt(2)) < 1e-9)
    assert(scaled.iterations == base.iterations && scaled.x.zip(base.x).forall((a, b) => abs(a - b) < 1e-9))
    assert(units.iterations == base.iterations && units.x.zip(base.x).forall((a, b) => abs(a - 1000 * b) < 1e-6))
  }

  "a system with no root" should "be declined, never certified as a fit" in {
    val r = solve(Seq("x^2 + 1", "y - 1"), vars("x", "y"), Vector(0.7, 0.0))
    assert(r.left.exists(_.isInstanceOf[RootFailure.NotARoot]), r.toString)
  }

  "a root that is not isolated" should "be declined" in {
    // x^2 = y, y = 0: the Jacobian is singular at the root (0, 0)
    val r = solve(Seq("x^2 - y", "y"), vars("x", "y"), Vector(0.5, 0.5))
    assert(r.isLeft, r.toString)
  }

  "a root" should "be polished to the rounding level, one Newton step past the target (F_0338)" in {
    // the target stopped at Newton's fourth iterate for x^2 = 2, 665857/470832, |x - √2| ≈ 1.6e-12
    val r = answer(solve(Seq("x^2 - 2"), vars("x"), Vector(1.0)))
    assert(abs(r.x(0) - sqrt(2.0)) <= 4 * Math.ulp(sqrt(2.0)), s"${r.x(0)} vs ${sqrt(2.0)}")
    assert(r.x(0) != 665857.0 / 470832.0)
    val q = answer(solve(Seq("x^2 - 9/4"), vars("x"), Vector(1.0)))
    assert(abs(q.x(0) - 1.5) <= 4 * Math.ulp(1.5), q.toString)
  }

  "a start where the Jacobian is singular" should "still move, by the Cauchy point (F_0333)" in {
    // J = [[1, 2y], [1, -1]] is singular on y = -1/2, where the gradient of |F|^2 is (-3.25, 3.25): a descent
    // step exists, and the old full-radius step overshot it and stopped at the start (|F| = 2.79).  Roots:
    // (2, 1) and (-1, -2).  (A start on a line of symmetry, such as (1, 1) for x^2 + y^2 = 4, x*y = 1, stays
    // on it under any gradient step and ends at a genuine stationary point of |F|^2: a different case.)
    val r = answer(solve(Seq("x + y^2 - 3", "x - y - 1"), vars("x", "y"), Vector(0.0, -0.5)))
    assert(abs(r.x(0) + r.x(1) * r.x(1) - 3) < 1e-9 && abs(r.x(0) - r.x(1) - 1) < 1e-9, r.toString)
  }

  it should "name a continuum as one, not as a residual minimum (F_0333)" in {
    // x = y is a line of roots; the Cauchy point reaches it, and the rank test then declines
    val r = solve(Seq("x - y", "x - y"), vars("x", "y"), Vector(1.0, 2.0))
    assert(r.left.exists(_.isInstanceOf[RootFailure.SingularJacobian]), r.toString)
  }

  it should "still decline a genuine stationary point of the residual (F_0333)" in {
    // x^2 + 1 at 0: J = 0 and F = 1, the gradient of |F|^2 vanishes -- nothing to descend
    assert(solve(Seq("x^2 + 1"), vars("x"), Vector(0.0)).left.exists(_.isInstanceOf[RootFailure.NotARoot]))
  }

  "bounds" should "keep the root inside the box" in {
    val x = vars("x")
    assert(abs(answer(solve(Seq("x^2 - 4"), x, Vector(-1.0))).x(0) + 2) < 1e-9)
    // from 0.5 in [0, 10]: the positive root.  (A start of -1 would be clamped to 0, where the Jacobian
    // of x^2 - 4 vanishes: a stationary point of the residual, declined as NotARoot -- honestly.)
    val boxed = answer(solve(Seq("x^2 - 4"), x, Vector(0.5), Some((Vector(0.0), Vector(10.0)))))
    assert(abs(boxed.x(0) - 2) < 1e-9, boxed.toString)
    assert(solve(Seq("x^2 - 4"), x, Vector(0.0), Some((Vector(0.0), Vector(10.0)))).left.exists(_.isInstanceOf[RootFailure.NotARoot]))
    // a root outside the box is not a root of the bounded problem
    assert(solve(Seq("x^2 - 4"), x, Vector(0.5), Some((Vector(0.0), Vector(1.0)))).isLeft)
  }

  "a warm start at the root" should "answer in no iterations" in {
    val r = answer(solve(Seq("x^2 + y^2 - 4", "x - y"), vars("x", "y"), Vector(sqrt(2), sqrt(2))))
    assert(r.iterations == 0)
  }

  "the callback form" should "certify against the caller's scales" in {
    val f = (p: Array[Double]) => Array(p(0) * p(0) + p(1) * p(1) - 4, p(0) - p(1))
    val j = (p: Array[Double]) => Array(2 * p(0), 2 * p(1), 1.0, -1.0)
    val s = (p: Array[Double]) => Array(p(0) * p(0) + p(1) * p(1) + 4, abs(p(0)) + abs(p(1)))
    val inf = Double.PositiveInfinity
    val r = answer(nsolve(f, j, s, Vector(1.0, 0.0), Vector(-inf, -inf), Vector(inf, inf), env))
    assert(abs(r.x(0) - sqrt(2)) < 1e-9 && abs(r.x(1) - sqrt(2)) < 1e-9, r.x.toString)
    // a wrongly sized or non-finite array is "not evaluable"
    assert(nsolve(f, j, (_: Array[Double]) => Array(1.0), Vector(1.0, 0.0), Vector(-inf, -inf), Vector(inf, inf), env)
             == Left(RootFailure.NotEvaluable))
  }

  "a malformed problem" should "be refused by name" in {
    assert(solve(Seq("x + y"), vars("x", "y"), Vector(0.0, 0.0)).left.exists(_.isInstanceOf[RootFailure.InvalidInput]))
    assert(solve(Seq("ln(x)"), vars("x"), Vector(-1.0)) == Left(RootFailure.NotEvaluable))
    assert(solve(Seq("x - 1"), vars("x"), Vector(0.0), Some((Vector(2.0), Vector(1.0)))).left.exists(_.isInstanceOf[RootFailure.InvalidInput]))
  }

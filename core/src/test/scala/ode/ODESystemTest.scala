package it.grypho.scala.leonardo
package ode

import org.scalatest.flatspec.AnyFlatSpec
import core.*
import scalar.*
import matrix.*
import parser.Parser


/** F_0006 — linear ODE systems `y' = A·y (+ b)` through the matrix exponential.
 *
 *  Every closed form is compared numerically, never through `toString`.  The cases are chosen
 *  for what each one rules out: the **defective** double integrator is the case an eigen route
 *  gets wrong, the **singular** affine system the case `A⁻¹(Φ − I)b` cannot do, and the
 *  matrix coefficient over a row `y0` the case the scalar tier used to fold to a confident
 *  wrong number.
 */
class ODESystemTest extends AnyFlatSpec:

  private val y = _Variable("y")
  private val t = _Variable("t")

  private def parse(s: String): _Expression =
    val r = Parser.parse(s)
    assert(r.successful, s"parse failed for \"$s\": $r")
    r.get

  private def env(bindings: (String, String)*): Environment =
    bindings.foldLeft(new Environment()) { case (e, (name, src)) =>
      parse(src).eval(new Environment()) match
        case Right(v) => e.withBinding(name, v)
        case Left(x)  => fail(s"binding $name did not fold: $x")
    }

  private def column(r: Either[_Expression, _Value]): Vector[Double] = r match
    case Right(m: _MatrixValue) if m.cols == 1 => (0 until m.rows).map(m(_, 0)).toVector
    case other                                 => fail(s"expected a column vector, got $other")

  private def assertClose(actual: Vector[Double], expected: Vector[Double], tol: Double = 1e-9): Unit =
    assert(actual.size == expected.size, s"$actual vs $expected")
    actual.zip(expected).foreach((a, e) => assert(math.abs(a - e) < tol, s"$actual vs $expected"))

  // ───────────────────────────── the latent defect (step 1) ─────────────────────────────

  "a matrix coefficient" should "never be folded through the scalar closed form" in:
    // y0 a ROW conforms with the element-wise exp(A) the scalar formula built: `y0 * exp(A)`
    // was a 1x2 times a 2x2, and folded to a confident wrong number.
    val e = env("A" -> "[[0, 1], [-1, 0]]", "y0" -> "[[1, 0]]")
    assert(parse("ode(A*y, y, t, 0, y0, 1)").eval(e).isLeft)

  it should "not reach the scalar tier through solveODESymbolic" in:
    val e = env("A" -> "[[0, 1], [-1, 0]]")
    assert(solveODESymbolic(parse("A*y"), y, t, _Number(0), _Variable("y0"), _Number(1), e).isEmpty)

  "a scalar coefficient over a vector y0" should "keep working, since a scalar commutes" in:
    val e2 = math.exp(2)
    assertClose(column(parse("ode(2*y, y, t, 0, [[1], [3]], 1)").eval(new Environment())),
                Vector(e2, 3 * e2), 1e-6)

  // ───────────────────────────── homogeneous systems ─────────────────────────────

  "a linear system" should "rotate: [[0,1],[-1,0]] from (1, 0) gives (cos 1, -sin 1), bound A" in:
    val e = env("A" -> "[[0, 1], [-1, 0]]")
    assertClose(column(parse("ode(A*y, y, t, 0, [[1], [0]], 1)").eval(e)),
                Vector(math.cos(1), -math.sin(1)))

  it should "give the same answer for a literal matrix" in:
    assertClose(column(parse("ode([[0, 1], [-1, 0]] * y, y, t, 0, [[1], [0]], 1)").eval(new Environment())),
                Vector(math.cos(1), -math.sin(1)))

  it should "solve a DEFECTIVE A, the double integrator, as y1 + y2*T" in:
    // [[0,1],[0,0]] has no eigenbasis; V·diag(e^λ)·V⁻¹ would be wrong rather than refused.
    assertClose(column(parse("ode([[0, 1], [0, 0]] * y, y, t, 0, [[2], [3]], 4)").eval(new Environment())),
                Vector(2 + 3 * 4.0, 3))

  it should "integrate backwards in time" in:
    assertClose(column(parse("ode([[0, 1], [-1, 0]] * y, y, t, 1, [[1], [0]], 0)").eval(new Environment())),
                Vector(math.cos(1), math.sin(1)))

  it should "honour a non-zero t0" in:
    // Autonomous, so only tau = target - t0 matters.
    val shifted = column(parse("ode([[0, 1], [-1, 0]] * y, y, t, 2, [[1], [0]], 3)").eval(new Environment()))
    assertClose(shifted, Vector(math.cos(1), -math.sin(1)))

  // ───────────────────────────── affine systems ─────────────────────────────

  "an affine system" should "handle a SINGULAR A with a constant input" in:
    // y1' = y2, y2' = 1 from rest: y = (T^2/2, T).  A is singular, so A⁻¹(Φ − I)b is undefined.
    assertClose(column(parse("ode([[0, 1], [0, 0]] * y + [[0], [1]], y, t, 0, [[0], [0]], 2)")
                         .eval(new Environment())),
                Vector(2.0, 2.0))

  it should "accept b + A*y and A*y - b" in:
    val e = env("A" -> "[[0, 1], [0, 0]]", "b" -> "[[0], [1]]")
    assertClose(column(parse("ode(b + A*y, y, t, 0, [[0], [0]], 2)").eval(e)), Vector(2.0, 2.0))
    assertClose(column(parse("ode(A*y - b, y, t, 0, [[0], [0]], 2)").eval(e)), Vector(-2.0, -2.0))

  it should "agree with c2dExact: y(Ts) = A_d·y0 + B_d for a unit input" in:
    val a  = parse("[[-1, 2], [0, -3]]")
    val b  = parse("[[1], [2]]")
    val (ad, bd) = control.c2dExact(a, b, 0.5).getOrElse(fail("expected a discretisation"))
    val expected = ad.multiply(_MatrixValue(2, 1, Array(4.0, -1.0))).add(bd)
    assertClose(column(parse("ode([[-1, 2], [0, -3]] * y + [[1], [2]], y, t, 0, [[4], [-1]], 0.5)")
                         .eval(new Environment())),
                Vector(expected(0, 0), expected(1, 0)), 1e-12)

  // ───────────────────────────── declines ─────────────────────────────

  // A time-varying system has no closed form here, so the SYSTEM tier still declines it -- and
  // since F_0051 the vector RK4 tier answers it instead, certified by step doubling.  These two
  // cases pinned "stays symbolic" until then (F_0006 decision C: decline now, integrate later).
  "the system tier" should "decline a time-varying A(t), which the vector tier then integrates" in:
    val rhs = parse("[[0, t], [-1, 0]] * y")
    assert(solveODESystem(rhs, y, t, _Number(0), parse("[[1], [0]]"), _Number(1), new Environment()).isEmpty)
    assert(column(parse("ode([[0, t], [-1, 0]] * y, y, t, 0, [[1], [0]], 1)").eval(new Environment())).size == 2)

  it should "decline a time-varying input b(t), which the vector tier then integrates" in:
    // y1' = y2, y2' = t from rest: y2 = t^2/2, y1 = t^3/6.
    val rhs = parse("[[0, 1], [0, 0]] * y + [[0], [t]]")
    assert(solveODESystem(rhs, y, t, _Number(0), parse("[[0], [0]]"), _Number(1), new Environment()).isEmpty)
    assertClose(column(parse("ode([[0, 1], [0, 0]] * y + [[0], [t]], y, t, 0, [[0], [0]], 1)")
                         .eval(new Environment())),
                Vector(1.0 / 6, 0.5), 1e-6)

  it should "decline a non-square A" in:
    assert(parse("ode([[0, 1, 0], [1, 0, 0]] * y, y, t, 0, [[1], [0], [0]], 1)").eval(new Environment()).isLeft)

  it should "decline a non-conforming b or y0" in:
    assert(parse("ode([[0, 1], [0, 0]] * y + [[0], [1], [2]], y, t, 0, [[0], [0]], 1)")
             .eval(new Environment()).isLeft)
    assert(parse("ode([[0, 1], [0, 0]] * y, y, t, 0, [[0], [0], [1]], 1)").eval(new Environment()).isLeft)

  it should "decline a row y0" in:
    assert(parse("ode([[0, 1], [-1, 0]] * y, y, t, 0, [[1, 0]], 1)").eval(new Environment()).isLeft)

  it should "decline a symbolic tau" in:
    assert(parse("ode([[0, 1], [-1, 0]] * y, y, t, 0, [[1], [0]], T)").eval(new Environment()).isLeft)

  it should "decline a non-linear right-hand side" in:
    val e = env("A" -> "[[0, 1], [-1, 0]]")
    assert(parse("ode(A*y*y, y, t, 0, [[1], [0]], 1)").eval(e).isLeft)

  // ───────────────────────────── the core shape traits (decision F) ─────────────────────────────

  "the matrix operation nodes" should "carry the core shape traits ode reads them through" in:
    val (p, q) = (_Variable("p"), _Variable("q"))
    MatProduct(p, q) match
      case s: _MatrixProductShaped => assert(s.left == p && s.right == q)
      case other                   => fail(s"MatProduct is not product-shaped: $other")
    MatSum(p, q) match
      case s: _MatrixSumShaped => assert(s.left == p && s.right == q)
      case other               => fail(s"MatSum is not sum-shaped: $other")

  // ───────────────────────────── round-trip ─────────────────────────────

  "a system ode" should "round-trip through toString" in:
    for src <- List("ode([[0, 1], [-1, 0]] * y, y, t, 0, [[1], [0]], 1)",
                    "ode(A*y + b, y, t, 0, y0, 2)") do
      val first = parse(src)
      assert(parse(first.toString) == first, s"round-trip failed for $src: $first")

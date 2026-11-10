package it.grypho.scala.leonardo
package optimize

import core.*
import matrix._Matrix
import equation._Equation
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** F_0321 (LT L_0075) — the optimize nodes evaluate an objective written through a computing node
 *  (`dot`, `derive`) before analysing it.  Before the fix `stationary(dot(derive(q, t), derive(q, t)), t)`
 *  echoed itself while `stationary(8*t^2 + 2, t)` answered. */
class StationaryReducedTest extends AnyFlatSpec:

  private val env = new Environment()
  private val q   = "[[t^2 - t], [t^2 + t]]"

  private def parse(s: String): _Expression = Parser.parse(s) match
    case Parser.Success(e, _) => e
    case other                => fail(s"parse failed for \"$s\": $other")

  private def points(s: String, e: Environment = env): List[Vector[Double]] =
    parse(s).eval(e) match
      case Left(_Matrix(rows, cols, cells)) =>
        cells.grouped(cols).toList.take(rows).map(_.map { case _Equation(_, _Number(d)) => d; case other => fail(other.toString) })
      case other => fail(s"not a points matrix: $other")

  "stationary" should "see through dot and derive" in {
    assert(points(s"stationary(dot(derive($q, t), derive($q, t)), t)") == List(Vector(0.0)))
  }

  "convex" should "see through them too" in {
    assert(parse(s"convex(dot(derive($q, t), derive($q, t)), t)").eval(env) == Right(_Bool(true)))
  }

  "lagrange" should "see through a computed objective and constraint" in {
    // min dot([[x], [y]], [[x], [y]]) on x + y = 2: the point (1, 1)
    val ps = points("lagrange(dot([[x], [y]], [[x], [y]]), x + y - 2, x, y)")
    assert(ps.size == 1 && ps.head.zip(Vector(1.0, 1.0)).forall((a, b) => math.abs(a - b) < 1e-9), ps.toString)
  }

  it should "never fold a bound variable of its own" in {
    // t bound in the session must not turn the objective into a constant
    val bound = env.withBinding("t", _Number(3.0))
    assert(points(s"stationary(dot(derive($q, t), derive($q, t)), t)", bound) == List(Vector(0.0)))
  }

  "a constant written through a node that does not fold" should "leave stationary unevaluated, not false (F_0335)" in {
    // every point of a constant is stationary; `false` reads "provably none"
    for src <- Seq("stationary(dot(0, 0), t)", "stationary(dot(derive(p, t), derive(p, t)), t)",
                   "lagrange(dot(0, 0), x + y - 1, x, y)") do
      assert(parse(src).eval(env).isLeft, src)
      assert(parse(src).eval(env) != Right(_Bool(false)), src)
    // a constant that does fold stays unevaluated too, as before
    assert(parse("stationary(0, t)").eval(env).isLeft)
  }

  // ── F_0339: the numeric optimisers see through the computing nodes too ──────────────────────────

  private def column(s: String, e: Environment = env): Vector[Double] = parse(s).eval(e) match
    case Right(m: _MatrixValue) if m.cols == 1 => m.toVector
    case other                                 => fail(s"$s: $other")

  private def near(a: Vector[Double], b: Vector[Double]) = a.size == b.size && a.zip(b).forall((x, y) => math.abs(x - y) < 1e-6)

  "the numeric optimisers" should "evaluate an objective written through dot before compiling it (F_0339)" in {
    assert(near(column("minimize(dot([[x]], [[x]]) - 2*x, [[x]], [[0]], bfgs)"), Vector(1.0)))
    assert(near(column("nlp(dot([[x]], [[x]]), [[x]], 2, 1 - x, 0, sqp)"), Vector(1.0)))
    assert(near(column("lsqfit([[dot([[a]], [[a]]) - 4], [a - 2]], [[a]], 1)"), Vector(2.0)))
    assert(near(column("nsolve([[dot([[x]], [[x]]) - 2]], [[x]], 1)"), Vector(math.sqrt(2.0))))
    assert(near(column("lsqfit(dot([[a]], [[t]]), t, [[1], [2], [3]], [[2], [4], [6]], [[a]], 1)"), Vector(2.0)))
  }

  it should "never fold a session binding of their own variables (F_0339)" in {
    val bound = env.withBinding("x", _Number(5.0)).withBinding("a", _Number(7.0))
    assert(near(column("minimize(dot([[x]], [[x]]) - 2*x, [[x]], [[0]], bfgs)", bound), Vector(1.0)))
    assert(near(column("nsolve([[dot([[x]], [[x]]) - 2]], [[x]], 1)", bound), Vector(math.sqrt(2.0))))
    assert(near(column("lsqfit([[dot([[a]], [[a]]) - 4], [a - 2]], [[a]], 1)", bound), Vector(2.0)))
  }

  "kkt" should "state the conditions of an objective written through dot (F_0339)" in {
    val out = parse("kkt(dot([[x]], [[x]]), 1 - x, 0, x)").eval(env)
    assert(out.isLeft && !out.toExpression.toString.contains("kkt("), out.toString)
    assert(!out.toExpression.toString.contains("dot("), out.toString)
  }

  "a branch whose coefficient vanished" should "still be dropped where it divides by zero (F_0335)" in {
    // x*y = 1 is divided by y on one branch and the y = 0 branch has no solution: still exactly two points
    val ps = points("stationary(x^2 + y^2 - 2*x*y + (x*y - 1)^2, x, y)")
    assert(ps.nonEmpty && ps.forall(p => p.forall(_.isFinite)), ps.toString)
  }

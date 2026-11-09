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

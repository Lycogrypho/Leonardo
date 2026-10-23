package it.grypho.scala.leonardo
package equation

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** Regressions from the audit of 2026-10-23 in the solvers: each case failed before its fix. */
class EquationEdgeCaseTest extends AnyFlatSpec:

  private val env = new Environment()
  private def ev(s: String): Either[_Expression, _Value] = Parser.parse(s).get.eval(env)
  private def text(r: Either[_Expression, _Value]): String = r.fold(_.toString, _.toString)

  "solve's numeric scan" should "not report a pole as a root (F_0187)" in {
    ev("solve(1/x = 0, x)") match
      case Left(_: _Solve) => succeed
      case other           => fail(s"answered $other")
    assert(!text(ev("solve(tan(x) = 0, x)")).contains("1.5707"))
    assert(text(ev("solve(x^3 = 2, x)")).contains("1.2599"))
  }

  "the inequality solver" should "exclude the critical points when every piece holds (F_0188)" in {
    val t = text(ev("solve(1/x^2 > 0, x)"))
    assert(t.contains("!=") && !t.contains("true"), t)
    assert(text(ev("solve(x/(x - 1) > 0, x)")).contains("or"))
  }

  it should "sample past a large-magnitude critical point (F_0188)" in {
    val t = text(ev("solve(1/(x - 1e17) > 0, x)"))
    assert(t.contains(">") && t != "false", t)
  }

  "solve" should "reach the element-wise tier for a scalar times a column (F_0205)" in {
    assert(text(ev("solve(x*[[1],[2]] = [[3],[6]], x)")).contains("3.0"))
    assert(text(ev("solve(k*[[1, 2]] = [[3, 6]], k)")).contains("3.0"))
  }

  "solveSystem" should "swap rows on the symbolic path (F_0205)" in {
    val t = text(ev("solveSystem([[y = a, x + y = 3]], x, y)"))
    assert(!t.startsWith("solveSystem") && t.contains("a"), t)
  }

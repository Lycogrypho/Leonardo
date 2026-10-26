package it.grypho.scala.leonardo
package cli

import org.scalatest.flatspec.AnyFlatSpec

/** F_0224 at the prompt: `pretty on` shows answers and echoes in the conventional form, and
 *  `pretty off` -- the default -- and every saved script stay exactly as they were. */
class ConventionalReplTest extends AnyFlatSpec:

  private def session(pretty: Boolean): Session =
    val s = new Session()
    s.execute("exact on")
    if pretty then s.execute("pretty on")
    s

  private val lines = List(
    "4 - k*k",
    "F := [[2/3, -2/3]]",
    "simplify 3 - 2*y",
    "solveSystem([[x + 2*y = 3], [z = 1]], x, z)")

  "pretty on" should "show the course review's probes as a reader writes them" in {
    val s = session(pretty = true)
    assert(s.execute("4 - k*k") == "4 - k*k")
    assert(s.execute("F := [[2/3, -2/3]]") == "F := [[2/3, -2/3]]")
    assert(s.execute("simplify 3 - 2*y") == "3 - 2*y")
    val solved = s.execute("solveSystem([[x + 2*y = 3], [z = 1]], x, z)")
    assert(solved.contains("x = -(2*y - 3)") && !solved.contains("-1 *"), solved)
  }

  "pretty off" should "keep the internal form, which the existing suites pin" in {
    val s = session(pretty = false)
    assert(s.execute("4 - k*k") == "(4 + (-1 * (k * k)))")
  }

  "a saved script" should "not depend on the display" in {
    val on  = session(pretty = true)
    val off = session(pretty = false)
    lines.foreach { l => on.execute(l); off.execute(l) }
    def body(s: Session) = s.script.linesIterator.filterNot(_.startsWith("pretty")).toList
    assert(body(on) == body(off))
  }

package it.grypho.scala.leonardo
package scalar

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** F_0008 through the REPL: an interval binds, computes, displays outward, compares in three
 *  values, typesets, and survives `:save` / `:load` in both tiers. */
class IntervalReplTest extends AnyFlatSpec:

  private def session: Session = new Session()

  "an interval" should "bind, and give the dependency problem's honest answer for x^2 - x" in {
    val s = session
    s.execute("x := [1, 2]")
    assert(s.execute("x") == "[1.0, 2.0]")
    // [1, 4] - [1, 2] = [-1, 3]: interval arithmetic treats the two x's as independent.
    assert(s.execute("x^2 - x") == "[-1.0, 3.0]")
  }

  it should "display rounded outward at the session precision" in {
    val s = session
    assert(s.execute("[1/3, 2/3]") == "[0.33333, 0.66667]")
    s.execute("precision 2")
    assert(s.execute("[1/3, 2/3]") == "[0.33, 0.67]")
  }

  it should "compare in three values" in {
    val s = session
    assert(s.execute("[1, 2] < [3, 4]") == "true")
    assert(s.execute("[1, 3] < [2, 4]") == "unknown")
  }

  it should "typeset as an interval" in {
    val s = session
    s.execute("latex on")
    s.execute("[1, 2] + 1")
    assert(s.lastLatex.exists(_.contains("\\left[2.0, 3.0\\right]")), s.lastLatex)
  }

  it should "round-trip through :save and :load, a Double interval and an exact one alike" in {
    val s = session
    s.execute("x := [0.1, 0.30000000000000004]")
    s.execute("exact on")
    s.execute("y := [1/3, 1/2]")
    val script = s.script
    val t = session
    t.load(script)
    assert(t.script == script, s"$script\n---\n${t.script}")
    assert(t.execute("y + 1/6") == "[1/2, 2/3]")
  }

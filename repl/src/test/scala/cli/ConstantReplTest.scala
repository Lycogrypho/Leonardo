package it.grypho.scala.leonardo
package cli

import org.scalatest.flatspec.AnyFlatSpec


/** F_0289 (LT L_0063): in exact mode `pi` is a symbol, so its rounding no longer appears in answers,
 *  echoes or saved sessions; outside exact mode the values are what they were. */
class ConstantReplTest extends AnyFlatSpec:

  private def exactSession(): Session =
    val s = new Session()
    s.execute("exact on")
    s

  "exact mode" should "answer a trigonometric function at a multiple of pi exactly" in {
    val s = exactSession()
    assert(s.execute("cos(pi/2)") == "0")
    assert(s.execute("sin(pi/6)") == "1/2")
    val row = s.execute("tabulate(sin(k*pi), k, 1, 4)")
    assert(row == "[[0, 0, 0, 0]]", row)
  }

  it should "keep pi in an assignment and a definition" in {
    val s = exactSession()
    assert(s.execute("x := pi/2").contains("pi"))
    assert(s.execute("g := sin(pi*y/2)").contains("pi * y"))
    assert(s.execute("sin(x)") == "1")
    val script = s.script
    assert(script.contains("pi") && !script.contains("3.14159"), script)
  }

  it should "read a constant where a command needs a number" in {
    val s = exactSession()
    // y' = y, y(0) = 1: y(pi/4) = e^(pi/4) = 2.19328, the target read at the working precision
    val y = s.execute("ode(y, y, t, 0, 1, pi/4)")
    assert(y.startsWith("2.1932"), y)
  }

  "outside exact mode" should "evaluate the constants as before" in {
    val s = new Session()
    assert(s.execute("pi/2") == "1.5708")
    assert(s.execute("goldenratio") == "1.61803")
    assert(s.execute("eulergamma") == "0.57722")
    assert(s.execute("g := sin(pi*y/2)").contains("pi"))
  }

  "the new constant names" should "be reserved" in {
    val s = new Session()
    assert(!s.execute("goldenratio := 2").startsWith("goldenratio :="))
    assert(s.execute("gamma := 2").contains("2"))
  }

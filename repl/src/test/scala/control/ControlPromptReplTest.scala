package it.grypho.scala.leonardo
package control

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** F_0092 at the prompt: the analysis functions bind, compose with definitions, and a refusal
 *  says why. */
class ControlPromptReplTest extends AnyFlatSpec:

  "the REPL" should "answer stability and poles of a defined transfer function" in
  {
    val s = Session()
    s.execute("G := 1/(s^2 + 3*s + 2)")
    assert(s.execute("isStable(G, s)") == "true")
    assert(s.execute("tfpoles(G, s)") == s.execute("[[-2, -1]]"))
  }

  it should "bind A, B, C, D := stateSpace(...) and check a linearisation's controllability chain" in
  {
    val s = Session()
    s.execute("A, B, C, D := stateSpace([[0, 1], [-2, -3]], [[0], [1]], [[1, 0]], [[0]])")
    assert(s.execute("A") == s.execute("[[0, 1], [-2, -3]]"))
    assert(s.execute("D") == s.execute("[[0]]"))
  }

  it should "say why an analysis was declined" in
  {
    val s = Session()
    assert(s.execute("tfpoles(exp(-2*s)/(s + 1), s)").contains("note: tfpoles declined:"))
    assert(s.execute("isStable(1/(s^2 + a*s + 1), s)").contains("note: isStable declined:"))
    assert(s.execute("c2d(1/(s + 1), s, z, 0, tustin)").contains("note: c2d declined:"))
  }

package it.grypho.scala.leonardo
package control

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** F_0064 at the prompt: a named model simulates, the states bind by tuple assignment, and a
 *  refusal says why. */
class DlsimReplTest extends AnyFlatSpec:

  "the REPL" should "simulate a named model and bind outputs and states" in
  {
    val s = Session()
    s.execute("sys := stateSpace([[0.5]], [[1]], [[1]], 0)")
    assert(s.execute("dlsim(sys, [[1, 1, 1]], 0)") == s.execute("[[0, 1, 1.5]]"))
    s.execute("Y, X := dlsim(sys, [[1, 1, 1]], 0, states)")
    assert(s.execute("X") == s.execute("[[0, 1, 1.5, 1.75]]"))
  }

  it should "say why a simulation was declined" in
  {
    val s = Session()
    val out = s.execute("dlsim(stateSpace([[0.5]], [[1, 2]], [[1]], 0), [[1, 1, 1]], 0)")
    assert(out.contains("note: dlsim declined:"), out)
  }

package it.grypho.scala.leonardo
package control

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** F_0065 at the prompt: the LQR gain and the filter state bind by tuple assignment, the filter's
 *  output feeds back as its input, and a refusal says why. */
class RiccatiReplTest extends AnyFlatSpec:

  "the REPL" should "bind the LQR gain and solution, and close a stable loop" in
  {
    val s = Session()
    s.execute("K, P := dlqr([[1.2, 1], [0, 0.9]], [[0], [1]], eye(2), 1)")
    assert(s.execute("isStableDiscrete(1/(z - 0.5), z)") == "true")   // the predicate is wired
    val rho = s.execute("max(abs(eigen([[1.2, 1], [0, 0.9]] - [[0], [1]] * K)))")
    assert(rho.toDouble < 1.0, rho)
  }

  it should "feed kalmanStep's output back as its input" in
  {
    val s = Session()
    s.execute("sys := stateSpace(1, 0, 1, 0)")
    s.execute("x, P := kalmanStep(0, 1, 0, 2, sys, 0, 1)")
    s.execute("x, P := kalmanStep(x, P, 0, 2, sys, 0, 1)")
    assert(s.execute("x") == "[[1.33333]]")     // the running mean of a prior 0 (weight 1) and two 2s
  }

  it should "say why a problem with no stabilising solution was declined" in
  {
    val s = Session()
    val out = s.execute("dare([[2, 0], [0, 0.5]], [[0], [1]], eye(2), 1)")
    assert(out.contains("note: dare declined: no stabilising solution exists"), out)
  }

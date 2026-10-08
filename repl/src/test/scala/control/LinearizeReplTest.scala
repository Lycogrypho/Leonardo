package it.grypho.scala.leonardo
package control

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** F_0059 at the prompt: the model binds by tuple assignment, named fields are substituted
 *  before the derivatives are taken, and a refusal says why. */
class LinearizeReplTest extends AnyFlatSpec:

  "the REPL" should "bind A, B, C, D := linearize(...) from named fields" in
  {
    val s = Session()
    s.execute("F := [[x2], [-sin(x1) + u]]")
    s.execute("G := [[x1]]")
    s.execute("A, B, C, D := linearize(F, G, [[x1], [x2]], u, [[0], [0]], 0)")
    assert(s.execute("A") == s.execute("[[0, 1], [-1, 0]]"))
    assert(s.execute("B") == s.execute("[[0], [1]]"))
    assert(s.execute("C") == s.execute("[[1, 0]]"))
  }

  it should "ignore a session binding of a state name, the point decides" in
  {
    val s = Session()
    s.execute("x1 := 5")
    s.execute("A, B, C, D := linearize([[x2], [-sin(x1) + u]], [[x1]], [[x1], [x2]], u, [[0], [0]], 0)")
    assert(s.execute("A") == s.execute("[[0, 1], [-1, 0]]"))
  }

  it should "say why a linearisation was declined" in
  {
    val s = Session()
    val out = s.execute("linearize([[ln(x1) + u]], [[x1]], x1, u, -1, 0)")
    assert(out.contains("note: linearize declined: the operating point is outside the domain of F row 1"), out)
    assert(s.execute("linearize([[abs(x1) + u]], [[x1]], x1, u, 0, 0)")
      .contains("note: linearize declined: A(1, 1) is not a real number at the operating point"))
    assert(s.execute("linearize([[x1]], [[x1]], x1, x1, 0, 0)").contains("listed twice"))
  }

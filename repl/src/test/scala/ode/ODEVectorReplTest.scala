package it.grypho.scala.leonardo
package ode

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec


/** F_0051 through the REPL: answers, the reason note on a decline (F_0052 Decision A), and
 *  `:save` / `:load` of a definition using the new forms.  In the repl module because it
 *  constructs a `Session`. */
class ODEVectorReplTest extends AnyFlatSpec:

  private def session: Session = new Session()

  "odeStep and odeSolve" should "answer at the prompt" in
  {
    val s = session
    assert(s.execute("odeStep([[y]], [[y]], t, 0, [[1]], 0.1, euler)") == "[[1.1]]")
    assert(s.execute("odeSolve([[1]], [[x]], t, 0, [[0]], 1, 0.5, rk4)") == "[[0.0, 0.0], [0.5, 0.5], [1.0, 1.0]]")
  }

  "a declined system" should "say why in a note" in
  {
    val s = session
    val shape = s.execute("odeStep([[v]], [[x], [v]], t, 0, [[1], [0]], 0.1, rk4)")
    assert(shape.contains("note: odeStep declined: the right-hand side is 1x1, but the state is 2x1"), shape)
    val stiff = s.execute("ode([[-1000000 * (y - cos(t))]], [[y]], t, 0, [[0]], 1)")
    assert(stiff.contains("note: ode declined: the state left the finite numbers"), stiff)
    assert(s.lastLatex.isEmpty)
  }

  "a definition using the column form" should "survive :save / :load" in
  {
    val s1 = session
    s1.execute("osc := ode([[v], [-x]], [[x], [v]], t, 0, [[1], [0]], T)")
    s1.execute("T := 1")
    val s2 = session
    s2.load(s1.script)
    assert(s2.execute("osc") == s1.execute("osc"))
    assert(s2.execute("osc") == "[[0.5403], [-0.84147]]")
  }

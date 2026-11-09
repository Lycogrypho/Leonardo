package it.grypho.scala.leonardo
package optimize

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** F_0319 at the prompt: `nsolve` answers the root as a column with a note, reads equations, binds by
 *  assignment, and says why it declined. */
class NSolveReplTest extends AnyFlatSpec:

  "the REPL" should "solve a square system and say how" in {
    val out = Session().execute("nsolve([[x^2 + y^2 - 4], [x - y]], [[x], [y]], [[1], [0]])")
    assert(out.startsWith("[[1.41421], [1.41421]]") && out.contains("note: nsolve:") && out.contains("iterations"), out)
  }

  it should "read equations as cells, and bounds" in {
    val s = Session()
    assert(s.execute("nsolve([[x^2 + y^2 = 4], [x = y]], [[x], [y]], [[1], [0]])").startsWith("[[1.41421], [1.41421]]"))
    // from 1 inside [0, 10]: the positive root.  (A start of -1 clamps to 0, where the Jacobian of
    // x^2 - 4 vanishes: a stationary point of the residual, declined as NotARoot.)
    assert(s.execute("nsolve(x^2 - 4, [[x]], 1, 0, 10)").startsWith("[[2.0]]"))
    s.execute("r := nsolve([[x^2 + y^2 - 4], [x - y]], [[x], [y]], [[1], [0]])")
    assert(s.execute("at(r, 1, 1)").startsWith("1.41421"))
  }

  it should "say why there is no root" in {
    val out = Session().execute("nsolve([[x^2 + 1], [y - 1]], [[x], [y]], [[0.7], [0]])")
    assert(out.startsWith("nsolve(") && out.contains("note: nsolve declined:") && out.contains("not a root"), out)
  }

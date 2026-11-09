package it.grypho.scala.leonardo
package optimize

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** F_0321 at the prompt: LT L_0075's line answers without `simplify`. */
class StationaryReducedReplTest extends AnyFlatSpec:

  "the REPL" should "find the stationary point of a speed written through dot and derive" in {
    val s = Session()
    s.execute("q := [[t^2 - t], [t^2 + t]]")
    val out = s.execute("stationary(dot(derive(q, t), derive(q, t)), t)")
    assert(out.startsWith("[[t = 0") && out.contains("minimum"), out)
  }

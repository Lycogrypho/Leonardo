package it.grypho.scala.leonardo
package logic

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** F_0326 at the prompt (LT L_0082): with `p := true`, `p and q` answers `q`. */
class ConnectiveIdentityReplTest extends AnyFlatSpec:

  "the REPL" should "fold a known operand on either side" in {
    val s = Session()
    s.execute("p := true")
    assert(s.execute("p and q") == "q")
    assert(s.execute("p xor q") == "(not q)")
    assert(s.execute("q implies true") == "true")
    assert(s.execute("true implies q") == "q")
    assert(s.execute("q and false") == "false")
  }

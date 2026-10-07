package it.grypho.scala.leonardo
package statistics

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec


/** F_0057 through the REPL: the state goes in and comes out, so tuple assignment carries it from
 *  one update to the next (X-04, state in / state out).  In the repl module because it
 *  constructs a `Session`. */
class RlsReplTest extends AnyFlatSpec:

  "rls" should "carry its state from update to update through tuple assignment" in
  {
    val s = new Session()
    s.execute("th := [[0]]")
    s.execute("Pc := [[1]]")
    s.execute("th, Pc := rls(th, Pc, [[2]], 3, 1)")
    assert(s.execute("th") == "[[1.2]]")
    assert(s.execute("Pc") == "[[0.2]]")
    s.execute("th, Pc := rls(th, Pc, [[1]], 2, 1)")
    // denominator 1 + 0.2 = 1.2, gain 1/6, error 2 - 1.2 = 0.8: theta = 1.2 + 0.8/6
    assert(s.execute("th") == "[[1.33333]]")
  }

package it.grypho.scala.leonardo
package cli

import org.scalatest.flatspec.AnyFlatSpec


/** F_0293 (LT L_0073): notes that said nothing, and a field written as a scaled column. */
class ReplNoiseTest extends AnyFlatSpec:

  "an infinite answer in exact mode" should "carry no decimal note (F_0293)" in {
    val s = new Session()
    s.execute("exact on")
    assert(s.execute("limit(x^2, x, inf)") == "inf")
    assert(s.execute("limit(x^2, x, 3)") == "9")
  }

  "the rationalize offer" should "not restate an unevaluated call" in {
    val s = new Session()
    s.execute("exact on")
    assert(!s.execute("sum(x^k, k, 0, n)").contains("rationalize"))
    assert(!s.execute("limit(sin(1/x), x, 0)").contains("rationalize"))
  }

  "a column divided by a scalar" should "be read as a field" in {
    val s = new Session()
    assert(!s.execute("div([[x], [y], [z]]/r, x, y, z)").contains("div("))
    assert(s.execute("div(2*[[x], [y], [z]], x, y, z)") == "6.0")
    s.execute("r := 2")
    assert(s.execute("div([[x], [y], [z]]/r, x, y, z)") == "1.5")
  }

package it.grypho.scala.leonardo
package cli

import org.scalatest.flatspec.AnyFlatSpec


/** F_0323 at the prompt (LT L_0080): no decimal and no `rationalize` note for a based integer under
 *  `exact on`, and `:save` keeps the literal under the exact group. */
class ExactBasedReplTest extends AnyFlatSpec:

  private def exactSession(): Session =
    val s = Session()
    s.execute("exact on")
    s

  "exact on" should "add based literals exactly, with no note" in {
    val s = exactSession()
    assert(s.execute("0b1010 + 0b0110") == "16")
    assert(s.execute("0b1010 + 0") == "10")
    assert(s.execute("0b1010") == "0b1010")
    assert(s.execute("0xFF") == "0xFF")
  }

  it should "re-base without a note" in {
    val s = exactSession()
    assert(s.execute("tobase(10, 2)") == "0b1010")
    assert(s.execute("tobase(255, 16)") == "0xFF")
    assert(s.execute("tobase(41, 10)") == "41")
  }

  it should "keep a matrix, a negation and binom of based literals exact, with no note (F_0336)" in {
    val s = exactSession()
    assert(s.execute("[[0b1, 0b10]]") == "[[1, 2]]")
    assert(s.execute("det([[0b1, 0b10], [0b11, 0b100]])") == "-2")
    assert(s.execute("-0b101") == "-5")
    assert(s.execute("2 * -0b11") == "-6")
    assert(s.execute("binom(0b101, 2)") == "10")
    assert(s.execute("max([[0b11, 0b101]])") == "5")
  }

  it should "save and reload those bindings exact (F_0336)" in {
    val s = exactSession()
    s.execute("M := [[0b1, 0b10]]")
    s.execute("n := -0b101")
    val t = Session()
    t.load(s.script)
    assert(t.execute("M") == "[[1, 2]]", s.script)
    assert(t.execute("n + 1") == "-4", s.script)
  }

  it should "save a based binding under the exact group and reload it exact" in {
    val s = exactSession()
    s.execute("A := 0xff")
    val script = s.script
    assert(script.contains("A := 0xFF"), script)
    val t = Session()
    t.load(script)
    assert(t.execute("A + 1") == "256", t.execute("A + 1"))
  }

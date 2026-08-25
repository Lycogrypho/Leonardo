package it.grypho.scala.leonardo
package cli

import org.scalatest.flatspec.AnyFlatSpec


/** Issue 3.5 — a based value through the REPL: display, binding, and `:save` round-trip. */
class BasedSessionTest extends AnyFlatSpec:

  private def session: Session = new Session()

  "a based literal" should "echo in its own base" in
  {
    assert(session.execute("0xff").contains("0xFF"))
    assert(session.execute("0t1TT").contains("0t1TT"))
  }

  "a binding" should "remember its base, which is the whole point of the design" in
  {
    // The question that ruled out a bare digit matrix: is A still hex later?
    val s = session
    s.execute("A := 0xff")
    assert(s.execute("A").contains("0xFF"), s"A lost its base: ${s.execute("A")}")
  }

  it should "still be usable as a number" in
  {
    val s = session
    s.execute("A := 0xff")
    assert(s.execute("A + 1").contains("256"))
  }

  "a saved session" should "round-trip a based binding through the script" in
  {
    val s = session
    s.execute("A := 0xff")
    s.execute("B := balanced(5)")
    val script = s.script

    val restored = new Session()
    restored.load(script)
    assert(restored.execute("A").contains("0xFF"),  s"A did not survive :save/:load\n$script")
    assert(restored.execute("B").contains("0t1TT"), s"B did not survive :save/:load\n$script")
    // And the values are still right.
    assert(restored.execute("A + 0").contains("255"))
    assert(restored.execute("B + 0").contains("5"))
  }

  "a generic base" should "serialize as the call that rebuilds it" in
  {
    val s = session
    s.execute("C := tobase(255, 7)")
    val restored = new Session()
    restored.load(s.script)
    assert(restored.execute("C + 0").contains("255"), s"C did not survive:\n${s.script}")
  }

  "ordinary numbers" should "be unaffected by the new tier" in
  {
    val s = session
    assert(s.execute("2 + 3").contains("5"))
    assert(!s.execute("2 + 3").contains("0x"))
    s.execute("x := 255")
    assert(s.execute("x").contains("255"))
    assert(!s.execute("x").contains("0x"))
  }

package it.grypho.scala.leonardo
package scalar

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** REPL handling of complex values: display, the `:save`/`:load` round-trip, the reserved
 *  imaginary unit, and session precision.
 *
 *  Split out of `ComplexTest` by issue 5.2 phase 1.1.
 */
class ComplexReplTest extends AnyFlatSpec:

  "binding a complex value" should "store and display it" in
  {
    val s = Session()
    assert(s.execute("z := 2 + 3i") == "z := (2.0 + 3.0i)")
    assert(s.execute("z") == "(2.0 + 3.0i)")
  }

  "a complex binding" should "survive a script round-trip" in
  {
    val s = Session()
    s.execute("z := 2 + 3i")
    val restored = Session()
    restored.load(s.script)
    assert(restored.execute("z") == "(2.0 + 3.0i)")
  }

  "assigning to i" should "be rejected as a reserved word" in
  {
    val s = Session()
    assert(s.execute("i := 3").contains("reserved word"))
  }

  "a complex result in the REPL" should "respect session precision" in
  {
    val s = Session()
    s.execute("precision 2")
    assert(s.execute("1.23456 + 7.89012i") == "(1.23 + 7.89i)")
  }

package it.grypho.scala.leonardo
package cli

import org.scalatest.flatspec.AnyFlatSpec

/** F_0223: a definition whose formula already computes says so on a note line, so a reader can
 *  tell `L := chol(G)` (a value) from `M := chol(B)` (refused) without typing the name. */
class DefinitionValueNoteTest extends AnyFlatSpec:

  private def exact: Session =
    val s = new Session()
    s.execute("exact on")
    s

  "a definition that computes" should "append its current value as a note" in {
    val s = exact
    s.execute("G := [[4, 2], [2, 3]]")
    assert(s.execute("L := chol(G)") == "L := chol(G)\n  = [[2, 0], [1, 1.41421]]")
  }

  "a definition whose formula is refused" should "echo the formula alone" in {
    val s = exact
    s.execute("B := [[1, 2], [2, 1]]")
    assert(s.execute("M := chol(B)") == "M := chol(B)")
  }

  "a definition over a bound number" should "note the value it has now and stay late-bound" in {
    val s = new Session()
    s.execute("x := 2")
    assert(s.execute("f := x + 1") == "f := (x + 1.0)\n  = 3.0")
    s.execute("x := 5")
    assert(s.execute("f") == "6.0")
  }

  "a definition with a free name" should "carry no note" in {
    assert(new Session().execute("f := sin(x) + x") == "f := (sin(x) + x)")
  }

  "an exact matrix literal" should "carry no note, its echo already being its value" in {
    assert(exact.execute("G := [[4, 2], [2, 3]]") == "G := [[4, 2], [2, 3]]")
  }

  "the note under pretty on" should "stack the matrix under its own bracket" in {
    val s = exact
    s.execute("pretty on")
    s.execute("G := [[4, 2], [2, 3]]")
    assert(s.execute("L := chol(G)") == "L := chol(G)\n  = [[2,       0]\n     [1, 1.41421]]")
  }

  "a decomposition row" should "count as computed" in {
    val s = exact
    s.execute("A := [[1, 2], [3, 4]]")
    assert(s.execute("R := qr(A)").startsWith("R := qr(A)\n  = [["))
  }

  "a script replayed by load" should "carry no note" in {
    val s = new Session()
    val out = s.load("x := 2\nf := x + 1")
    assert(!out.contains("  = "))
    assert(s.execute("f") == "3.0")
  }

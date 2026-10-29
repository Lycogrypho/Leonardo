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
    // √2 since F_0231: the exact Cholesky factor is a surd, one radical per column.
    assert(s.execute("L := chol(G)") == "L := chol(G)\n  = [[2, 0], [1, √2]]")
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
    assert(s.execute("L := chol(G)") == "L := chol(G)\n  = [[2,  0]\n     [1, √2]]")
  }

  "a decomposition row" should "count as computed" in {
    val s = exact
    s.execute("A := [[1, 2], [3, 4]]")
    assert(s.execute("R := qr(A)").startsWith("R := qr(A)\n  = [["))
  }

  // F_0243: an exact matrix is the symbolic carrier, so its assignment is a definition; when the
  // formula names nothing the echo is the matrix it computes, exactly as a value binding's is.

  "an exact matrix literal under pretty on" should "stack its echo like a value binding" in {
    val s = exact
    s.execute("pretty on")
    assert(s.execute("A := [[-2, 3], [0, 2], [3, 1]]") == "A := [[-2, 3]\n [ 0, 2]\n [ 3, 1]]")
    s.execute("exact off")
    assert(s.execute("D := [[1, 2], [3, 4]]") == "D := [[1.0, 2.0]\n [3.0, 4.0]]")   // the layout matched
  }

  "an exact matrix formula naming nothing" should "echo the matrix it computes" in {
    val s = exact
    assert(s.execute("G := [[1, 2], [3, 4]] * 2") == "G := [[2, 4], [6, 8]]")
    assert(s.execute("H := transpose([[1, 2], [3, 4]])") == "H := [[1, 3], [2, 4]]")
    assert(s.execute("G") == "[[2, 4], [6, 8]]")
  }

  it should "still be saved as the formula, a definition" in {
    val s = exact
    s.execute("G := [[1, 2], [3, 4]] * 2")
    assert(s.script.linesIterator.contains("G := (2 * [[1, 2], [3, 4]])"), s.script)
  }

  "an exact matrix formula with a name in it" should "keep the formula echo and its note" in {
    val s = exact
    s.execute("B := [[1, 2], [3, 4]]")
    assert(s.execute("C := 2 * B") == "C := (2 * B)\n  = [[2, 4], [6, 8]]")
    assert(s.execute("F := [[1, 2], [3, w]]") == "F := [[1, 2], [3, w]]")
  }

  "an exact matrix formula that does not compute" should "echo the formula alone" in {
    assert(exact.execute("M := chol([[1, 2], [2, 1]])") == "M := chol([[1, 2], [2, 1]])")
  }

  "a script replayed by load" should "carry no note" in {
    val s = new Session()
    val out = s.load("x := 2\nf := x + 1")
    assert(!out.contains("  = "))
    assert(s.execute("f") == "3.0")
  }

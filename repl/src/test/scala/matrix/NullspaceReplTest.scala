package it.grypho.scala.leonardo
package matrix

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** F_0087 at the prompt: a null-space basis binds and is annihilated, a trivial one answers
 *  `false`, and a refused tolerance says why. */
class NullspaceReplTest extends AnyFlatSpec:

  "the REPL" should "bind a null-space basis that the matrix annihilates" in
  {
    val s = Session()
    s.execute("A := [[1, 2, 3], [2, 4, 6], [1, 0, 1]]")
    s.execute("N := nullspace(A)")
    assert(s.execute("max(abs(A * N))") == "0.0")
    assert(s.execute("nullspace([[1, 0], [0, 1]])") == "false")
  }

  it should "say why a tolerance was refused" in
  {
    val s = Session()
    val out = s.execute("orth([[1, 2], [2, 4]], 2)")
    assert(out.contains("note: orth declined: the tolerance must be a number in [0, 1)"), out)
  }

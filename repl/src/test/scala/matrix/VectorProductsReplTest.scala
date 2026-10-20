package it.grypho.scala.leonardo
package matrix

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** F_0039, F_0160-F_0162 at the prompt: `norm`, `dot`, `cross` and `trace` read the session's
 *  bindings, `simplify` reduces them as it reduces `det`, an exact session keeps them exact, and
 *  a definition holding one survives `:save`. */
class VectorProductsReplTest extends AnyFlatSpec:

  "the REPL" should "evaluate the four on bound matrices" in
  {
    val s = Session()
    s.execute("A := [[1, 2], [3, 4]]")
    s.execute("u := [[1], [2], [3]]")
    assert(s.execute("trace(A)") == "5.0")
    assert(s.execute("dot(u, u)") == "14.0")
    assert(s.execute("norm([[3], [4]])") == "5.0")
    assert(s.execute("norm(A, 1)") == "6.0")
    assert(s.execute("cross(u, [[0], [0], [1]])") == "[[2.0], [-1.0], [0.0]]")
  }

  it should "reduce them under simplify, as it reduces det" in
  {
    val s = Session()
    s.execute("A := [[1, 2], [3, 4]]")
    assert(s.execute("simplify trace(A)") == s.execute("trace(A)"))
    assert(s.execute("simplify norm(A, inf)") == "7.0")
  }

  it should "stay exact in an exact session" in
  {
    val s = Session()
    s.execute("exact on")
    assert(s.execute("dot([[1/2], [1/3]], [[1], [1]])") == "5/6")
    assert(s.execute("trace([[1/2, 0], [0, 1/3]])") == "5/6")
    assert(s.execute("norm([[1/2], [-1/3]], 1)") == "5/6")
  }

  it should "save and reload a definition that holds a norm" in
  {
    val s = Session()
    s.execute("n := norm(v, fro) + trace(M)")
    val t = Session()
    t.load(s.script)
    assert(t.script == s.script)
    t.execute("v := [[3], [4]]")
    t.execute("M := [[1, 0], [0, 1]]")
    assert(t.execute("n") == "7.0")
  }

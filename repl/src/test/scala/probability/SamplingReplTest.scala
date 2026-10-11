package it.grypho.scala.leonardo
package probability

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** F_0067 at the prompt: a seeded draw and a PRBS bind as columns, the same seed reproduces the
 *  same numbers, and a refusal says why. */
class SamplingReplTest extends AnyFlatSpec:

  "the REPL" should "bind a seeded draw and reproduce it from the same seed" in
  {
    val s = Session()
    s.execute("X := normal(0, 1)")
    s.execute("w := draw(X, 5, 42)")
    assert(s.execute("w") == s.execute("draw(X, 5, 42)"))
    assert(s.execute("w") != s.execute("draw(X, 5, 43)"))
    assert(s.execute("draw(X, 2, 42, 3)") == s.execute("submatrix(w, 4, 5, 1, 1)"))
  }

  it should "answer a PRBS as a column of ±1" in
  {
    val s = Session()
    s.execute("u := prbs(3, 7, 1)")
    assert(s.execute("sum(at(u, k, 1), k, 1, 7)") == "1.0")
    assert(s.execute("sum(at(u, k, 1)^2, k, 1, 7)") == "7.0")
  }

  it should "say why a draw or a PRBS was declined" in
  {
    val s = Session()
    val d = s.execute("draw(normal(0, 1), 0, 1)")
    assert(d.contains("note: draw declined: the count must be a whole number from 1 to 100000, not 0.0"), d)
    val p = s.execute("prbs(25, 10, 1)")
    assert(p.contains("note: prbs declined: the order must be a whole number from 2 to 20, not 25.0"), p)
  }

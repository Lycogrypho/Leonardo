package it.grypho.scala.leonardo
package equation

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** REPL-level behaviour of the equation domain.
 *
 *  Split out of `EquationTest` by issue 5.2 phase 1.1: these cases drive `cli.Session`, which
 *  lives in the `leonardo-repl` module, and the library module cannot depend on it. The
 *  package is unchanged, so `testOnly it.grypho.scala.leonardo.equation.*` still selects both
 *  halves.
 */
class EquationReplTest extends AnyFlatSpec:

  "an equation in the REPL" should "evaluate to true/false once variables are bound" in
  {
    val s = Session()
    // unbound: echoes the symbolic equation with both sides reduced
    assert(s.execute("10 * x = 2 * x + 1") == "(10.0 * x) = ((2.0 * x) + 1.0)")
    s.execute("x := 0.125")
    assert(s.execute("10 * x = 2 * x + 1") == "true")
    s.execute("x := 1")
    assert(s.execute("10 * x = 2 * x + 1") == "false")
  }

  "an exact comparison the working precision decided" should "say so (F_0177)" in
  {
    val s = Session()
    s.execute("exact on")
    // A cube root is still carried to the working precision, so the tolerance decides its identity.
    assert(s.execute("(2^(1/3))^3 == 2") ==
      "true\n  note: the two sides agree to the working precision (30 digits), not exactly")
    assert(s.execute("(2^(1/3))^3 <= 2") ==
      "true\n  note: the two sides agree to the working precision (30 digits), not exactly")
    assert(s.execute("1/2 == 2/4") == "true")
    assert(s.execute("1/3 == 0.33333") == "false")
    s.execute("exact precision 50")
    assert(s.execute("(2^(1/3))^3 == 2") ==
      "true\n  note: the two sides agree to the working precision (50 digits), not exactly")
  }

  "a square root" should "be exact since F_0231, so its identity needs no note" in
  {
    val s = Session()
    s.execute("exact on")
    assert(s.execute("(2^(1/2))^2 == 2") == "true")
    assert(s.execute("√2 * √8 == 4") == "true")
  }

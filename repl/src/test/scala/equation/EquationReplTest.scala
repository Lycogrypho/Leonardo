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

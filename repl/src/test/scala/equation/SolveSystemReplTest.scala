package it.grypho.scala.leonardo
package equation

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** REPL-level behaviour of `solveSystem`, including the named-equation-matrix form.
 *
 *  Split out of `SolveSystemTest` by issue 5.2 phase 1.1.
 */
class SolveSystemReplTest extends AnyFlatSpec:

  "solveSystem in the REPL" should "display solutions as a row-vector" in
  {
    val s = Session()
    assert(s.execute("solveSystem([[2*x + y = 5, x + 3*y = 10]], x, y)") == "[[x = 1.0, y = 3.0]]")
  }

  "solveSystem with a named equation matrix" should "work after binding S := [[...]]" in
  {
    val s = Session()
    s.execute("S := [[2*x + y = 5, x + 3*y = 10]]")
    assert(s.execute("solveSystem(S, x, y)") == "[[x = 1.0, y = 3.0]]")
  }

  "solveSystem with a bound coefficient" should "fold it numerically" in
  {
    val s = Session()
    s.execute("a := 2")
    // a=2: a*x + y = 5, x - y = 1 -> 2x + y = 5, x - y = 1 -> x=2, y=1
    assert(s.execute("solveSystem([[a*x + y = 5, x - y = 1]], x, y)") == "[[x = 2.0, y = 1.0]]")
  }

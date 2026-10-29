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

  // F_0242 (LT L_0046): more equations than unknowns.
  "solveSystem with more equations than unknowns" should "solve a consistent system" in
  {
    val s = Session()
    s.execute("exact on")
    assert(s.execute("solveSystem([[c = 1], [d = 2], [c + d = 3]], c, d)") == "[[c = 1, d = 2]]")
  }

  it should "say why an inconsistent one is left unevaluated" in
  {
    val s = Session()
    s.execute("exact on")
    val out = s.execute("solveSystem([[y = 1], [z = 2], [y + z = 4]], y, z)")
    assert(out.startsWith("solveSystem("), out)
    assert(out.contains("solveSystem declined: inconsistent: equation 3"), out)
  }

  "solveSystem with a bound coefficient" should "fold it numerically" in
  {
    val s = Session()
    s.execute("a := 2")
    // a=2: a*x + y = 5, x - y = 1 -> 2x + y = 5, x - y = 1 -> x=2, y=1
    assert(s.execute("solveSystem([[a*x + y = 5, x - y = 1]], x, y)") == "[[x = 2.0, y = 1.0]]")
  }

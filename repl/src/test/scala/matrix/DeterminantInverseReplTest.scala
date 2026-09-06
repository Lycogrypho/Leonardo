package it.grypho.scala.leonardo
package matrix

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** REPL-level determinant and inverse behaviour — the session originally reported in issue 1.1.
 *
 *  Split out of `DeterminantInverseTest` by issue 5.2 phase 1.1: these cases drive
 *  `cli.Session`, which lives in the `leonardo-repl` module.
 */
class DeterminantInverseReplTest extends AnyFlatSpec:

  "the REPL" should "compute det(A), inv(A) and 1/A on a bound matrix" in
  {
    val s = Session()
    s.execute("A := [[1, 2], [1, 3]]")
    assert(s.execute("det(A)") == "1.0")
    assert(s.execute("inv(A)") == "[[3.0, -2.0], [-1.0, 1.0]]")
    assert(s.execute("1 / A") == "[[3.0, -2.0], [-1.0, 1.0]]")
    assert(s.execute("eval inv(A)") == "[[3.0, -2.0], [-1.0, 1.0]]")
    assert(s.execute("simplify det(A)") == "1.0")
  }

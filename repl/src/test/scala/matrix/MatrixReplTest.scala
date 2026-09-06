package it.grypho.scala.leonardo
package matrix

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** REPL-level matrix element access.
 *
 *  Split out of `MatrixTest` by issue 5.2 phase 1.1.
 */
class MatrixReplTest extends AnyFlatSpec:

  "at(A, i, j) in the REPL" should "extract elements from a bound matrix" in
  {
    val s = Session()
    s.execute("A := [[1, 2], [3, 4]]")
    assert(s.execute("at(A, 2, 1)") == "3.0")
    assert(s.execute("at(A, 1, 2)") == "2.0")
  }

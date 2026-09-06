package it.grypho.scala.leonardo
package matrix

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** REPL-level behaviour of the matrix constructors `eye` and `zeros`.
 *
 *  Split out of `MatrixConstructorsTest` by issue 5.2 phase 1.1.
 */
class MatrixConstructorsReplTest extends AnyFlatSpec:

  "the REPL" should "evaluate eye and zeros expressions" in
  {
    val s = Session()
    assert(s.execute("eye(2)")    == "[[1.0, 0.0], [0.0, 1.0]]")
    assert(s.execute("zeros(2)")  == "[[0.0, 0.0], [0.0, 0.0]]")
    assert(s.execute("zeros(2, 3)") == "[[0.0, 0.0, 0.0], [0.0, 0.0, 0.0]]")
  }

  "the REPL" should "compute A + eye(n) correctly" in
  {
    val s = Session()
    s.execute("A := [[2, 1], [0, 3]]")
    assert(s.execute("A + eye(2)") == "[[3.0, 1.0], [0.0, 4.0]]")
  }

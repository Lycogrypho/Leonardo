package it.grypho.scala.leonardo
package matrix

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** REPL-level behaviour of the `lu` and `qr` decompositions, whose results are row-vector
 *  matrices of matrices.
 *
 *  Split out of `MatrixDecompositionTest` by issue 5.2 phase 1.1.
 */
class MatrixDecompositionReplTest extends AnyFlatSpec:

  "the REPL" should "compute lu(A) and display [[L, U, P]]" in
  {
    val s = Session()
    s.execute("A := [[1, 2], [3, 4]]")
    val out = s.execute("lu(A)")
    // Result is a 1x3 matrix: [[L, U, P]] displayed as a _Matrix of _MatrixValue strings
    assert(out.nonEmpty, "expected non-empty output from lu(A)")
    assert(!out.startsWith("parse error"))
  }

  "the REPL" should "compute qr(A) and display [[Q, R]]" in
  {
    val s = Session()
    s.execute("A := [[1, 2], [3, 4]]")
    val out = s.execute("qr(A)")
    assert(out.nonEmpty, "expected non-empty output from qr(A)")
    assert(!out.startsWith("parse error"))
  }

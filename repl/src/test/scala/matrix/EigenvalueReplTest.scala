package it.grypho.scala.leonardo
package matrix

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** REPL-level behaviour of `eigen`, including indexing into its row-vector result.
 *
 *  Split out of `EigenvalueTest` by issue 5.2 phase 1.1.
 */
class EigenvalueReplTest extends AnyFlatSpec:

  "the REPL" should "compute eigen([[4, 1], [1, 3]]) without error" in
  {
    val s   = Session()
    val out = s.execute("eigen([[4, 1], [1, 3]])")
    assert(out.nonEmpty,                     "expected non-empty output")
    assert(!out.startsWith("parse error"),   s"unexpected parse error: $out")
  }

  "the REPL" should "compute eigen(A) for a bound matrix" in
  {
    val s = Session()
    s.execute("A := [[2, 0], [0, 3]]")
    val out = s.execute("eigen(A)")
    assert(out.nonEmpty,                   "expected non-empty output")
    assert(!out.startsWith("parse error"), s"unexpected parse error: $out")
  }

  "at(eigen(A), 1, k)" should "extract individual eigenvalues" in
  {
    val s = Session()
    s.execute("A := [[0, -1], [1, 0]]")
    // eigen returns [[i, -i]] or [[-i, i]]; both extractions must be non-error
    val e1 = s.execute("at(eigen(A), 1, 1)")
    val e2 = s.execute("at(eigen(A), 1, 2)")
    assert(!e1.startsWith("parse error"), s"at eigen 1 failed: $e1")
    assert(!e2.startsWith("parse error"), s"at eigen 2 failed: $e2")
  }

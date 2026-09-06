package it.grypho.scala.leonardo
package matrix

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** REPL-level behaviour of the spectral (`eig`) and Jordan (`jordan`) decompositions,
 *  including indexing into their `[[V, D]]` / `[[P, J]]` row-vector results.
 *
 *  Split out of `EigDecompositionTest` by issue 5.2 phase 1.1.
 */
class EigDecompositionReplTest extends AnyFlatSpec:

  "at(eig(A), 1, 1)" should "extract V and at(eig(A), 1, 2) should extract D" in
  {
    val s = Session()
    s.execute("A := [[4, 1], [1, 3]]")
    val outV = s.execute("at(eig(A), 1, 1)")
    val outD = s.execute("at(eig(A), 1, 2)")
    assert(!outV.startsWith("parse error"), s"extracting V failed: $outV")
    assert(!outD.startsWith("parse error"), s"extracting D failed: $outD")
  }

  "the REPL" should "compute eig([[3,1],[1,3]]) without error" in
  {
    val s   = Session()
    val out = s.execute("eig([[3, 1], [1, 3]])")
    assert(out.nonEmpty, "expected non-empty output")
    assert(!out.startsWith("parse error"), s"parse error: $out")
  }

  "the REPL" should "compute jordan([[3,1],[1,3]]) without error" in
  {
    val s   = Session()
    val out = s.execute("jordan([[3, 1], [1, 3]])")
    assert(out.nonEmpty, "expected non-empty output")
    assert(!out.startsWith("parse error"), s"parse error: $out")
  }

package it.grypho.scala.leonardo
package matrix

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec


/** F_0055 through the REPL: tuple assignment of `mpcMatrices` (Decision D) and `:save`/`:load`
 *  of definitions built from the block-matrix words.  In the repl module because it constructs
 *  a `Session`. */
class BlockMatrixReplTest extends AnyFlatSpec:

  "mpcMatrices" should "bind through tuple assignment" in
  {
    val s = new Session()
    // Not `Gamma`: that name is the reserved Gamma function, so tuple assignment refuses it.
    s.execute("Phi, Gam := mpcMatrices(0.5, 1, 1, 3, 2)")
    assert(s.execute("Phi") == "[[0.5], [0.25], [0.125]]")
    assert(s.execute("Gam") == "[[1.0, 0.0], [0.5, 1.0], [0.25, 1.5]]")
  }

  "a definition using the block-matrix words" should "survive :save / :load" in
  {
    val s1 = new Session()
    s1.execute("W := blkdiag(q * eye(2), r)")
    s1.execute("S := submatrix(hcat(W, kron(ones(3, 1), [[1]])), 1, 2, 2, 4)")
    s1.execute("q := 2")
    s1.execute("r := 5")
    val s2 = new Session()
    s2.load(s1.script)
    assert(s2.execute("S") == s1.execute("S"))
    assert(s2.execute("S") == "[[0.0, 0.0, 1.0], [2.0, 0.0, 1.0]]")
  }

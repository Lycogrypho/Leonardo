package it.grypho.scala.leonardo
package matrix

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** F_0227 and F_0232 at the prompt: the probes the course review ran. */
class ExactPowerQrReplTest extends AnyFlatSpec:

  "an exact matrix power at the prompt" should "answer exact integers" in {
    val s = new Session()
    s.execute("exact on")
    s.execute("H := [[2, 1], [1, 2]]")
    assert(s.execute("H^10 * [[1], [0]]") == "[[29525], [29524]]")
  }

  "qr of dependent columns" should "say why it stays unevaluated" in {
    val out = new Session().execute("qr(hcat([[2], [1], [1]], [[4], [2], [2]]))")
    assert(out.endsWith("\n  note: qr declined: the columns are linearly dependent, so R would be singular; rank and nullspace show the dependence"))
  }

  "qr of a wide matrix" should "say it has more columns than rows" in {
    assert(new Session().execute("qr([[1, 2, 3], [4, 5, 6]])").contains("qr declined: a 2×3 matrix has more columns than rows"))
  }

  "qr that answers" should "carry no note" in {
    assert(!new Session().execute("qr([[1, 2], [3, 4]])").contains("note"))
  }

package it.grypho.scala.leonardo
package cli

import org.scalatest.flatspec.AnyFlatSpec

/** F_0273: under `exact on` every answer is exact or says it is a decimal (the owner's decision). */
class ExactModeNoteTest extends AnyFlatSpec:

  private def exactSession: Session =
    val s = new Session()
    s.execute("exact on")
    s

  "a letters-only input under exact on" should "answer with exact constants and no note" in {
    val s = exactSession
    assert(s.execute("derive(atan(x), x)") == "(1 / (1 + (x ^ 2)))")
    assert(s.execute("integral(y, y)") == "((y ^ 2) / 2)")
    assert(s.execute("integral(integral(1, y, 0, x), x, 0, 1)") == "1/2")
    assert(s.execute("div([[x], [y], [z]], x, y, z)") == "3")
  }

  "an answer with no exact method" should "say it is a decimal" in {
    val s = exactSession
    for line <- Seq("pade(exp(x), x, 2, 2)", "eig([[2, 1], [1, 3]])") do
      val out = s.execute(line)
      assert(out.contains("note:"), s"$line: $out")
  }

  "the default mode" should "carry no such note" in {
    val s = new Session()
    assert(!s.execute("pade(exp(x), x, 2, 2)").contains("note:"))
    assert(s.execute("derive(atan(x), x)") == "(1.0 / (1.0 + (x ^ 2.0)))")
  }

package it.grypho.scala.leonardo
package cli

import org.scalatest.flatspec.AnyFlatSpec

/** F_0171 — at the prompt, calculus on letters bound to exact values answers exactly (V3).
 *
 *  The probe that found it: in exact mode, with `a := 1/3` and `t := 2`, `derive(a/t, t)`
 *  answered `-0.08333`, bound first or through a late-bound definition alike.
 */
class BoundTierReplTest extends AnyFlatSpec:

  private def exactSession: Session =
    val s = new Session()
    for line <- List("exact on", "a := 1/3", "t := 2") do s.execute(line)
    s

  "derive" should "answer exactly with its letters bound first" in {
    assert(exactSession.execute("derive(a/t, t)") == "-1/12")
  }

  it should "answer exactly through a late-bound definition" in {
    val s = new Session()
    for line <- List("exact on", "F := derive(a/t, t)", "a := 1/3", "t := 2") do s.execute(line)
    assert(s.execute("F") == "-1/12")
  }

  "sum" should "answer exactly over a letter bound to an exact value" in {
    assert(exactSession.execute("sum(a*k, k, 1, 3)") == "2")
  }

  "a session with exact mode off" should "answer as before" in {
    val s = new Session()
    for line <- List("a := 0.5", "t := 2") do s.execute(line)
    assert(s.execute("derive(a/t, t)") == "-0.125")
  }

package it.grypho.scala.leonardo
package logic

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** F_0325 at the prompt (LT L_0081): the three probes of the course review simplify. */
class LogicCombiningReplTest extends AnyFlatSpec:

  "simplify" should "apply De Morgan, the combining law and the absorbed implication" in {
    val s = Session()
    assert(s.execute("simplify not (not p and not q)") == "(p or q)")
    assert(s.execute("simplify (p and q) or (p and not q)") == "p")
    assert(s.execute("simplify (p implies q) and p") == "(p and q)")
  }

  "the normal forms" should "still answer the clauses of the formula" in {
    val s = Session()
    assert(s.execute("cnf((p or r) and ((p or r) or s))") == "(p or r)")
  }

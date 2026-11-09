package it.grypho.scala.leonardo
package logic

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** F_0322 at the prompt (LT L_0079): `iff` and `equivalent(f, g)`, the two things the BL lessons built
 *  by hand out of `truth f xor g`. */
class IffReplTest extends AnyFlatSpec:

  "the REPL" should "read iff, show it, and tabulate it" in {
    val s = Session()
    assert(s.execute("p iff q") == "(p iff q)")
    val rows = s.execute("truth p iff q").linesIterator.toList
    assert(rows.head == "p     q     | (p iff q)")
    assert(rows.tail.map(_.split('|').last.trim) == List("true", "false", "false", "true"), rows.toString)
    assert(s.execute("simplify p iff p") == "true")
    assert(s.execute("cnf(p iff q)") == "(((not p) or q) and (p or (not q)))")
  }

  it should "refuse the two words as names" in {
    val s = Session()
    assert(s.execute("iff := 3") == "cannot assign to 'iff': it is a reserved word")
    assert(s.execute("equivalent := 3") == "cannot assign to 'equivalent': it is a reserved word")
  }

  it should "answer equivalent(f, g) on named formulas, with no note when they agree" in {
    val s = Session()
    s.execute("f := p implies q")
    s.execute("g := not p or q")
    assert(s.execute("equivalent(f, g)") == "true")
    assert(s.execute("equivalent(f, p and q)") .startsWith("false"))
  }

  it should "name the first differing row when they do not" in {
    val out = Session().execute("equivalent(p and q, p or q)")
    assert(out == "false\n  note: equivalent: not equivalent at p = false, q = true (the left side is false, the right side is true)", out)
  }

  it should "say why it declined" in {
    val graded = Session().execute("equivalent(p, unknown)")
    assert(graded.startsWith("equivalent(") && graded.contains("note: equivalent declined: a row does not reduce to true or false"), graded)
    val many = (1 to 17).map(i => s"x$i").mkString(" and ")
    val out = Session().execute(s"equivalent($many, $many)")
    assert(out.contains("note: equivalent declined: 17 names exceed the table's limit of 16"), out)
  }

  "pretty on" should "write iff at the loosest level, bracketing a chain" in {
    val s = Session()
    s.execute("pretty on")
    assert(s.execute("p iff q or r") == "p iff q or r")
    assert(s.execute("(p iff q) iff r") == "(p iff q) iff r")
  }

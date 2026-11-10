package it.grypho.scala.leonardo
package logic

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** F_0325 step 2 at the prompt (LT L_0081): `mindnf` and `mincnf`, the minimisation exercise a logic course
 *  sets, done by hand until now. */
class MinimiseReplTest extends AnyFlatSpec:

  "mindnf" should "drop the consensus term that dnf keeps" in {
    val s = Session()
    val minimal = s.execute("mindnf((p or q) and (not p or r))")
    assert(minimal == "((p and r) or ((not p) and q))")
    assert(s.execute("dnf((p or q) and (not p or r))").length > minimal.length)   // dnf keeps the consensus term
  }

  "mincnf" should "answer the minimal product of sums" in {
    assert(Session().execute("mincnf((p and r) or (not p and q) or (q and r))") == "((p or q) and ((not p) or r))")
  }

  it should "compose inside an expression, and read a named formula" in {
    val s = Session()
    s.execute("f := (p or q) and (not p or r)")
    assert(s.execute("mindnf(f)") == "((p and r) or ((not p) and q))")
    assert(s.execute("equivalent(f, mindnf(f))") == "true")
  }

  "a cyclic cover" should "say how many minimal forms there are" in {
    val f = "(not a and not b) or (not a and not c) or (not b and c) or (b and not c) or (a and c) or (a and b)"
    val out = Session().execute(s"mindnf($f)")
    assert(out == "(((a and b) or ((not a) and (not c))) or ((not b) and c))\n" +
                  "  note: mindnf: 2 minimal forms exist; this is the first in canonical order", out)
  }

  "a refusal" should "leave the call as written and say why" in {
    val graded = Session().execute("mindnf(truth(0.3) and p)")
    assert(graded.startsWith("mindnf(") && graded.contains("note: mindnf declined: a graded truth value"), graded)
  }

  "the two words" should "be reserved" in {
    val s = Session()
    assert(s.execute("mindnf := 3") == "cannot assign to 'mindnf': it is a reserved word")
    assert(s.execute("mincnf := 3") == "cannot assign to 'mincnf': it is a reserved word")
  }

  "a saved definition" should "come back through :save and :load" in {
    val s = Session()
    s.execute("g := mindnf((p or q) and (not p or r))")
    val t = Session()
    t.load(s.script)
    assert(t.execute("g") == "((p and r) or ((not p) and q))")
  }

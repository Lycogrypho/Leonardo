package it.grypho.scala.leonardo
package scalar

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** F_0286 at the prompt: a generic limit notes the conditions it assumed (the owner's decision). */
class LimitAlgebraReplTest extends AnyFlatSpec:

  "a generic limit" should "note what it assumed" in {
    val s = new Session()
    val out = s.execute("limit(x/(x - a), x, 2)")
    assert(out.contains("note:") && out.contains("assuming") && out.contains("!= 0"), out)
  }

  "a limit with nothing assumed" should "carry no such note" in {
    val s = new Session()
    assert(!s.execute("limit(a/x, x, 2)").contains("assuming"))
    assert(s.execute("limit(ln(t), t, 0, +)") == "-inf")
  }

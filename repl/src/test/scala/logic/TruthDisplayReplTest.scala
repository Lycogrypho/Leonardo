package it.grypho.scala.leonardo
package logic

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** F_0327 (LT L_0082) -- the `truth` header names the formula as typed, and a `?` that comes from a
 *  non-proposition says so.  Before, the header was the definitions substituted in full and a row
 *  of `?` came with no word on why. */
class TruthDisplayReplTest extends AnyFlatSpec:

  private def header(table: String): String = table.linesIterator.next()

  "the truth header" should "keep the names the formula was typed with, not their bodies" in {
    val s = Session()
    s.execute("f := a and (b or not c)")
    s.execute("g := (a and b) or (a and not c)")
    val table = s.execute("truth f xor g")
    assert(header(table) == "a     b     c     | (f xor g)", header(table))
    // the columns are still the atoms, and f and g agree on every row
    assert(table.linesIterator.drop(1).forall(_.endsWith("| false")), table)
    assert(!table.contains("note:"))
  }

  it should "be the expression itself when no definition is involved" in {
    assert(header(Session().execute("truth a and b")) == "a     b     | (a and b)")
  }

  "a row that is not a boolean because of a relation" should "be explained" in {
    val out = Session().execute("truth x > 0 or (x <= 0 and y > 100)")
    assert(out.linesIterator.drop(1).take(4).forall(_.endsWith("| ?")), out)
    assert(out.contains("note: truth enumerates names as booleans; x > 0.0 is not a proposition"), out)
  }

  it should "be explained for truth3 too, in its own words" in {
    val out = Session().execute("truth3 x > 0 and y")
    assert(out.contains("note: truth3 enumerates names as false, unknown or true; x > 0.0 is not a proposition"), out)
  }

  "a table whose rows all reduce" should "carry no note" in {
    val s = Session()
    assert(!s.execute("truth a implies (b or a)").contains("note"))
    assert(!s.execute("truth 2 > 1 and a").contains("note"))
  }

  "a `?` from a graded constant" should "carry no note either: truth3 is what that needs" in {
    val out = Session().execute("truth a and unknown")
    assert(out.contains("?") && !out.contains("note"), out)
  }

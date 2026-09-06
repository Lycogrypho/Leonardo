package it.grypho.scala.leonardo
package core

import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** Issue 3.5 — base conversion, with the base carried on the value. */
class BaseConversionTest extends AnyFlatSpec:

  private val env = new Environment()

  private def run(src: String): String =
    Parser.parse(src) match
      case Parser.Success(e, _) => e.eval(env).fold(_.toString, _.toString)
      case other                => fail(s"parse failed for '$src': $other")

  // ── the point of the whole design: the value remembers its base ────────────

  "a based value" should "still print in its base after being stored" in
  {
    // This is the property that ruled out returning a bare digit matrix: `0xFF` has to
    // still be FF later, not an anonymous 255.
    assert(run("0xff") == "0xFF")
    assert(run("0b1011") == "0b1011")
    assert(run("0o17") == "0o17")
  }

  it should "read as its numeric value everywhere else" in
  {
    // The widening extractor: every existing `case _Number(x)` site keeps matching.
    assert(run("0xff + 1") == "256.0")
    assert(run("0b1011 * 2") == "22.0")
    assert(run("0xff = 255") == "true")
  }

  it should "lose the base through arithmetic, which is the documented shallow tag" in
  {
    // Propagating a base through `0xFF + 0b1011` would need a rule for whose base wins.
    assert(run("0xff + 0b1") == "256.0")
  }

  // ── the smart factory collapses when there is nothing to remember ──────────

  "base 10" should "collapse to a plain number" in
  {
    assert(_Based.of(255, 10).isInstanceOf[_Number])
  }

  "a non-integer" should "collapse to a plain number" in
  {
    assert(_Based.of(2.5, 16).isInstanceOf[_Number])
  }

  "an out-of-range base" should "collapse rather than be constructed" in
  {
    assert(_Based.of(255, 1).isInstanceOf[_Number])
    assert(_Based.of(255, 37).isInstanceOf[_Number])
  }

  // ── tobase, for a generic radix ────────────────────────────────────────────

  "tobase" should "convert to hexadecimal" in
  {
    assert(run("tobase(255, 16)") == "0xFF")
  }

  it should "handle a base with no literal syntax by printing the call" in
  {
    // Base 7 has no prefix, so the printed form is the call that produces it -- which
    // re-parses, keeping the round-trip invariant.
    assert(run("tobase(255, 7)") == "tobase(255, 7)")
  }

  it should "stay symbolic for a non-integer or an impossible base" in
  {
    assert(run("tobase(2.5, 16)") == "tobase(2.5, 16.0)")
    assert(run("tobase(255, 99)") == "tobase(255.0, 99.0)")
  }

  // ── balanced ternary, sharing 4.G's alphabet ───────────────────────────────

  "balanced ternary" should "use the digits T, 0 and 1" in
  {
    // 5 = 9 - 3 - 1 = 1*3^2 + (-1)*3^1 + (-1)*3^0  ->  1TT
    assert(run("balanced(5)") == "0t1TT")
    assert(run("balanced(0)") == "0t0")
    assert(run("balanced(1)") == "0t1")
  }

  it should "round-trip through the literal form" in
  {
    for n <- Vector(-13, -1, 0, 1, 5, 13, 42, 121) do
      val printed = run(s"balanced($n)")
      val back    = run(printed)
      assert(back == printed, s"$n printed as $printed but re-parsed to $back")
      assert(run(s"$printed + 0") == s"${n.toDouble}", s"$printed should be worth $n")
  }

  // ── literals ───────────────────────────────────────────────────────────────

  "literals" should "parse in every supported prefix" in
  {
    assert(run("0b1011 + 0") == "11.0")
    assert(run("0o17 + 0")   == "15.0")
    assert(run("0xff + 0")   == "255.0")
    assert(run("0t1TT + 0")  == "5.0")
  }

  it should "be case-insensitive in prefix and digits" in
  {
    assert(run("0XFF + 0") == "255.0")
    assert(run("0Xff + 0") == "255.0")
  }

  it should "decline a malformed literal rather than mis-reading it" in
  {
    // `0b1011x` is NOT a based literal -- the word-boundary guard rejects it -- and the
    // grammar then reads it the way it already read `0abc` before this issue existed:
    // implicit multiplication, `0 * b1011x`. That is pre-existing behaviour, not something
    // the based literals introduced, and it is pinned here so the distinction is on record:
    // a *valid* prefix is now a literal, an invalid one is unchanged.
    assert(run("0b1011x") == "0.0")            // 0 * b1011x
    assert(run("0xffz") == "0.0")              // 0 * xffz
    // The digit set is enforced per prefix: 2 is not a binary digit.
    assert(run("0b12") == "0.0")               // 0 * b12
  }

  "an ordinary zero" should "still parse as a number" in
  {
    assert(run("0") == "0.0")
    assert(run("0.5") == "0.5")
    assert(run("0 + 1") == "1.0")
  }

  // ── round-trip ─────────────────────────────────────────────────────────────

  "every based form" should "re-parse to the same value" in
  {
    for src <- Vector("0xFF", "0b1011", "0o17", "0t1TT", "tobase(255, 7)") do
      val once  = run(src)
      val twice = run(once)
      assert(once == twice, s"'$src' printed as $once but re-parsed to $twice")
  }

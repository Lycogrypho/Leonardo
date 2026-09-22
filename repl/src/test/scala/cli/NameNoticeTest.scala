package it.grypho.scala.leonardo
package cli

import org.scalatest.flatspec.AnyFlatSpec

import parser.Parser

/** Issue F_0030 — the REPL says when it has invented a name.
 *
 *  **The hazard is that the grammar cannot refuse one.**  An unrecognised identifier is a
 *  perfectly good variable and juxtaposition is multiplication, so `sqrt(x)` evaluates to the
 *  product of a free variable named `sqrt` with `x` — no error, an answer, and a wrong one.
 *  These cases pin the two diagnostics that expose it, and the three rules they must obey:
 *  the call-syntax note is unconditional, the new-name note is a setting, and **neither may
 *  leave the LaTeX channel filled**, since a note is text the formula does not carry (F_0020).
 */
class NameNoticeTest extends AnyFlatSpec:

  private def session: Session = new Session()

  // --- the call-syntax note, which is unconditional ----------------------------------

  "a name used as a function" should "be reported when the grammar has no such function" in
  {
    // THE case this issue exists for: `sqrt` is not a production and not a reserved word, so
    // the parser is perfectly happy and the answer is a silent product.
    val out = session.execute("sqrt(4)")
    assert(out.contains("sqrt"), out)
    assert(out.contains("not a function"), out)
  }

  it should "catch a miscapitalised built-in, which is the same failure wearing a disguise" in
  {
    for wrong <- List("Sin(x)", "COS(x)", "Exp(x)") do
      assert(session.execute(wrong).contains("not a function"), wrong)
  }

  it should "say nothing for a real function, however it is spelled" in
  {
    for good <- List("sin(0)", "log(100)", "log(8, 2)", "fact(4)", "Gamma(3)", "fresnelS(0)") do
      assert(!session.execute(good).contains("not a function"), good)
  }

  it should "report a defined name too, since a definition takes no arguments" in
  {
    val s = session
    s.execute("f := sin(x)")
    // `f(2)` is not application, it is `f * 2` -- worth saying, because the user plainly meant
    // application and the grammar silently gave them a product.
    assert(s.execute("f(2)").contains("not a function"))
  }

  it should "not fire on a parenthesised product, which needs no identifier" in
  {
    for fine <- List("2(x + 1)", "(a + b)(c + d)", "x * (y + 1)") do
      assert(!session.execute(fine).contains("not a function"), fine)
  }

  it should "stay silent when the setting that governs the OTHER note is off" in
  {
    // The two diagnostics are independent: this one is not behind `names`.
    val s = session
    assert(s.execute("names") == "names = off")
    assert(s.execute("sqrt(4)").contains("not a function"))
  }

  // --- the new-name note, which is a setting ------------------------------------------

  "new names" should "be off by default" in
  {
    assert(!session.execute("x + y").contains("new name"))
  }

  it should "list the free variables a line introduces once enabled" in
  {
    val s = session
    s.execute("names on")
    val out = s.execute("x + y")
    assert(out.contains("new name"), out)
    assert(out.contains("x") && out.contains("y"), out)
  }

  it should "name the variable an assignment creates, and the ones its body uses" in
  {
    val s = session
    s.execute("names on")
    val out = s.execute("a := sin(x)")
    assert(out.contains("a") && out.contains("x"), out)
  }

  it should "say each name once, or a session becomes a stream of reminders" in
  {
    val s = session
    s.execute("names on")
    assert(s.execute("x + 1").contains("new name"))
    assert(!s.execute("x + 2").contains("new name"), "x was already announced")
  }

  it should "not announce a name that is already bound or defined" in
  {
    val s = session
    s.execute("k := 3")          // announced while off, so not yet in the seen set
    s.execute("names on")
    assert(!s.execute("k + 1").contains("new name"), "k is bound, so it is not new")
  }

  // --- the rules both notes obey -------------------------------------------------------

  "a note" should "clear the LaTeX channel, since it is text the formula does not carry" in
  {
    // F_0020's rule: a consumer shows the formula INSTEAD of the text, so offering one beside
    // a note would drop the note silently -- which is the whole feature.
    val s = session
    s.execute("latex on")
    assert(s.execute("sin(0)").nonEmpty && s.lastLatex.isDefined, "a plain result still renders")
    s.execute("sqrt(4)")
    assert(s.lastLatex.isEmpty, "a call-syntax note must suppress the formula")
  }

  it should "be suppressed while a script is replayed" in
  {
    // A `:load` is a transcript, not a conversation; narrating every line of it would bury
    // the output the script actually produced.  Same reasoning as `load` clearing the channel.
    val s = session
    s.execute("names on")
    val out = s.load("x + 1\nsqrt(4)\n")
    assert(!out.contains("new name"), out)
    assert(!out.contains("not a function"), out)
  }

  // --- the setting itself ---------------------------------------------------------------

  "the names setting" should "toggle, report and refuse a third value" in
  {
    val s = session
    assert(s.execute("names on") == "names = on")
    assert(s.execute("names") == "names = on")
    assert(s.execute("names off") == "names = off")
    assert(s.execute("names maybe").contains("expects 'on' or 'off'"))
  }

  it should "be a reserved word, so a binding cannot shadow the command" in
  {
    assert(Parser.ReservedWords.contains("names"))
    assert(session.execute("names := 3").contains("reserved word"))
  }

  it should "be persisted by a script round-trip, like every other setting" in
  {
    val s = session
    s.execute("names on")
    assert(s.script.linesIterator.contains("names on"))

    val restored = session
    restored.load(s.script)
    assert(restored.execute("names") == "names = on")
  }

  it should "appear in the settings a front end reads" in
  {
    assert(session.settings.exists((k, v) => k == "names" && v == "off"))
  }

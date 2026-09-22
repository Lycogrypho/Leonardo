package it.grypho.scala.leonardo
package cli

import org.scalatest.flatspec.AnyFlatSpec

import parser.Parser

/** Issue F_0016 step 5 — the `latex on | off` toggle and the LaTeX side channel.
 *
 *  Two properties carry the design and are pinned here rather than argued.  **The printed
 *  answer never changes**: the toggle adds a channel, it does not reroute the existing one,
 *  so a terminal REPL and every existing test see identical text with it on.  And **the
 *  channel is per-command**: it holds the LaTeX of the last *expression* and is empty after
 *  anything else, so a caller can never render the previous result beside this one's text.
 */
class LatexModeTest extends AnyFlatSpec:

  private def session: Session = new Session()

  "latex" should "be off by default" in
  {
    assert(session.execute("latex") == "latex = off")
  }

  it should "toggle on and off" in
  {
    val s = session
    assert(s.execute("latex on") == "latex = on")
    assert(s.execute("latex") == "latex = on")
    assert(s.execute("latex off") == "latex = off")
  }

  it should "refuse an argument that is neither" in
  {
    assert(session.execute("latex maybe").contains("expects 'on' or 'off'"))
  }

  it should "be a reserved word, so a binding cannot shadow the command" in
  {
    assert(Parser.ReservedWords.contains("latex"))
    assert(session.execute("latex := 3").contains("reserved word"))
  }

  "a saved session" should "round-trip the toggle" in
  {
    val s = session
    s.execute("latex on")
    val restored = new Session()
    restored.load(s.script)
    assert(restored.execute("latex") == "latex = on")
  }

  // --- mutual exclusion with `pretty` (F_0031) ------------------------------------------

  "latex on" should "turn pretty off, and say so" in
  {
    // A consumer shows the formula BESIDE the text, so a stacked matrix would be the same
    // layout twice.  The exclusion is performed on the session rather than hidden in the
    // page precisely so `settings` and `:save` report what is really in force -- a control
    // reading `pretty = on` while nothing stacked is the defect F_0031 was filed for.
    val s = session
    s.execute("pretty on")
    val out = s.execute("latex on")
    assert(out.contains("latex = on"), out)
    assert(out.contains("pretty"), s"the exclusion must be visible: $out")
    assert(s.execute("pretty") == "pretty = off")
  }

  it should "say nothing extra when pretty was already off" in
  {
    assert(session.execute("latex on") == "latex = on")
  }

  "pretty on" should "turn latex off, the same rule read the other way" in
  {
    val s = session
    s.execute("latex on")
    val out = s.execute("pretty on")
    assert(out.contains("pretty = on"), out)
    assert(out.contains("latex"), s"the exclusion must be visible: $out")
    assert(s.execute("latex") == "latex = off")
  }

  it should "leave a stacked matrix stacked, since latex is now off" in
  {
    val s = session
    s.execute("latex on")
    s.execute("pretty on")
    assert(s.execute("[[1, 2], [3, 4]]").contains("\n"))
  }

  "turning either off" should "not turn the other on" in
  {
    // The exclusion is one-directional per command: it only ever CLEARS the other flag.
    // Restoring it would guess at a state the user never asked for.
    val s = session
    s.execute("latex on")
    s.execute("latex off")
    assert(s.execute("pretty") == "pretty = off")
  }

  "the exclusion" should "survive a save and load" in
  {
    val s = session
    s.execute("pretty on")
    s.execute("latex on")
    val restored = new Session()
    restored.load(s.script)
    assert(restored.execute("latex") == "latex = on")
    assert(restored.execute("pretty") == "pretty = off")
  }

  // --- the side channel ---------------------------------------------------------------

  "the latex channel" should "stay empty while the toggle is off" in
  {
    val s = session
    s.execute("derive(sin(x), x)")
    assert(s.lastLatex.isEmpty)
  }

  it should "carry the rendered result while the toggle is on" in
  {
    val s = session
    s.execute("latex on")
    s.execute("1/x + 1")
    s.lastLatex match
      case None    => fail("no LaTeX was recorded for an expression result")
      case Some(l) => assert(l.contains("\\frac"), s"unexpected rendering: $l")
  }

  it should "leave the printed answer byte-identical" in
  {
    val off = session
    val on  = session
    on.execute("latex on")
    for line <- List("derive(sin(x)^2, x)", "2 + 3 * 4", "simplify x + 0", "[[1, 2], [3, 4]]") do
      assert(off.execute(line) == on.execute(line), s"the toggle changed the text of: $line")
  }

  it should "be cleared by a command that is not an expression" in
  {
    val s = session
    s.execute("latex on")
    s.execute("x + 1")
    assert(s.lastLatex.nonEmpty)
    s.execute("help")
    assert(s.lastLatex.isEmpty, "a help listing must not inherit the previous result's LaTeX")
  }

  it should "be cleared by a script, whose output is a transcript and not a result" in
  {
    // A front end that substitutes the formula for the text would otherwise typeset the
    // LAST line of the script and drop every other line the load produced.
    val s = session
    s.execute("latex on")
    val out = s.load(
      """a := 2
        |a + 1
        |a * 10""".stripMargin)
    assert(out.linesIterator.size == 3, s"a script's whole output is its answer: $out")
    assert(s.lastLatex.isEmpty, "a :load must not be reduced to its last line's formula")
  }

  it should "be cleared when the answer carries a domain note the formula would drop" in
  {
    // 3.3 slice F: the note is the whole value of the answer here, and it is not in the
    // rendering -- so there is nothing for a formula to replace without losing it.
    val s = session
    s.execute("latex on")
    val out = s.execute("limit(ln(x), x, -1)")
    assert(out.contains("note:"), s"expected a domain note, got: $out")
    assert(s.lastLatex.isEmpty, "a formula must not replace text carrying a domain note")
  }

  it should "be cleared by a parse error" in
  {
    val s = session
    s.execute("latex on")
    s.execute("x + 1")
    s.execute("sin(")
    assert(s.lastLatex.isEmpty)
  }

  it should "cover simplify and expand as well as bare evaluation" in
  {
    val s = session
    s.execute("latex on")
    s.execute("simplify x * 1")
    assert(s.lastLatex.contains("x"))
    s.execute("expand (a + b)^2")
    s.lastLatex match
      case None    => fail("expand recorded no LaTeX")
      case Some(l) => assert(l.contains("\\cdot"), s"unexpected rendering: $l")
  }

  it should "render Greek and the operator macros, which is the point of the toggle" in
  {
    val s = session
    s.execute("latex on")
    s.execute("sin(theta)")
    s.lastLatex match
      case None    => fail("no LaTeX was recorded")
      case Some(l) => assert(l.contains("\\sin") && l.contains("\\theta"), s"unexpected rendering: $l")
  }

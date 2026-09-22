package it.grypho.scala.leonardo
package web

import scala.scalajs.js
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.BeforeAndAfterAll

/** The page driven against [[FakeDom]] — the checks that used to live in a Node script
 *  (issue F_0027, Decision C).
 *
 *  **[[App]] holds one `Session` and one page for the life of the object**, so the suite
 *  installs the DOM once and the cases run in order against it, exactly as a reader's session
 *  accumulates.  That is why the bindings made early are still there later, which is itself
 *  one of the things worth checking.
 */
class AppTest extends AnyFlatSpec with BeforeAndAfterAll:

  override def beforeAll(): Unit = FakeDom.install()

  import FakeDom.*

  "the page" should "reach only for element ids index.html defines" in
  {
    // THE property this suite exists for: `getElementById` throws for an unknown id, so
    // `install()` having completed at all means App asked for nothing the page lacks. A typo
    // in an id is otherwise invisible until someone opens the page.
    assert(Ids.forall(id => !js.isUndefined(byId(id))))
  }

  it should "evaluate a line through the same Session.step the terminal REPL uses" in
  {
    typeLine("2+3*4");                 assert(lastOutput() == "14.0")
    typeLine("derive(sin(x)^2, x)");   assert(lastOutput().contains("cos"))
    typeLine("solve(x^2 - 4 > 0, x)"); assert(lastOutput().contains("or"))
  }

  it should "keep one session across lines, not one per line" in
  {
    typeLine("a := 7")
    typeLine("a * 6")
    assert(lastOutput() == "42.0")
  }

  it should "reuse :save unchanged, on the browser's own storage" in
  {
    typeLine(":save work")
    assert(lastOutput().contains("saved to work") || lastOutput().contains("storage is not available"))
  }

  "the history ring" should "walk back, and clear the box past the newest entry" in
  {
    press("ArrowUp");   assert(byId("input").value.asInstanceOf[String] == ":save work")
    press("ArrowUp");   assert(byId("input").value.asInstanceOf[String] == "a * 6")
    press("ArrowDown"); press("ArrowDown")
    assert(byId("input").value.asInstanceOf[String] == "")
  }

  // Placed AFTER the history cases deliberately: the math field submits through `submit`, so
  // what it sends joins the history like any typed line - which is the point, and which would
  // otherwise move the entries those cases name.
  "the math field" should "submit through the same path the prompt uses" in
  {
    // A second input, not a replacement: it reaches `submit`, so the transcript, the history
    // and every setting behave exactly as they do for a typed line.
    typeMath("\\sin(x)")
    assert(mathError() == "", mathError())
    assert(lastOutput().contains("sin"), lastOutput())
    assert(byId("mathfield").value.asInstanceOf[String] == "", "a submitted field is cleared")
    press("ArrowUp")
    assert(byId("input").value.asInstanceOf[String] == "sin(x)", "it joins the history")
  }

  it should "show a refusal rather than submit something the grammar would misread" in
  {
    // `int` is a binder the reader declines by name. Submitting it would not fail -- the
    // grammar would read it as a product of free variables -- which is the whole reason the
    // refusal happens here and not there.
    val before = lastOutput()
    typeMath("\\int x")
    assert(mathError().contains("integral"), mathError())
    assert(lastOutput() == before, "nothing should have been submitted")
    assert(byId("mathfield").value.asInstanceOf[String] != "", "the field keeps what was written")
  }

  "plot" should "emit a document Plotly could read, unlocked" in
  {
    typeLine("plot x^2 x 0 2 5")
    assert(lastOutput().contains("plotted 5 points"))
    val trace = drawn.data.asInstanceOf[js.Array[js.Dynamic]](0)
    assert(trace.mode.asInstanceOf[String] == "lines")
    assert(trace.y.asInstanceOf[js.Array[Double]].toVector == Vector(0.0, 0.25, 1.0, 2.25, 4.0))
    assert(drawn.layout.yaxis.title.asInstanceOf[String] == "x^2")
    // A function plot is not geometry: locking it would squash every ordinary figure.
    assert(js.isUndefined(drawn.layout.yaxis.scaleanchor))
  }

  "points" should "lock the axes, which is why Plotly was chosen over Vega-Lite" in
  {
    typeLine("points x x -1 1 3")
    assert(lastOutput().contains("plotted 3 points"))
    assert(drawn.layout.yaxis.scaleanchor.asInstanceOf[String] == "x")
  }

  "bode" should "draw two linked panels over a geometric grid, phase unwrapped" in
  {
    typeLine("bode 1/(s+1)^3 s 0.01 100 60")
    assert(lastOutput().contains("plotted 60 points"))
    val traces = drawn.data.asInstanceOf[js.Array[js.Dynamic]]
    assert(traces.length == 2)
    assert(drawn.layout.xaxis.`type`.asInstanceOf[String] == "log")
    assert(drawn.layout.xaxis2.matches.asInstanceOf[String] == "x")

    // The phase must end near -270 rather than wrapping up to +90, which is the whole of
    // F_0004: the jump is an artefact of atan2's branch cut, not of the plant.
    val phase = traces(1).y.asInstanceOf[js.Array[Double]].toVector
    assert(phase.last < -260 && phase.last > -271, s"phase ended at ${phase.last}")

    // A geometric grid: the ratio between neighbours is constant, so a linear one fails here.
    val w = traces(0).x.asInstanceOf[js.Array[Double]].toVector
    assert(math.abs(w(1) / w(0) - w(2) / w(1)) < 1e-9)
  }

  "nyquist" should "draw the same sweep in the plane, axes locked" in
  {
    typeLine("nyquist 1/(s+1) s 0.01 100 40")
    assert(lastOutput().contains("plotted 40 points"))
    assert(drawn.layout.yaxis.scaleanchor.asInstanceOf[String] == "x")
  }

  "latex on" should "typeset a result BESIDE its text, not instead of it" in
  {
    typeLine("latex on")
    assert(lastOutput() == "latex = on", "the toggle itself must stay readable as text")
    typeLine("1/x + 1")
    assert(typeset.last == "\\frac{1.0}{x} + 1.0")
    assert(lastMath().contains("\\frac"))
    // F_0031: the formula used to REPLACE the text, which made `latex on` swallow whatever
    // `pretty` did and left that setting looking broken. The text is the canonical grammar
    // form -- what `:save` writes and what a reader has to retype -- so it stays.
    assert(lastOutput() == "((1.0 / x) + 1.0)")
  }

  it should "move the pretty control, since the session turns that setting off" in
  {
    // The exclusion lives in `Session`, so the panel follows it like any other command the
    // user could have typed -- which is what stops a control reporting a setting that is no
    // longer in force.
    val pretty = field("set-pretty").getOrElse(fail("no pretty control"))
    typeLine("pretty on")
    assert(pretty.checked.asInstanceOf[Boolean])
    typeLine("latex on")
    assert(!pretty.checked.asInstanceOf[Boolean], "latex on must clear the pretty control")
  }

  it should "restore the text when switched off" in
  {
    typeLine("latex off")
    typeLine("1/x + 1")
    assert(lastOutput() == "((1.0 / x) + 1.0)")
  }

  "the settings panel" should "be built from the session, and offer no dead control" in
  {
    assert(controlCount > 0)
    // `colors` is reported by the session and deliberately not offered: the page has no
    // syntax highlighting, so the control would change nothing visible.
    assert(field("set-colors").isEmpty)
  }

  it should "issue the command a user would have typed, and follow one that was" in
  {
    field("set-latex") match
      case None => fail("no latex control")
      case Some(box) =>
        assert(!box.checked.asInstanceOf[Boolean])
        box.checked = true
        change(box)
        assert(lastOutput() == "latex = on", "a control issues the command")
        typeLine("latex off")
        assert(!box.checked.asInstanceOf[Boolean], "a typed command moves the control")
  }

  it should "carry a number and a choice, and snap a refused value back" in
  {
    val digits = field("set-precision").getOrElse(fail("no precision control"))
    digits.value = "9"; change(digits)
    assert(lastOutput() == "precision = 9")
    typeLine("precision 5")
    assert(digits.value.asInstanceOf[String] == "5")

    val tnorm = field("set-logic").getOrElse(fail("no t-norm control"))
    tnorm.value = "lukasiewicz"; change(tnorm)
    assert(lastOutput().contains("lukasiewicz"))

    // A refused value must not leave the field showing a state the session is not in.
    digits.value = "99"; change(digits)
    assert(lastOutput().contains("precision expects"))
    assert(digits.value.asInstanceOf[String] == "5")
  }

  "a failure" should "read as a message rather than as a dead page" in
  {
    typeLine("plot");             assert(lastOutput().startsWith("usage: plot"))
    typeLine("bode");             assert(lastOutput().startsWith("usage: bode"))
    typeLine("bode 1/s s 0 10");  assert(lastOutput().contains("logarithmic"))
    typeLine("plot x x 2 1");     assert(lastOutput().startsWith("plot:"))
    typeLine("sin(");             assert(lastOutput().contains("parse error"))
    typeLine("a * 6");            assert(lastOutput() == "42.0", "and the session survives")
  }

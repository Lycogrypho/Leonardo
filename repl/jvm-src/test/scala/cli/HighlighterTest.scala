package it.grypho.scala.leonardo
package cli

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.BeforeAndAfter


/** Prompt syntax highlighting -- JVM ONLY (issue F_0003 phase 2).
 *
 *  Split out of `ReplSessionTest` when `repl` became a cross-build: `LeonardoHighlighter`
 *  implements JLine's `Highlighter` over `AttributedStyle`, neither of which exists off the
 *  JVM.  The scheme *names* it accepts are shared (`ColorSchemes`), which is what lets the
 *  `colors` command itself stay platform-neutral -- only the styling is terminal-bound.
 */
class HighlighterTest extends AnyFlatSpec with BeforeAndAfter:

  private var session: Session = _

  before { session = new Session() }

  "LeonardoHighlighter with none scheme" should "preserve buffer content unchanged" in
  {
    val h = LeonardoHighlighter(() => "none")
    val inputs = List(
      "sin(pi) + 3.14",
      "simplify x + 0",
      "derive(f, x)",
      "x := 2 * pi",
      "solve(x^2 = 4, x)",
      "e + i + pi"
    )
    for input <- inputs do
      assert(h.highlightBuffer(input).toString == input,
        s"none scheme must not alter content for: $input")
  }

  "LeonardoHighlighter with dark scheme" should "preserve buffer content unchanged" in
  {
    val h = LeonardoHighlighter(() => "dark")
    val inputs = List("simplify sin(x) + 0", "x := 3", "1.5e-3 + 2", "pi * e")
    for input <- inputs do
      assert(h.highlightBuffer(input).toString == input,
        s"dark scheme must not alter content for: $input")
  }

  "LeonardoHighlighter with light scheme" should "preserve buffer content unchanged" in
  {
    val h = LeonardoHighlighter(() => "light")
    val inputs = List("expand (x + 1)^2", "integral(x^2, x)", "cos(pi) + i")
    for input <- inputs do
      assert(h.highlightBuffer(input).toString == input,
        s"light scheme must not alter content for: $input")
  }

  "LeonardoHighlighter" should "colour 'ode' as a function like the other functionals" in
  {
    val h        = LeonardoHighlighter(() => "dark")
    val odeStyle = h.highlightBuffer("ode(y, y, t, 0, 1, 1)").styleAt(0)
    val sinStyle = h.highlightBuffer("sin(x)").styleAt(0)
    val varStyle = h.highlightBuffer("abc").styleAt(0)
    assert(odeStyle == sinStyle, "'ode' should share the function colour")
    assert(odeStyle != varStyle, "'ode' should not be coloured as a plain variable")
    // and content must still be preserved
    assert(h.highlightBuffer("ode(y, y, t, 0, 1, 1)").toString == "ode(y, y, t, 0, 1, 1)")
  }

  "the highlighter" should "accept logic keywords without error" in
  {
    val h = LeonardoHighlighter(() => "dark")
    assert(h.highlightBuffer("truth a and not b or true").toString == "truth a and not b or true")
  }

  "the highlighter" should "accept the unknown literal without error" in
  {
    val h = LeonardoHighlighter(() => "dark")
    assert(h.highlightBuffer("truth3 a and unknown").toString == "truth3 a and unknown")
  }

  "the highlighter" should "accept the fuzzy vocabulary without error" in
  {
    val h = LeonardoHighlighter(() => "dark")
    val line = "defuzz(very(trimf(x, 0, 5, 10)), x, 0, 10)"
    assert(h.highlightBuffer(line).toString == line)
  }

  "the highlighter" should "accept the Greek glyphs without error" in
  {
    val h = LeonardoHighlighter(() => "dark")
    val line = "Γ(5) + β(1, 2)"
    assert(h.highlightBuffer(line).toString == line)
  }


package it.grypho.scala.leonardo
package cli

import org.scalatest.flatspec.AnyFlatSpec


/** Issue 3.3 slice F — the domain diagnostic appended to a symbolic fallback.
 *
 *  The note is **rendering only**: it is appended to what the REPL prints and is never
 *  folded into `eval`, so every library result stays byte-identical.  The tests below pin
 *  both halves of that — the note appears when it should, and nothing else grows one.
 */
class DomainNoteTest extends AnyFlatSpec:

  private def session: Session = new Session()

  "a limit outside the domain" should "explain itself instead of echoing back silently" in
  {
    val out = session.execute("limit(ln(x), x, -1)")
    assert(out.contains("note:"), s"expected a diagnostic, got: $out")
    assert(out.contains("undefined at x = -1"), s"expected the offending point, got: $out")
    assert(out.contains("> 0"), s"expected the violated requirement, got: $out")
  }

  "a limit inside the domain" should "carry no note" in
  {
    val out = session.execute("limit(ln(x), x, 1)")
    assert(!out.contains("note:"), s"no diagnostic expected, got: $out")
  }

  "asin outside its interval" should "name the closed unit requirement" in
  {
    val out = session.execute("limit(asin(x), x, 3)")
    assert(out.contains("note:"), s"expected a diagnostic, got: $out")
    assert(out.contains("[-1, 1]"), s"expected the interval requirement, got: $out")
  }

  "a definite integral crossing a pole" should "explain the bound" in
  {
    val out = session.execute("integral(1 / x, x, 0, 1)")
    assert(out.contains("note:"), s"expected a diagnostic, got: $out")
    assert(out.contains("!= 0"), s"expected the non-zero requirement, got: $out")
  }

  // ── the note must not leak into ordinary results ───────────────────────────

  "ordinary evaluation" should "be untouched" in
  {
    val s = session
    assert(!s.execute("2 + 3").contains("note:"))
    assert(!s.execute("sin(0)").contains("note:"))
    assert(!s.execute("derive(x^2, x)").contains("note:"))
  }

  "a symbolic result with no domain problem" should "stay quiet" in
  {
    // Gives up for want of a rule, not for a domain reason -- so there is nothing to say.
    val out = session.execute("integral(exp(x^2), x)")
    assert(!out.contains("note:"), s"no diagnostic expected, got: $out")
  }

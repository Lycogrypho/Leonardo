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

  // F_0078: nothing deeper than MaxExpressionDepth reaches the walkers -- and, the decisive part,
  // an expression AT the limit simplifies on the shallowest platform: this suite runs on Node.
  "the depth limit" should "hold on every platform, and explain itself where it refuses" in
  {
    val s = session
    // Fresh names, so no memoised walk shortens the recursion and flatters the measurement.
    // Relative to the limit, so raising the constant past what Node's stack can walk fails here
    // (checked by mutation: at 350 this test fails on Node with a RangeError).
    val n    = core.MaxExpressionDepth - 10
    val near = (1 to n).map(i => s"q$i").mkString(" + ")
    val ok   = s.execute(s"simplify $near")
    assert(!ok.contains("rror"), s"a $n-term sum must simplify on this platform: ${ok.take(120)}")
    val viaSum = s.execute(s"simplify sum(k*u, k, 1, $n)")
    assert(!viaSum.contains("rror"), viaSum.take(120))
    val over = s.execute("sum(k*u, k, 1, 300)")
    assert(over.contains("note: sum declined: a symbolic sum of 300 terms would be deeper"), over)
    val typed = s.execute((1 to 300).map(i => s"r$i").mkString(" + "))
    assert(typed.contains(s"deeper than ${core.MaxExpressionDepth} levels"), typed.take(200))
    // Definitions compose after parsing: each line is shallow, the substituted tree is not.
    s.execute("part := " + (1 to 150).map(i => s"p$i").mkString(" + "))
    val composed = s.execute("simplify part + " + (1 to 150).map(i => s"o$i").mkString(" + "))
    assert(composed.contains("deeper than") && composed.contains("once its definitions are substituted"), composed.take(200))
  }

  // F_0077: a series whose derivatives outgrow the budget declines and says why.
  "a series past the derivative budget" should "decline with a note instead of hanging" in
  {
    val s = session
    // erf has no power-series recurrence (F_0085), so this one takes the derivative path.
    val over = s.execute("taylor(erf(sin(x)), x, 0, 20)")
    assert(over.contains("note: taylor declined: the derivative of order"), over)
    assert(s.lastLatex.isEmpty, "a note is prose the formula does not carry")
    assert(s.execute("pade(1 + x^2, x, 1, 1)").contains("note: pade declined: no [1/1] approximant exists"))
    assert(!s.execute("taylor(exp(x), x, 0, 4)").contains("note:"))
  }

  // F_0076: an expansion past its budget stays as written and says why.
  "an expansion past the term budget" should "be left as written with a note" in
  {
    val s = session
    val over = s.execute("expand (a + b + c + d)^20")
    assert(over.contains("note: expand declined: the expansion has"), over)
    assert(s.lastLatex.isEmpty, "a note is prose the formula does not carry")
    s.execute("latex on")
    val fine = s.execute("expand (a + b)^2")
    assert(!fine.contains("note:"), fine)
    assert(s.lastLatex.nonEmpty, "an expansion without a note still offers its formula")
  }

  // F_0079: a reduction or tabulation that declines on its range explains itself.
  "a sum past the term cap" should "say which cap, instead of echoing back silently" in
  {
    val s = session
    val over = s.execute("sum(k, k, 1, 1001)")
    assert(over.contains("note: sum declined: 1001 terms exceed the limit of 1000"), over)
    assert(s.lastLatex.isEmpty, "a note is prose the formula does not carry")
    assert(s.execute("product(k, k, 0.5, 3)").contains("note: product declined: the bounds must be whole numbers"))
    assert(s.execute("tabulate(k, k, 1, 2000)").contains("note: tabulate declined: 2000 terms"))
  }

  it should "stay quiet when the range is not the reason" in
  {
    val s = session
    assert(!s.execute("sum(k, k, 1, 1000)").contains("note:"), "at the cap it answers")
    assert(!s.execute("sum(k, k, 1, n)").contains("note:"), "a symbolic bound is an answer")
    assert(!s.execute("tabulate(k, k, 1, 3)").contains("note:"))
  }

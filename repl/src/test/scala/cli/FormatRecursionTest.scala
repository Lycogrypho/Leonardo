package it.grypho.scala.leonardo
package cli

import org.scalatest.flatspec.AnyFlatSpec

/** Issue F_0035 — the session precision and `pretty` must reach a COMPOSITE result.
 *
 *  `formatExpression` used to handle the leaf values and the two matrix carriers and then fall
 *  through to `other.toString`, which is hard-wired to `Environment.DefaultPrecision` and knows
 *  nothing of `pretty`.  So `1/3` honoured the session precision and `1/3 + cos(x)` did not —
 *  display precision worked only on results simple enough not to need it.
 *
 *  The fix **rebuilds** the tree with each state-dependent leaf replaced by a display-only node
 *  and then calls `toString` on it, so every composite's own printed form is reused rather than
 *  re-implemented.  These cases therefore come in two halves: what must now change, and what
 *  must not have moved at all.
 */
class FormatRecursionTest extends AnyFlatSpec:

  private def session: Session =
    val s = new Session()
    s.execute("precision 2")
    s

  /** The second row of a stacked matrix must sit under the first row's opening bracket.
   *
   *  Stated as an invariant rather than as an expected string because it is the same claim at
   *  column 0 and nested inside a larger expression — which is the whole point of the
   *  alignment pass.
   */
  private def assertAligned(out: String): Unit =
    val lines = out.linesIterator.toVector
    assert(lines.sizeIs >= 2, s"expected a stacked matrix, got:\n$out")
    val firstRow = lines(0).indexOf('[', lines(0).indexOf('[') + 1)
    assert(lines(1).indexOf('[') == firstRow,
      s"row 2 must align under row 1's bracket (column $firstRow):\n$out")

  // --- what must now change -------------------------------------------------------------

  "the session precision" should "reach a scalar composite" in
  {
    val out = session.execute("1/3 + cos(x)")
    assert(out.contains("0.33"), out)
    assert(!out.contains("0.33333"), s"toString's fixed precision leaked through: $out")
  }

  it should "reach inside a function argument" in
  {
    val out = session.execute("sin(1/3 + x)")
    assert(out.contains("0.33") && !out.contains("0.33333"), out)
  }

  it should "reach inside a functional's binder-carrying node" in
  {
    val out = session.execute("integral(1/3 + sin(x), x)")
    assert(out.contains("0.33") && !out.contains("0.33333"), out)
  }

  it should "reach a matrix operation, which is neither a leaf nor a matrix" in
  {
    // The case reported on 2026-09-23: `MatScale` is a matrix OPERATION, so it fell through.
    val out = session.execute("[[1/3, 2], [3, 4]] * cos(x)")
    assert(out.contains("0.33") && !out.contains("0.33333"), out)
  }

  it should "reach both sides of a relation" in
  {
    val out = session.execute("1/3 + x = 2/3")
    assert(out.contains("0.33") && out.contains("0.67"), out)
    assert(!out.contains("0.33333"), out)
  }

  "pretty" should "stack a matrix nested inside a larger expression, aligned" in
  {
    val s = session
    s.execute("pretty on")
    val out = s.execute("[[12, sin(x)], [exp(x), exp(-x)]] * cos(x)")
    assertAligned(out)
    assert(out.contains("cos(x)"), s"the rest of the expression must survive:\n$out")
  }

  it should "align a matrix that is the whole result, exactly as before" in
  {
    val s = session
    s.execute("pretty on")
    assertAligned(s.execute("[[1, 2], [3, 4]]"))
  }

  // --- what must NOT have moved ----------------------------------------------------------

  "a bare leaf" should "render exactly as it did" in
  {
    assert(session.execute("1/3") == "0.33")
  }

  "a matrix of matrices" should "stay single-line under pretty on" in
  {
    // The existing rule: only the OUTERMOST matrix stacks, or the grid breaks.
    val s = session
    s.execute("pretty on")
    val out = s.execute("lu([[4, 3], [6, 3]])")
    assert(!out.contains("\n"), s"a decomposition result must stay single-line:\n$out")
  }

  ":save" should "keep writing full precision, never the displayed text" in
  {
    // `serializeValue` is a separate path on purpose: a value shown rounded must still
    // round-trip, so the script carries what `toString`/`exact` produce.
    val s = session
    s.execute("a := 1/3")
    assert(s.script.contains("0.33333"), s"the script must not be rounded to the display:\n${s.script}")
  }

  "simplify" should "still ignore the session precision" in
  {
    // It renders through `toString` deliberately; that is existing, documented behaviour and
    // this issue must not quietly change it.
    assert(session.execute("simplify 1/3 + cos(x)").contains("0.33333"))
  }

  "the display markers" should "never reach the answer" in
  {
    // The alignment pass brackets a stacked block with control characters; one escaping would
    // corrupt every consumer, `:save` and the browser transcript included.
    // The byte values rather than the private constants, so this pins what must not appear
    // rather than agreeing with whatever the formatter happens to use -- and spelled
    // `n.toChar`, since a character literal would put the control byte into this file.
    // A newline is NOT a marker: a stacked matrix is made of them, which is the point.
    val s = session
    s.execute("pretty on")
    for line <- List("[[1, 2], [3, 4]]", "[[1, 2], [3, 4]] * cos(x)", "1/3 + cos(x)", "lu([[4, 3], [6, 3]])") do
      val out = s.execute(line)
      assert(!out.exists(c => c == 1.toChar || c == 2.toChar),
        s"a marker escaped in '$line': ${out.map(_.toInt).mkString(",")}")
      assert(!out.exists(c => c.isControl && c != '\n'),
        s"an unexpected control character in '$line': ${out.map(_.toInt).mkString(",")}")
  }

package it.grypho.scala.leonardo
package scalar

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** The REPL's `samples` command: point counts, defined-function arguments, the empty-result
 *  and argument-error paths, and the reserved-word guard.
 *
 *  Split out of `SampleTest` by issue 5.2 phase 1.1 — `samples` is a session command, so it
 *  cannot be exercised from the library module. The parser-only half of the same behaviour
 *  stayed behind in `SampleTest`.
 */
class SampleReplTest extends AnyFlatSpec:

  "the samples command" should "return n tab-separated x/y lines" in
  {
    val s   = Session()
    val out = s.execute("samples x*x x 0 1 5")
    val lines = out.linesIterator.toList
    assert(lines.length == 5)
    lines.foreach { line =>
      val cols = line.split("\t")
      assert(cols.length == 2, s"expected two tab-separated columns in: $line")
    }
  }

  "the samples command" should "respect the optional n argument" in
  {
    val s = Session()
    assert(s.execute("samples x x 0 1 10").linesIterator.length == 10)
  }

  "the samples command" should "default to 200 points" in
  {
    val s = Session()
    assert(s.execute("samples x x 0 1").linesIterator.length == 200)
  }

  "the samples command" should "accept a defined function name as the expression" in
  {
    val s = Session()
    s.execute("f := x * x")
    val out = s.execute("samples f x 0 1 3")
    assert(out.linesIterator.length == 3)
  }

  "the samples command" should "report (no finite values) when all points are filtered" in
  {
    val s = Session()
    // log(-x) for x in [1,2] -> complex -> filtered
    val out = s.execute("samples log(0 - x) x 1 2 3")
    assert(out.contains("no finite values"))
  }

  // --- the grid can silently produce fewer values than asked for (F_0044) ----------------

  "the samples command" should "say how many samples had a value, not silently shrink" in
  {
    // A sweep drops every point with no finite value and used to do it without a word, so a
    // shrunken result read as success. The pole at x = 0 costs one of eleven samples.
    // (A whole-indexed expression is REFUSED rather than noted -- see the case below -- so the
    // note is pinned here on the loss that has no better remedy than reporting it.)
    val s   = Session()
    val out = s.execute("samples 1/x x -1 1 11")
    assert(out.contains("10 of 11"), out)
  }

  it should "refuse an integer-indexed expression with the grid that would work" in
  {
    // `sum`'s bound is the sampled variable, so the expression has a value only at whole
    // numbers.  Rather than draw the one or two points the grid happens to hit, say so and name
    // the count that lands on every integer -- 101 over 0..100 (F_0044 step 2).
    val s   = Session()
    val out = s.execute("samples sum(k, k, 1, n) n 0 100")
    assert(out.contains("whole"), out)
    assert(out.contains("0 100 101"), out)
  }

  it should "sample an integer-indexed expression happily once the grid fits" in
  {
    // The recipe has to work, or the refusal is just an obstacle: at one point per integer
    // every sample evaluates, so nothing is lost and nothing is refused.
    val s   = Session()
    val out = s.execute("samples sum(k, k, 1, n) n 0 100 101")
    assert(!out.contains("whole"), out)
    assert(out.linesIterator.length == 101, out)
  }

  it should "not refuse an ordinary expression that merely has a pole" in
  {
    // The refusal is keyed on the STRUCTURE -- a reduction whose bound is the sampled variable
    // -- not on the loss alone, or every pole would be reported as a grid problem.
    val s   = Session()
    val out = s.execute("samples 1/x x -1 1 11")
    assert(!out.contains("whole"), out)
  }

  it should "stay quiet when every sample had a value" in
  {
    // The count is reported only when something was lost -- no arbitrary threshold, and no
    // noise on the ordinary path.
    val s   = Session()
    val out = s.execute("samples x x 0 1 10")
    assert(!out.contains("of 10"), out)
    assert(out.linesIterator.length == 10, out)
  }

  "the samples command with lo >= hi" should "report an error" in
  {
    val s = Session()
    assert(s.execute("samples x x 5 1").contains("lo must be"))
  }

  "the samples command with too few arguments" should "show usage" in
  {
    val s = Session()
    assert(s.execute("samples x x").contains("usage"))
  }

  "the samples command with underscore variable name" should "accept x_1 and sample the expression" in
  {
    val s = Session()
    // x_1 is a valid variable name (grammar extended to [a-zA-Z][a-zA-Z0-9_]*); the command
    // should produce data rows, not a usage error.
    val out = s.execute("samples x_1*x_1 x_1 0 1 5")
    assert(!out.contains("usage"), s"expected data rows, got: $out")
    assert(out.linesIterator.length == 5)
  }

  "assigning to an underscore variable name" should "be accepted by the REPL" in
  {
    val s = Session()
    assert(s.execute("f_1 := 2").contains("f_1"))
    assert(s.execute("f_1") == "2.0")
  }

  "samples := 3" should "be rejected as a reserved word" in
  {
    val s = Session()
    assert(s.execute("samples := 3").contains("reserved word"))
  }

  "help samples" should "return topic-specific help" in
  {
    assert(Session().execute("help samples").contains("samples"))
  }

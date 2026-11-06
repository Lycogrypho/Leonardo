package it.grypho.scala.leonardo
package mcp

import org.scalatest.flatspec.AnyFlatSpec
import scala.concurrent.duration.*

class ToolsTest extends AnyFlatSpec:

  private def worker() = SessionWorker(30.seconds)
  private def call(w: SessionWorker, tool: String, args: (String, String)*): ToolAnswer =
    Tools.call(tool, args.toMap, w)
  /** The answer's first line: `names` is forced on, so a first use of a name adds a note below it. */
  private def first(r: ToolAnswer): String = r.text.linesIterator.nextOption().getOrElse("")

  "evaluate" should "run a line on the session and keep its state" in {
    val w = worker()
    assert(!call(w, "evaluate", "line" -> "a := 3").isError)
    val r = call(w, "evaluate", "line" -> "a^2 + 1")
    assert(first(r) == "10.0" && !r.isError)
  }

  it should "return the LaTeX of the result as a second part, once its names are known" in {
    val w = worker()
    // a first use announces the new names, and an answer carrying a note offers no LaTeX (F_0020)
    assert(call(w, "evaluate", "line" -> "(a + b)/(c + d)").latex.isEmpty)
    val r = call(w, "evaluate", "line" -> "(a + b)/(c + d)")
    assert(r.latex.exists(_.contains("\\frac")), r.toString)
  }

  it should "report an unknown call, since names are forced on" in {
    val r = call(worker(), "evaluate", "line" -> "sqrt(2)")
    assert(r.text.contains("sqrt") && r.text.contains("note"), r.text)
  }

  "the typed tools" should "each assemble the command the grammar defines" in {
    val w = worker()
    assert(first(call(w, "derive", "expression" -> "sin(x)", "variable" -> "x")) == "cos(x)")
    assert(first(call(w, "integrate", "expression" -> "2*x", "variable" -> "x", "lower" -> "0", "upper" -> "3")) == "9.0")
    assert(first(call(w, "limit", "expression" -> "sin(x)/x", "variable" -> "x", "point" -> "0")) == "1.0")
    assert(first(call(w, "simplify", "expression" -> "x + 0")) == "x")
    val solved = call(w, "solve", "equation" -> "2*x + 6 = 0", "variable" -> "x")
    assert(solved.text.contains("-3"), solved.text)
    // the typed solve answers without binding, unlike a bare `solve` at the prompt
    assert(first(call(w, "evaluate", "line" -> "x")) == "x")
  }

  it should "refuse malformed arguments before reaching the session" in {
    val w = worker()
    assert(call(w, "derive", "expression" -> "sin(x)", "variable" -> "2x").isError)
    assert(call(w, "derive", "expression" -> "sin(x)").isError)
    assert(call(w, "evaluate", "line" -> "x := 1\ny := 2").isError)
    assert(call(w, "integrate", "expression" -> "x", "variable" -> "x", "lower" -> "0").isError)
    assert(call(w, "limit", "expression" -> "x", "variable" -> "x", "point" -> "0", "direction" -> "up").isError)
    assert(call(w, "no-such-tool").isError)
  }

  "scala_value" should "return Scala source for the line's result" in {
    val r = call(worker(), "scala_value", "line" -> "derive(x^3, x)")
    assert(!r.isError && r.text.startsWith(ToScala.Import) && r.text.contains("scalar."), r.text)
    assert(call(worker(), "scala_value", "line" -> "precision 6").isError)
  }

  "every tool" should "name its required parameters" in {
    for t <- Tools.all do
      assert(t.params.exists(_.required), t.name)
      assert(t.description.nonEmpty)
  }

  "the session worker" should "answer a timed-out request at once and keep the session's state" in {
    val w = worker()
    assert(!call(w, "evaluate", "line" -> "a := 5").isError)
    val t0 = System.nanoTime()
    val slow = w.run(_.execute("integral(abs(sin(1000*x)), x, 0, 100)"), 50.millis)
    assert(slow.left.exists(_.contains("timed out")), slow.toString)
    assert((System.nanoTime() - t0) / 1e9 < 5.0)
    assert(first(call(w, "evaluate", "line" -> "a")) == "5.0")
  }

  it should "serve the session's script" in {
    val w = worker()
    call(w, "evaluate", "line" -> "r := 2")
    assert(w.run(_.script).exists(_.contains("r := 2")))
  }

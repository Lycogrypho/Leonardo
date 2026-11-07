package it.grypho.scala.leonardo
package mcp

import org.scalatest.flatspec.AnyFlatSpec

import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}
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
    val w  = worker()
    val go = CountDownLatch(1)   // F_0317: a quadrature that a warm JVM declined inside the limit was the "slow" line
    try
      assert(!call(w, "evaluate", "line" -> "a := 5").isError)
      val t0   = System.nanoTime()
      val slow = w.run(_ => blockOn(go), 200.millis)
      assert(slow.left.exists(_.contains("the request was abandoned")), slow.toString)
      assert((System.nanoTime() - t0) / 1e9 < 5.0)
      assert(first(call(w, "evaluate", "line" -> "a")) == "5.0")
    finally go.countDown()
  }

  /** A computation that runs until `go` is released, ignoring interruption as a real computation does
   *  (F_0317: the test decides when it ends, not the clock). */
  private def blockOn(go: CountDownLatch): Unit =
    var released = false
    while !released do
      try { go.await(); released = true }
      catch case _: InterruptedException => ()

  /** Until every abandoned worker of `w` has ended, at most `seconds`. */
  private def drained(w: SessionWorker, seconds: Int = 10): Boolean =
    val deadline = System.nanoTime() + seconds * 1_000_000_000L
    while w.abandonedRunning > 0 && System.nanoTime() < deadline do Thread.sleep(10)
    w.abandonedRunning == 0

  it should "refuse new requests while too many abandoned computations still run (F_0312)" in {
    val w  = SessionWorker(30.seconds, maxAbandoned = 2)
    val go = CountDownLatch(1)
    try
      // half a second, not 50 ms: under a full run's load a stall before the lock exceeded 50 ms, and the
      // "timed out waiting" refusal (F_0316) abandons nothing -- which is why the assertion names the abandonment
      assert(w.run(_ => blockOn(go), 500.millis).left.exists(_.contains("the request was abandoned")))
      assert(w.run(_ => blockOn(go), 500.millis).left.exists(_.contains("the request was abandoned")))
      assert(w.abandonedRunning == 2, s"abandoned still running: ${w.abandonedRunning}")
      val refused = w.run(_.execute("1 + 1"))
      assert(refused.left.exists(_.contains("2 abandoned computations")), refused.toString)
    finally go.countDown()
    assert(drained(w), "the released computations did not end")
    assert(w.run(_.execute("1 + 1")).exists(_.startsWith("2.0")))
  }

  it should "start an empty session when the restore itself times out (F_0312)" in {
    val hold = CountDownLatch(1)
    val w    = SessionWorker(500.millis, onRestore = () => blockOn(hold))
    try
      assert(w.run(_.execute("a := 5")).isRight)
      val out = w.run(_ => blockOn(hold))
      assert(out.left.exists(s => s.contains("timed out as well") && s.contains("starts empty")), out.toString)
      // the empty session was built on the caller's thread, so no restore is pending
      assert(first(call(w, "evaluate", "line" -> "a")) == "a")
    finally hold.countDown()
  }

  it should "name a restore that fails rather than call it a timeout (F_0315)" in {
    val w = SessionWorker(30.seconds, onRestore = () => throw IllegalStateException("boom"))
    assert(w.run(_.execute("a := 5")).isRight)
    val out = w.run(_ => throw RuntimeException("x"))
    assert(out.left.exists(s => s.contains("the request failed (RuntimeException)") &&
                                s.contains("failed as well (IllegalStateException)") &&
                                s.contains("starts empty") && !s.contains("timed out")), out.toString)
    assert(first(call(w, "evaluate", "line" -> "a")) == "a")
  }

  it should "count a request's limit from its arrival, not from its turn at the session (F_0316)" in {
    val w       = SessionWorker(30.seconds)
    val go      = CountDownLatch(1)
    val entered = CountDownLatch(1)
    val pool    = Executors.newSingleThreadExecutor()
    try
      val first = pool.submit(() => w.run(_ => { entered.countDown(); blockOn(go) }, 1.second))
      assert(entered.await(10, TimeUnit.SECONDS), "the first request never reached the session")
      // queued behind the first, which holds the session for its second and then its restore
      val queued = w.run(_.execute("1 + 1"), 300.millis)
      assert(queued.left.exists(s => s.contains("waiting for the session") && s.contains("nothing was started")), queued.toString)
      assert(first.get(20, TimeUnit.SECONDS).left.exists(_.contains("the request was abandoned")))
      assert(w.abandonedRunning == 1, "the queued request must not have been abandoned")
    finally
      go.countDown()
      pool.shutdownNow()
  }

  it should "serve the session's script" in {
    val w = worker()
    call(w, "evaluate", "line" -> "r := 2")
    assert(w.run(_.script).exists(_.contains("r := 2")))
  }

package it.grypho.scala.leonardo

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec


class ReplSessionTest extends AnyFlatSpec:

  def session: Session = new Session()

  "a constant assignment" should "bind a numeric value" in
  {
    val s = session
    assert(s.execute("x = 3.001") == "x = 3.001")
    assert(s.execute("x") == "3.001")
  }

  "a constant expression assignment" should "fold and bind the result" in
  {
    val s = session
    s.execute("y = 2 * 3")
    assert(s.execute("y") == "6.0")
  }

  "an assignment with free variables" should "become a definition" in
  {
    val s = session
    assert(s.execute("f = sin(x) + x") == "f = (sin(x) + x)")
    // stays symbolic while x is unbound
    assert(s.execute("f") == "(sin(x) + x)")
  }

  "a definition" should "evaluate numerically once its variable is bound" in
  {
    val s = session
    s.execute("f = sin(x) + x")
    s.execute("x = 0")
    assert(s.execute("f") == "0.0")
  }

  "definitions" should "be late-bound through other definitions" in
  {
    val s = session
    s.execute("f = x + 1")
    s.execute("g = f * 2")
    s.execute("x = 4")
    assert(s.execute("g") == "10.0")
    // redefining f changes g too
    s.execute("f = x + 2")
    assert(s.execute("g") == "12.0")
  }

  "a self-referential definition" should "not loop forever" in
  {
    val s = session
    s.execute("f = f + 1")
    // inner f stays a free variable; result is symbolic, not a hang
    assert(s.execute("f") == "(f + 1.0)")
  }

  // --- issue 1.1: reserved constants cannot be assigned ---

  "assigning to e" should "be rejected, keeping e the built-in constant" in
  {
    val s = session
    assert(s.execute("e = 5") == "cannot assign to 'e': it is a built-in constant")
    assert(s.execute("e") == "2.71828")           // still Euler's number
    assert(!s.execute("env").contains("e = "))    // no binding was stored
  }

  "assigning to pi" should "be rejected, keeping pi the built-in constant" in
  {
    val s = session
    assert(s.execute("pi = 3") == "cannot assign to 'pi': it is a built-in constant")
    assert(s.execute("pi") == "3.14159")
  }

  "a loaded script containing a reserved-name assignment" should "surface the warning" in
  {
    val s = session
    val out = s.load("x = 2\npi = 3\nx")
    assert(out.contains("cannot assign to 'pi'"))
    assert(out.contains("2.0"))                   // rest of the script still ran
  }

  "names that merely start with a reserved name" should "still be assignable" in
  {
    val s = session
    assert(s.execute("eps = 0.001") == "eps = 0.001")
    assert(s.execute("pivot = 2") == "pivot = 2.0")
  }

  // --- issue 1.1: derivative with respect to a defined function ---

  "derive(g, f) with f and g defined over x" should "apply the chain rule, not return 0" in
  {
    val s = session
    s.execute("f = sin(x)")
    s.execute("g = f^2")
    s.execute("x = 0.5")
    // dg/df = 2f = 2*sin(0.5) ≈ 0.95885 — before the fix this was 0.0
    assert(s.execute("derive(g, f)") == "0.95885")
  }

  "derive(g, f) with x unbound" should "stay symbolic instead of answering 0" in
  {
    val s = session
    s.execute("f = sin(x)")
    s.execute("g = f^2")
    val out = s.execute("derive(g, f)")
    assert(out != "0.0", "chain-rule derivative must not collapse to 0")
    assert(out.contains("sin(x)"))   // symbolic quotient over the shared variable
  }

  "derive with respect to a multi-variable definition" should "be rejected with a message" in
  {
    val s = session
    s.execute("h = x + y")
    val out = s.execute("derive(h, h)")
    assert(out.contains("cannot derive with respect to 'h'"))
    assert(out.contains("(x, y)"))
  }

  "call syntax on a defined name" should "fail with a hint about the bare name" in
  {
    val s = session
    s.execute("f = sin(x)")
    s.execute("g = f^2")
    val out = s.execute("derive(g(x), f(x))")
    assert(out.startsWith("parse error"))
    assert(out.contains("function-call syntax") && out.contains("bare name"))
  }

  "derive with respect to a plain variable" should "be unaffected by the binder rewrite" in
  {
    val s = session
    s.execute("g = x^2")
    s.execute("x = 3")
    assert(s.execute("derive(g, x)") == "6.0")
  }

  "reassigning a definition with a constant" should "turn it into a binding" in
  {
    val s = session
    s.execute("f = x + 1")
    s.execute("f = 5")
    assert(s.execute("f") == "5.0")
  }

  "derive through the expression path" should "differentiate a definition" in
  {
    val s = session
    s.execute("f = x * x")
    s.execute("x = 3")
    assert(s.execute("derive(f, x)") == "6.0")
  }

  "simplify" should "simplify without numeric evaluation" in
  {
    val s = session
    s.execute("x = 3")          // binding must NOT leak into simplify
    assert(s.execute("simplify x + 0") == "x")
  }

  "expand" should "distribute products over sums" in
  {
    val s = session
    assert(s.execute("expand x * (y + z)") == "((x * y) + (x * z))")
  }

  "precision" should "affect evaluation" in
  {
    val s = session
    s.execute("precision 2")
    s.execute("x = 3.14159")
    assert(s.execute("x") == "3.14")
  }

  "precision with a bad argument" should "report an error" in
  {
    val s = session
    assert(s.execute("precision abc").startsWith("precision expects"))
  }

  "unset" should "remove a binding" in
  {
    val s = session
    s.execute("x = 3")
    assert(s.execute("unset x") == "x unset")
    assert(s.execute("x") == "x")
    assert(s.execute("unset x") == "x is not set")
  }

  "env" should "list precision, bindings, and definitions" in
  {
    val s = session
    s.execute("x = 3")
    s.execute("f = x + 1")
    val state = s.execute("env")
    assert(state.contains("precision = 5"))
    assert(state.contains("x = 3.0"))
    assert(state.contains("f = (x + 1.0)"))
  }

  "an unparsable line" should "report a parse error" in
  {
    val s = session
    assert(s.execute("sin(").startsWith("parse error"))
  }

  "a blank line" should "produce no output" in
  {
    assert(session.execute("   ") == "")
  }

  "help" should "list the commands" in
  {
    assert(session.execute("help").contains("simplify"))
  }

  // --- session scripts: :save (script) / :load (load) ---

  "script" should "serialize precision, bindings, and definitions as replayable commands" in
  {
    val s = session
    s.execute("precision 4")
    s.execute("x = 3")
    s.execute("f = x + 1")
    val script = s.script
    assert(script.contains("precision 4"))
    assert(script.contains("x = 3.0"))
    assert(script.contains("f = (x + 1.0)"))
  }

  "a saved script" should "reconstruct an equivalent session when loaded" in
  {
    val original = session
    original.execute("precision 3")
    original.execute("a = 2")
    original.execute("g = a * x")
    val script = original.script

    val restored = session
    restored.load(script)
    // same precision effect, same binding, same late-bound definition
    assert(restored.execute("a") == "2.0")
    restored.execute("x = 5")
    assert(restored.execute("g") == "10.0")
    assert(restored.script.contains("precision 3"))
  }

  "load" should "skip blank lines and # comments" in
  {
    val s = session
    val out = s.load(
      """# a comment
        |x = 2
        |
        |x + 1""".stripMargin)
    assert(s.execute("x") == "2.0")
    assert(out.contains("3.0"))    // x + 1 evaluated
  }

  "load" should "run a multi-line script and return the last non-empty output" in
  {
    val s = session
    val out = s.load("x = 10\nx * 2")
    assert(out.linesIterator.toList.last == "20.0")
  }

  ":load at the execute level" should "report it is interactive-only" in
  {
    assert(session.execute(":load foo.txt").contains("interactive"))
  }

  ":save at the execute level" should "report it is interactive-only" in
  {
    assert(session.execute(":save foo.txt").contains("interactive"))
  }

  "saveFile then loadFile" should "round-trip session state through a real file" in
  {
    val tmp = java.io.File.createTempFile("leonardo-session", ".txt")
    tmp.deleteOnExit()
    try
      val original = session
      original.execute("precision 6")
      original.execute("k = 7")
      original.execute("h = k + x")
      assert(Session.saveFile(original, tmp.getPath) == s"saved to ${tmp.getPath}")

      val restored = session
      val out = Session.loadFile(restored, tmp.getPath)
      assert(!out.startsWith("could not read"), out)
      assert(restored.execute("k") == "7.0")
      restored.execute("x = 1")
      assert(restored.execute("h") == "8.0")
    finally tmp.delete()
  }

  "loadFile on a missing file" should "report a read error" in
  {
    assert(Session.loadFile(session, "no-such-file-xyz.txt").startsWith("could not read"))
  }

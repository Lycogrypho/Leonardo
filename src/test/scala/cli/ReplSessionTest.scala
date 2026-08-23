package it.grypho.scala.leonardo
package cli

import cli.{Session, LeonardoHighlighter}
import org.scalatest.flatspec.AnyFlatSpec
import core.Environment

class ReplSessionTest extends AnyFlatSpec:

  def session: Session = new Session()
  def env: Environment = new Environment()

  "a constant assignment" should "bind a numeric value" in
  {
    val s = session
    assert(s.execute("x := 3.001") == "x := 3.001")
    assert(s.execute("x") == "3.001")
  }

  "a constant expression assignment" should "fold and bind the result" in
  {
    val s = session
    s.execute("y := 2 * 3")
    assert(s.execute("y") == "6.0")
  }

  "an assignment with free variables" should "become a definition" in
  {
    val s = session
    assert(s.execute("f := sin(x) + x") == "f := (sin(x) + x)")
    // stays symbolic while x is unbound
    assert(s.execute("f") == "(sin(x) + x)")
  }

  "a definition" should "evaluate numerically once its variable is bound" in
  {
    val s = session
    s.execute("f := sin(x) + x")
    s.execute("x := 0")
    assert(s.execute("f") == "0.0")
  }

  "definitions" should "be late-bound through other definitions" in
  {
    val s = session
    s.execute("f := x + 1")
    s.execute("g := f * 2")
    s.execute("x := 4")
    assert(s.execute("g") == "10.0")
    // redefining f changes g too
    s.execute("f := x + 2")
    assert(s.execute("g") == "12.0")
  }

  "a self-referential definition" should "not loop forever" in
  {
    val s = session
    s.execute("f := f + 1")
    // inner f stays a free variable; result is symbolic, not a hang
    assert(s.execute("f") == "(f + 1.0)")
  }

  // --- issue 4.3: assignment is ":="; bare "=" is an equation ---

  "\"x = 2 * x + 1\"" should "be an equation, never an assignment" in
  {
    val s = session
    // unbound: echoes the symbolic equation, binds nothing
    assert(s.execute("x = 2 * x + 1") == "x = ((2.0 * x) + 1.0)")
    assert(s.execute("x") == "x")
    // x = 2x + 1 holds at x = -1
    s.execute("x := -1")
    assert(s.execute("x = 2 * x + 1") == "true")
    s.execute("x := 0")
    assert(s.execute("x = 2 * x + 1") == "false")
  }

  "an old-style \"=\" script line" should "not create a binding (no backward compatibility)" in
  {
    val s = session
    s.load("x = 2")               // equation echo, not an assignment
    assert(s.execute("x") == "x") // x stayed unbound
  }

  // --- issue 1.1: reserved constants cannot be assigned ---

  "assigning to e" should "be rejected, keeping e the built-in constant" in
  {
    val s = session
    assert(s.execute("e := 5") == "cannot assign to 'e': it is a built-in constant")
    assert(s.execute("e") == "2.71828")           // still Euler's number
    assert(!s.execute("env").contains("e := "))   // no binding was stored
  }

  "assigning to pi" should "be rejected, keeping pi the built-in constant" in
  {
    val s = session
    assert(s.execute("pi := 3") == "cannot assign to 'pi': it is a built-in constant")
    assert(s.execute("pi") == "3.14159")
  }

  "a loaded script containing a reserved-name assignment" should "surface the warning" in
  {
    val s = session
    val out = s.load("x := 2\npi := 3\nx")
    assert(out.contains("cannot assign to 'pi'"))
    assert(out.contains("2.0"))                   // rest of the script still ran
  }

  "names that merely start with a reserved name" should "still be assignable" in
  {
    val s = session
    assert(s.execute("eps := 0.001") == "eps := 0.001")
    assert(s.execute("pivot := 2") == "pivot := 2.0")
  }

  "assigning to a reserved word" should "be rejected before the command vocabulary is shadowed" in
  {
    val s = session
    for name <- List("simplify", "eval", "env", "sin", "derive", "precision", "quit",
                     "colors", "pretty") do
      assert(s.execute(s"$name := 3") == s"cannot assign to '$name': it is a reserved word",
        s"'$name' must be rejected as an assignment target")
    // commands still work afterwards
    assert(s.execute("simplify x + 0") == "x")
    assert(s.execute("env").contains("precision = 5"))
  }

  // --- issue 4.1: matrices in the REPL ---

  "a matrix literal assignment" should "bind a dense matrix value" in
  {
    val s = session
    assert(s.execute("M := [[1, 2], [3, 4]]") == "M := [[1.0, 2.0], [3.0, 4.0]]")
    assert(s.execute("M") == "[[1.0, 2.0], [3.0, 4.0]]")
  }

  "matrix arithmetic through the ordinary operators" should "evaluate on bound matrices" in
  {
    val s = session
    s.execute("M := [[1, 2], [3, 4]]")
    assert(s.execute("M + M") == "[[2.0, 4.0], [6.0, 8.0]]")
    assert(s.execute("2 * M") == "[[2.0, 4.0], [6.0, 8.0]]")
    assert(s.execute("M * M") == "[[7.0, 10.0], [15.0, 22.0]]")
    assert(s.execute("transpose(M)") == "[[1.0, 3.0], [2.0, 4.0]]")
  }

  "a session script with a matrix binding" should "replay to the same state" in
  {
    val s = session
    s.execute("M := [[1, 2], [3, 4]]")
    val replayed = new Session()
    replayed.load(s.script)
    assert(replayed.execute("M") == "[[1.0, 2.0], [3.0, 4.0]]")
    assert(replayed.execute("M + M") == "[[2.0, 4.0], [6.0, 8.0]]")
  }

  "a matrix with free variables" should "become a definition and bind late" in
  {
    val s = session
    s.execute("A := [[x, 2]]")
    s.execute("x := 5")
    assert(s.execute("A") == "[[5.0, 2.0]]")
  }

  // --- simplify/expand execute matrix algebra ---

  "simplify of a matrix-product definition" should "execute the multiplication" in
  {
    val s = session
    s.execute("A := [[1, 2], [3, 4]]")
    s.execute("B := [[5, 6], [7, 8]]")
    s.execute("C := A * B")
    assert(s.execute("simplify C") == "[[19.0, 22.0], [43.0, 50.0]]")
  }

  "simplify of a symbolic matrix product" should "multiply and simplify each element" in
  {
    val s = session
    s.execute("A := [[x + 0]]")         // free x â†’ definition
    s.execute("B := [[3]]")             // constant â†’ dense binding
    s.execute("C := A * B")
    // element before simplification: (x + 0) * 3 â†’ simplified: x * 3
    assert(s.execute("simplify C") == "[[(x * 3.0)]]")
  }

  "expand of a symbolic matrix product" should "distribute inside the elements" in
  {
    val s = session
    s.execute("A := [[x]]")
    s.execute("B := [[x + 1]]")
    s.execute("C := A * B")
    assert(s.execute("expand C") == "[[((x * x) + (x * 1.0))]]")
  }

  "simplify of a transpose definition" should "execute the transposition" in
  {
    val s = session
    s.execute("A := [[1, 2], [3, 4]]")
    s.execute("T := transpose(A)")
    assert(s.execute("simplify T") == "[[1.0, 3.0], [2.0, 4.0]]")
  }

  "simplify of a matrix sum and scale" should "execute through the ordinary operators" in
  {
    val s = session
    s.execute("A := [[1, 2]]")
    s.execute("S := A + A")
    s.execute("D := 3 * A")
    assert(s.execute("simplify S") == "[[2.0, 4.0]]")
    assert(s.execute("simplify D") == "[[3.0, 6.0]]")
  }

  "scalar simplify" should "still ignore numeric bindings" in
  {
    val s = session
    s.execute("x := 3")
    assert(s.execute("simplify x + 0") == "x")
  }

  // --- issue 1.1: derivative with respect to a defined function ---

  "derive(g, f) with f and g defined over x" should "apply the chain rule, not return 0" in
  {
    val s = session
    s.execute("f := sin(x)")
    s.execute("g := f^2")
    s.execute("x := 0.5")
    // dg/df = 2f = 2*sin(0.5) â‰ˆ 0.95885 â€" before the fix this was 0.0
    assert(s.execute("derive(g, f)") == "0.95885")
  }

  "derive(g, f) with x unbound" should "stay symbolic instead of answering 0" in
  {
    val s = session
    s.execute("f := sin(x)")
    s.execute("g := f^2")
    val out = s.execute("derive(g, f)")
    assert(out != "0.0", "chain-rule derivative must not collapse to 0")
    assert(out.contains("sin(x)"))   // symbolic quotient over the shared variable
  }

  "derive with respect to a multi-variable definition" should "be rejected with a message" in
  {
    val s = session
    s.execute("h := x + y")
    val out = s.execute("derive(h, h)")
    assert(out.contains("cannot derive with respect to 'h'"))
    assert(out.contains("(x, y)"))
  }

  "call syntax on a defined name" should "fail with a hint about the bare name" in
  {
    val s = session
    s.execute("f := sin(x)")
    s.execute("g := f^2")
    val out = s.execute("derive(g(x), f(x))")
    assert(out.startsWith("parse error"))
    assert(out.contains("function-call syntax") && out.contains("bare name"))
  }

  "derive with respect to a plain variable" should "be unaffected by the binder rewrite" in
  {
    val s = session
    s.execute("g := x^2")
    s.execute("x := 3")
    assert(s.execute("derive(g, x)") == "6.0")
  }

  "reassigning a definition with a constant" should "turn it into a binding" in
  {
    val s = session
    s.execute("f := x + 1")
    s.execute("f := 5")
    assert(s.execute("f") == "5.0")
  }

  "derive through the expression path" should "differentiate a definition" in
  {
    val s = session
    s.execute("f := x * x")
    s.execute("x := 3")
    assert(s.execute("derive(f, x)") == "6.0")
  }

  "assigning the derivative of a definition to a name" should "substitute the definition, not collapse to 0" in
  {
    val s = session
    s.execute("p := 2 * x^2 + 3*x + 4")
    // q := derive(p, x) must differentiate the SUBSTITUTED p, not treat p as a
    // constant w.r.t. x (which gave q := 0.0 before the fix). Stored raw and late-bound.
    assert(s.execute("q := derive(p, x)") == "q := derive(p, x)")
    // evaluating q reduces the derivative: d/dx(2x^2 + 3x + 4) = 4x + 3
    assert(s.execute("q") == "((2.0 * (2.0 * x)) + 3.0)")
    s.execute("x := 1")
    assert(s.execute("q") == "7.0")
  }

  "an assigned derivative" should "stay late-bound to its definition" in
  {
    val s = session
    s.execute("p := x^2")
    s.execute("q := derive(p, x)")
    // redefining p updates q (derivative recomputed at use time)
    s.execute("p := x^3")
    s.execute("x := 2")
    assert(s.execute("q") == "12.0")   // d/dx(x^3) = 3x^2 = 12 at x = 2
  }

  "simplify" should "simplify without numeric evaluation" in
  {
    val s = session
    s.execute("x := 3")         // binding must NOT leak into simplify
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
    s.execute("x := 3.14159")
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
    s.execute("x := 3")
    assert(s.execute("unset x") == "x unset")
    assert(s.execute("x") == "x")
    assert(s.execute("unset x") == "x is not set")
  }

  "env" should "list precision, bindings, and definitions" in
  {
    val s = session
    s.execute("x := 3")
    s.execute("f := x + 1")
    val state = s.execute("env")
    assert(state.contains("precision = 5"))
    assert(state.contains("x := 3.0"))
    assert(state.contains("f := (x + 1.0)"))
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
    assert(session.execute("help").contains(":="))
  }

  "help <topic>" should "return topic-specific help containing the topic name" in
  {
    assert(session.execute("help simplify").contains("simplify"))
    assert(session.execute("help precision").contains("precision"))
    assert(session.execute("help solve").contains("solve"))
    assert(session.execute("? :=").contains(":="))
  }

  "help <topic>" should "not return the full listing for a known topic" in
  {
    val out = session.execute("help precision")
    assert(!out.contains(":load"), "topic help should not include unrelated commands")
  }

  "help with an unknown topic" should "fall back to the full listing" in
  {
    val out = session.execute("help xyzzy_unknown")
    assert(out.contains(":=") && out.contains("simplify"), "full listing on unknown topic")
  }

  "? <topic>" should "behave identically to help <topic>" in
  {
    assert(session.execute("? simplify") == session.execute("help simplify"))
  }

  // --- session scripts: :save (script) / :load (load) ---

  "script" should "serialize precision, bindings, and definitions as replayable := commands" in
  {
    val s = session
    s.execute("precision 4")
    s.execute("x := 3")
    s.execute("f := x + 1")
    val script = s.script
    assert(script.contains("precision 4"))
    assert(script.contains("x := 3.0"))
    assert(script.contains("f := (x + 1.0)"))
  }

  "a saved script" should "reconstruct an equivalent session when loaded" in
  {
    val original = session
    original.execute("precision 3")
    original.execute("a := 2")
    original.execute("g := a * x")
    val script = original.script

    val restored = session
    restored.load(script)
    // same precision effect, same binding, same late-bound definition
    assert(restored.execute("a") == "2.0")
    restored.execute("x := 5")
    assert(restored.execute("g") == "10.0")
    assert(restored.script.contains("precision 3"))
  }

  "load" should "skip blank lines and # comments" in
  {
    val s = session
    val out = s.load(
      """# a comment
        |x := 2
        |
        |x + 1""".stripMargin)
    assert(s.execute("x") == "2.0")
    assert(out.contains("3.0"))    // x + 1 evaluated
  }

  "load" should "run a multi-line script and return the last non-empty output" in
  {
    val s = session
    val out = s.load("x := 10\nx * 2")
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
      original.execute("k := 7")
      original.execute("h := k + x")
      assert(Session.saveFile(original, tmp.getPath) == s"saved to ${tmp.getPath}")

      val restored = session
      val out = Session.loadFile(restored, tmp.getPath)
      assert(!out.startsWith("could not read"), out)
      assert(restored.execute("k") == "7.0")
      restored.execute("x := 1")
      assert(restored.execute("h") == "8.0")
    finally tmp.delete()
  }

  "loadFile on a missing file" should "report a read error" in
  {
    assert(Session.loadFile(session, "no-such-file-xyz.txt").startsWith("could not read"))
  }

  // --- issue 1.2: deeply nested ^ chain must not crash the REPL ---

  "a deeply nested ^ chain" should "return a parse-error string rather than throwing" in
  {
    val s = session
    val bomb = "2" + "^-2" * 600
    val out = s.execute(bomb)
    assert(out.startsWith("parse error"), s"expected 'parse error', got: ${out.take(80)}")
  }

  // --- issue 1.4: integral with a definition as binder silently gives wrong answer ---

  "integral(x^2, f) when f := x" should "apply change-of-variable (slope 1) and give x^3/3" in
  {
    val s = session
    s.execute("f := x")
    // change-of-var: integral(x^2 * d(x)/dx, x) = integral(x^2 * 1, x) = integral(x^2, x) = x^3/3
    assert(s.execute("integral(x^2, f)") == "((x ^ 3.0) / 3.0)")
  }

  "integral(g, f) when f := sin(x) and g := f^2" should "not produce an answer containing definition name 'f'" in
  {
    val s = session
    s.execute("f := sin(x)")
    s.execute("g := f^2")
    val out = s.execute("integral(g, f)")
    // The bug produced ((sin(x) ^ 2.0) * f) â€" the definition name 'f' in the result.
    // After fix: change-of-var gives integral(sin(x)^2 * cos(x), x) which stays symbolic
    // but no longer references the definition name.
    assert(!out.matches(".*\\bf\\b.*"), s"result must not reference definition name 'f' but got: $out")
    assert(out.contains("sin(x)"), s"result should reference sin(x) but got: $out")
  }

  "integral(x, f) when f has several free variables" should "be rejected with an error message" in
  {
    val s = session
    s.execute("f := x + y")
    val out = s.execute("integral(x, f)")
    assert(out.contains("cannot integrate with respect to 'f'"), s"expected error but got: $out")
    assert(out.contains("several free variables"), s"expected 'several free variables' but got: $out")
  }

  "integral(x, f, 0, 1) when f := sin(x)" should "be rejected (definite integral with definition binder)" in
  {
    val s = session
    s.execute("f := sin(x)")
    val out = s.execute("integral(x, f, 0, 1)")
    assert(out.contains("cannot compute a definite integral"), s"expected error but got: $out")
  }

  "integral(x^2, x) with a plain variable binder" should "still work correctly" in
  {
    val s = session
    assert(s.execute("integral(x^2, x)") == "((x ^ 3.0) / 3.0)")
  }

  // --- issue 2.3: Parser lazy val (regression â€" all existing parse paths must still work) ---

  "Parser" should "produce consistent results across multiple parses of the same input" in
  {
    // Exercises the lazy-val grammar graph being reused correctly on repeated calls.
    assert(parser.Parser.parse("sin(x) + cos(x)").get == parser.Parser.parse("sin(x) + cos(x)").get)
    assert(parser.Parser.parse("x^2 + 2*x + 1").get == parser.Parser.parse("x^2 + 2*x + 1").get)
  }

  // --- issue 2.4: :save script round-trips _Number precision and _Bool bindings ---

  "session.script" should "serialize _Number bindings with full Double precision" in
  {
    val s = session
    s.execute("precision 8")
    s.execute("x := 3.00000001")
    // toString rounds to DefaultPrecision=5; d.toString gives "3.00000001"
    assert(s.script.contains("x := 3.00000001"),
      s"expected full-precision serialization but script was:\n${s.script}")
  }

  // Since the boolean-logic domain, `true`/`false` are grammar literals (_Bool
  // constants), so a _Bool binding serializes directly as the literal and round-trips.

  "session.script" should "serialize _Bool(true) as the literal 'true'" in
  {
    val s = session
    s.execute("h := 2 = 2")   // evaluates to _Bool(true)
    val sc = s.script
    assert(sc.contains("h := true"), s"expected 'h := true'; got:\n$sc")
  }

  "session.script" should "serialize _Bool(false) as the literal 'false'" in
  {
    val s = session
    s.execute("h := 2 = 3")   // evaluates to _Bool(false)
    val sc = s.script
    assert(sc.contains("h := false"), s"expected 'h := false'; got:\n$sc")
  }

  "a _Bool binding saved and reloaded" should "still evaluate as a boolean" in
  {
    val s1 = session
    s1.execute("ok := 2 = 2")
    val s2 = session
    s2.load(s1.script)
    assert(s2.execute("ok") == "true")
  }

  // --- issue 2.5: precision must be capped to prevent a session-hang ---

  "precision 15" should "be accepted (at the cap)" in
  {
    val s = session
    assert(s.execute("precision 15") == "precision = 15")
  }

  "precision 16" should "be rejected with a helpful error" in
  {
    val s = session
    val out = s.execute("precision 16")
    assert(out.contains("16"), s"error must name the bad value; got: $out")
    assert(out.contains("15"), s"error must name the cap; got: $out")
  }

  "precision -1" should "still be rejected" in
  {
    val s = session
    val out = s.execute("precision -1")
    assert(out.contains("non-negative") || out.contains("expects"), s"expected rejection; got: $out")
  }

  // --- issue 2.7: :load / :save must use UTF-8, not the platform default charset ---

  "Session.saveFile / loadFile" should "round-trip the session state through a temp file" in
  {
    val s1 = session
    s1.execute("precision 8")
    s1.execute("x := 3.00000001")
    s1.execute("g := sin(y) + y")   // y is unbound â†’ stays a definition after reload
    val tmp = java.io.File.createTempFile("leonardo_test", ".leo")
    try
      val saveMsg = Session.saveFile(s1, tmp.getPath)
      assert(saveMsg.startsWith("saved"), s"save failed: $saveMsg")
      val s2 = session
      Session.loadFile(s2, tmp.getPath)
      assert(s2.execute("env").contains("precision = 8"))
      assert(s2.execute("x") == "3.00000001")
      assert(s2.execute("g") == "(sin(y) + y)")
    finally
      tmp.delete()
  }

  // --- issue 1.6: _MatrixValue display must respect session precision ---

  "a matrix result" should "respect precision 2" in
  {
    val s = session
    s.execute("precision 2")
    assert(s.execute("[[1.23456789, 2]]") == "[[1.23, 2.0]]")
  }

  "a matrix result" should "respect precision 0 (whole numbers)" in
  {
    val s = session
    s.execute("precision 0")
    assert(s.execute("[[1.7, 2.3]]") == "[[2.0, 2.0]]")
  }

  "a scalar result after setting precision 2" should "still honour precision (regression)" in
  {
    val s = session
    s.execute("precision 2")
    assert(s.execute("1.23456789") == "1.23")
  }

  // --- issue 4.5b: syntax highlighting color schemes ---

  "default color scheme" should "be dark" in
  {
    assert(session.currentColorScheme == "dark")
  }

  "colors dark" should "accept the dark scheme and report it" in
  {
    val s = session
    assert(s.execute("colors dark") == "colors = dark")
    assert(s.currentColorScheme == "dark")
  }

  "colors light" should "switch to the light scheme" in
  {
    val s = session
    assert(s.execute("colors light") == "colors = light")
    assert(s.currentColorScheme == "light")
  }

  "colors none" should "disable highlighting" in
  {
    val s = session
    assert(s.execute("colors none") == "colors = none")
    assert(s.currentColorScheme == "none")
  }

  "bare colors" should "report the active scheme without changing it" in
  {
    val s = session
    s.execute("colors light")
    assert(s.execute("colors") == "colors = light")
    assert(s.currentColorScheme == "light")
  }

  "colors with an unknown name" should "report an error and list available schemes" in
  {
    val s = session
    val out = s.execute("colors rainbow")
    assert(out.contains("rainbow"),  s"error must echo the bad name; got: $out")
    assert(out.contains("dark") && out.contains("light") && out.contains("none"),
      s"error must list available schemes; got: $out")
    assert(s.currentColorScheme == "dark")   // unchanged
  }

  "session.script" should "include the colors command for persistence" in
  {
    val s = session
    s.execute("colors none")
    assert(s.script.contains("colors none"), s"script must persist colors; got:\n${s.script}")
  }

  "colors setting" should "round-trip through script / load" in
  {
    val s1 = session
    s1.execute("colors light")
    val s2 = session
    s2.load(s1.script)
    assert(s2.currentColorScheme == "light")
  }

  "colors setting" should "round-trip through saveFile / loadFile" in
  {
    val tmp = java.io.File.createTempFile("leonardo-colors", ".txt")
    tmp.deleteOnExit()
    try
      val s1 = session
      s1.execute("colors none")
      Session.saveFile(s1, tmp.getPath)
      val s2 = session
      Session.loadFile(s2, tmp.getPath)
      assert(s2.currentColorScheme == "none")
    finally tmp.delete()
  }

  // --- issue 4.L slice A: exact rational arithmetic ---

  "exact mode" should "be off by default, leaving the Double path untouched" in
  {
    val s = session
    assert(s.execute("0.1 + 0.2") == "0.3", "display rounding hides it, but this is a Double")
    assert(s.execute("exact") == "exact = off, working precision = 30")
  }

  it should "report both settings when switched on" in
  {
    val s = session
    assert(s.execute("exact on") == "exact = on, working precision = 30")
    assert(s.execute("exact off") == "exact = off, working precision = 30")
  }

  it should "make tenths add exactly, and show the fraction" in
  {
    val s = session
    s.execute("exact on")
    assert(s.execute("0.1 + 0.2") == "3/10")
    assert(s.execute("1/3 * 3") == "1")
    assert(s.execute("1/3 + 1/6") == "1/2")
  }

  it should "fall back to a decimal once a fraction stops being readable" in
  {
    val s = session
    s.execute("exact on")
    // Exact, but a 30-digit-over-30-digit fraction tells the reader nothing.
    assert(s.execute("pi") == "3.14159")
  }

  it should "accept a working precision and keep it separate from display precision" in
  {
    val s = session
    assert(s.execute("exact precision 50") == "exact = off, working precision = 50")
    assert(s.execute("precision 3") == "precision = 3")
    assert(s.execute("exact") == "exact = off, working precision = 50",
           "display precision must not disturb the working precision")
  }

  it should "reject a non-numeric or non-positive working precision" in
  {
    val s = session
    assert(s.execute("exact precision zero").contains("expects an integer"))
    assert(s.execute("exact precision 0").contains("at least 1"))
  }

  it should "reject an unrecognised mode with a helpful message" in
  {
    val s = session
    assert(s.execute("exact maybe").contains("expects 'on', 'off' or 'precision <n>'"))
  }

  "an exact binding" should "serialize as the exact fraction even when it displays rounded" in
  {
    val s = session
    s.execute("exact on")
    s.execute("p := pi")
    assert(s.execute("p") == "3.14159", "displays readably")
    val saved = s.script
    assert(saved.linesIterator.exists(l => l.startsWith("p := ") && l.contains("/")),
           s"the saved script must carry the exact fraction:\n$saved")
  }

  it should "survive a save/load round-trip unchanged" in
  {
    val s = session
    s.execute("exact on")
    s.execute("q := 1/3")
    val saved   = s.script
    val restored = session
    restored.load(saved)
    assert(restored.execute("q") == "1/3")
    assert(restored.execute("exact") == "exact = on, working precision = 30")
  }

  "the exact setting" should "be persisted by the session script" in
  {
    val s = session
    s.execute("exact on")
    s.execute("exact precision 40")
    val saved = s.script
    assert(saved.linesIterator.contains("exact on"), s"missing 'exact on':\n$saved")
    assert(saved.linesIterator.contains("exact precision 40"), s"missing precision:\n$saved")
  }

  "exact" should "be a reserved word, like every other command word" in
  {
    val s = session
    assert(s.execute("exact := 5").contains("reserved word"))
  }
  // --- issue 4.L slice B: exact matrices and factorials through the REPL ---

  "an exact matrix" should "display as fractions rather than rounded decimals" in
  {
    val s = session
    s.execute("exact on")
    assert(s.execute("[[1/2, 1/3], [1/4, 1/5]]") == "[[1/2, 1/3], [1/4, 1/5]]")
    assert(s.execute("det([[1/2, 1/3], [1/4, 1/5]])") == "1/60")
  }

  it should "invert exactly, so A * inv(A) is exactly the identity" in
  {
    val s = session
    s.execute("exact on")
    s.execute("H := [[1, 1/2, 1/3], [1/2, 1/3, 1/4], [1/3, 1/4, 1/5]]")
    assert(s.execute("inv(H)") == "[[9, -36, 30], [-36, 192, -180], [30, -180, 180]]")
    assert(s.execute("H * inv(H)") == "[[1, 0, 0], [0, 1, 0], [0, 0, 1]]")
  }

  it should "survive a save/load round-trip" in
  {
    val s = session
    s.execute("exact on")
    s.execute("M := [[1/2, 1/3], [1/4, 1/5]]")
    val restored = session
    restored.load(s.script)
    assert(restored.execute("det(M)") == "1/60")
  }

  "an exact factorial" should "print the whole integer past the Double ceiling" in
  {
    val s = session
    s.execute("exact on")
    // 171! overflows a Double, so the plain path gives up; the exact one prints all 310
    // digits. Rendering it through toDouble would have printed "Infinity" for an exact value.
    val f = s.execute("fact(171)")
    assert(f.length == 310, s"171! should be 310 digits, got ${f.length}: ${f.take(40)}...")
    assert(f.forall(_.isDigit), s"expected a plain integer, got ${f.take(40)}...")
  }

  it should "stay symbolic past the compute cap" in
  {
    val s = session
    s.execute("exact on")
    assert(s.execute("fact(20000)").contains("fact"), "past the cap it stays symbolic")
  }

  "simplify" should "agree with eval about exact values" in
  {
    val s = session
    s.execute("exact on")
    assert(s.execute("simplify 1/3 + 1/3") == "2/3")
    assert(s.execute("1/3 + 1/3") == "2/3")
  }
  // --- issues 4.O / 4.P: special functions and the probability domain ---

  "the 4.O functions" should "evaluate through the REPL" in
  {
    val s = session
    assert(s.execute("erf(1)").startsWith("0.8427"))
    assert(s.execute("digamma(1)").startsWith("-0.5772"))
    assert(s.execute("gammaQ(1, 1)") == "0.36788", "Q(1,1) = e^-1")
  }

  "a distribution" should "bind to a name and answer questions about itself" in
  {
    val s = session
    assert(s.execute("X := normal(0, 1)") == "X := normal(0.0, 1.0)")
    assert(s.execute("cdf(X, 0)") == "0.5")
    assert(s.execute("expect(X)") == "0.0")
    assert(s.execute("variance(X)") == "1.0")
  }

  it should "survive a save/load round-trip" in
  {
    val s = session
    s.execute("D := binomial(10, 0.3)")
    val restored = session
    restored.load(s.script)
    assert(restored.execute("expect(D)") == "3.0")
    assert(restored.execute("D") == "binomial(10.0, 0.3)")
  }

  "expectation" should "apply linearity through the REPL" in
  {
    val s = session
    s.execute("X := normal(5, 2)")
    assert(s.execute("expect(2*X + 3, X)") == "13.0")
    assert(s.execute("variance(2*X + 3, X)") == "16.0")
  }

  it should "stay symbolic where linearity does not apply" in
  {
    val s = session
    s.execute("X := normal(0, 1)")
    assert(s.execute("expect(X^2, X)").contains("expect"), "E[X^2] is not linear")
  }
  // --- issue 4.R: comparison operators ---

  "comparisons" should "evaluate through the REPL" in
  {
    val s = session
    assert(s.execute("2 > 1") == "true")
    assert(s.execute("2 < 1") == "false")
    assert(s.execute("2 >= 2") == "true")
    assert(s.execute("1 != 2") == "true")
  }

  it should "compose with the logic tier and with bindings" in
  {
    val s = session
    s.execute("x := 5")
    assert(s.execute("x > 0 and x < 10") == "true")
    assert(s.execute("not (x > 10)") == "true")
  }

  it should "reject a chained comparison rather than half-support it" in
  {
    val s = session
    assert(s.execute("0 < x < 1").startsWith("parse error"))
  }

  "prob with a predicate" should "work through the REPL" in
  {
    val s = session
    s.execute("X := normal(0, 1)")
    assert(s.execute("prob(X < 0)") == "0.5")
    assert(s.execute("prob(-1 < X and X < 1)") == "0.68269")
    // ...and an unrecognised predicate stays symbolic rather than answering something else.
    assert(s.execute("prob(sin(X) < 1)").contains("prob"))
  }
  // --- issue 4.Q: the statistics domain ---

  "descriptive statistics" should "work on a bound sample" in
  {
    val s = session
    s.execute("data := [[2,4,4,4,5,5,7,9]]")
    assert(s.execute("mean(data)") == "5.0")
    assert(s.execute("pstddev(data)") == "2.0")
    assert(s.execute("pvariance(data)") == "4.0")
  }

  "regression" should "recover a noiseless fit through the REPL" in
  {
    val s = session
    s.execute("X := [[1,1],[1,2],[1,3],[1,4]]")
    s.execute("y := [[5],[7],[9],[11]]")
    // y = 3 + 2x; regress adds no intercept, so the ones column is explicit.
    assert(s.execute("regress(X, y)") == "[[3.0], [2.0]]")
  }

  "a significance test" should "read the way statistics is written" in
  {
    val s = session
    s.execute("sample := [[5,6,7,8,9]]")
    assert(s.execute("ttest(sample, 5) < 0.05") == "true")
    assert(s.execute("ttest(sample, 7) < 0.05") == "false")
  }

  "the new distributions" should "bind and answer like the others" in
  {
    val s = session
    assert(s.execute("T := studentt(4)") == "T := studentt(4.0)")
    assert(s.execute("cdf(T, 0)") == "0.5")
    assert(s.execute("mean(chisq(7))") == "7.0")
  }
  // --- issue 4.6: pretty-print matrices (multi-line, column-aligned) ---

  "pretty on" should "enable multi-line matrix display and report it" in
  {
    val s = session
    assert(s.execute("pretty on") == "pretty = on")
  }

  "pretty off" should "report the disabled state" in
  {
    val s = session
    s.execute("pretty on")
    assert(s.execute("pretty off") == "pretty = off")
  }

  "bare pretty" should "show the current setting without changing it" in
  {
    val s = session
    assert(s.execute("pretty") == "pretty = off")
    s.execute("pretty on")
    assert(s.execute("pretty") == "pretty = on")
  }

  "pretty with an invalid argument" should "report an error and stay off" in
  {
    val s = session
    val out = s.execute("pretty maybe")
    assert(out.contains("maybe"), s"error must echo the bad value; got: $out")
    assert(s.execute("pretty") == "pretty = off")
  }

  "a 2x2 matrix with pretty on" should "display multi-line, right-aligned" in
  {
    val s = session
    s.execute("M := [[1, 2], [3, 4]]")
    s.execute("pretty on")
    assert(s.execute("M") == "[[1.0, 2.0]\n [3.0, 4.0]]")
  }

  "pretty display" should "right-align columns of differing widths" in
  {
    val s = session
    s.execute("M := [[1, 200], [30, 4]]")
    s.execute("pretty on")
    assert(s.execute("M") == "[[ 1.0, 200.0]\n [30.0,   4.0]]")
  }

  "a single-row matrix with pretty on" should "stay on one line (no line breaks needed)" in
  {
    val s = session
    s.execute("M := [[1, 2, 3]]")
    s.execute("pretty on")
    assert(s.execute("M") == "[[1.0, 2.0, 3.0]]")
  }

  "a matrix with pretty off (the default)" should "keep the single-line form" in
  {
    val s = session
    s.execute("M := [[1, 2], [3, 4]]")
    assert(s.execute("M") == "[[1.0, 2.0], [3.0, 4.0]]")
  }

  "a decomposition result (matrix of matrices) with pretty on" should "stay single-line" in
  {
    val s = session
    s.execute("pretty on")
    // lu(A) evaluates to a 1x3 row [[L, U, P]]; its cells are themselves matrices, so it
    // must not be stacked (that would break the grid). It stays the single-line form.
    val out = s.execute("lu([[4, 3], [6, 3]])")
    assert(!out.contains("\n"), s"nested matrix result must stay single-line; got:\n$out")
  }

  "session.script" should "persist the pretty setting" in
  {
    val s = session
    s.execute("pretty on")
    assert(s.script.contains("pretty on"), s"script must persist pretty; got:\n${s.script}")
  }

  "pretty setting" should "round-trip through script / load" in
  {
    val s1 = session
    s1.execute("pretty on")
    val s2 = session
    s2.load(s1.script)
    assert(s2.execute("pretty") == "pretty = on")
  }

  // LeonardoHighlighter -- verify content is preserved for all three schemes.

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

  // --- ode: end-to-end evaluation and assignment through Session ---

  "an ode expression" should "evaluate numerically at the session precision" in
  {
    val s = session
    // y' = y, y(0) = 1 at t = 1 → e (closed-form tier), displayed at precision 5
    assert(s.execute("ode(y, y, t, 0, 1, 1)") == "2.71828")
  }

  "an ode result" should "be assignable to a name and reused" in
  {
    val s = session
    assert(s.execute("r := ode(y, y, t, 0, 1, 1)").startsWith("r :="))
    assert(s.execute("r") == "2.71828")
  }

  // --- issue 1.4: evaluation errors are reported, never crash the session ---

  "an evaluation that throws (Int-overflow matrix dimension)" should "be reported, not crash" in
  {
    val s = session
    // eye(50000): 50000*50000 overflows Int to a negative size → NegativeArraySizeException
    // during eval. Without the guard this propagates through Session.step and kills the loop;
    // the withParsed Try must turn it into a graceful, non-empty message instead.
    val out = s.execute("eye(50000)")
    assert(out.nonEmpty, "overflowing constructor must return gracefully, not throw")
    // the session is still usable afterwards
    assert(s.execute("1 + 1") == "2.0")
  }

  "samples with a malformed number literal" should "report an error, not throw" in
  {
    val s = session
    // "1..2" satisfies the samples regex's [\d.]+ class but is not a valid Double; the bound
    // parse runs outside withParsed, so toDouble would crash the loop — toDoubleOption reports.
    val out = s.execute("samples x x 1..2 3")
    assert(out.startsWith("samples:"), s"expected a samples error message, got: $out")
    assert(s.execute("2 * 3") == "6.0")   // session survives
  }

  "samples with valid bounds" should "still produce sampled pairs (regression)" in
  {
    val s   = session
    val out = s.execute("samples x x 0 2 3")
    // three points at x = 0, 1, 2 for f(x) = x
    assert(out.linesIterator.size == 3, s"expected 3 sample rows, got: $out")
  }

  // --- issue 4.5: REPL read-loop dispatch (Session.step) ---
  // The JLine line-editing / persistent-history plumbing in repl() itself is
  // interactive-only and not unit-testable, but the loop's dispatch logic is
  // factored into the pure Session.step and covered here.

  "step on quit, exit, or end-of-input" should "stop the loop (None)" in
  {
    assert(Session.step(session, Some("quit")).isEmpty)
    assert(Session.step(session, Some("exit")).isEmpty)
    assert(Session.step(session, None).isEmpty)   // Ctrl-D
  }

  "step on an ordinary expression" should "continue and carry the evaluated output" in
  {
    val s = session
    s.execute("x := 4")
    assert(Session.step(s, Some("x + 1")).contains("5.0"))
  }

  "step on a := assignment" should "continue and mutate the session" in
  {
    val s = session
    assert(Session.step(s, Some("k := 3")).contains("k := 3.0"))
    assert(s.execute("k") == "3.0")
  }

  "step on a blank or Ctrl-C-abandoned line" should "continue with empty output" in
  {
    // Ctrl-C is surfaced to the loop as an empty line, so it must never stop it.
    assert(Session.step(session, Some("")).contains(""))
  }

  "step on :load and :save" should "perform the file IO the bare execute path refuses" in
  {
    val tmp = java.io.File.createTempFile("leonardo-step", ".txt")
    tmp.deleteOnExit()
    try
      val s1 = session
      s1.execute("a := 9")
      val saved = Session.step(s1, Some(s":save ${tmp.getPath}"))
      assert(saved.exists(_.startsWith("saved to")), s"expected save confirmation, got: $saved")

      val s2 = session
      val loaded = Session.step(s2, Some(s":load ${tmp.getPath}"))
      assert(loaded.exists(!_.startsWith("could not read")), s"expected a successful load, got: $loaded")
      assert(s2.execute("a") == "9.0")
      // contrast: the same tokens through execute are refused as interactive-only
      assert(s2.execute(":save foo.txt").contains("interactive"))
    finally tmp.delete()
  }

  // --- auto-bind on solve ---

  "a single-solution solve" should "auto-bind the variable" in
  {
    val s = session
    assert(s.execute("solve(2*x + 4 = 0, x)") == "x := -2.0")
    assert(s.execute("x") == "-2.0")
  }

  "a multiple-solution solve" should "auto-bind numbered variables and leave the original unbound" in
  {
    val s = session
    val out = s.execute("solve(x^2 = 4, x)")
    assert(out.contains("x_1 :=") && out.contains("x_2 :="), s"expected x_1/x_2 bindings but got: $out")
    assert(s.execute("x_1") == "-2.0" || s.execute("x_1") == "2.0")
    assert(s.execute("x_2") != s.execute("x_1"))
    assert(s.execute("x") == "x", "original x must remain unbound")
  }

  "a solve with no solution" should "stay symbolic without binding anything" in
  {
    val s = session
    val out = s.execute("solve(x^2 + 1 = 0, x)")
    assert(out.startsWith("solve("), s"expected symbolic solve node but got: $out")
    assert(s.execute("x") == "x")
  }

  "auto-bound solve result" should "be usable in the next expression" in
  {
    val s = session
    s.execute("solve(3*x = 9, x)")
    assert(s.execute("x * 2") == "6.0")
  }

  "a named equation passed to solve" should "also auto-bind numbered roots" in
  {
    val s = session
    s.execute("h := x^2 - 9 = 0")
    s.execute("solve(h, x)")
    assert(s.execute("x") == "x", "x must remain unbound when there are two solutions")
    val x1 = s.execute("x_1")
    val x2 = s.execute("x_2")
    assert(Set(x1, x2) == Set("-3.0", "3.0"), s"expected roots Â±3 but got $x1, $x2")
  }




  // â"€â"€â"€ Issue 1.2: direct product of matrix-bound variables â"€â"€â"€â"€â"€â"€â"€â"€â"€â"€â"€â"€â"€â"€â"€â"€â"€â"€

  "a product of two matrix-bound variables" should "evaluate to a numeric matrix" in
  {
    val s = session
    s.execute("M1 := [[1.0, 2.0], [3.0, 4.0]]")
    s.execute("M2 := [[5.0, 6.0], [7.0, 8.0]]")
    val result = s.execute("M1 * M2")
    // Should NOT contain symbolic Product, MatScale, etc.
    assert(
      !result.contains("*") || result.startsWith("[["),
      s"Expected numeric matrix, got: $result"
      )
    assert(
      result == "[[19.0, 22.0], [43.0, 50.0]]",
      s"Wrong product result: $result"
      )
  }

  "a triple product of matrix-bound variables with inverse" should "recover the original matrix" in
  {
    val s = session
    s.execute("A := [[1, 2, 3], [3, 2, 1], [1, 1, 1]]")
    // Bind P_A and J_A as concrete matrices (the values from jordan(A) at precision 5)
    s.execute("P_A := [[0.40825, 0.60923, -0.70711], [-0.8165, 0.72, 0.70711], [0.40825, 0.33231, 0.0]]")
    s.execute("J_A := [[0.0, 0.0, 0.0], [0.0, 5.0, 0.0], [0.0, 0.0, -1.0]]")

    val result = s.execute("P_A * J_A * inv(P_A)")
    // Result MUST be a numeric matrix (all elements are numbers, no symbolic nodes)
    assert(
      result.matches("""\[\[[-\d., ]+], \[[-\d., ]+], \[[-\d., ]+]]"""),
      s"Expected numeric matrix like [[1.0, 2.0, 3.0], ...], got: $result"
      )
  }

  "a matrix operation times a matrix-bound variable" should "multiply in the written order" in
  {
    val s = session
    // AÂ·B â‰  BÂ·A for these two, so a swapped dispatch would produce the wrong result.
    s.execute("M1 := [[1.0, 2.0], [3.0, 4.0]]")
    val result = s.execute("transpose(M1) * M1") // matrix-shaped left, variable right
    assert(
      result == "[[10.0, 14.0], [14.0, 20.0]]",
      s"Expected transpose(M1)*M1 in the written order, got: $result"
      )
  }

  "a product stored as a definition" should "evaluate to a numeric matrix via eval" in
  {
    val s = session
    s.execute("M1 := [[1.0, 2.0], [3.0, 4.0]]")
    s.execute("M2 := [[5.0, 6.0], [7.0, 8.0]]")
    // A1 will be stored as a definition since eval with emptyEnv can't reduce it
    s.execute("A1 := M1 * M2")
    val result = s.execute("eval A1")
    assert(
      result == "[[19.0, 22.0], [43.0, 50.0]]",
      s"Wrong product result via eval: $result"
      )
  }

  "simplify on a product of matrix-bound variables" should "reduce to a numeric matrix" in
  {
    val s = session
    s.execute("M1 := [[1.0, 2.0], [3.0, 4.0]]")
    s.execute("M2 := [[5.0, 6.0], [7.0, 8.0]]")
    val result = s.execute("simplify M1 * M2")
    assert(
      result == "[[19.0, 22.0], [43.0, 50.0]]",
      s"Wrong product result via simplify: $result"
      )
  }

  // â"€â"€â"€ Issue 1.1: precision in decomposition display â"€â"€â"€â"€â"€â"€â"€â"€â"€â"€â"€â"€â"€â"€â"€â"€â"€â"€â"€â"€â"€â"€â"€â"€â"€

  "jordan(A) display" should "respect the session precision" in
  {
    val s = session
    s.execute("A := [[1, 2, 3], [3, 2, 1], [1, 1, 1]]")
    s.execute("precision 10")
    val jordan = s.execute("jordan(A)")
    // At precision 5 (default), numbers show 5 significant digits: 0.40825
    // At precision 10, they should show more digits, e.g. 0.4082482905
    assert(
      !jordan.contains("0.40825") || jordan.contains("0.408248"),
      s"jordan(A) should respect precision 10, got: $jordan"
      )
  }

  "an eigenvalue decomposition display" should "respect the session precision" in
  {
    val s = session
    s.execute("A := [[1, 2, 3], [3, 2, 1], [1, 1, 1]]")
    s.execute("precision 10")
    val eig = s.execute("eig(A)")
    // At precision 5, eigenvalue 5.0 stays 5.0 (exact), but eigenvectors have more digits
    // The output should reflect precision 10 for the irrational entries
    assert(
      eig.startsWith("[["),
      s"eig(A) should return a matrix, got: $eig"
      )
    assert(
      eig.contains("0.408248"),
      s"eig(A) eigenvector entries should show precision-10 digits, got: $eig"
      )
  }

  // --- 4.1: tuple assignment (L, U, P := lu(A)) ---

  "tuple assignment L, U, P := lu(A)" should "bind three matrices" in
  {
    val s = session
    s.execute("A := [[1, 2, 3], [3, 2, 1], [1, 1, 1]]")
    val out = s.execute("L, U, P := lu(A)")
    assert(out.contains("L :="), s"expected L bound, got: $out")
    assert(out.contains("U :="), s"expected U bound, got: $out")
    assert(out.contains("P :="), s"expected P bound, got: $out")
    // L must be available as a matrix in subsequent expressions
    val lVal = s.execute("L")
    assert(lVal.startsWith("[["), s"L should be a matrix, got: $lVal")
  }

  "tuple assignment Q, R := qr(A)" should "bind two matrices" in
  {
    val s = session
    s.execute("A := [[1, 2], [3, 4], [5, 6]]")
    val out = s.execute("Q, R := qr(A)")
    assert(out.contains("Q :="), s"expected Q bound, got: $out")
    assert(out.contains("R :="), s"expected R bound, got: $out")
    val qVal = s.execute("Q")
    assert(qVal.startsWith("[["), s"Q should be a matrix, got: $qVal")
  }

  "tuple assignment P, J := jordan(A)" should "bind two matrices" in
  {
    val s = session
    s.execute("A := [[1, 2, 3], [3, 2, 1], [1, 1, 1]]")
    val out = s.execute("P, J := jordan(A)")
    assert(out.contains("P :="), s"expected P bound, got: $out")
    assert(out.contains("J :="), s"expected J bound, got: $out")
  }

  "tuple assignment V, D := eig(A)" should "bind two matrices" in
  {
    val s = session
    s.execute("A := [[4, 1], [1, 3]]")
    val out = s.execute("V, D := eig(A)")
    assert(out.contains("V :="), s"expected V bound, got: $out")
    assert(out.contains("D :="), s"expected D bound, got: $out")
  }

  "tuple assignment with wrong name count" should "report the mismatch" in
  {
    val s = session
    s.execute("A := [[1, 2], [3, 4]]")
    val out = s.execute("L, U := lu(A)")
    assert(out.contains("2 names") && out.contains("3 elements"), s"expected mismatch message, got: $out")
  }

  "tuple assignment to a reserved constant" should "be rejected" in
  {
    val s = session
    s.execute("A := [[1, 2, 3], [3, 2, 1], [1, 1, 1]]")
    val out = s.execute("e, U, P := lu(A)")
    assert(out.contains("cannot assign to 'e'"), s"expected reserved-constant rejection, got: $out")
  }

  "tuple assignment to a reserved word" should "be rejected" in
  {
    val s = session
    s.execute("A := [[1, 2], [3, 4]]")
    val out = s.execute("Q, sin := qr(A)")
    assert(out.contains("cannot assign to 'sin'"), s"expected reserved-word rejection, got: $out")
  }

  "tuple assignment when RHS is not a 1xn matrix" should "report an error" in
  {
    val s = session
    s.execute("x := 3")
    val out = s.execute("a, b := x + 1")
    assert(out.contains("tuple assignment"), s"expected tuple-assignment error, got: $out")
  }

  // --- issue 4.9: consolidate freezes the simplified + evaluated result ---

  "consolidate with a fully numeric result" should "bind the frozen value" in
  {
    val s = session
    s.execute("x := 2")
    s.execute("f := x + 1")
    assert(s.execute("g := consolidate(f + f)") == "g := 6.0")
    assert(s.execute("g") == "6.0")
  }

  "a consolidated value" should "not change when a dependency is redefined" in
  {
    val s = session
    s.execute("x := 2")
    s.execute("f := x + 1")
    s.execute("g := consolidate(f + f)")
    // g is frozen at 6.0; a plain definition g := f + f would follow x here
    s.execute("x := 100")
    assert(s.execute("g") == "6.0")
  }

  "consolidate with a free variable remaining" should "store a frozen simplified definition" in
  {
    val s = session
    s.execute("a := 3")
    // a folds in (eval uses current bindings); y stays free → frozen definition
    assert(s.execute("h := consolidate(a * y)") == "h := (3.0 * y)")
    assert(s.execute("h") == "(3.0 * y)")
  }

  "a consolidated symbolic definition" should "ignore later redefinition of the folded dependency" in
  {
    val s = session
    s.execute("a := 3")
    s.execute("h := consolidate(a * y)")
    s.execute("a := 9")
    // frozen: still 3.0 * y, unlike a late-bound definition which would become 9.0 * y
    assert(s.execute("h") == "(3.0 * y)")
    s.execute("y := 2")
    assert(s.execute("h") == "6.0")
  }

  "consolidate over a matrix product" should "execute the multiplication and freeze" in
  {
    val s = session
    s.execute("A := [[1, 2], [3, 4]]")
    s.execute("B := [[5, 6], [7, 8]]")
    s.execute("C := consolidate(A * B)")
    assert(s.execute("C") == "[[19.0, 22.0], [43.0, 50.0]]")
  }

  "consolidate to a reserved constant" should "be rejected" in
  {
    val s = session
    val out = s.execute("e := consolidate(1 + 1)")
    assert(out.contains("cannot assign to 'e'"), s"expected reserved-constant rejection, got: $out")
  }

  "a consolidated definition" should "survive a script round-trip" in
  {
    val s = session
    s.execute("a := 3")
    s.execute("h := consolidate(a * y)")
    // reload the serialized script into a fresh session
    val restored = session
    restored.load(s.script)
    assert(restored.execute("h") == "(3.0 * y)")
    // still frozen after reload: redefining a does not disturb h
    restored.execute("a := 9")
    assert(restored.execute("h") == "(3.0 * y)")
  }
  // --- issue 4.E: boolean logic domain ---

  "a boolean expression" should "evaluate at the REPL" in
  {
    val s = session
    assert(s.execute("true and false") == "false")
    assert(s.execute("true or false") == "true")
    assert(s.execute("not true") == "false")
    assert(s.execute("true implies false") == "false")
    assert(s.execute("true xor true") == "false")
  }

  "a boolean expression over bindings" should "reduce through the environment" in
  {
    val s = session
    s.execute("a := 2 = 2")     // _Bool(true)
    s.execute("b := 2 = 3")     // _Bool(false)
    assert(s.execute("a and b") == "false")
    assert(s.execute("a or b") == "true")
    assert(s.execute("a and c") == "(true and c)")   // c free: stays symbolic, a reduced
  }

  "simplify with connectives" should "run the logic pass after the scalar pass" in
  {
    val s = session
    assert(s.execute("simplify a and true") == "a")
    assert(s.execute("simplify not not a") == "a")
    assert(s.execute("simplify a or (a and b)") == "a")
    // the injected scalar leaf pass reaches inside the connective
    assert(s.execute("simplify not (x + 0 = x)") == "(not x = x)")
  }

  "simplify without connectives" should "behave exactly as before" in
  {
    val s = session
    assert(s.execute("simplify x + 0") == "x")
  }

  "the truth command" should "print the table over the free variables" in
  {
    val s = session
    val out = s.execute("truth a and b")
    val lines = out.linesIterator.toList
    assert(lines.size == 5, s"expected header + 4 rows; got:\n$out")
    assert(lines.head.startsWith("a     b     | "), s"unexpected header: ${lines.head}")
    assert(lines(1).startsWith("false false | false"))
    assert(lines(4).startsWith("true  true  | true"))
  }

  "the truth command on a definition" should "substitute it first" in
  {
    val s = session
    s.execute("f := a and b")
    val out = s.execute("truth f or a")
    assert(out.linesIterator.toList.size == 5, s"expected header + 4 rows; got:\n$out")
  }

  "the bare truth command" should "print a usage message" in
  {
    val s = session
    assert(s.execute("truth") == "usage: truth <expr>")
  }

  "assigning to a logic keyword" should "be rejected" in
  {
    val s = session
    assert(s.execute("true := 3").contains("reserved word"))
    assert(s.execute("and := 3").contains("reserved word"))
    assert(s.execute("truth := 3").contains("reserved word"))
  }

  "a _Bool literal binding" should "round-trip through a script" in
  {
    val s1 = session
    s1.execute("ok := true")
    val s2 = session
    s2.load(s1.script)
    assert(s2.execute("ok") == "true")
  }

  "the highlighter" should "accept logic keywords without error" in
  {
    val h = LeonardoHighlighter(() => "dark")
    assert(h.highlightBuffer("truth a and not b or true").toString == "truth a and not b or true")
  }

  // --- issue 4.F: ternary (Kleene) logic ---

  "the unknown literal" should "evaluate and bind like any other value" in
  {
    val s = session
    assert(s.execute("unknown") == "unknown")
    assert(s.execute("u := unknown") == "u := unknown")
    assert(s.execute("u") == "unknown")
    assert(s.execute("not u") == "unknown")
  }

  "the Kleene identities" should "hold at the REPL" in
  {
    val s = session
    assert(s.execute("false and unknown") == "false")
    assert(s.execute("true or unknown") == "true")
    assert(s.execute("unknown and unknown") == "unknown")
    assert(s.execute("not unknown") == "unknown")
    assert(s.execute("unknown implies unknown") == "unknown")
    assert(s.execute("true and unknown") == "unknown")
  }

  "simplify over unknown" should "not apply the crisp-only complement rule" in
  {
    val s = session
    assert(s.execute("simplify unknown and not unknown") == "unknown")
    // a free variable is still treated as a crisp atom
    assert(s.execute("simplify a and not a") == "false")
  }

  "an unknown binding" should "round-trip through a script" in
  {
    val s1 = session
    s1.execute("u := unknown")
    assert(s1.script.contains("u := unknown"))
    val s2 = session
    s2.load(s1.script)
    assert(s2.execute("u") == "unknown")
    assert(s2.execute("u and false") == "false")
  }

  "the truth3 command" should "print the three-valued table" in
  {
    val s = session
    val out = s.execute("truth3 not a")
    val lines = out.linesIterator.toList
    assert(lines.size == 4, s"expected header + 3 rows; got:\n$out")
    assert(lines(1).startsWith("false   | true"), s"unexpected row: ${lines(1)}")
    assert(lines(2).startsWith("unknown | unknown"), s"unexpected row: ${lines(2)}")
    assert(lines(3).startsWith("true    | false"), s"unexpected row: ${lines(3)}")
  }

  it should "enumerate 3^n rows for n variables" in
  {
    val s = session
    assert(s.execute("truth3 a and b").linesIterator.toList.size == 10)  // header + 9
  }

  "the bare truth3 command" should "print a usage message" in
  {
    val s = session
    assert(s.execute("truth3") == "usage: truth3 <expr>")
  }

  "the boolean truth command" should "still work unchanged alongside truth3" in
  {
    val s = session
    assert(s.execute("truth a and b").linesIterator.toList.size == 5)
  }

  "assigning to unknown or truth3" should "be rejected" in
  {
    val s = session
    assert(s.execute("unknown := 3").contains("reserved word"))
    assert(s.execute("truth3 := 3").contains("reserved word"))
  }

  "the highlighter" should "accept the unknown literal without error" in
  {
    val h = LeonardoHighlighter(() => "dark")
    assert(h.highlightBuffer("truth3 a and unknown").toString == "truth3 a and unknown")
  }
  // --- issue 4.G: symmetric ternary encoding ---

  "the logic symmetric toggle" should "default to off and report its state" in
  {
    val s = session
    assert(s.execute("logic symmetric") == "logic symmetric = off")
    assert(s.execute("logic symmetric on") == "logic symmetric = on")
    assert(s.execute("logic symmetric") == "logic symmetric = on")
    assert(s.execute("logic symmetric off") == "logic symmetric = off")
  }

  it should "reject a bad mode and an unknown logic setting" in
  {
    val s = session
    assert(s.execute("logic symmetric maybe").contains("expects 'on' or 'off'"))
    assert(s.execute("logic tnorm product").contains("unknown logic setting"))
  }

  "truth values under the symmetric encoding" should "print as -1 / 0 / 1" in
  {
    val s = session
    s.execute("logic symmetric on")
    assert(s.execute("true") == "1")
    assert(s.execute("false") == "-1")
    assert(s.execute("unknown") == "0")
    assert(s.execute("2 = 2") == "1")
  }

  it should "read -1 / 0 / 1 as truth values in connective positions" in
  {
    val s = session
    s.execute("logic symmetric on")
    assert(s.execute("-1 and 0") == "-1")     // false and unknown = false
    assert(s.execute("1 or 0") == "1")        // true or unknown = true
    assert(s.execute("0 and 0") == "0")       // unknown and unknown = unknown
    assert(s.execute("not 0") == "0")
    assert(s.execute("1 and 0") == "0")
  }

  it should "leave ordinary arithmetic alone" in
  {
    val s = session
    s.execute("logic symmetric on")
    assert(s.execute("2 * 3 + 1") == "7.0")
    assert(s.execute("0 + 1") == "1.0")
  }

  it should "apply to a variable bound to a digit" in
  {
    val s = session
    s.execute("logic symmetric on")
    s.execute("a := 0")
    assert(s.execute("a and 1") == "0")
    assert(s.execute("a and -1") == "-1")
  }

  it should "echo assignments in the symmetric alphabet" in
  {
    val s = session
    s.execute("logic symmetric on")
    assert(s.execute("u := unknown") == "u := 0")
    assert(s.execute("t := 2 = 2") == "t := 1")
  }

  it should "spell the truth tables in digits" in
  {
    val s = session
    s.execute("logic symmetric on")
    val out = s.execute("truth3 not a").linesIterator.toList
    assert(out.size == 4, s"expected header + 3 rows; got:\n${out.mkString("\n")}")
    assert(out(1).startsWith("-1 | 1"), s"unexpected row: ${out(1)}")
    assert(out(2).startsWith("0  | 0"), s"unexpected row: ${out(2)}")
    assert(out(3).startsWith("1  | -1"), s"unexpected row: ${out(3)}")
  }

  it should "not apply the crisp-only complement rule to the digit 0" in
  {
    val s = session
    s.execute("logic symmetric on")
    assert(s.execute("simplify 0 and not 0") == "0")
  }

  "the symmetric toggle" should "be persisted by a script round-trip" in
  {
    val s1 = session
    s1.execute("logic symmetric on")
    s1.execute("u := unknown")
    assert(s1.script.contains("logic symmetric on"))
    // :save always writes the word spelling, so the script is portable across the toggle
    assert(s1.script.contains("u := unknown"))
    val s2 = session
    s2.load(s1.script)
    assert(s2.execute("logic symmetric") == "logic symmetric = on")
    assert(s2.execute("u") == "0")
  }

  "the default alphabet" should "be completely unaffected while the toggle is off" in
  {
    val s = session
    assert(s.execute("true") == "true")
    assert(s.execute("unknown") == "unknown")
    assert(s.execute("u := unknown") == "u := unknown")
    // digits stay plain numbers, so a connective over them stays symbolic
    assert(s.execute("1 and 0") == "(1.0 and 0.0)")
    assert(s.execute("truth3 not a").linesIterator.toList(2).startsWith("unknown | unknown"))
  }

  "assigning to logic" should "be rejected" in
  {
    val s = session
    assert(s.execute("logic := 3").contains("reserved word"))
  }
  // --- issue 4.H: fuzzy logic ---

  "the logic semantics setting" should "default to minmax and switch by name" in
  {
    val s = session
    assert(s.execute("logic").linesIterator.toList.head == "logic minmax")
    assert(s.execute("logic product") == "logic product")
    assert(s.execute("logic lukasiewicz") == "logic lukasiewicz")
    assert(s.execute("logic minmax") == "logic minmax")
  }

  it should "reject an unknown name and list the options" in
  {
    val s = session
    val out = s.execute("logic godel")
    assert(out.contains("unknown logic setting"))
    assert(out.contains("minmax") && out.contains("product") && out.contains("lukasiewicz"))
  }

  it should "report both logic settings on the bare command" in
  {
    val s = session
    s.execute("logic product")
    s.execute("logic symmetric on")
    val out = s.execute("logic").linesIterator.toList
    assert(out.contains("logic product"))
    assert(out.contains("logic symmetric = on"))
  }

  "graded degrees" should "combine by the active t-norm" in
  {
    val s = session
    assert(s.execute("truth(0.3) and truth(0.7)") == "truth(0.3)")
    s.execute("logic product")
    assert(s.execute("truth(0.3) and truth(0.5)") == "truth(0.15)")
    s.execute("logic lukasiewicz")
    assert(s.execute("truth(0.3) and truth(0.5)") == "false")   // max(0, -0.2) collapses to false
    assert(s.execute("truth(0.8) and truth(0.7)") == "unknown")   // 0.5 is the Kleene midpoint
  }

  "the hedges and curves" should "evaluate at the REPL" in
  {
    val s = session
    assert(s.execute("very(0.5)") == "truth(0.25)")
    assert(s.execute("somewhat(0.25)") == "unknown")        // sqrt(0.25) = 0.5
    assert(s.execute("trimf(5, 0, 5, 10)") == "true")       // apex collapses to true
    assert(s.execute("trimf(2.5, 0, 5, 10)") == "unknown")  // exactly one half
    assert(s.execute("gaussmf(3, 3, 1)") == "true")
  }

  it should "compose with the connectives" in
  {
    val s = session
    assert(s.execute("very(0.5) and somewhat(0.25)") == "truth(0.25)")
    assert(s.execute("trimf(2.5, 0, 5, 10) and true") == "unknown")
  }

  "defuzz" should "return the crisp representative of a curve" in
  {
    val s = session
    assert(s.execute("defuzz(trimf(x, 0, 5, 10), x, 0, 10)") == "5.0")
    assert(s.execute("defuzz(gaussmf(x, 3, 1), x, 0, 6)") == "3.0")
  }

  "a graded binding" should "round-trip through a script" in
  {
    val s1 = session
    s1.execute("d := very(0.5)")
    assert(s1.execute("d") == "truth(0.25)")
    assert(s1.script.contains("d := truth(0.25)"))
    val s2 = session
    s2.load(s1.script)
    assert(s2.execute("d") == "truth(0.25)")
    assert(s2.execute("d and false") == "false")
  }

  "the semantics setting" should "be persisted by a script round-trip" in
  {
    val s1 = session
    s1.execute("logic product")
    val s2 = session
    s2.load(s1.script)
    assert(s2.execute("logic").linesIterator.toList.head == "logic product")
    assert(s2.execute("truth(0.3) and truth(0.5)") == "truth(0.15)")
  }

  "simplify under product semantics" should "not apply the lattice-only idempotence rule" in
  {
    val s = session
    // min-max is a lattice, so idempotence folds the conjunction away
    assert(s.execute("simplify truth(0.3) and truth(0.3)") == "truth(0.3)")
    s.execute("logic product")
    // under product a and a = a^2, so the structural rule is gated off; simplify does
    // not evaluate the truth(...) nodes, so the conjunction simply stays
    assert(s.execute("simplify truth(0.3) and truth(0.3)") == "(truth(0.3) and truth(0.3))")
    // evaluation, by contrast, applies the product t-norm
    assert(s.execute("truth(0.3) and truth(0.3)") == "truth(0.09)")
    // a free variable is still a crisp atom, so idempotence holds under every semantics
    assert(s.execute("simplify a and a") == "a")
  }

  "assigning to a fuzzy keyword" should "be rejected" in
  {
    val s = session
    for name <- List("very", "somewhat", "trimf", "gaussmf", "defuzz", "truth") do
      assert(s.execute(s"$name := 3").contains("reserved word"), s"'$name' must be reserved")
  }

  "the highlighter" should "accept the fuzzy vocabulary without error" in
  {
    val h = LeonardoHighlighter(() => "dark")
    val line = "defuzz(very(trimf(x, 0, 5, 10)), x, 0, 10)"
    assert(h.highlightBuffer(line).toString == line)
  }
  // --- issue 4.K: Greek-letter aliases and their insertion chords ---

  "the Greek aliases" should "parse to the same nodes as the ASCII spellings" in
  {
    val s = session
    assert(s.execute("Γ(5)") == s.execute("Gamma(5)"))
    assert(s.execute("β(1, 4)") == s.execute("Beta(1, 4)"))
    assert(s.execute("Γ(5)") == "24.0")
    assert(s.execute("β(1, 4)") == "0.25")
  }

  it should "compose like any other function" in
  {
    val s = session
    assert(s.execute("Γ(3) + 1") == "3.0")          // 2! + 1
    assert(s.execute("2 * Γ(4)") == "12.0")         // 2 * 3!
  }

  "the ASCII spelling" should "remain what toString emits, so :save stays portable" in
  {
    val s1 = session
    s1.execute("g := Γ(4) + x")
    val script = s1.script
    assert(script.contains("Gamma("), s"expected the ASCII spelling in:\n$script")
    assert(!script.contains("Γ"), s"the Greek glyph must not reach a script:\n$script")
    // and the script still replays
    val s2 = session
    s2.load(script)
    s2.execute("x := 0")
    assert(s2.execute("g") == "6.0")
  }

  "lowercase gamma and beta" should "still be ordinary variables alongside the aliases" in
  {
    val s = session
    s.execute("gamma := 3")
    s.execute("beta := 4")
    assert(s.execute("gamma + beta") == "7.0")
  }

  "capital Latin B" should "NOT be the Beta function (the homoglyph we refused to add)" in
  {
    val s = session
    // B is an ordinary variable; B(1,4) is not a function call
    s.execute("B := 5")
    assert(s.execute("B") == "5.0")
    assert(!parser.Parser.parse("B(1, 4)").successful, "B(...) must not be a Beta call")
  }

  "the highlighter" should "accept the Greek glyphs without error" in
  {
    val h = LeonardoHighlighter(() => "dark")
    val line = "Γ(5) + β(1, 2)"
    assert(h.highlightBuffer(line).toString == line)
  }
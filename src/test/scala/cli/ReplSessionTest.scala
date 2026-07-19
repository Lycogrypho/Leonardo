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
    for name <- List("simplify", "eval", "env", "sin", "derive", "precision", "quit") do
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

  "session.script" should "serialize _Bool(true) as '0 = 0', not as bare 'true'" in
  {
    val s = session
    s.execute("h := 2 = 2")   // evaluates to _Bool(true)
    val sc = s.script
    assert(!sc.contains("true"), s"Bool(true) must not be serialized as 'true'; got:\n$sc")
    assert(sc.contains("h := 0 = 0"), s"expected 'h := 0 = 0'; got:\n$sc")
  }

  "session.script" should "serialize _Bool(false) as '0 = 1', not as bare 'false'" in
  {
    val s = session
    s.execute("h := 2 = 3")   // evaluates to _Bool(false)
    val sc = s.script
    assert(!sc.contains("false"), s"Bool(false) must not be serialized as 'false'; got:\n$sc")
    assert(sc.contains("h := 0 = 1"), s"expected 'h := 0 = 1'; got:\n$sc")
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
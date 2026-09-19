package it.grypho.scala.leonardo
package latex

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** The LaTeX emitter's harness (issue F_0016 step 1).
 *
 *  Two kinds of case, deliberately kept apart:
 *
 *   -  **Targeted bracketing cases** pin exact output for the handful of expressions the
 *      precedence pass was designed around — where a bracket must appear, and where one must
 *      *disappear*.  These are the entry's worked examples and may legitimately change as the
 *      rendering improves; each says what it is protecting.
 *   -  **Property checks over a corpus** assert what must hold for *every* rendering whatever
 *      it looks like: balanced braces, balanced `\left`/`\right`, a `\frac` always carrying
 *      its two arguments.  Pinning whole strings for the corpus would make every cosmetic
 *      improvement a test edit — the `IntegralTableTest` lesson.
 */
class ToLatexTest extends AnyFlatSpec:

  private def tex(input: String): String =
    val r = Parser.parse(input)
    assert(r.successful, s"parse failed for \"$input\": $r")
    ToLatex(r.get)

  // ── the worked examples: brackets that must vanish ────────────────────────────────

  "a fraction" should "swallow the parentheses of both of its arguments" in
  {
    // THE case the entry names: toString would print ((a + b) / (c + d)), and a faithful
    // transcription would be correct and unreadable. \frac's braces already group.
    assert(tex("(a+b)/(c+d)") == "\\frac{a + b}{c + d}")
  }

  it should "need no brackets as a factor, but demand them as the base of a power" in
  {
    // Visually a fraction IS atomic in a product -- but \frac{x}{y}^2 reads as y carrying
    // the exponent, so the power base is the one slot that must re-bracket it.
    assert(tex("x/y*z")   == "\\frac{x}{y} \\cdot z")
    assert(tex("(x/y)^2") == "\\left(\\frac{x}{y}\\right)^{2.0}")
  }

  "precedence" should "bracket exactly where the tree disagrees with the notation" in
  {
    assert(tex("a+b*c")   == "a + b \\cdot c",                  "no brackets where none needed")
    assert(tex("(a+b)*c") == "\\left(a + b\\right) \\cdot c",   "a grouped sum keeps its group")
    assert(tex("(a+b)^2") == "\\left(a + b\\right)^{2.0}",      "a sum as a power base")
    assert(tex("x^2*y")   == "x^{2.0} \\cdot y",                "a power needs none in a product")
    assert(tex("x^(2+n)") == "x^{2.0 + n}",                     "a braced exponent needs none")
  }

  it should "re-bracket a power under a power, which LaTeX flatly rejects" in
  {
    // x^{2}^{3} is not ugly output, it is a double-superscript ERROR: the document fails to
    // render at all. This is the case that forced Power to report below Atomic.
    assert(tex("(x^2)^3") == "\\left(x^{2.0}\\right)^{3.0}")
  }

  "subtraction" should "render as subtraction although no such node exists" in
  {
    // `a - b` parses to Sum(a, (-1)*b) and `a - 3k` folds the sign into the coefficient;
    // both arrive on every subtraction anyone types, and the naive rendering of either is
    // `a + -1 \cdot b`.
    assert(tex("a - b")       == "a - b")
    assert(tex("a - 3*k")     == "a - 3.0 k")
    assert(tex("a - (b + c)") == "a - \\left(b + c\\right)", "a compound subtrahend keeps its group")
    assert(tex("-x")          == "-x")
    // Built directly rather than parsed: the parser folds signs into coefficients, so a bare
    // negative literal beside a coefficient is a shape only construction reaches -- and it is
    // exactly where `2.0 -3.0` would read as subtraction. The bracket is the point.
    assert(ToLatex(scalar.Product(_Number(2), _Number(-3))) == "2.0 \\left(-3.0\\right)")
  }

  "multiplication" should "juxtapose a numeric coefficient and spell out the rest" in
  {
    // `2x` is how mathematics writes it; `x \cdot y` stays explicit because in THIS grammar
    // `xy` is a single identifier, so juxtaposed output would depict an input that means
    // something else.
    assert(tex("2*x") == "2.0 x")
    assert(tex("x*y") == "x \\cdot y")
  }

  "a named function" should "render as an upright name over grouped arguments" in
  {
    assert(tex("sin(x)")      == "\\mathrm{sin}\\left(x\\right)")
    assert(tex("log(x, 2)")   == "\\mathrm{log}\\left(x, 2.0\\right)")
    assert(tex("sin(a + b)")  == "\\mathrm{sin}\\left(a + b\\right)",
           "a function's own delimiters are a Grouped slot")
  }

  "a node with no rule yet" should "fall back to escaped source rather than to broken LaTeX" in
  {
    val out = tex("derive(x^2, x)")
    assert(out.startsWith("\\mathrm{"), s"fallback must be upright source text, got: $out")
    assert(!out.contains("^") || out.contains("\\textasciicircum"),
           s"a raw ^ in a fallback is a LaTeX error, got: $out")
  }

  "escaping" should "neutralise every character LaTeX reads as markup" in
  {
    val out = ToLatex.escape("""a^b_c{d}e\f~g%h&i#j$k""")
    for expected <- List("\\textasciicircum{}", "\\textasciitilde{}", "\\textbackslash{}",
                         "\\_", "\\{", "\\}", "\\%", "\\&", "\\#", "\\$") do
      assert(out.contains(expected), s"missing $expected in: $out")
    // And the letters between them survive untouched, in order.
    assert("abcdefghijk".forall(out.contains), s"payload characters lost in: $out")
  }

  // ── the properties: what must hold for EVERY rendering ──────────────────────────────

  /** A broad sample of what the parser builds — covered nodes, fallback nodes, and mixtures,
   *  so the properties are exercised across the boundary between the two.
   */
  private val corpus = List(
    "x", "2.5", "-2.5", "a + b - c", "2*x^2 - 3*x + 1", "(a+b)/(c-d)",
    "1/(1 + 1/(1 + 1/x))", "((a+b)*(c+d))^(n+1)", "-(a+b)/2",
    "sin(x)^2 + cos(x)^2", "exp(-x^2/2)", "ln(x/y)", "Gamma(n+1)",
    "tan(2*x + 1)", "fact(n)", "binom(n, k)",
    "derive(sin(x)^2, x)", "integral(1/x, x)", "limit(sin(x)/x, x, 0)",
    "laplace(t^2, t, s)", "solve(x^2 - 4 > 0, x)", "[[1, 2], [3, 4]]",
    "det([[a, b], [c, d]])", "a and not b", "x < 2 or x > 3",
    "normal(0, 1)", "fib(10)", "0xFF + 1", "sqrtish123 * x"
  )

  private def balanced(s: String, open: Char, close: Char): Boolean =
    var depth = 0
    for c <- s do
      if c == open then depth += 1
      else if c == close then depth -= 1
      if depth < 0 then return false
    depth == 0

  "every corpus rendering" should "balance its braces and its \\left/\\right pairs" in
  {
    for src <- corpus do
      val out = tex(src)
      assert(out.nonEmpty, s"empty rendering for $src")
      assert(balanced(out, '{', '}'), s"unbalanced braces for $src: $out")
      assert("\\\\left".r.findAllIn(out).size == "\\\\right".r.findAllIn(out).size,
             s"unbalanced \\left/\\right for $src: $out")
  }

  it should "give every \\frac exactly the two arguments the macro demands" in
  {
    // \frac not followed by a brace group takes whatever single TOKEN comes next as an
    // argument -- so \frac12 renders as one-half. Not malformed, silently WRONG.
    for src <- corpus do
      val out = tex(src)
      var i   = out.indexOf("\\frac")
      while i >= 0 do
        assert(out.length > i + 5 && out(i + 5) == '{',
               s"\\frac without a braced numerator for $src: $out")
        i = out.indexOf("\\frac", i + 1)
  }

  it should "never leak an unescaped superscript or subscript outside a power" in
  {
    // A raw ^ or _ anywhere but the Power rule's own `^{` is either an escaping failure in
    // the fallback or a future rule forgetting its braces; both fail to compile as LaTeX.
    for src <- corpus do
      val out = tex(src)
      // Character scans, not regexes: the obvious lookbehind (?<!\\)_ is rejected by
      // Scala.js's Pattern below ES2018, so it passed on the JVM and threw on Node -- the
      // cross-build catching a platform split in its own harness.
      for i <- out.indices if out(i) == '^' do
        assert(i + 1 < out.length && out(i + 1) == '{',
               s"raw ^ without braces for $src: $out")
      for i <- out.indices if out(i) == '_' do
        assert(i > 0 && out(i - 1) == '\\', s"raw _ for $src: $out")
  }

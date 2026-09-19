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

  // ── step 2: the compound rules ────────────────────────────────────────────

  "a half power" should "become a radical, in each of the three shapes it arrives in" in
  {
    // The exponent reaches the renderer as a Double literal, as a Ratio of two literals, or
    // — in exact mode — as a _Rational. All three are the same square root to a reader.
    assert(tex("x^0.5")   == "\\sqrt{x}")
    assert(tex("x^(1/2)") == "\\sqrt{x}")
    assert(ToLatex(scalar.Power(_Variable("x"),
                               _Rational.fromDecimalString("0.5").get)) == "\\sqrt{x}")
  }

  it should "take its radicand ungrouped, since \\sqrt brackets for itself" in
  {
    // The same argument as \frac: the radical's own rule already delimits, so transcribing
    // toString's parentheses would give \sqrt{\left(a + b\right)} — correct and worse.
    assert(tex("(a+b)^0.5") == "\\sqrt{a + b}")
  }

  "a unit fraction exponent" should "become an n-th root" in
  {
    assert(tex("x^(1/3)") == "\\sqrt[3]{x}")
    assert(tex("x^(1/4)") == "\\sqrt[4]{x}")
    // Not a unit fraction: 2/3 is a genuine power and must stay one.
    assert(tex("x^(2/3)") == "x^{\\frac{2.0}{3.0}}")
  }

  "an exact rational" should "render as a fraction rather than collapse to a decimal" in
  {
    // Showing 1/3 as 0.33333 in LaTeX would throw away exactly what the exact tier exists to
    // preserve. The value is reduced for display: 2/6 and 1/3 are the same number.
    def r(s: String) = ToLatex(_Rational.fromDecimalString(s).get)
    assert(r("0.5")  == "\\frac{1}{2}")
    assert(r("0.25") == "\\frac{1}{4}")
    assert(r("-0.5") == "-\\frac{1}{2}", "the sign belongs outside the fraction")
    assert(r("3")    == "3", "an integral rational is an integer, not 3/1")
  }

  it should "bracket itself where a bare fraction would be misread" in
  {
    val half = _Rational.fromDecimalString("0.5").get
    // A negative rational starts with a minus, so it is a sum-level term: `2 \cdot -\frac12`
    // must not be emitted.
    val neg = _Rational.fromDecimalString("-0.5").get
    assert(ToLatex(scalar.Product(_Number(2), neg)) == "2.0 \\left(-\\frac{1}{2}\\right)")
    // A positive one is atomic in a product but must re-bracket as a power base, exactly as
    // a Ratio does.
    assert(ToLatex(scalar.Product(half, _Variable("x"))) == "\\frac{1}{2} \\cdot x")
    assert(ToLatex(scalar.Power(half, _Number(2))) == "\\left(\\frac{1}{2}\\right)^{2.0}")
  }

  "a matrix" should "render as a pmatrix, dense or symbolic" in
  {
    assert(tex("[[1, 2], [3, 4]]") ==
           "\\begin{pmatrix} 1.0 & 2.0 \\\\ 3.0 & 4.0 \\end{pmatrix}")
    assert(tex("[[a, b], [c, d]]") ==
           "\\begin{pmatrix} a & b \\\\ c & d \\end{pmatrix}")
  }

  it should "leave its cells ungrouped and stay atomic where it is used" in
  {
    // Each cell sits in its own alignment slot, so nothing there ever needs brackets.
    assert(tex("[[a + b, c]]") == "\\begin{pmatrix} a + b & c \\end{pmatrix}")
    // \begin{pmatrix} carries its own delimiters, so a matrix is visually atomic -- even as
    // the base of a power, the one slot that brackets a \frac.
    assert(tex("[[1, 2]]^2") == "\\begin{pmatrix} 1.0 & 2.0 \\end{pmatrix}^{2.0}")
  }

  "a matrix OPERATION" should "still fall back, and that is the current boundary" in
  {
    // `2*[[1,2]]` is not a Product: the parser dispatches structurally on matrix-shaped
    // operands, so it builds MatScale -- a `matrix` node with no rule yet. Pinned so the
    // boundary is visible rather than discovered, and so step 3 has a failing case to fix.
    val out = tex("2*[[1, 2]]")
    assert(out.startsWith("\\mathrm{"), s"expected the fallback for MatScale, got: $out")
  }

  // ── step 3: the binder rules ──────────────────────────────────────────────────
  //
  // These are the nine nodes NamedFunction deliberately excludes: each prints a binder its
  // `children` omit, and each has established notation that a generic `name(args)` could
  // never produce.  The binder is what makes them a separate problem and a separate step.

  "an integral" should "carry its measure, and its limits when it has them" in
  {
    assert(tex("integral(x^2, x)")       == "\\int x^{2.0} \\,dx")
    assert(tex("integral(x^2, x, 0, 1)") == "\\int_{0.0}^{1.0} x^{2.0} \\,dx")
  }

  it should "bracket a sum, because the measure alone does not delimit one" in
  {
    // `\int a + b \,dx` genuinely reads as (\int a) + b\,dx. A product needs no help, since
    // nothing can detach from it.
    assert(tex("integral(a + b, x)") == "\\int \\left(a + b\\right) \\,dx")
    assert(tex("integral(a*b, x)")   == "\\int a \\cdot b \\,dx")
  }

  "a derivative" should "render as the operator applied to a bracketed operand" in
  {
    assert(tex("derive(x^2, x)")      == "\\frac{d}{dx}\\left(x^{2.0}\\right)")
    assert(tex("derive(sin(y), y)")   == "\\frac{d}{dy}\\left(\\mathrm{sin}\\left(y\\right)\\right)")
  }

  "a limit" should "put the approach under the operator, and its direction on the point" in
  {
    assert(tex("limit(sin(x)/x, x, 0)")    ==
           "\\lim_{x \\to 0.0} \\frac{\\mathrm{sin}\\left(x\\right)}{x}")
    assert(tex("limit(1/x, x, 0, +)")      == "\\lim_{x \\to 0.0^{+}} \\frac{1.0}{x}")
    assert(tex("limit(1/x, x, 0, -)")      == "\\lim_{x \\to 0.0^{-}} \\frac{1.0}{x}")
  }

  "a transform" should "render in its own calligraphic operator with the result variable" in
  {
    // The binder is consumed by the transform and the OTHER variable is what the answer is a
    // function of -- which is precisely the distinction `children` drops and toString keeps.
    assert(tex("laplace(t, t, s)")        == "\\mathcal{L}\\left\\{t\\right\\}(s)")
    assert(tex("fourier(t, t, w)")        == "\\mathcal{F}\\left\\{t\\right\\}(w)")
    assert(tex("invlaplace(1/s, s, t)")   ==
           "\\mathcal{L}^{-1}\\left\\{\\frac{1.0}{s}\\right\\}(t)")
    assert(tex("ztrans(n, n, z)")         == "\\mathcal{Z}\\left\\{n\\right\\}(z)")
    assert(tex("invztrans(z, z, n)")      == "\\mathcal{Z}^{-1}\\left\\{z\\right\\}(n)")
  }

  "a calculus binder" should "never lose the variable its children omit" in
  {
    // The whole reason these nodes are excluded from NamedFunction: rendering from `children`
    // alone would emit the integrand and silently drop the `dx`. Asserted structurally so it
    // holds however the surrounding notation is later polished.
    for (src, binder) <- List("integral(q, u)" -> "u", "derive(q, u)" -> "u",
                              "limit(q, u, 0)" -> "u") do
      assert(tex(src).contains(binder), s"binder '$binder' lost from $src: ${tex(src)}")
  }

  "a transform" should "consume its binder and name the result variable instead" in
  {
    // The opposite of the rule above, and not an exception to it: a transform INTEGRATES its
    // binder away, so `t` is genuinely absent from L{q}(s) -- the answer is a function of s.
    // Writing the binder into the output would be wrong, not merely redundant.
    val out = tex("laplace(q, u, s)")
    assert(out == "\\mathcal{L}\\left\\{q\\right\\}(s)")
    assert(!out.contains("u"), s"a consumed binder must not appear: $out")
  }

  "a node with no rule yet" should "fall back to escaped source rather than to broken LaTeX" in
  {
    // A solver node, chosen because it is squarely outside the emitter's scope and carries a
    // `^` to exercise the escaping. (Was `derive(x^2, x)` until step 3 gave derivatives a
    // rule -- a fallback example has to be a node nothing has claimed yet.)
    val out = tex("solve(x^2 = 4, x)")
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

  /** Whether `open`/`close` nest correctly: never negative, and zero at the end. */
  private def balanced(s: String, open: Char, close: Char): Boolean =
    val depths = s.scanLeft(0)((d, c) => if c == open then d + 1 else if c == close then d - 1 else d)
    depths.forall(_ >= 0) && depths.last == 0

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
      // A `_` is legitimate when it opens a structural subscript -- `\lim_{…}`, `\int_{…}` --
      // so the test is the same as the one above: braces must follow. Anything else is an
      // escaping failure. (Tightened at step 3: until the binders landed, nothing emitted a
      // subscript at all, and the rule could afford to forbid `_` outright.)
      for i <- out.indices if out(i) == '_' do
        assert((i > 0 && out(i - 1) == '\\') || (i + 1 < out.length && out(i + 1) == '{'),
               s"raw _ for $src: $out")
  }

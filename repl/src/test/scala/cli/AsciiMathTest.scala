package it.grypho.scala.leonardo
package cli

import org.scalatest.flatspec.AnyFlatSpec

import parser.Parser

/** Issue F_0017 slice 2 - the AsciiMath reader.
 *
 *  **The source is MathLive's `convertLatexToAsciiMath`**, so every input here is written the
 *  way that converter emits it (spaces and all), not the way a person would type AsciiMath.
 *  The reader's job is to turn that into Leonardo's own grammar or to **refuse**, and the
 *  refusal half is the important one: the grammar has no unknown-identifier error, so anything
 *  passed through unrecognised becomes a silent product rather than a complaint (F_0030).
 *
 *  Every accepted case is asserted twice - the exact text, and that `Parser.parse` accepts it -
 *  because a reader whose output merely *looks* right is the failure this suite exists to catch.
 */
class AsciiMathTest extends AnyFlatSpec:

  /** The grammar text, or the refusal message, as one string for a compact expectation. */
  private def read(source: String): String = AsciiMath.toGrammar(source).merge

  /** Asserts the conversion AND that the grammar accepts what came out.
   *
   *  **Whitespace is not preserved**, deliberately: AsciiMath's spacing is presentational
   *  (` int  x d x`, `sin (x)`) and the grammar's is not, so the reader drops it and lets the
   *  emitter decide.  Expectations here are therefore written as the reader emits them.
   */
  private def converts(source: String, expected: String): Unit =
    AsciiMath.toGrammar(source) match
      case Left(message) => fail(s"refused '$source': $message")
      case Right(text) =>
        assert(text == expected, s"converting '$source'")
        assert(Parser.parse(text).successful, s"'$text' must parse")

  // --- the arithmetic core, which is already the grammar -------------------------------

  "the arithmetic core" should "pass through unchanged, because AsciiMath already spells it" in
  {
    converts("(a+b)/(c+d)", "(a+b)/(c+d)")
    converts("x^2", "x^2")
    converts("2x", "2x")
    converts("theta+alpha", "theta+alpha")
    converts("2 * (1)/(3)", "2*(1)/(3)")
    converts("-3k", "-3k")
  }

  it should "carry the relations, including the Unicode MathLive emits" in
  {
    converts("x<=2", "x<=2")
    converts("a!=b", "a!=b")
    converts("a≠b", "a!=b")      // MathLive writes \ne as the Unicode sign
    converts("x≤2", "x<=2")
    converts("x≥2", "x>=2")
  }

  // --- functions, where the spellings differ -------------------------------------------

  "a function call" should "lose the space AsciiMath puts before the bracket" in
  {
    // NOT cosmetic: the grammar matches the literal "sin(", so `sin (x)` does not parse at all.
    converts("sin (x)", "sin(x)")
    converts("cos (2x)", "cos(2x)")
  }

  it should "use the grammar's spelling where mathematics and the grammar disagree" in
  {
    converts("arcsin (x)", "asin(x)")
    converts("arccos (x)", "acos(x)")
    converts("arctan (x)", "atan(x)")
  }

  it should "be refused when the grammar has no such function" in
  {
    // The whole point: passing this through would give a product with a free variable, which
    // is an answer to a different question rather than an error.  (`abs` was the example
    // here until F_0036 made it a real function -- itself a measure of how visible the gap
    // was; `floor` is today's plausible-but-absent name.)
    assert(read("floor(x)").contains("floor"))
    assert(read("floor(x)").contains("not a function"))
    assert(read("foo(x)").contains("not a function"))
  }

  // --- the shapes that need rewriting ---------------------------------------------------

  "a radical" should "become a fractional exponent, since the grammar has no sqrt" in
  {
    converts("sqrt(x)", "(x)^(1/2)")
    converts("sqrt(x+1)", "(x+1)^(1/2)")
    converts("root(3)(x)", "(x)^(1/3)")
  }

  "a subscripted log" should "become the two-argument call" in
  {
    converts("log _2(x)", "log(x, 2)")
    converts("log _10(x+1)", "log(x+1, 10)")
  }

  "a matrix" should "become the bracket form" in
  {
    converts("((1,2),(3,4))", "[[1, 2], [3, 4]]")
    converts("((1,2))", "[[1, 2]]")
  }

  it should "not mistake ordinary nested brackets for one" in
  {
    converts("((a+b))", "((a+b))")
  }

  // --- the calculus notations (F_0033) ---------------------------------------------------
  //
  // Every input below is `convertLatexToAsciiMath`'s recorded output for the LaTeX a math
  // field holds, re-probed 2026-09-23 against the vendored build -- parsed shapes, never
  // guessed ones, which is the condition F_0033 set for lifting the refusal.

  "an integral" should "close over its differential" in
  {
    converts(" int  x d x", "integral(x, x)")
    converts(" int  sin (x) d x", "integral(sin(x), x)")
    converts(" int  x^2+1 d x", "integral(x^2+1, x)")
  }

  it should "carry its bounds, glued to the integrand exactly as MathLive writes them" in
  {
    converts(" int  _a^b x d x", "integral(x, x, a, b)")
    converts(" int  _0^1x^2 d x", "integral(x^2, x, 0, 1)")
    converts(" int  _0^1sin (2x) d x", "integral(sin(2x), x, 0, 1)")
  }

  it should "nest an iterated integral, each differential closing the innermost" in
  {
    converts(" int   int  x y d x d y", "integral(integral(x*y, x), y)")
  }

  it should "be refused when no differential closes it" in
  {
    val message = read(" int  x")
    assert(message.contains("integral"), s"the refusal must name the spelling: $message")
  }

  "a derivative" should "be read from the d/dx fraction, which no ordinary quotient spells" in
  {
    converts("(d)/(d x)sin (x)", "derive(sin(x), x)")
    converts("(d)/(d x)(x^2+1)", "derive((x^2+1), x)")
  }

  it should "nest a higher order, since the grammar's derive takes one variable" in
  {
    converts("(d^2)/(d x^2)sin (x)", "derive(derive(sin(x), x), x)")
  }

  it should "read the partial-derivative glyph the same way" in
  {
    converts("(∂)/(∂x)x y", "derive(x*y, x)")
  }

  "a limit" should "be read from its subscript, direction and infinity included" in
  {
    converts("lim _(x->0)(sin (x))/(x)", "limit((sin(x))/(x), x, 0)")
    converts("lim _(x->0^+)(1)/(x)", "limit((1)/(x), x, 0, +)")
    converts("lim _(x->oo)(1)/(x)", "limit((1)/(x), x, inf)")
    converts("lim _(x->a)f", "limit(f, x, a)")
  }

  "an absolute value" should "become abs(...), from every bar MathLive spells" in
  {
    // Recorded 2026-09-23: `\left|x\right|` => "|x|", `\lvert x+1\rvert` => the Unicode bar.
    converts("|x|", "abs(x)")
    converts("|x+1|", "abs(x+1)")
    converts("∣x+1∣", "abs(x+1)")
    converts("|(a)/(b)|", "abs((a)/(b))")           // `\left|\frac{a}{b}\right|`
    converts("|2+3i|", "abs(2+3i)")                 // the modulus is the same notation
    converts("|a|-|b|", "abs(a)-abs(b)")            // sequential pairing, bar by bar
    converts("|x+(|y|)|", "abs(x+(abs(y)))")        // brackets give a fresh level
  }

  it should "refuse what the bars cannot say unambiguously" in
  {
    // A bar is its own closer, so nesting without brackets is ambiguous to reparse; and the
    // norm bars mean something the grammar does not have.
    assert(read("||a|-|b||").contains("abs"), read("||a|-|b||"))
    assert(read("|a").contains("unbalanced"), read("|a"))
    assert(read("∥x∥").contains("norm"), read("∥x∥"))
  }

  "juxtaposed names" should "become a product, never one identifier" in
  {
    // Pre-existing and found by the integral cases: dropping ALL whitespace turned MathLive's
    // `x y` into the single variable `xy` -- a confidently wrong answer, the exact failure
    // this reader exists to prevent. Two adjacent Name tokens are always a product in
    // AsciiMath, because a multi-character name arrives as ONE token.
    converts("x y", "x*y")
    converts("2x", "2x")          // a digit cannot continue an identifier: unchanged
  }

  // --- what it refuses, and why ----------------------------------------------------------

  "a summation" should "become the grammar's reduction (F_0037)" in
  {
    // Recorded from `convertLatexToAsciiMath`: the subscript is a group `(k=1)` and the
    // superscript a bare token, so the spec parses exactly like `lim`'s.
    converts(" sum  _(k=1)^n k^2", "sum(k^2, k, 1, n)")
    converts(" prod  _(k=1)^5k", "product(k, k, 1, 5)")
    converts(" sum  _(k=0)^4(((4) choose (k)))", "sum(((binom(4, k))), k, 0, 4)")
  }

  it should "refuse a reduction whose index shape is missing" in
  {
    // Without `k=lo` there is no index and no lower bound to recover -- the `lim` rule again.
    for source <- List(" sum  k", " sum  _k^n k") do
      assert(read(source).contains("sum(f, k, lo, hi)"), s"'$source': ${read(source)}")
  }

  "a binder without its shape" should "still be refused with the grammar's spelling" in
  {
    for (source, word) <- List("lim x" -> "limit", " oint  f d z" -> "integral") do
      val message = read(source)
      assert(message.contains(word), s"'$source' should name the grammar spelling '$word': $message")
  }

  it should "refuse the transform notation, which carries braces the grammar never uses" in
  {
    val message = read("L{f}(s)")
    assert(AsciiMath.toGrammar("L{f}(s)").isLeft)
    assert(message.contains("laplace"), s"should name the grammar spelling: $message")
  }

  "a refusal" should "be a Left, so the page can show it rather than submit nonsense" in
  {
    assert(AsciiMath.toGrammar("foo(x)").isLeft)
    assert(AsciiMath.toGrammar("sin (x)").isRight)
  }

  "an empty source" should "convert to nothing rather than fail" in
  {
    // Not run through the parser: an empty string is not an expression, and the page simply
    // has nothing to submit.  A refusal here would be noise on an untouched math field.
    assert(AsciiMath.toGrammar("") == Right(""))
    assert(AsciiMath.toGrammar("   ") == Right(""))
  }

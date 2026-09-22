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
    // is an answer to a different question rather than an error.
    assert(read("abs(x)").contains("abs"))
    assert(read("abs(x)").contains("not a function"))
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

  // --- what it refuses, and why ----------------------------------------------------------

  "a binder" should "be refused with the grammar's spelling, not guessed at" in
  {
    // MathLive emits ` int  x d x` and `lim _(x->0)...`; pattern-matching those out of an
    // undocumented intermediate form is exactly what this reader declines to do.
    for (source, word) <- List(" int  x d x" -> "integral", "lim _(x->0)(sin x)/(x)" -> "limit") do
      val message = read(source)
      assert(message.contains("not accepted"), source)
      assert(message.contains(word), s"'$source' should name the grammar spelling '$word'")
  }

  it should "refuse the transform notation, which carries braces the grammar never uses" in
  {
    val message = read("L{f}(s)")
    assert(AsciiMath.toGrammar("L{f}(s)").isLeft)
    assert(message.contains("laplace"), s"should name the grammar spelling: $message")
  }

  "a refusal" should "be a Left, so the page can show it rather than submit nonsense" in
  {
    assert(AsciiMath.toGrammar("abs(x)").isLeft)
    assert(AsciiMath.toGrammar("sin (x)").isRight)
  }

  "an empty source" should "convert to nothing rather than fail" in
  {
    // Not run through the parser: an empty string is not an expression, and the page simply
    // has nothing to submit.  A refusal here would be noise on an untouched math field.
    assert(AsciiMath.toGrammar("") == Right(""))
    assert(AsciiMath.toGrammar("   ") == Right(""))
  }

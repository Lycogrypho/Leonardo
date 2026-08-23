package it.grypho.scala.leonardo
package equation

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** Comparison operators — issue 4.R slice A. */
class ComparisonTest extends AnyFlatSpec:

  private val env = new Environment()

  private def parse(s: String): _Expression =
    Parser.parse(s) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for \"$s\": $other")

  private def bool(s: String, e: Environment = env): Boolean =
    parse(s).eval(e) match
      case Right(_Bool(b)) => b
      case other           => fail(s"\"$s\" did not reduce to a boolean: $other")

  // --- the operators themselves ---

  "the comparison operators" should "evaluate on numbers" in
  {
    assert(bool("2 > 1") && !bool("2 < 1"))
    assert(bool("1 < 2") && !bool("1 > 2"))
    assert(bool("2 >= 2") && bool("2 <= 2") && bool("3 >= 2") && bool("1 <= 2"))
    assert(!bool("2 > 2") && !bool("2 < 2"))
    assert(bool("1 != 2") && !bool("2 != 2"))
  }

  it should "tokenise longest-first, so >= is not > followed by =" in
  {
    // The failure mode this guards: `a >= b` matching `>` and then choking on the `=`.
    assert(parse("3 >= 2").isInstanceOf[_Comparison])
    assert(parse("3 <= 2").isInstanceOf[_Comparison])
    assert(parse("3 != 2").isInstanceOf[_Comparison])
    assert(parse("3 == 2").isInstanceOf[_EqualityCheck])
    assert(parse("3 = 2").isInstanceOf[_Equation])
  }

  it should "stay symbolic while an operand is free, and reduce once bound" in
  {
    assert(parse("x <= 3").eval(env).isLeft)
    assert(bool("x <= 3", env.withBinding("x", _Number(2.0))))
    assert(!bool("x <= 3", env.withBinding("x", _Number(4.0))))
  }

  it should "be non-associative, like =" in
  {
    // Chained comparisons are a separate feature; a half-supported `0 < x < 1` would be
    // worse than a clean parse error.
    assert(!Parser.parse("0 < x < 1").successful)
    assert(!Parser.parse("a = b = c").successful)
  }

  it should "round-trip through toString" in
  {
    for s <- List("x < 2", "x > 2", "x <= 2", "x >= 2", "x != 2", "x == 2", "x = 2") do
      assert(parse(parse(s).toString) == parse(s), s"round-trip failed for $s")

    // The case that actually bit.  Because a comparison does NOT distribute, it survives
    // inside a product -- and an unparenthesised rendering came back as `(2x) < 1`, a
    // different expression.  That is why _Comparison.toString is fully parenthesised.
    for s <- List("2 * (x < 1)", "(x < 1) * 2", "not (x < 1)", "(x < 1) and (y > 2)") do
      assert(parse(parse(s).toString) == parse(s), s"nested round-trip failed for $s")
  }

  // --- the trichotomy property ---

  "exactly one of <, == and >" should "hold for any comparable pair" in
  {
    // The property the whole relation tier rests on. It only holds because `ordering` and
    // `compareSides` agree about what "equal" means -- including inside the tolerance band,
    // where a naive strict `<` would report both `a == b` and `a < b`.
    val tol = 0.5 * math.pow(10, -env.precision)   // 5e-6 at the default precision
    val pairs = List((1.0, 2.0), (2.0, 1.0), (2.0, 2.0), (0.0, -1.0),
                     (1.0, 1.0 + tol / 2), (1.0, 1.0 + tol * 10), (1e8, 1e8 + 1.0))
    for (a, b) <- pairs do
      val lt = bool(s"$a < $b")
      val eq = bool(s"$a == $b")
      val gt = bool(s"$a > $b")
      assert(List(lt, eq, gt).count(identity) == 1,
             s"trichotomy failed for $a, $b: < = $lt, == = $eq, > = $gt")
      // ...and the non-strict forms are exactly the disjunctions.
      assert(bool(s"$a <= $b") == (lt || eq), s"<= inconsistent at $a, $b")
      assert(bool(s"$a >= $b") == (gt || eq), s">= inconsistent at $a, $b")
      assert(bool(s"$a != $b") == !eq,        s"!= inconsistent at $a, $b")
  }

  it should "agree with equality inside the tolerance band" in
  {
    // Two values a hair apart are EQUAL at the session precision, so neither < nor > holds.
    assert(bool("1 == 1.000001"), "within 5e-6 at precision 5")
    assert(!bool("1 < 1.000001"), "...so < must not hold either")
    assert(bool("1 <= 1.000001"))
  }

  // --- exact operands ---

  "exact operands" should "compare exactly, without the display tolerance" in
  {
    // 1/3 = 0.3333... is genuinely greater than 0.33333, and in the exact tier the
    // comparison should say so rather than calling them equal at precision 5.
    def exact(s: String): Either[_Expression, _Value] =
      Parser.parse(s, Some(30)) match
        case Parser.Success(e, _) => e.eval(new Environment(workingPrecision = 30))
        case other                => fail(s"parse failed: $other")

    assert(exact("1/3 > 0.33333") == Right(_Bool(true)))
    assert(exact("1/3 == 0.33333") == Right(_Bool(false)), "exact values are not tolerantly equal")
    assert(exact("1/3 != 0.33333") == Right(_Bool(true)))
    assert(exact("1/3 == 2/6") == Right(_Bool(true)), "...but equal values still are")
    assert(exact("1/3 < 1/2") == Right(_Bool(true)))
  }

  // --- what has no order ---

  "a complex operand" should "leave an ordering symbolic but let != reduce" in
  {
    // There is no natural total order on the complex numbers, so `<` must not invent one.
    assert(parse("(2 + 3i) < 1").eval(env).isLeft)
    assert(parse("(2 + 3i) > 1").eval(env).isLeft)
    // Inequality is well defined regardless.
    assert(bool("(2 + 3i) != 1"))
    assert(!bool("(2 + 3i) != (2 + 3i)"))
  }

  "a matrix operand" should "leave an ordering symbolic but let != reduce" in
  {
    assert(parse("[[1,2]] < [[3,4]]").eval(env).isLeft)
    assert(bool("[[1,2]] != [[3,4]]"))
    assert(!bool("[[1,2]] != [[1,2]]"))
  }

  // --- the _ElementWise trap ---

  "a comparison" should "NOT distribute over its sides" in
  {
    // _Equation and _EqualityCheck are _ElementWise, so `2 * (x = 1)` really does become
    // `2x = 2`. For an inequality that is invalid -- a negative multiplier flips the
    // direction -- so _Comparison deliberately omits the marker. This pins that.
    val distributed = parse("2 * (x = 1)").eval(env).fold(_.toString, _.toString)
    assert(distributed.contains("="), s"sanity: equations do distribute, got $distributed")

    // Assert the STRUCTURE, not the rendering.  The first version of this test matched on
    // toString and "failed" because `Product(2, x<1)` printed as `(2.0 * x < 1.0)` -- which
    // was not distribution at all, but did expose a genuine round-trip bug (see below).
    parse("2 * (x < 1)").eval(env) match
      case Left(scalar.Product(_, _: _Comparison)) => ()
      case Left(other) => fail(s"the comparison was rewritten: $other")
      case other       => fail(s"expected a symbolic result, got $other")
  }

  it should "not be solvable" in
  {
    // solve() requires an _Equation; inequality solving would need interval-valued
    // solutions, which the solver has no carrier for.
    assert(parse("solve(x < 2, x)").eval(env).isLeft)
  }

  // --- composition with the logic tier ---

  "comparisons" should "compose with the boolean connectives" in
  {
    // The payoff: `and`/`or`/`not` already exist and take untyped operands, so this needed
    // no change to the logic package at all.
    assert(bool("1 < 2 and 3 > 2"))
    assert(!bool("1 < 2 and 3 < 2"))
    assert(bool("1 > 2 or 3 > 2"))
    assert(bool("not (1 > 2)"))
    assert(bool("1 < 2 implies 2 < 3"))
    val e = env.withBinding("x", _Number(5.0))
    assert(bool("x > 0 and x < 10", e))
  }

  it should "work inside function arguments, where relations already nested" in
  {
    assert(parse("2 * (x < 1)").eval(env).isLeft)   // parses at all
    assert(bool("not (2 < 1)"))
  }

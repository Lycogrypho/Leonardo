package it.grypho.scala.leonardo
package parser

import core.*
import scalar.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/**
 * Back-and-forth (round-trip) tests:
 *
 *   string â”€â”€parseâ”€â”€â–¶ AST â”€â”€toStringâ”€â”€â–¶ string â”€â”€parseâ”€â”€â–¶ AST'
 *
 * The parser produces a canonical AST, so toString does not reproduce the exact
 * input ("-2" becomes "(-1.0 * 2.0)", "1" becomes "1.0", etc.). What MUST hold is
 * that the printed form re-parses to something *equivalent*. Two invariants are
 * checked:
 *
 *   1. Eval-equivalence: parse(s) and parse(parse(s).toString) evaluate to the
 *      same result under the same variable bindings. This is the meaningful
 *      "or something equivalent" property.
 *
 *   2. String fixpoint: for expressions free of the unary-minus-on-literal quirk,
 *      toString is a fixpoint of (parse âˆ˜ toString) â€” printing the re-parsed AST
 *      yields the identical string. This is a stronger, purely syntactic check.
 */
class RoundTripTest extends AnyFlatSpec:

  def parse(input: String): _Expression =
    val result = Parser.parse(input)
    assert(result.successful, s"parse failed for \"$input\": $result")
    result.get

  def evalUnder(ast: _Expression, bindings: Seq[(String, Double)]): Either[_Expression, _Value] =
    val e = bindings.foldLeft(new Environment())((en, kv) => en.withBinding(kv._1, _Number(kv._2)))
    ast.eval(e)

  /** Parse, print, re-parse, and assert the two ASTs evaluate equivalently. */
  def assertEquivalent(input: String, bindings: (String, Double)*): Unit =
    val ast1    = parse(input)
    val printed = ast1.toString
    val pr      = Parser.parse(printed)
    assert(pr.successful, s"toString output did not re-parse: \"$printed\" ($pr)")
    val ast2 = pr.get
    (evalUnder(ast1, bindings), evalUnder(ast2, bindings)) match
      case (Right(_Number(a)), Right(_Number(b))) =>
        assert(math.abs(a - b) < 1e-6,
          s"round-trip changed value of \"$input\" -> \"$printed\": $a vs $b")
      case (Left(a), Left(b)) =>
        assert(a == b,
          s"round-trip changed symbolic form of \"$input\" -> \"$printed\": $a vs $b")
      case (l, r) =>
        fail(s"round-trip changed evaluated-ness of \"$input\" -> \"$printed\": $l vs $r")

  // input, then the variable bindings used to evaluate both sides
  val equivalenceCases: List[(String, List[(String, Double)])] = List(
    ("1",                       Nil),
    ("1 + 2",                   Nil),
    ("10 - 3",                  Nil),
    ("3 * 4",                   Nil),
    ("10 / 4",                  Nil),
    ("2 + 3 * 4",               Nil),
    ("(2 + 3) * 4",             Nil),
    ("-2",                      Nil),
    ("-3 + 5",                  Nil),
    ("exp(0)",                  Nil),
    ("log(1)",                  Nil),
    ("sin(0)",                  Nil),
    ("cos(0)",                  Nil),
    ("tg(0)",                   Nil),
    ("pow(2, 10)",              Nil),
    ("2 ^ 10",                  Nil),
    ("2 ^ 3 ^ 2",              Nil),
    ("x ^ 2",                   List("x" -> 4.0)),
    ("exp(x)",                  List("x" -> 1.0)),
    ("sin(a) + cos(a)",         List("a" -> 0.0)),
    ("3a",                      List("a" -> 2.0)),
    ("3sin(a)",                 List("a" -> 0.0)),
    ("sin(a)cos(b)",            List("a" -> 0.5, "b" -> 1.2)),
    ("tg(x + 2)",               List("x" -> 0.3)),
    ("pow(x, 2)",               List("x" -> 3.0)),
    ("-3k",                     List("k" -> 4.0)),
    ("exp(cos(a))",             List("a" -> 1.1)),
    ("derive(x * x, x)",        List("x" -> 3.0)),
    ("derive(sin(x), x)",       List("x" -> 0.0)),
    ("derive(cos(3x), x)",      List("x" -> 0.4)),
    ("integral(x, x, 0, 1)",    Nil),
    ("integral(x * x, x, 0, 1)", Nil)
  )

  for (input, bindings) <- equivalenceCases do
    s"round-trip (eval) of \"$input\"" should "preserve meaning" in
    {
      assertEquivalent(input, bindings*)
    }

  // --- symbolic expressions that stay symbolic on both sides ---

  "round-trip of unbound \"x + y\"" should "stay equal and symbolic" in
  {
    assertEquivalent("x + y")
  }

  "round-trip of indefinite \"integral(x, x)\"" should "stay equal and symbolic" in
  {
    assertEquivalent("integral(x, x)")
  }

  // --- string fixpoint: toString output is stable under re-parse + re-print ---
  // (excludes inputs with unary minus on a literal, whose canonical form is not
  //  a fixpoint: "-2" -> "(-1.0 * 2.0)" -> "(-1.0 * (1.0 * 2.0))" -> ...)

  val fixpointCases: List[String] = List(
    "1",
    "1 + 2",
    "3 * 4",
    "10 / 4",
    "2 + 3 * 4",
    "3a",
    "sin(a)cos(b)",
    "exp(x)",
    "exp(cos(a))",
    "tg(x + 2)",
    "pow(2, 10)",
    "pow(x, 2)",
    "2 ^ 10",
    "2 ^ 3 ^ 2",
    "x ^ 2",
    "derive(x * x, x)",
    "integral(x, x, 0, 1)",
    "integral(x, x)"
  )

  for input <- fixpointCases do
    s"toString of parsed \"$input\"" should "be a fixpoint of (parse âˆ˜ toString)" in
    {
      val printed1 = parse(input).toString
      val printed2 = parse(printed1).toString
      assert(printed1 == printed2,
        s"toString not stable for \"$input\": \"$printed1\" vs \"$printed2\"")
    }

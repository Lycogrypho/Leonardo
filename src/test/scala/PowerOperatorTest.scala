package it.grypho.scala.leonardo

import core.*
import scalar.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/**
 * Semantics of the infix `^` operator: right-associativity, precedence relative
 * to `*` `/` `+`, equivalence with the `pow(...)` function form, and interaction
 * with implicit multiplication and parentheses.
 */
class PowerOperatorTest extends AnyFlatSpec:

  def parse(input: String): _Expression =
    val r = Parser.parse(input)
    assert(r.successful, s"parse failed for \"$input\": $r")
    r.get

  def evalNum(input: String, bindings: (String, Double)*): Double =
    val e = new Environment()
    bindings.foreach((name, value) => e.assign(name, _Number(value)))
    parse(input).eval(e) match
      case Right(_Number(x)) => x
      case other             => fail(s"expected a numeric result but got: $other")

  "2 ^ 10" should "evaluate to 1024" in
  {
    assert(evalNum("2 ^ 10") == 1024.0)
  }

  "the ^ operator" should "be right-associative: 2 ^ 3 ^ 2 = 2 ^ (3 ^ 2) = 512" in
  {
    assert(evalNum("2 ^ 3 ^ 2") == 512.0)
  }

  it should "bind tighter than * on the right: 2 ^ 3 * 4 = 32" in
  {
    assert(evalNum("2 ^ 3 * 4") == 32.0)
  }

  it should "bind tighter than * on the left: 2 * 3 ^ 2 = 18" in
  {
    assert(evalNum("2 * 3 ^ 2") == 18.0)
  }

  it should "bind tighter than +: 1 + 2 ^ 3 = 9" in
  {
    assert(evalNum("1 + 2 ^ 3") == 9.0)
  }

  "pow(2, 3) and 2 ^ 3" should "parse to the same AST" in
  {
    assert(parse("pow(2, 3)") == parse("2 ^ 3"))
  }

  "a parenthesised base (1 + 1) ^ 3" should "evaluate to 8" in
  {
    assert(evalNum("(1 + 1) ^ 3") == 8.0)
  }

  "a parenthesised exponent 2 ^ (1 + 2)" should "evaluate to 8" in
  {
    assert(evalNum("2 ^ (1 + 2)") == 8.0)
  }

  "implicit multiplication around ^ : 3x^2 at x=2" should "evaluate to 12" in
  {
    assert(evalNum("3x^2", "x" -> 2.0) == 12.0)
  }

  "x ^ 2 at x=5" should "evaluate to 25" in
  {
    assert(evalNum("x ^ 2", "x" -> 5.0) == 25.0)
  }

  "a negated power -2 ^ 2" should "evaluate to -4 (^ binds tighter than unary minus)" in
  {
    assert(evalNum("-2 ^ 2") == -4.0)
  }

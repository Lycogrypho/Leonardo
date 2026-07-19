package it.grypho.scala.leonardo
package equation

import core.*
import scalar.*
import equation.*
import parser.Parser
import cli.Session
import org.scalatest.flatspec.AnyFlatSpec


class EquationTest extends AnyFlatSpec:

  val x = _Variable("x")
  val y = _Variable("y")

  def parse(input: String): _Expression =
    val result = Parser.parse(input)
    assert(result.successful, s"parse failed for \"$input\": $result")
    result.get

  def envWith(bindings: (String, Double)*): Environment =
    bindings.foldLeft(new Environment())((e, kv) => e.withBinding(kv._1, _Number(kv._2)))

  def evalBool(e: _Expression, env: Environment = new Environment()): Boolean =
    e.eval(env) match
      case Right(_Bool(b)) => b
      case other           => fail(s"expected a _Bool but got: $other")

  // --- parsing ---

  "\"10 * x = 2 * x + 1\"" should "parse to an _Equation" in
  {
    parse("10 * x = 2 * x + 1") match
      case _Equation(Product(_, _), Sum(_, _)) => succeed
      case other => fail(s"unexpected shape: $other")
  }

  "an equation" should "round-trip through toString" in
  {
    val first = parse("10 * x = 2 * x + 1")
    assert(parse(first.toString) == first)
  }

  "\"a = b = c\"" should "fail to parse (= is non-associative)" in
  {
    assert(!Parser.parse("a = b = c").successful)
  }

  "\"(a = b)\"" should "parse to an _Equation (equations are now valid sub-expressions)" in
  {
    parse("(a = b)") match
      case _Equation(_Variable("a"), _Variable("b")) => succeed
      case other => fail(s"unexpected: $other")
  }

  // --- _EqualityCheck (==) ---

  "\"x == 5\"" should "parse to an _EqualityCheck" in
  {
    parse("x == 5") match
      case _EqualityCheck(_Variable("x"), _Number(5.0)) => succeed
      case other => fail(s"unexpected: $other")
  }

  "an _EqualityCheck" should "round-trip through toString" in
  {
    val first = parse("x + 1 == y * 2")
    assert(parse(first.toString) == first)
  }

  "x == 5 at x = 5" should "evaluate to true" in
  {
    assert(evalBool(parse("x == 5"), envWith("x" -> 5.0)))
  }

  "x == 5 at x = 3" should "evaluate to false" in
  {
    assert(!evalBool(parse("x == 5"), envWith("x" -> 3.0)))
  }

  "sin(pi) == 0" should "be true under the precision tolerance" in
  {
    assert(evalBool(parse("sin(pi) == 0")))
  }

  "an _EqualityCheck with a free variable" should "stay symbolic" in
  {
    parse("x == 5").eval(new Environment()) match
      case Left(_EqualityCheck(_, _)) => succeed
      case other => fail(s"expected symbolic _EqualityCheck but got: $other")
  }

  "simplify of an _EqualityCheck" should "simplify both sides" in
  {
    assert(simplify(_EqualityCheck(Sum(x, _Number(0)), Product(_Number(1), y)))
      == _EqualityCheck(x, y))
  }

  // --- evaluation to _Bool ---

  "10x = 2x + 1 at x = 0.125" should "evaluate to true" in
  {
    assert(evalBool(parse("10 * x = 2 * x + 1"), envWith("x" -> 0.125)))
  }

  "10x = 2x + 1 at x = 1" should "evaluate to false" in
  {
    assert(!evalBool(parse("10 * x = 2 * x + 1"), envWith("x" -> 1.0)))
  }

  "sin(pi) = 0" should "be true under the precision tolerance" in
  {
    assert(evalBool(parse("sin(pi) = 0")))
  }

  "the equality tolerance" should "follow env.precision" in
  {
    val eq = _Equation(_Number(1.0000001), _Number(1))
    assert(evalBool(eq, new Environment()))                    // precision 5: equal
    assert(!evalBool(eq, new Environment(precision = 8)))      // precision 8: distinct
  }

  "an equation with a free variable" should "stay symbolic with both sides reduced" in
  {
    _Equation(Sum(_Number(1), _Number(2)), x).eval(new Environment()) match
      case Left(_Equation(_Number(3.0), v)) => assert(v == x)
      case other => fail(s"expected symbolic equation but got: $other")
  }

  "an equation over concrete matrices" should "compare element-wise" in
  {
    assert(evalBool(parse("[[1, 2]] = [[1, 2]]")))
    assert(!evalBool(parse("[[1, 2]] = [[1, 3]]")))
    assert(!evalBool(parse("[[1, 2]] = [[1], [2]]")))   // dimension mismatch â†’ false
  }

  // --- algorithms distribute over both sides (_ElementWise) ---

  "simplify of an equation" should "simplify both sides" in
  {
    assert(simplify(_Equation(Sum(x, _Number(0)), Product(_Number(1), y)))
      == _Equation(x, y))
  }

  "derive of an equation" should "differentiate both sides" in
  {
    assert(derive(_Equation(Power(x, _Number(2)), x), x)
      == _Equation(Product(_Number(2), x), _Number(1)))
  }

  // --- generic traversals ---

  "substitute" should "replace definitions inside both sides" in
  {
    val substituted = substitute(_Equation(_Variable("f"), y), Map("f" -> Sum(x, _Number(1))))
    assert(substituted == _Equation(Sum(x, _Number(1)), y))
  }

  "dependsOn" should "see variables on either side" in
  {
    assert(dependsOn(_Equation(x, _Number(1)), x))
    assert(dependsOn(_Equation(_Number(1), y), y))
    assert(!dependsOn(_Equation(x, _Number(1)), y))
  }

  // --- REPL flow ---

  "an equation in the REPL" should "evaluate to true/false once variables are bound" in
  {
    val s = Session()
    // unbound: echoes the symbolic equation with both sides reduced
    assert(s.execute("10 * x = 2 * x + 1") == "(10.0 * x) = ((2.0 * x) + 1.0)")
    s.execute("x := 0.125")
    assert(s.execute("10 * x = 2 * x + 1") == "true")
    s.execute("x := 1")
    assert(s.execute("10 * x = 2 * x + 1") == "false")
  }

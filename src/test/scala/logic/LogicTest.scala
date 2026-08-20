package it.grypho.scala.leonardo
package logic

import core.*
import scalar.*
import equation.{_Equation, _EqualityCheck}
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


class LogicTest extends AnyFlatSpec:

  val a = _Variable("a")
  val b = _Variable("b")
  val T = _Bool(true)
  val F = _Bool(false)

  def parse(input: String): _Expression =
    val result = Parser.parse(input)
    assert(result.successful, s"parse failed for \"$input\": $result")
    result.get

  def envWith(bindings: (String, Boolean)*): Environment =
    bindings.foldLeft(new Environment())((e, kv) => e.withBinding(kv._1, _Bool(kv._2)))

  def evalBool(e: _Expression, env: Environment = new Environment()): Boolean =
    e.eval(env) match
      case Right(_Bool(v)) => v
      case other           => fail(s"expected a _Bool but got: $other")

  // --- eval: full truth tables of every connective ---

  "And" should "evaluate the full boolean truth table" in
  {
    for x <- List(true, false); y <- List(true, false) do
      assert(evalBool(And(_Bool(x), _Bool(y))) == (x && y), s"And($x, $y)")
  }

  "Or" should "evaluate the full boolean truth table" in
  {
    for x <- List(true, false); y <- List(true, false) do
      assert(evalBool(Or(_Bool(x), _Bool(y))) == (x || y), s"Or($x, $y)")
  }

  "Not" should "evaluate both boolean values" in
  {
    assert(evalBool(Not(T)) == false)
    assert(evalBool(Not(F)) == true)
  }

  "Implies" should "evaluate the full boolean truth table" in
  {
    for x <- List(true, false); y <- List(true, false) do
      assert(evalBool(Implies(_Bool(x), _Bool(y))) == (!x || y), s"Implies($x, $y)")
  }

  "Xor" should "evaluate the full boolean truth table" in
  {
    for x <- List(true, false); y <- List(true, false) do
      assert(evalBool(Xor(_Bool(x), _Bool(y))) == (x != y), s"Xor($x, $y)")
  }

  // --- short-circuit: the right operand is never needed ---

  "false and (1/0 = 0)" should "short-circuit to false although the right side cannot reduce" in
  {
    // 1/0 stays symbolic, so without the short-circuit the And would stay Left
    val unreducible = _Equation(Ratio(_Number(1), _Number(0)), _Number(0))
    assert(evalBool(And(F, unreducible)) == false)
  }

  "true or (1/0 = 0)" should "short-circuit to true although the right side cannot reduce" in
  {
    val unreducible = _Equation(Ratio(_Number(1), _Number(0)), _Number(0))
    assert(evalBool(Or(T, unreducible)) == true)
  }

  "false implies (1/0 = 0)" should "short-circuit to true although the right side cannot reduce" in
  {
    val unreducible = _Equation(Ratio(_Number(1), _Number(0)), _Number(0))
    assert(evalBool(Implies(F, unreducible)) == true)
  }

  // --- partial reduction: free variables stay symbolic ---

  "a connective over a free variable" should "stay symbolic with reduced operands" in
  {
    And(a, T).eval(new Environment()) match
      case Left(And(_Variable("a"), _Bool(true))) => succeed
      case other => fail(s"unexpected result: $other")
  }

  it should "reduce once the variable is bound" in
  {
    assert(evalBool(And(a, T), envWith("a" -> true)) == true)
    assert(evalBool(And(a, T), envWith("a" -> false)) == false)
  }

  "a connective over a non-boolean value" should "stay symbolic rather than throw" in
  {
    And(_Number(1), T).eval(new Environment()) match
      case Left(And(_Number(1.0), _Bool(true))) => succeed
      case other => fail(s"unexpected result: $other")
  }

  // --- equations as operands ---

  "equations under a connective" should "reduce through the tolerant equality" in
  {
    val e = And(_Equation(_Number(1), _Number(1)), _Equation(_Number(2), _Number(3)))
    assert(evalBool(e) == false)
  }

  "an equality check under a connective" should "reduce as well" in
  {
    val e = Or(_EqualityCheck(_Number(2), _Number(3)), _EqualityCheck(_Number(1), _Number(1)))
    assert(evalBool(e) == true)
  }

  // --- parsing ---

  "\"true and false\"" should "parse to And of the boolean literals and evaluate" in
  {
    parse("true and false") match
      case And(_Bool(true), _Bool(false)) => succeed
      case other => fail(s"unexpected shape: $other")
    assert(evalBool(parse("true and false")) == false)
  }

  "\"a or b and c\"" should "parse with and binding tighter than or" in
  {
    parse("a or b and c") match
      case Or(_Variable("a"), And(_Variable("b"), _Variable("c"))) => succeed
      case other => fail(s"unexpected shape: $other")
  }

  "\"not a and b\"" should "parse as (not a) and b" in
  {
    parse("not a and b") match
      case And(Not(_Variable("a")), _Variable("b")) => succeed
      case other => fail(s"unexpected shape: $other")
  }

  "\"x = 1 and y = 2\"" should "parse as a conjunction of two equations" in
  {
    parse("x = 1 and y = 2") match
      case And(_Equation(_Variable("x"), _Number(1.0)), _Equation(_Variable("y"), _Number(2.0))) => succeed
      case other => fail(s"unexpected shape: $other")
  }

  "\"a implies b implies c\"" should "parse right-associatively" in
  {
    parse("a implies b implies c") match
      case Implies(_Variable("a"), Implies(_Variable("b"), _Variable("c"))) => succeed
      case other => fail(s"unexpected shape: $other")
  }

  "\"a xor b or c\"" should "parse with xor binding tighter than or" in
  {
    parse("a xor b or c") match
      case Or(Xor(_Variable("a"), _Variable("b")), _Variable("c")) => succeed
      case other => fail(s"unexpected shape: $other")
  }

  "\"(a and b) or c\"" should "group through the parenthesised logic branch" in
  {
    parse("(a and b) or c") match
      case Or(And(_Variable("a"), _Variable("b")), _Variable("c")) => succeed
      case other => fail(s"unexpected shape: $other")
  }

  "\"(x + 1) * 2\"" should "still parse as plain arithmetic" in
  {
    parse("(x + 1) * 2") match
      case Product(Sum(_Variable("x"), _Number(1.0)), _Number(2.0)) => succeed
      case other => fail(s"unexpected shape: $other")
  }

  "keyword prefixes" should "stay ordinary variables" in
  {
    assert(parse("andrew") == _Variable("andrew"))
    assert(parse("nota") == _Variable("nota"))
    assert(parse("truex") == _Variable("truex"))
    assert(parse("orbit") == _Variable("orbit"))
  }

  "bare connective keywords" should "be reserved words, not variables" in
  {
    assert(!Parser.parse("and").successful)
    assert(!Parser.parse("2 * or").successful)
  }

  "a deep not chain" should "fail cleanly rather than blow the stack" in
  {
    val deep = "not " * 600 + "a"
    assert(!Parser.parse(deep).successful)
  }

  // --- toString round-trip ---

  "connective toString" should "re-parse to the identical AST" in
  {
    val cases = List(
      And(a, b), Or(a, b), Not(a), Implies(a, b), Xor(a, b),
      Or(Not(And(a, b)), _Variable("c")),
      And(_Equation(_Variable("x"), _Number(1)), _Equation(_Variable("y"), _Number(2))),
      Implies(Implies(a, b), _Variable("c")),
      And(T, F)
    )
    for e <- cases do
      val printed = e.toString
      val reparsed = Parser.parse(printed)
      assert(reparsed.successful, s"toString output did not re-parse: \"$printed\" ($reparsed)")
      assert(reparsed.get == e, s"round-trip changed \"$printed\": ${reparsed.get}")
  }

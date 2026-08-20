package it.grypho.scala.leonardo
package logic

import core.*
import scalar.Ratio
import equation._Equation
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


class TernaryLogicTest extends AnyFlatSpec:

  val a = _Variable("a")
  val b = _Variable("b")
  val T = _Bool(true)
  val F = _Bool(false)
  val U = _Truth.Unknown

  def parse(input: String): _Expression =
    val result = Parser.parse(input)
    assert(result.successful, s"parse failed for \"$input\": $result")
    result.get

  /** Evaluates to a concrete truth value, failing the test otherwise. */
  def evalTruth(e: _Expression, env: Environment = new Environment()): _Value =
    e.eval(env) match
      case Right(v) if asTruth(v).isDefined => v
      case other                            => fail(s"expected a truth value but got: $other")

  // --- _Truth: the smart factory and its collapse invariant ---

  "_Truth.of" should "collapse the crisp endpoints back to _Bool" in
  {
    assert(_Truth.of(0.0) == _Bool(false))
    assert(_Truth.of(1.0) == _Bool(true))
  }

  it should "keep an intermediate degree as a _Truth" in
  {
    assert(_Truth.of(0.5) == U)
    assert(_Truth.of(0.25) == _Truth.of(0.25))
  }

  it should "clamp out-of-range degrees into [0, 1]" in
  {
    assert(_Truth.of(-2.0) == _Bool(false))
    assert(_Truth.of(7.0) == _Bool(true))
  }

  it should "map NaN to unknown rather than producing a NaN carrier" in
  {
    assert(_Truth.of(Double.NaN) == U)
  }

  "_Truth.Unknown" should "print and re-parse as \"unknown\"" in
  {
    assert(U.toString == "unknown")
    assert(U.display(5) == "unknown")
    assert(parse("unknown") == U)
  }

  it should "evaluate to itself" in
  {
    assert(evalTruth(U) == U)
  }

  // --- the Kleene identities: the regression suite for the widened rule table ---

  "false and unknown" should "be false (0 annihilates min)" in
  {
    assert(evalTruth(And(F, U)) == F)
    assert(evalTruth(And(U, F)) == F)
  }

  "true or unknown" should "be true (1 annihilates max)" in
  {
    assert(evalTruth(Or(T, U)) == T)
    assert(evalTruth(Or(U, T)) == T)
  }

  "unknown and unknown" should "be unknown" in
  {
    assert(evalTruth(And(U, U)) == U)
  }

  "unknown or unknown" should "be unknown" in
  {
    assert(evalTruth(Or(U, U)) == U)
  }

  "not unknown" should "be unknown (0.5 is the negation fixpoint)" in
  {
    assert(evalTruth(Not(U)) == U)
    assert(evalTruth(Not(Not(U))) == U)
  }

  "true and unknown" should "be unknown" in
  {
    assert(evalTruth(And(T, U)) == U)
  }

  "false or unknown" should "be unknown" in
  {
    assert(evalTruth(Or(F, U)) == U)
  }

  "unknown implies unknown" should "be unknown" in
  {
    assert(evalTruth(Implies(U, U)) == U)
  }

  "implication with unknown" should "follow max(1 - a, b)" in
  {
    assert(evalTruth(Implies(F, U)) == T)   // false implies anything
    assert(evalTruth(Implies(U, T)) == T)
    assert(evalTruth(Implies(T, U)) == U)
    assert(evalTruth(Implies(U, F)) == U)
  }

  "unknown xor unknown" should "be unknown, agreeing with the (a and not b) or (not a and b) desugaring" in
  {
    assert(evalTruth(Xor(U, U)) == U)
    // the desugaring the normal forms use must give the same answer
    assert(evalTruth(Or(And(U, Not(U)), And(Not(U), U))) == U)
  }

  "xor with a crisp operand" should "follow the lattice rule" in
  {
    assert(evalTruth(Xor(U, T)) == U)
    assert(evalTruth(Xor(U, F)) == U)
    assert(evalTruth(Xor(T, F)) == T)
    assert(evalTruth(Xor(T, T)) == F)
  }

  // --- the boolean table is a special case of the graded one ---

  "the graded rule table" should "reproduce the classical truth tables exactly" in
  {
    for x <- List(true, false); y <- List(true, false) do
      assert(evalTruth(And(_Bool(x), _Bool(y))) == _Bool(x && y), s"And($x, $y)")
      assert(evalTruth(Or(_Bool(x), _Bool(y))) == _Bool(x || y), s"Or($x, $y)")
      assert(evalTruth(Implies(_Bool(x), _Bool(y))) == _Bool(!x || y), s"Implies($x, $y)")
      assert(evalTruth(Xor(_Bool(x), _Bool(y))) == _Bool(x != y), s"Xor($x, $y)")
  }

  it should "return a _Bool (never a _Truth) for every crisp result" in
  {
    assert(evalTruth(And(T, T)).isInstanceOf[_Bool])
    assert(evalTruth(Or(U, T)).isInstanceOf[_Bool])
    assert(evalTruth(Not(T)).isInstanceOf[_Bool])
  }

  // --- short-circuits stay sound in the widened table ---

  "the And short-circuit" should "still fire on a false left operand" in
  {
    val unreducible = _Equation(Ratio(_Number(1), _Number(0)), _Number(0))
    assert(evalTruth(And(F, unreducible)) == F)
  }

  "the Or short-circuit" should "still fire on a true left operand" in
  {
    val unreducible = _Equation(Ratio(_Number(1), _Number(0)), _Number(0))
    assert(evalTruth(Or(T, unreducible)) == T)
  }

  "an unknown left operand" should "NOT short-circuit (it does not decide)" in
  {
    val unreducible = _Equation(Ratio(_Number(1), _Number(0)), _Number(0))
    And(U, unreducible).eval(new Environment()) match
      case Left(And(u, _)) => assert(u == U)
      case other           => fail(s"expected a symbolic residual, got: $other")
  }

  // --- environment binding ---

  "a variable bound to unknown" should "widen in connective positions" in
  {
    val env = new Environment().withBinding("a", U)
    assert(evalTruth(And(a, T), env) == U)
    assert(evalTruth(Not(a), env) == U)
    assert(evalTruth(And(a, F), env) == F)
  }

  "mixed _Bool and _Truth operands" should "combine without a type error" in
  {
    val env = new Environment().withBinding("a", U).withBinding("b", T)
    assert(evalTruth(And(a, b), env) == U)
    assert(evalTruth(Or(a, b), env) == T)
  }

  // --- crisp-only guards on the classical rewrite rules ---

  "the complement rule" should "not fire on a _Truth operand" in
  {
    assert(simplifyLogicFully(And(U, Not(U))) == U)   // folds to unknown, NOT false
    assert(simplifyLogicFully(Or(U, Not(U))) == U)    // NOT true
  }

  it should "still fire on a free variable (documented crisp-atom restriction)" in
  {
    assert(simplifyLogicFully(And(a, Not(a))) == F)
    assert(simplifyLogicFully(Or(a, Not(a))) == T)
  }

  it should "not fire when the shared operand merely contains an unknown" in
  {
    val body = And(a, U)
    assert(simplifyLogicFully(And(body, Not(body))) == And(body, Not(body)))
  }

  "a implies a and a xor a" should "not fold when the operand carries a _Truth" in
  {
    val body = Or(a, U)
    assert(simplifyLogicFully(Implies(body, body)) == Implies(body, body))
    assert(simplifyLogicFully(Xor(body, body)) == Xor(body, body))
    // crisp atoms still fold
    assert(simplifyLogicFully(Implies(a, a)) == T)
    assert(simplifyLogicFully(Xor(a, a)) == F)
  }

  "the lattice-valid rules" should "still apply over _Truth operands" in
  {
    assert(simplifyLogicFully(And(U, U)) == U)              // idempotence
    assert(simplifyLogicFully(Or(U, T)) == T)               // absorbing element
    assert(simplifyLogicFully(And(U, T)) == U)              // identity element
    assert(simplifyLogicFully(And(U, F)) == F)
    assert(simplifyLogicFully(Not(Not(U))) == U)            // double negation
    assert(simplifyLogicFully(Or(U, And(U, a))) == U)       // absorption
  }

  "simplifyLogic constant folding" should "reduce mixed crisp/graded constants" in
  {
    assert(simplifyLogicFully(And(U, Or(T, a))) == U)
    assert(simplifyLogicFully(Not(U)) == U)
  }

  // --- normal forms are gated on crispness ---

  "toCNF and toDNF" should "return the input unchanged when it carries a _Truth" in
  {
    val e = Not(And(a, U))
    assert(toCNF(e) == e)
    assert(toDNF(e) == e)
  }

  it should "still normalise a crisp expression" in
  {
    assert(toCNF(Not(And(a, b))) == Or(Not(a), Not(b)))
  }

  // --- three-valued tables ---

  "kleeneTable" should "enumerate 3^n rows, first variable most significant" in
  {
    val rows = kleeneTable(And(a, b), List(a, b))
    assert(rows.size == 9)
    assert(rows.head._1 == Map("a" -> F, "b" -> F))
    assert(rows.last._1 == Map("a" -> T, "b" -> T))
    assert(rows.map(_._2) == Vector(
      Some(F), Some(F), Some(F),      // a = false
      Some(F), Some(U), Some(U),      // a = unknown
      Some(F), Some(U), Some(T)))     // a = true
  }

  it should "tabulate not a over the three values" in
  {
    assert(kleeneTable(Not(a), List(a)).map(_._2) == Vector(Some(T), Some(U), Some(F)))
  }

  it should "yield None for a row that does not reduce to a truth value" in
  {
    val rows = kleeneTable(And(a, _Equation(_Variable("x"), _Number(1))), List(a))
    assert(rows.map(_._2) == Vector(Some(F), None, None))
  }

  it should "be rejected beyond the variable cap" in
  {
    val vars = (1 to MaxKleeneTableVars + 1).map(i => _Variable(s"v$i")).toList
    val e = vars.map(v => v: _Expression).reduceLeft(And.apply)
    assert(kleeneTable(e, vars).isEmpty)
  }

  "the boolean truthTable" should "report a graded row as None" in
  {
    // a is enumerated as a boolean, but the unknown operand makes "true and unknown" graded
    assert(truthTable(And(a, U), List(a)).map(_._2) == Vector(Some(false), None))
  }

  // --- parsing and round-trip ---

  "unknown" should "be word-boundary guarded" in
  {
    assert(parse("unknownx") == _Variable("unknownx"))
    assert(Parser.ReservedWords.contains("unknown"))
  }

  "connectives over unknown" should "parse and evaluate" in
  {
    assert(evalTruth(parse("false and unknown")) == F)
    assert(evalTruth(parse("not unknown")) == U)
    assert(evalTruth(parse("unknown or unknown")) == U)
  }

  "a ternary expression" should "round-trip through toString" in
  {
    for e <- List[_Expression](U, And(U, T), Not(U), Implies(a, U), Xor(U, b)) do
      val printed  = e.toString
      val reparsed = Parser.parse(printed)
      assert(reparsed.successful, s"toString output did not re-parse: \"$printed\" ($reparsed)")
      assert(reparsed.get == e, s"round-trip changed \"$printed\": ${reparsed.get}")
  }

  // --- the ternary value set is closed under every connective ---

  "the three-valued set" should "be closed under all five connectives" in
  {
    val values = List[_Value](F, U, T)
    for x <- values; y <- values do
      for r <- List(And(x, y), Or(x, y), Implies(x, y), Xor(x, y)) do
        assert(values.contains(evalTruth(r)), s"$r escaped {false, unknown, true}")
      assert(values.contains(evalTruth(Not(x))), s"not $x escaped {false, unknown, true}")
  }

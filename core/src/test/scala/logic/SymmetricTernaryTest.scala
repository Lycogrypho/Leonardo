package it.grypho.scala.leonardo
package logic

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


class SymmetricTernaryTest extends AnyFlatSpec:

  val a = _Variable("a")
  val T = _Bool(true)
  val F = _Bool(false)
  val U = _Truth.Unknown

  /** The symmetric encoding turned on. */
  val sym: Environment = new Environment(Environment.DefaultPrecision, Map.empty, symmetricLogic = true)
  /** The default encoding. */
  val plain: Environment = new Environment()

  def parse(input: String): _Expression =
    val result = Parser.parse(input)
    assert(result.successful, s"parse failed for \"$input\": $result")
    result.get

  def evalIn(e: _Expression, env: Environment): Either[_Expression, _Value] = e.eval(env)

  def evalTruth(e: _Expression, env: Environment): _Value =
    e.eval(env) match
      case Right(v) if asTruth(v, env.symmetricLogic).isDefined => v
      case other => fail(s"expected a truth value but got: $other")

  /** The symmetric digit for a truth value, or a failure. */
  def digitOf(v: _Value): Double =
    _Truth.toSymmetric(v) match
      case Some(d) => d
      case None    => fail(s"not a truth value: $v")

  // --- the affine map round-trips ---

  "fromSymmetric" should "map the three digits to the three truth values" in
  {
    assert(_Truth.fromSymmetric(-1.0) == F)
    assert(_Truth.fromSymmetric(0.0) == U)
    assert(_Truth.fromSymmetric(1.0) == T)
  }

  "the symmetric map" should "round-trip for -1, 0, 1" in
  {
    for s <- List(-1.0, 0.0, 1.0) do
      assert(digitOf(_Truth.fromSymmetric(s)) == s, s"round-trip failed for $s")
  }

  it should "round-trip in the other direction for the three truth values" in
  {
    for v <- List[_Value](F, U, T) do
      assert(_Truth.fromSymmetric(digitOf(v)) == v, s"round-trip failed for $v")
  }

  "toSymmetric" should "return None for a non-truth value" in
  {
    assert(_Truth.toSymmetric(_Number(3.0)).isEmpty)
  }

  "_Truth.symmetric" should "be 2d - 1 on the instance" in
  {
    assert(U.symmetric == 0.0)
  }

  // --- asTruth reads the digits only under the symmetric encoding ---

  "asTruth" should "read -1 / 0 / 1 as truth values only when symmetric is on" in
  {
    assert(asTruth(_Number(-1.0), symmetric = true).contains(0.0))
    assert(asTruth(_Number(0.0),  symmetric = true).contains(0.5))
    assert(asTruth(_Number(1.0),  symmetric = true).contains(1.0))
    for d <- List(-1.0, 0.0, 1.0) do
      assert(asTruth(_Number(d)).isEmpty, s"$d must not be a truth value by default")
  }

  it should "not read any other number as a truth value, even when symmetric is on" in
  {
    for d <- List(2.0, 0.5, -3.0) do
      assert(asTruth(_Number(d), symmetric = true).isEmpty, s"$d must not be a truth value")
  }

  it should "keep reading the word spellings under the symmetric encoding" in
  {
    assert(asTruth(T, symmetric = true).contains(1.0))
    assert(asTruth(U, symmetric = true).contains(0.5))
  }

  // --- the Kleene identities restated in symmetric digits ---

  "the Kleene identities" should "hold verbatim in symmetric digits" in
  {
    val cases = List(
      // (expression, expected symmetric digit)
      (parse("-1 and 0"),  -1.0),   // false and unknown = false
      (parse("1 or 0"),     1.0),   // true or unknown = true
      (parse("0 and 0"),    0.0),   // unknown and unknown = unknown
      (parse("not 0"),      0.0),   // not unknown = unknown
      (parse("0 implies 0"), 0.0),  // unknown implies unknown = unknown
      (parse("1 and 0"),    0.0),   // true and unknown = unknown
      (parse("-1 or 0"),    0.0),   // false or unknown = unknown
      (parse("0 xor 0"),    0.0),   // unknown xor unknown = unknown
      (parse("1 and 1"),    1.0),
      (parse("1 and -1"),  -1.0),
      (parse("not 1"),     -1.0),
      (parse("not -1"),     1.0),
      (parse("1 xor -1"),   1.0)
    )
    for (e, expected) <- cases do
      assert(digitOf(evalTruth(e, sym)) == expected, s"$e should be $expected")
  }

  it should "agree digit for digit with the default alphabet" in
  {
    // "-1 and 0" (symmetric) must equal "false and unknown" (default) as a value
    assert(evalTruth(parse("-1 and 0"), sym) == evalTruth(And(F, U), plain))
    assert(evalTruth(parse("0 or 0"), sym) == evalTruth(Or(U, U), plain))
    assert(evalTruth(parse("not 0"), sym) == evalTruth(Not(U), plain))
  }

  "the word spellings" should "still work under the symmetric encoding" in
  {
    assert(evalTruth(parse("true and 0"), sym) == U)
    assert(evalTruth(parse("unknown and 1"), sym) == U)
  }

  // --- the default alphabet is untouched (no silent reinterpretation) ---

  "numeric digits in connective positions" should "stay symbolic in the default mode" in
  {
    And(_Number(1.0), _Number(0.0)).eval(plain) match
      case Left(And(_Number(1.0), _Number(0.0))) => succeed
      case other => fail(s"1 and 0 must stay symbolic by default, got: $other")
  }

  "ordinary arithmetic" should "be unaffected by the symmetric encoding" in
  {
    assert(parse("2 * 3 + 1").eval(sym) == Right(_Number(7.0)))
    assert(parse("0 + 1").eval(sym) == Right(_Number(1.0)))
    // 0 stays a plain number outside connective positions
    assert(parse("0 * 5").eval(sym) == Right(_Number(0.0)))
  }

  // --- a bound variable participates, which is why the flag lives in Environment ---

  "a variable bound to a symmetric digit" should "widen in connective positions" in
  {
    val bound = sym.withBinding("a", _Number(0.0))
    assert(evalTruth(And(a, T), bound) == U)
    assert(evalTruth(Not(a), bound) == U)
    assert(evalTruth(And(a, F), bound) == F)
  }

  it should "stay symbolic when the same binding is read in the default mode" in
  {
    val bound = plain.withBinding("a", _Number(0.0))
    And(a, T).eval(bound) match
      case Left(_) => succeed
      case other   => fail(s"expected symbolic in default mode, got: $other")
  }

  "withBinding" should "carry the symmetric flag through" in
  {
    assert(sym.withBinding("a", T).symmetricLogic)
    assert(!plain.withBinding("a", T).symmetricLogic)
  }

  // --- short-circuits use the digits too ---

  "the short-circuits" should "fire on the symmetric digits" in
  {
    val unreducible = parse("1/0 = 0")
    assert(evalTruth(And(_Number(-1.0), unreducible), sym) == F)   // -1 is false
    assert(evalTruth(Or(_Number(1.0), unreducible), sym) == T)     //  1 is true
  }

  it should "NOT fire on the digit 0, which is unknown rather than false" in
  {
    val unreducible = parse("1/0 = 0")
    And(_Number(0.0), unreducible).eval(sym) match
      case Left(And(_Number(0.0), _)) => succeed
      case other => fail(s"0 must not short-circuit as false, got: $other")
  }

  // --- the crisp gate treats the digit 0 as graded ---

  "the complement rule" should "not fire on the digit 0 under the symmetric encoding" in
  {
    val zero = _Number(0.0)
    assert(simplifyLogicFully(And(zero, Not(zero)), identity, symmetric = true) == U)
    assert(simplifyLogicFully(Or(zero, Not(zero)), identity, symmetric = true) == U)
  }

  it should "still fire on the crisp digits -1 and 1" in
  {
    assert(simplifyLogicFully(And(a, Not(a)), identity, symmetric = true) == F)
    // 1 and not 1 folds by constant folding to false
    assert(simplifyLogicFully(And(_Number(1.0), Not(_Number(1.0))), identity, symmetric = true) == F)
  }

  "simplifyLogic constant folding" should "fold the symmetric digits when the flag is set" in
  {
    assert(simplifyLogicFully(And(_Number(1.0), _Number(0.0)), identity, symmetric = true) == U)
    assert(simplifyLogicFully(Not(_Number(-1.0)), identity, symmetric = true) == T)
    // and must NOT fold them by default
    assert(simplifyLogicFully(And(_Number(1.0), _Number(0.0))) == And(_Number(1.0), _Number(0.0)))
  }

  "toCNF" should "leave an expression carrying the digit 0 unchanged under the encoding" in
  {
    val e = Or(a, Not(a))
    assert(toCNF(e) == T)                                    // crisp: folds
    val graded = Or(_Number(0.0), Not(_Number(0.0)))
    assert(toCNF(graded, symmetric = true) == graded)         // graded: untouched
  }

  // --- tables under the encoding ---

  "kleeneTable" should "accept a symmetric-digit result as truth-valued" in
  {
    val rows = kleeneTable(And(a, _Number(0.0)), List(a), sym)
    assert(rows.size == 3)
    assert(rows.map(_._2.map(digitOf)) == Vector(Some(-1.0), Some(0.0), Some(0.0)))
  }

  it should "report the same rows as the default alphabet, only spelled differently" in
  {
    val symRows   = kleeneTable(Not(a), List(a), sym).map(_._2)
    val plainRows = kleeneTable(Not(a), List(a), plain).map(_._2)
    assert(symRows == plainRows, "the encoding must not change the values, only their spelling")
  }

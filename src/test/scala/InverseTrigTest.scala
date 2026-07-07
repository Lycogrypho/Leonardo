package it.grypho.scala.leonardo

import core.*
import scalar.*
import scalar.Syntax.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


class InverseTrigTest extends AnyFlatSpec:

  def parse(input: String): _Expression =
    val result = Parser.parse(input)
    assert(result.successful, s"parse failed for \"$input\": $result")
    result.get

  val env: Environment = new Environment()

  // --- parsing ---

  "asin(x)" should "parse as Asin(_Variable(\"x\"))" in
  {
    assert(parse("asin(x)") == Asin(_Variable("x")))
  }

  "acos(x)" should "parse as Acos(_Variable(\"x\"))" in
  {
    assert(parse("acos(x)") == Acos(_Variable("x")))
  }

  "atan(x)" should "parse as Atan(_Variable(\"x\"))" in
  {
    assert(parse("atan(x)") == Atan(_Variable("x")))
  }

  "Asin(x).toString" should "be \"asin(x)\"" in
  {
    assert(Asin(_Variable("x")).toString == "asin(x)")
  }

  "Acos(x).toString" should "be \"acos(x)\"" in
  {
    assert(Acos(_Variable("x")).toString == "acos(x)")
  }

  "Atan(x).toString" should "be \"atan(x)\"" in
  {
    assert(Atan(_Variable("x")).toString == "atan(x)")
  }

  // --- numeric evaluation ---

  "asin(0)" should "evaluate to 0" in
  {
    parse("asin(0)").eval(env) match
      case Right(_Number(x)) => assert(math.abs(x) < 1e-4)
      case other             => fail(s"expected 0 but got $other")
  }

  "asin(1)" should "evaluate to pi/2" in
  {
    parse("asin(1)").eval(env) match
      case Right(_Number(x)) => assert(math.abs(x - math.Pi / 2) < 1e-4)
      case other             => fail(s"expected pi/2 but got $other")
  }

  "acos(1)" should "evaluate to 0" in
  {
    parse("acos(1)").eval(env) match
      case Right(_Number(x)) => assert(math.abs(x) < 1e-4)
      case other             => fail(s"expected 0 but got $other")
  }

  "acos(0)" should "evaluate to pi/2" in
  {
    parse("acos(0)").eval(env) match
      case Right(_Number(x)) => assert(math.abs(x - math.Pi / 2) < 1e-4)
      case other             => fail(s"expected pi/2 but got $other")
  }

  "atan(0)" should "evaluate to 0" in
  {
    parse("atan(0)").eval(env) match
      case Right(_Number(x)) => assert(math.abs(x) < 1e-4)
      case other             => fail(s"expected 0 but got $other")
  }

  "atan(1)" should "evaluate to pi/4" in
  {
    parse("atan(1)").eval(env) match
      case Right(_Number(x)) => assert(math.abs(x - math.Pi / 4) < 1e-4)
      case other             => fail(s"expected pi/4 but got $other")
  }

  // --- domain errors stay symbolic ---

  "asin(2)" should "stay symbolic (out of domain)" in
  {
    parse("asin(2)").eval(env) match
      case Left(_)  => succeed
      case Right(x) => fail(s"expected symbolic but got $x")
  }

  "acos(-2)" should "stay symbolic (out of domain)" in
  {
    parse("acos(-2)").eval(env) match
      case Left(_)  => succeed
      case Right(x) => fail(s"expected symbolic but got $x")
  }

  // --- symbolic (unbound variable) ---

  "asin(x) with x unbound" should "remain symbolic" in
  {
    parse("asin(x)").eval(env) match
      case Left(_)  => succeed
      case Right(x) => fail(s"expected symbolic but got $x")
  }

  // --- simplification ---

  "simplify(asin(0))" should "return 0" in
  {
    assert(Asin(_Number(0)).simplify() == _Number(0))
  }

  "simplify(acos(1))" should "return 0" in
  {
    assert(Acos(_Number(1)).simplify() == _Number(0))
  }

  "simplify(atan(0))" should "return 0" in
  {
    assert(Atan(_Number(0)).simplify() == _Number(0))
  }

  // --- differentiation ---

  "derive(asin(x), x)" should "equal 1/sqrt(1-x^2) at x=0.5" in
  {
    val e = new Environment()
    e.assign("x", _Number(0.5))
    val d = _Derivative(Asin(_Variable("x")), _Variable("x"))
    d.eval(e) match
      case Right(_Number(v)) =>
        assert(math.abs(v - 1.0 / math.sqrt(1 - 0.25)) < 1e-4)
      case other => fail(s"expected numeric but got $other")
  }

  "derive(acos(x), x)" should "equal -1/sqrt(1-x^2) at x=0.5" in
  {
    val e = new Environment()
    e.assign("x", _Number(0.5))
    val d = _Derivative(Acos(_Variable("x")), _Variable("x"))
    d.eval(e) match
      case Right(_Number(v)) =>
        assert(math.abs(v - (-1.0 / math.sqrt(1 - 0.25))) < 1e-4)
      case other => fail(s"expected numeric but got $other")
  }

  "derive(atan(x), x)" should "equal 1/(1+x^2) at x=1" in
  {
    val e = new Environment()
    e.assign("x", _Number(1.0))
    val d = _Derivative(Atan(_Variable("x")), _Variable("x"))
    d.eval(e) match
      case Right(_Number(v)) =>
        assert(math.abs(v - 0.5) < 1e-4)
      case other => fail(s"expected numeric but got $other")
  }

  // --- round-trip: parse → toString → parse gives same AST ---

  "asin(x)" should "round-trip through toString" in
  {
    assert(parse(Asin(_Variable("x")).toString) == Asin(_Variable("x")))
  }

  "acos(x)" should "round-trip through toString" in
  {
    assert(parse(Acos(_Variable("x")).toString) == Acos(_Variable("x")))
  }

  "atan(x)" should "round-trip through toString" in
  {
    assert(parse(Atan(_Variable("x")).toString) == Atan(_Variable("x")))
  }

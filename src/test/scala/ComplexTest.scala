package it.grypho.scala.leonardo

import core.*
import scalar.*
import equation.*
import parser.Parser
import cli.Session
import org.scalatest.flatspec.AnyFlatSpec


class ComplexTest extends AnyFlatSpec:

  val env = new Environment()
  val x   = _Variable("x")

  def parse(input: String): _Expression =
    val r = Parser.parse(input)
    assert(r.successful, s"parse failed for \"$input\": $r")
    r.get

  def evalValue(input: String): _Value =
    parse(input).eval(env) match
      case Right(v) => v
      case Left(e)  => fail(s"expected a value for \"$input\" but stayed symbolic: $e")

  def parts(v: _Value): (Double, Double) =
    _Complex.parts(v).getOrElse(fail(s"not a numeric value: $v"))

  def assertComplex(v: _Value, re: Double, im: Double): org.scalatest.Assertion =
    val (r, i) = parts(v)
    assert(math.abs(r - re) < 1e-9 && math.abs(i - im) < 1e-9, s"expected $re + ${im}i but got $v")

  // --- the imaginary unit ---

  "the token i" should "parse to the imaginary unit _Complex(0, 1)" in
  {
    assert(parse("i") == _Complex.of(0, 1))
  }

  "bare i" should "be a reserved word (not a variable)" in
  {
    parse("i") match
      case _: _Complex => succeed
      case other       => fail(s"expected the imaginary unit but got $other")
  }

  "identifiers merely starting with i" should "stay ordinary variables" in
  {
    assert(parse("im") == _Variable("im"))
    assert(parse("i1") == _Variable("i1"))
    assert(parse("idx") == _Variable("idx"))
  }

  "i squared" should "collapse to the real -1" in
  {
    assert(evalValue("i*i") == _Number(-1.0))
  }

  "3i" should "be implicit multiplication yielding _Complex(0, 3)" in
  {
    assertComplex(evalValue("3i"), 0.0, 3.0)
  }

  // --- field arithmetic ---

  "(2 + 3i) + (1 - i)" should "be 3 + 2i" in
  {
    assertComplex(evalValue("(2 + 3i) + (1 - i)"), 3.0, 2.0)
  }

  "(2 + 3i) * (1 - i)" should "be 5 + i" in
  {
    // (2+3i)(1-i) = 2 - 2i + 3i - 3i^2 = 2 + i + 3 = 5 + i
    assertComplex(evalValue("(2 + 3i) * (1 - i)"), 5.0, 1.0)
  }

  "(2 + 3i) / (1 - i)" should "be -0.5 + 2.5i" in
  {
    // (2+3i)/(1-i) = (2+3i)(1+i)/2 = (2 + 2i + 3i - 3)/2 = (-1 + 5i)/2
    assertComplex(evalValue("(2 + 3i) / (1 - i)"), -0.5, 2.5)
  }

  "a complex minus itself" should "collapse to real 0" in
  {
    assert(evalValue("(2 + 3i) - (2 + 3i)") == _Number(0.0))
  }

  "0 * i" should "short-circuit to real 0" in
  {
    assert(evalValue("0 * i") == _Number(0.0))
  }

  // --- full complex closure: roots and logs of negatives ---

  "sqrt(-1) written as (-1)^0.5" should "be i" in
  {
    assertComplex(evalValue("(0 - 1)^0.5"), 0.0, 1.0)
  }

  "(-8)^(1/3)" should "be the principal complex cube root 1 + i√3" in
  {
    assertComplex(evalValue("pow(0 - 8, 1/3)"), 1.0, math.sqrt(3))
  }

  "log(-1)" should "be iπ" in
  {
    assertComplex(evalValue("log(0 - 1)"), 0.0, math.Pi)
  }

  "log(i)" should "be iπ/2" in
  {
    assertComplex(evalValue("log(i)"), 0.0, math.Pi / 2)
  }

  // --- elementary functions on complex arguments ---

  "exp(i·π)" should "be -1 (Euler's identity)" in
  {
    assertComplex(evalValue("exp(i*pi)"), -1.0, 0.0)
  }

  "exp(i·π/2)" should "be i" in
  {
    assertComplex(evalValue("exp(i*pi/2)"), 0.0, 1.0)
  }

  "sin(i)" should "be i·sinh(1)" in
  {
    assertComplex(evalValue("sin(i)"), 0.0, math.sinh(1))
  }

  "cos(i)" should "be cosh(1) (real)" in
  {
    val v = evalValue("cos(i)")
    assert(v == _Number(math.cosh(1)))
  }

  // --- domain errors still stay symbolic ---

  "log(0)" should "stay symbolic even under complex closure" in
  {
    val e = Log(_Number(0))
    assert(e.eval(env) == Left(e))
  }

  "0 ^ -1" should "stay symbolic even under complex closure" in
  {
    val e = Power(_Number(0), _Number(-1))
    assert(e.eval(env) == Left(e))
  }

  "a complex divided by zero" should "stay symbolic" in
  {
    val e = Ratio(_Complex.of(2, 3), _Number(0))
    assert(e.eval(env) == Left(e))
  }

  // --- display / round-trip ---

  "a full complex value" should "display as (a + bi) and round-trip" in
  {
    val z = _Complex.of(2, 3)
    assert(z.toString == "(2.0 + 3.0i)")
    assertComplex(evalValue(z.toString), 2.0, 3.0)
  }

  "a complex with negative imaginary part" should "display with a minus" in
  {
    assert(_Complex.of(2, -3).toString == "(2.0 - 3.0i)")
    assert(_Complex.of(0, -1).toString == "-i")
    assert(_Complex.of(0, 1).toString == "i")
  }

  "a complex with a rounded-away imaginary part" should "display as a real" in
  {
    // exp(i·π) has a floating-point residual imaginary part that rounds to 0
    assert(evalValue("exp(i*pi)").toString == "-1.0")
  }

  "display(precision)" should "round both parts to the requested decimals" in
  {
    _Complex.of(1.23456, 7.89012) match
      case c: _Complex => assert(c.display(2) == "(1.23 + 7.89i)")
      case other       => fail(s"expected a complex value but got $other")
  }

  // --- calculus treats a complex as a constant ---

  "the derivative of a complex constant" should "be 0" in
  {
    assert(derive(_Complex.of(2, 3), x) == _Number(0))
  }

  "derive(x * i, x)" should "be i" in
  {
    assert(derive(parse("x*i"), x) == _Complex.of(0, 1))
  }

  // --- equations with complex operands ---

  "an equation of equal complex values" should "reduce to true" in
  {
    assert(parse("(1 + i) + (1 - i) = 2").eval(env) == Right(_Bool(true)))
  }

  "an equation of unequal complex values" should "reduce to false" in
  {
    assert(parse("i = 1").eval(env) == Right(_Bool(false)))
  }

  "i == -1*i*i*i (both equal i)" should "reduce to true" in
  {
    // -i^3 = -(-i) = i
    assert(parse("i == 0 - i*i*i").eval(env) == Right(_Bool(true)))
  }

  // --- REPL flow ---

  "binding a complex value" should "store and display it" in
  {
    val s = Session()
    assert(s.execute("z := 2 + 3i") == "z := (2.0 + 3.0i)")
    assert(s.execute("z") == "(2.0 + 3.0i)")
  }

  "a complex binding" should "survive a script round-trip" in
  {
    val s = Session()
    s.execute("z := 2 + 3i")
    val restored = Session()
    restored.load(s.script)
    assert(restored.execute("z") == "(2.0 + 3.0i)")
  }

  "assigning to i" should "be rejected as a reserved word" in
  {
    val s = Session()
    assert(s.execute("i := 3").contains("reserved word"))
  }

  "a complex result in the REPL" should "respect session precision" in
  {
    val s = Session()
    s.execute("precision 2")
    assert(s.execute("1.23456 + 7.89012i") == "(1.23 + 7.89i)")
  }

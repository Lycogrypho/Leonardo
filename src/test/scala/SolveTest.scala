package it.grypho.scala.leonardo

import core.*
import scalar.*
import equation.*
import parser.Parser
import cli.Session
import org.scalatest.flatspec.AnyFlatSpec


class SolveTest extends AnyFlatSpec:

  val x = _Variable("x")

  def parseEq(input: String): _Equation =
    val result = Parser.parse(input)
    assert(result.successful, s"parse failed for \"$input\": $result")
    result.get match
      case eq: _Equation => eq
      case other         => fail(s"expected an equation but got: $other")

  def num(s: List[_Equation]): List[Double] =
    s.map {
      case _Equation(_, _Number(d)) => d
      case other                    => fail(s"expected a numeric solution but got: $other")
    }

  // --- linear tier ---

  "solve(10x = 2x + 1)" should "give x = 0.125" in
  {
    assert(solve(parseEq("10 * x = 2 * x + 1"), x) == List(_Equation(x, _Number(0.125))))
  }

  "solve of a linear equation with symbolic coefficients" should "give x = -b/a" in
  {
    val a = _Variable("a")
    val b = _Variable("b")
    assert(solve(parseEq("a * x + b = 0"), x)
      == List(_Equation(x, Ratio(Product(_Number(-1), b), a))))
  }

  "solve with bound coefficients" should "fold the solution through the environment" in
  {
    val env = new Environment().withBinding("a", _Number(2))
    assert(solve(parseEq("a * x = 4"), x, env) == List(_Equation(x, _Number(2))))
  }

  // --- quadratic tier ---

  "solve(x^2 = 4)" should "give both roots in ascending order" in
  {
    assert(num(solve(parseEq("x^2 = 4"), x)) == List(-2.0, 2.0))
  }

  "solve(x^2 - 2x + 1 = 0)" should "give the single double root" in
  {
    assert(num(solve(parseEq("x^2 - 2 * x + 1 = 0"), x)) == List(1.0))
  }

  "solve(x^2 + 1 = 0)" should "have no real solutions" in
  {
    assert(solve(parseEq("x^2 + 1 = 0"), x).isEmpty)
  }

  "solve of a symbolic quadratic" should "give the two ±√Δ closed forms" in
  {
    val solutions = solve(parseEq("x^2 + b * x + c = 0"), x)
    assert(solutions.size == 2)
    assert(solutions.forall(_.lhs == x))
  }

  // --- degenerate ---

  "solve of an equation without the variable" should "be empty" in
  {
    assert(solve(parseEq("2 = 2"), x).isEmpty)
    assert(solve(parseEq("y + 1 = 2"), x).isEmpty)
  }

  // --- numeric fallback ---

  "solve(exp(x) = 5)" should "find ln 5 numerically" in
  {
    val roots = num(solve(parseEq("exp(x) = 5"), x))
    assert(roots.size == 1)
    assert(math.abs(roots.head - math.log(5)) < 1e-6)
  }

  "solve(x^3 = 8)" should "find the real cube root numerically" in
  {
    val roots = num(solve(parseEq("x^3 = 8"), x))
    assert(roots.size == 1)
    assert(math.abs(roots.head - 2.0) < 1e-6)
  }

  "solve(sin(x) = 0)" should "find several roots including 0, capped" in
  {
    val roots = num(solve(parseEq("sin(x) = 0"), x))
    assert(roots.nonEmpty && roots.size <= 8)
    assert(roots.exists(r => math.abs(r) < 1e-6) || roots.exists(r => math.abs(r % math.Pi) < 1e-6))
  }

  "solve(exp(x) = 0)" should "find no roots" in
  {
    assert(solve(parseEq("exp(x) = 0"), x).isEmpty)
  }

  "solve with an unbound extra variable in a transcendental form" should "be empty" in
  {
    assert(solve(parseEq("sin(x) = y"), x).isEmpty)
  }

  // --- parser + _Solve node ---

  "\"solve(x^2 = 4, x)\"" should "parse to a _Solve node and round-trip" in
  {
    val first = Parser.parse("solve(x^2 = 4, x)")
    assert(first.successful, s"parse failed: $first")
    assert(first.get.isInstanceOf[_Solve])
    assert(Parser.parse(first.get.toString).get == first.get)
  }

  "bare \"solve\"" should "be a reserved word" in
  {
    assert(!Parser.parse("solve").successful)
  }

  "_Solve.eval" should "present one solution as an equation and several as a row vector" in
  {
    _Solve(parseEq("10 * x = 2 * x + 1"), x).eval(new Environment()) match
      case Left(e: _Equation) => assert(e == _Equation(x, _Number(0.125)))
      case other              => fail(s"unexpected: $other")

    _Solve(parseEq("x^2 = 4"), x).eval(new Environment()) match
      case Left(m: matrix._Matrix) =>
        assert(m.rows == 1 && m.cols == 2)
        assert(m(0, 0) == _Equation(x, _Number(-2)) && m(0, 1) == _Equation(x, _Number(2)))
      case other => fail(s"unexpected: $other")
  }

  "_Solve with no known solution" should "stay symbolic" in
  {
    val node = _Solve(parseEq("x^2 + 1 = 0"), x)
    assert(node.eval(new Environment()) == Left(node))
  }

  // --- REPL flow ---

  "solve in the REPL" should "answer with the solution set" in
  {
    val s = Session()
    assert(s.execute("solve(10 * x = 2 * x + 1, x)") == "x = 0.125")
    assert(s.execute("solve(x^2 = 4, x)") == "[[x = -2.0, x = 2.0]]")
    s.execute("a := 2")
    assert(s.execute("solve(a * x = 4, x)") == "x = 2.0")
  }

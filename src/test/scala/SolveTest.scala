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

  // --- named equations via h := lhs = rhs ---

  "solve with a named linear equation" should "work after binding h := lhs = rhs" in
  {
    val s = Session()
    s.execute("h := 10 * x = 2 * x + 1")
    assert(s.execute("solve(h, x)") == "x := 0.125")
    assert(s.execute("x") == "0.125")
  }

  "solve with a named quadratic equation" should "work after binding h := lhs = rhs" in
  {
    val s = Session()
    s.execute("h := x^2 = 4")
    val out = s.execute("solve(h, x)")
    assert(out.contains("x_1 :=") && out.contains("x_2 :="), s"expected numbered roots but got: $out")
    assert(Set(s.execute("x_1"), s.execute("x_2")) == Set("-2.0", "2.0"))
  }

  "solve(h, x) where h is an _EqualityCheck (==)" should "stay symbolic" in
  {
    val s = Session()
    s.execute("h := x == 5")
    // _EqualityCheck is not solvable; _Solve stays symbolic
    assert(s.execute("solve(h, x)") == "solve(x == 5.0, x)")
  }

  "solve(h, x) where h is an unbound variable" should "stay symbolic" in
  {
    val s = Session()
    assert(s.execute("solve(h, x)") == "solve(h, x)")
  }

  // --- REPL flow ---

  "solve in the REPL" should "auto-bind the single solution and keep numerics accessible" in
  {
    val s = Session()
    assert(s.execute("solve(10 * x = 2 * x + 1, x)") == "x := 0.125")
    assert(s.execute("x") == "0.125")
  }

  "solve of a quadratic in the REPL" should "bind numbered roots" in
  {
    val s = Session()
    val out = s.execute("solve(x^2 = 4, x)")
    assert(out.contains("x_1 :=") && out.contains("x_2 :="), s"expected numbered roots but got: $out")
  }

  "solve with a bound coefficient in the REPL" should "substitute and yield a single root" in
  {
    val s = Session()
    s.execute("a := 2")
    assert(s.execute("solve(a * x = 4, x)") == "x := 2.0")
  }

  // --- issue 1.5: identity equations must return Nil, not MaxNumericRoots fake roots ---

  "solve(sin(x) = sin(x))" should "return no solutions (identity, not 8 grid points)" in
  {
    // Before the fix, the numeric fallback's fa == 0.0 branch collected every grid
    // point where f ≡ 0, filling found up to MaxNumericRoots = 8 arbitrary results.
    assert(solve(parseEq("sin(x) = sin(x)"), x).isEmpty)
  }

  "solve(sin(x) = sin(x)) via REPL" should "stay symbolic rather than emit fake solutions" in
  {
    val s = Session()
    val out = s.execute("solve(sin(x) = sin(x), x)")
    // No concrete solution set — stays as the unsolved node (not a matrix of fake roots)
    assert(!out.startsWith("[["), s"expected no solution matrix but got: $out")
  }

  "solve(sin(x) = 0) after the fix" should "still find genuine roots via sign-change detection" in
  {
    // Regression: the neighbourhood guard only affects the fa == 0.0 branch. Roots
    // found via sign-change bisection (the vast majority) are unaffected. All
    // returned values must actually satisfy sin(r) ≈ 0.
    val roots = num(solve(parseEq("sin(x) = 0"), x))
    assert(roots.nonEmpty, "sin(x) = 0 must have at least one root in [-100, 100]")
    assert(roots.forall(r => math.abs(math.sin(r)) < 1e-5),
      s"all found values must be genuine roots: $roots")
  }

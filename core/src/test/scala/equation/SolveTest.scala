package it.grypho.scala.leonardo
package equation

import core.*
import scalar.*
import equation.*
import parser.Parser
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

  // Named equations (h := lhs = rhs) and REPL flow live in equation/SolveReplTest.scala
  // (issue 5.2 phase 1.1).

  // --- issue 1.5: identity equations must return Nil, not MaxNumericRoots fake roots ---

  "solve(sin(x) = sin(x))" should "return no solutions (identity, not 8 grid points)" in
  {
    // Before the fix, the numeric fallback's fa == 0.0 branch collected every grid
    // point where f ≡ 0, filling found up to MaxNumericRoots = 8 arbitrary results.
    assert(solve(parseEq("sin(x) = sin(x)"), x).isEmpty)
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

  // --- issue 1.1: ill-conditioned quadratics must not lose the small root ---

  /** Relative error of `actual` against `expected` (which must be non-zero). */
  def relError(actual: Double, expected: Double): Double =
    math.abs((actual - expected) / expected)

  "solve(x^2 + 1e8*x + 1 = 0)" should "recover BOTH roots, not just the large one" in
  {
    // Textbook (-b +- sqrt(delta))/2a cancels catastrophically here: sqrt(delta) is
    // equal to b to within a few ulps, so the subtracting branch kept only ~1 digit
    // and returned -7.45e-9 for a root whose true value is -1e-8 (25% error).
    val roots = num(solve(parseEq("x^2 + 1e8*x + 1 = 0"), x))
    assert(roots.size == 2, s"expected two roots, got: $roots")
    assert(relError(roots.head, -1e8) < 1e-12, s"large root wrong: ${roots.head}")
    assert(relError(roots(1), -1e-8) < 1e-12, s"small root wrong: ${roots(1)}")
  }

  it should "hold for the mirrored sign of b as well" in
  {
    // exercises the opposite branch of the sign(b) selection
    val roots = num(solve(parseEq("x^2 - 1e8*x + 1 = 0"), x))
    assert(roots.size == 2, s"expected two roots, got: $roots")
    assert(relError(roots.head, 1e-8) < 1e-12, s"small root wrong: ${roots.head}")
    assert(relError(roots(1), 1e8) < 1e-12, s"large root wrong: ${roots(1)}")
  }

  "the roots of an ill-conditioned quadratic" should "satisfy the equation they came from" in
  {
    // the property that actually matters: substituting the root back must give ~0
    // relative to the scale of the terms involved
    for eq <- List("x^2 + 1e8*x + 1 = 0", "x^2 - 1e8*x + 1 = 0", "x^2 + 1e6*x + 4 = 0") do
      for r <- num(solve(parseEq(eq), x)) do
        val residual = r * r + (if eq.contains("- 1e8") then -1e8 * r else if eq.contains("1e6") then 1e6 * r else 1e8 * r) +
                       (if eq.contains("1e6") then 4.0 else 1.0)
        assert(math.abs(residual) < 1e-6 * math.max(1.0, math.abs(r)),
          s"$eq: root $r leaves residual $residual")
  }

  "Vieta's relations" should "hold for an ill-conditioned quadratic" in
  {
    // product of roots = c/a = 1, sum = -b/a = -1e8; the product is the relation the
    // stable form is built on, so it is the sharpest check available
    val roots = num(solve(parseEq("x^2 + 1e8*x + 1 = 0"), x))
    assert(relError(roots.head * roots(1), 1.0) < 1e-12, s"product: ${roots.head * roots(1)}")
    assert(relError(roots.head + roots(1), -1e8) < 1e-12, s"sum: ${roots.head + roots(1)}")
  }

  "the well-conditioned cases" should "be unaffected by the stable form" in
  {
    assert(num(solve(parseEq("x^2 = 4"), x)) == List(-2.0, 2.0))
    assert(num(solve(parseEq("x^2 - 3 * x + 2 = 0"), x)) == List(1.0, 2.0))
    assert(num(solve(parseEq("x^2 - 2 * x + 1 = 0"), x)) == List(1.0))
    assert(solve(parseEq("x^2 + 1 = 0"), x).isEmpty)
  }

  "a quadratic with b = 0" should "still work (signum(0) would collapse the stable form)" in
  {
    assert(num(solve(parseEq("x^2 - 9 = 0"), x)) == List(-3.0, 3.0))
    assert(num(solve(parseEq("2 * x^2 - 8 = 0"), x)) == List(-2.0, 2.0))
  }

  "a quadratic with c = 0" should "give the zero root exactly" in
  {
    assert(num(solve(parseEq("x^2 + 5 * x = 0"), x)) == List(-5.0, 0.0))
  }
package it.grypho.scala.leonardo
package equation

import core.*
import scalar.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** Issue 3.2 — solving inequalities. */
class InequalityTest extends AnyFlatSpec:

  private val env = new Environment()

  /** Parses and evaluates, returning the result as a string for shape comparison. */
  private def solved(src: String): String =
    Parser.parse(src) match
      case Parser.Success(e, _) => e.eval(env).fold(_.toString, _.toString)
      case other                => fail(s"parse failed for '$src': $other")

  /** True when the solver declined and the node stayed symbolic. */
  private def staysSymbolic(src: String): Boolean = solved(src).startsWith("solve(")

  // ── the sign table (slice A) ───────────────────────────────────────────────

  private val a = _Variable("a")

  "sign" should "read a concrete number" in
  {
    assert(sign(_Number(3.0), env).contains(1))
    assert(sign(_Number(-2.5), env).contains(-1))
    assert(sign(_Number(0.0), env).contains(0))
  }

  it should "know exp is strictly positive whatever its argument" in
  {
    assert(sign(Exp(a), env).contains(1))
    assert(sign(Product(_Number(-1), Exp(a)), env).contains(-1))
  }

  it should "know a^2 + 1 is strictly positive" in
  {
    assert(sign(Sum(Power(a, _Number(2)), _Number(1)), env).contains(1))
  }

  it should "refuse a^2, which may be zero" in
  {
    // Not Some(0): it is zero only at one point, so it cannot be divided by.
    assert(sign(Power(a, _Number(2)), env).isEmpty)
  }

  it should "refuse a bare variable and a compound of unknown sign" in
  {
    assert(sign(a, env).isEmpty)
    assert(sign(Sum(a, _Number(-1)), env).isEmpty)
    assert(sign(Sin(a), env).isEmpty)
  }

  it should "use a binding when the environment supplies one" in
  {
    val bound = new Environment(variables = Map("a" -> _Number(-4.0)))
    assert(sign(a, bound).contains(-1))
  }

  it should "multiply signs through a product and a ratio" in
  {
    assert(sign(Product(_Number(-2), Exp(a)), env).contains(-1))
    assert(sign(Ratio(Exp(a), _Number(-1)), env).contains(-1))
  }

  "isNonZero" should "accept a strict sign and reject an undeterminable one" in
  {
    assert(isNonZero(Exp(a), env))
    assert(!isNonZero(Power(a, _Number(2)), env))
    assert(!isNonZero(_Number(0.0), env))
  }

  // ── linear (slice B) ───────────────────────────────────────────────────────

  "a linear inequality" should "solve with a positive coefficient" in
  {
    assert(solved("solve(x + 1 < 3, x)") == "(x < 2.0)")
  }

  it should "FLIP the direction for a negative coefficient" in
  {
    // -2x < 6  is  x > -3, not x < -3. The single most important case in the issue.
    assert(solved("solve(-2x < 6, x)") == "(x > -3.0)")
  }

  it should "flip <= to >= as well" in
  {
    assert(solved("solve(-x <= 4, x)") == "(x >= -4.0)")
  }

  it should "handle a coefficient on the right-hand side" in
  {
    assert(solved("solve(10 > 2x, x)") == "(x < 5.0)")
  }

  it should "solve when the coefficient is symbolic but provably positive" in
  {
    // exp(a) is strictly positive, so the direction is known without knowing its value.
    assert(solved("solve(exp(a) * x > 0, x)") == "(x > 0.0)")
  }

  it should "refuse a coefficient of unknown sign" in
  {
    assert(staysSymbolic("solve(a * x < 6, x)"))
    assert(staysSymbolic("solve((a - 1) * x < 6, x)"))
  }

  it should "reduce a relation not containing the variable to a constant" in
  {
    assert(solved("solve(2 < 3, x)") == "true")
    assert(solved("solve(5 < 1, x)") == "false")
  }

  // ── quadratic (slice C) ────────────────────────────────────────────────────

  "a quadratic inequality" should "split outside the roots for > 0" in
  {
    assert(solved("solve(x^2 - 4 > 0, x)") == "((x < -2.0) or (x > 2.0))")
  }

  it should "bound between the roots for < 0" in
  {
    assert(solved("solve(x^2 - 4 < 0, x)") == "((x > -2.0) and (x < 2.0))")
  }

  it should "include the endpoints for >=" in
  {
    assert(solved("solve(x^2 - 4 >= 0, x)") == "((x <= -2.0) or (x >= 2.0))")
  }

  it should "answer true when an upward parabola has no real roots" in
  {
    assert(solved("solve(x^2 + 1 > 0, x)") == "true")
  }

  it should "answer false for the empty set" in
  {
    assert(solved("solve(x^2 + 1 < 0, x)") == "false")
  }

  it should "handle a double root" in
  {
    // (x-1)^2 > 0 everywhere except x = 1.
    assert(solved("solve(x^2 - 2x + 1 > 0, x)") == "(x != 1.0)")
    assert(solved("solve(x^2 - 2x + 1 >= 0, x)") == "true")
    assert(solved("solve(x^2 - 2x + 1 < 0, x)") == "false")
  }

  it should "normalise a downward parabola by flipping" in
  {
    // 4 - x^2 > 0  <=>  x^2 - 4 < 0  <=>  -2 < x < 2
    assert(solved("solve(4 - x^2 > 0, x)") == "((x > -2.0) and (x < 2.0))")
  }

  it should "read a leading minus as applying to the whole additive chain" in
  {
    // Not a solver property but a grammar one, pinned here because it looks like a solver
    // bug: `expr ::= opt("+"|"-") ~ simpleExpr`, so the sign covers the ENTIRE chain and
    // `-x^2 + 4` means `-(x^2 + 4)`, which is <= -4 everywhere. `false` is the right answer.
    assert(solved("solve(-x^2 + 4 > 0, x)") == "false")
  }

  // ── conjunctions (slice D) ─────────────────────────────────────────────────

  "a conjunction" should "solve each side and recombine" in
  {
    assert(solved("solve(2x > 2 and x < 5, x)") == "((x > 1.0) and (x < 5.0))")
  }

  it should "recombine a disjunction too" in
  {
    assert(solved("solve(x < 0 or 2x > 10, x)") == "((x < 0.0) or (x > 5.0))")
  }

  it should "stay symbolic when one side cannot be solved" in
  {
    assert(staysSymbolic("solve(x > 1 and a * x < 6, x)"))
  }

  // ── rational (slice E) ─────────────────────────────────────────────────────

  "a rational inequality" should "exclude the pole and split at both critical points" in
  {
    // (x-1)/(x-2) > 0 on (-inf, 1) and (2, inf); x = 2 is a pole, not a root.
    assert(solved("solve((x - 1) / (x - 2) > 0, x)") == "((x < 1.0) or (x > 2.0))")
  }

  it should "bound between a root and a pole for < 0" in
  {
    assert(solved("solve((x - 1) / (x - 2) < 0, x)") == "((x > 1.0) and (x < 2.0))")
  }

  it should "solve a reciprocal" in
  {
    assert(solved("solve(1 / x > 0, x)") == "(x > 0.0)")
  }

  // ── the node contract ──────────────────────────────────────────────────────

  "an equation" should "still solve exactly as before" in
  {
    assert(solved("solve(2x = 6, x)") == "x = 3.0")
  }

  "the solution" should "round-trip through the parser" in
  {
    val s = solved("solve(x^2 - 4 > 0, x)")
    Parser.parse(s) match
      case Parser.Success(_, _) => succeed
      case other                => fail(s"solution '$s' did not re-parse: $other")
  }

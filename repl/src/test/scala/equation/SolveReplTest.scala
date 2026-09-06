package it.grypho.scala.leonardo
package equation

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

/** REPL-level behaviour of `solve`: named equations bound with `:=`, auto-binding of the
 *  solution, numbered roots for a quadratic, and the issue-1.5 regression that an identity
 *  must stay symbolic rather than emit a grid of fabricated roots.
 *
 *  Split out of `SolveTest` by issue 5.2 phase 1.1.
 */
class SolveReplTest extends AnyFlatSpec:

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

  "solve(sin(x) = sin(x)) via REPL" should "stay symbolic rather than emit fake solutions" in
  {
    val s = Session()
    val out = s.execute("solve(sin(x) = sin(x), x)")
    // No concrete solution set -- stays as the unsolved node (not a matrix of fake roots)
    assert(!out.startsWith("[["), s"expected no solution matrix but got: $out")
  }

package it.grypho.scala.leonardo
package domain

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** Issue 3.3 slice E — the `domain/` package surface. */
class DomainNodeTest extends AnyFlatSpec:

  private val env = new Environment()

  private def run(src: String): String =
    Parser.parse(src) match
      case Parser.Success(e, _) => e.eval(env).fold(_.toString, _.toString)
      case other                => fail(s"parse failed for '$src': $other")

  private def staysSymbolic(src: String): Boolean =
    val r = run(src)
    r.startsWith("domain(") || r.startsWith("differentiable(") || r.startsWith("singularities(")

  // ── domain ─────────────────────────────────────────────────────────────────

  "domain(ln(x), x)" should "answer x > 0" in
  {
    assert(run("domain(ln(x), x)") == "(x > 0.0)")
  }

  "domain(1/x, x)" should "exclude the pole as a union" in
  {
    assert(run("domain(1 / x, x)") == "((x < 0.0) or (x > 0.0))")
  }

  "domain(asin(x), x)" should "answer the closed unit interval" in
  {
    assert(run("domain(asin(x), x)") == "((x >= -1.0) and (x <= 1.0))")
  }

  "domain of a polynomial" should "be true" in
  {
    assert(run("domain(x^2 + 1, x)") == "true")
  }

  "domain(1/(x^2 - 1), x)" should "give three pieces" in
  {
    val r = run("domain(1 / (x^2 - 1), x)")
    assert(r.count(_ == 'o') >= 2, s"expected two 'or's joining three pieces, got $r")
  }

  "an infinite exclusion set" should "stay symbolic rather than be truncated" in
  {
    // tan and Gamma exclude infinitely many points; the language has no quantifier, and a
    // partial list would read as exhaustive.
    assert(staysSymbolic("domain(tan(x), x)"))
    assert(staysSymbolic("domain(Gamma(x), x)"))
  }

  "a symbolic coefficient" should "fall back to the raw constraint" in
  {
    // Intervals cannot be placed, but the constraint itself is still the honest answer.
    assert(run("domain(ln(x - a), x)") == "((x + (-1.0 * a)) > 0.0)")
  }

  // ── the complex arm ────────────────────────────────────────────────────────

  "domain(ln(x), x, complex)" should "need only a non-zero argument" in
  {
    assert(run("domain(ln(x), x, complex)") == "((x < 0.0) or (x > 0.0))")
  }

  "domain(asin(x), x, complex)" should "be empty, per the Asin convention" in
  {
    // The library leaves asin symbolic on complex input, so it computes nothing there.
    assert(run("domain(asin(x), x, complex)") == "false")
  }

  "real and complex" should "stay usable as ordinary variable names" in
  {
    // The keywords are matched only in the third-argument position.
    assert(run("real + 1") == "(real + 1.0)")
    assert(run("complex * 2") == "(complex * 2.0)")
  }

  // ── differentiable ─────────────────────────────────────────────────────────

  "differentiable(step(x), x)" should "exclude only the step" in
  {
    // F_0056 Decision B: was `false` until 2026-10-06, a defect the old pin tolerated.
    assert(run("differentiable(step(x), x)") == "((x < 0.0) or (x > 0.0))")
  }

  "differentiable(ln(x), x)" should "match where ln is defined" in
  {
    assert(run("differentiable(ln(x), x)") == "(x > 0.0)")
  }

  "differentiable(1/(1 - x^2), x)" should "exclude both poles exactly, as domain does (F_0278)" in
  {
    // The derivative's denominator (1 - x^2)^2 has double roots, which root-finding scattered to
    // 1 ± 5e-9: the answer read `x <= 1` beside `x > 1`.
    assert(run("differentiable(1/(1 - x^2), x)") == run("domain(1/(1 - x^2), x)"))
    assert(run("domain(1/(1 - x^2)^2, x)") == run("domain(1/(1 - x^2), x)"))
  }

  "a kink smoothed by a vanishing factor" should "not be excluded (F_0278)" in
  {
    // x*abs(x) has f'(0) = 0, and the library computes it; abs(x) itself has no derivative at 0.
    assert(run("differentiable(x*abs(x), x)") == "true")
    assert(run("differentiable(abs(x), x)") == "((x < 0.0) or (x > 0.0))")
    // A point outside the function's own domain is never re-included.
    assert(run("differentiable(1/x, x)") == "((x < 0.0) or (x > 0.0))")
    assert(run("differentiable(step(x), x)") == "((x < 0.0) or (x > 0.0))")
  }

  "a constant fractional exponent" should "restrict the base as a literal one does (F_0278)" in
  {
    // The library raises a negative base to 2/3 as the principal complex value, so the real
    // domain is x >= 0 -- whether the exponent is typed folded or as a quotient.
    assert(run("domain(x^(2/3), x)") == "(x >= 0.0)")
    assert(run("domain(x^0.5, x)") == "(x >= 0.0)")
    assert(run("differentiable(x^(2/3), x)") == "(x > 0.0)")
    // A negative fractional exponent divides by the base: zero is out too.
    assert(run("domain(x^(-1/3), x)") == "(x > 0.0)")
    assert(run("domain(x^-0.5, x)") == "(x > 0.0)")
    // Over the complex plane only the division by the base remains.
    assert(run("domain(x^-0.5, x, complex)") == "((x < 0.0) or (x > 0.0))")
    assert(run("domain(x^0.5, x, complex)") == "true")
  }

  // ── singularities ──────────────────────────────────────────────────────────

  "singularities(1/(x-1), x)" should "report one simple pole" in
  {
    // 2xN matrix: row 1 locations, row 2 orders.
    assert(run("singularities(1 / (x - 1), x)") == "[[1.0], [1.0]]")
  }

  "singularities of a polynomial" should "be the empty set, not a matrix" in
  {
    // `false` is how 3.2 already renders an empty set, and _Matrix cannot be 2x0.
    assert(run("singularities(x^2 + 1, x)") == "false")
  }

  "a transcendental" should "stay symbolic rather than claim none" in
  {
    // The distinction that matters: "cannot enumerate" must not collapse into "none".
    assert(staysSymbolic("singularities(tan(x), x)"))
  }

  // ── node contract ──────────────────────────────────────────────────────────

  "the answer" should "re-parse" in
  {
    val s = run("domain(1 / x, x)")
    Parser.parse(s) match
      case Parser.Success(_, _) => succeed
      case other                => fail(s"'$s' did not re-parse: $other")
  }

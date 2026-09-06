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

  "differentiable(step(x), x)" should "be false" in
  {
    assert(run("differentiable(step(x), x)") == "false")
  }

  "differentiable(ln(x), x)" should "match where ln is defined" in
  {
    assert(run("differentiable(ln(x), x)") == "(x > 0.0)")
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

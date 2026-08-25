package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** Issue 6.21 — the pattern unifier, the rule engine, and the data-driven integral table. */
class RewriteTest extends AnyFlatSpec:

  private val env = new Environment()
  private val x   = _Variable("x")
  private val y   = _Variable("y")

  private def run(src: String): String =
    Parser.parse(src) match
      case Parser.Success(e, _) => e.eval(env).fold(_.toString, _.toString)
      case other                => fail(s"parse failed for '$src': $other")

  /** Numeric value of a parsed expression at `x = at`. */
  private def at(src: String, v: Double): Option[Double] =
    Parser.parse(src) match
      case Parser.Success(e, _) =>
        e.eval(new Environment(variables = Map("x" -> _Number(v)))) match
          case Right(_Number(d)) if !d.isNaN && !d.isInfinite => Some(d)
          case _                                              => None
      case _ => None

  // ── the unifier ────────────────────────────────────────────────────────────

  "unify" should "bind a hole to any sub-expression" in
  {
    assert(unify(_Pattern("a"), Sin(x)).contains(Map("a" -> Sin(x))))
  }

  it should "match structurally and bind each hole" in
  {
    val pattern = Sum(_Pattern("a"), _Pattern("b"))
    assert(unify(pattern, Sum(x, _Number(2))).contains(Map("a" -> x, "b" -> _Number(2))))
  }

  it should "enforce binding consistency" in
  {
    // f(?a, ?a) must NOT match f(1, 2) -- the same name has to bind the same subtree, or a
    // rule fires on an expression it does not describe.
    val pattern = Sum(_Pattern("a"), _Pattern("a"))
    assert(unify(pattern, Sum(_Number(1), _Number(2))).isEmpty)
    assert(unify(pattern, Sum(_Number(1), _Number(1))).isDefined)
  }

  it should "reject a different node type" in
  {
    assert(unify(Sin(_Pattern("a")), Cos(x)).isEmpty)
  }

  it should "compare leaves by value" in
  {
    assert(unify(_Number(2), _Number(2)).isDefined)
    assert(unify(_Number(2), _Number(3)).isEmpty)
  }

  it should "distinguish fields that are not children" in
  {
    // The binder of a _Derivative is not a child, so a naive type+children test would let
    // these match. `rebuild` comparison is what catches it.
    assert(unify(_Derivative(_Pattern("a"), x), _Derivative(Sin(x), x)).isDefined)
    assert(unify(_Derivative(_Pattern("a"), x), _Derivative(Sin(x), y)).isEmpty)
  }

  // ── instantiation ──────────────────────────────────────────────────────────

  "instantiate" should "fill holes from the bindings" in
  {
    val t = Sum(_Pattern("a"), _Pattern("a"))
    assert(instantiate(t, Map("a" -> x)).contains(Sum(x, x)))
  }

  it should "decline a template naming an unbound hole" in
  {
    assert(instantiate(_Pattern("missing"), Map("a" -> x)).isEmpty)
  }

  // ── the engine ─────────────────────────────────────────────────────────────

  "applyRules" should "use the first matching rule" in
  {
    val rs = List(RewriteRule(Sin(_Pattern("a")), _Number(1), name = "first"),
                  RewriteRule(Sin(_Pattern("a")), _Number(2), name = "second"))
    assert(applyRules(rs, Sin(x)).contains(_Number(1)))
  }

  it should "respect a condition" in
  {
    val onlyNumbers = RewriteRule(Sin(_Pattern("a")), _Number(1),
                                  condition = _.get("a").exists(_.isInstanceOf[_Number]))
    assert(applyRules(List(onlyNumbers), Sin(_Number(0))).isDefined)
    assert(applyRules(List(onlyNumbers), Sin(x)).isEmpty)
  }

  "rewriteFully" should "reach a fixpoint and terminate on a cyclic rule set" in
  {
    // a + b -> b + a is non-terminating; the step cap must stop it rather than hang.
    val swap = RewriteRule(Sum(_Pattern("a"), _Pattern("b")), Sum(_Pattern("b"), _Pattern("a")))
    val out  = rewriteFully(List(swap), Sum(x, y))
    assert(out.isInstanceOf[Sum], s"should still be a Sum, got $out")
  }

  // ── the integral table, end to end ─────────────────────────────────────────

  "the data table" should "integrate tan" in
  {
    // Previously stayed symbolic. d/dx(-ln(cos x)) = tan x.
    val r = run("integral(tan(x), x)")
    assert(!r.startsWith("integral("), s"should now integrate, got $r")
    // Verify by differentiating back at a few points.
    for p <- Vector(0.3, 0.7, 1.0) do
      val d = at(s"derive($r, x)", p)
      assert(d.exists(g => math.abs(g - math.tan(p)) < 1e-6),
             s"d/dx of the antiderivative should be tan; at $p got $d")
  }

  it should "integrate ln(v)/v" in
  {
    val r = run("integral(ln(x) / x, x)")
    assert(!r.startsWith("integral("), s"should now integrate, got $r")
    for p <- Vector(1.5, 2.0, 3.0) do
      val d = at(s"derive($r, x)", p)
      assert(d.exists(g => math.abs(g - math.log(p) / p) < 1e-6),
             s"at $p expected ${math.log(p) / p}, got $d")
  }

  it should "integrate 1/(v ln v)" in
  {
    val r = run("integral(1 / (x * ln(x)), x)")
    assert(!r.startsWith("integral("), s"should now integrate, got $r")
    for p <- Vector(1.5, 2.0, 3.0) do
      val d = at(s"derive($r, x)", p)
      assert(d.exists(g => math.abs(g - 1.0 / (p * math.log(p))) < 1e-6),
             s"at $p expected ${1.0 / (p * math.log(p))}, got $d")
    }

  it should "not fire on the same shape in a different variable" in
  {
    // The `?v` hole must bind the ACTUAL integration variable; tan(y) integrated dx is a
    // constant multiple, not -ln(cos y).
    val r = run("integral(tan(y), x)")
    assert(!r.contains("ln"), s"a rule about x must not fire on tan(y) dx, got $r")
  }

  // ── the safety property: nothing that already worked may change ────────────

  "the compiled arms" should "still take precedence" in
  {
    assert(run("integral(exp(x), x)").contains("exp"))
    assert(run("integral(1 / x, x)") == "ln(x)")
    assert(run("integral(x^2, x)") == "((x ^ 3.0) / 3.0)")
  }

  "an integrand no rule covers" should "still stay symbolic" in
  {
    assert(run("integral(exp(x^2), x)").startsWith("integral("))
  }

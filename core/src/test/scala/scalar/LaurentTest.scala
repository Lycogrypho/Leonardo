package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** Issue 3.4 — Laurent series about an isolated singularity. */
class LaurentTest extends AnyFlatSpec:

  private val env = new Environment()
  private val x   = _Variable("x")

  private def run(src: String): String =
    Parser.parse(src) match
      case Parser.Success(e, _) => e.eval(env).fold(_.toString, _.toString)
      case other                => fail(s"parse failed for '$src': $other")

  private def staysSymbolic(src: String): Boolean = run(src).startsWith("laurent(")

  /** Numeric value of the parsed expression at `x = at`. */
  private def valueAt(src: String, at: Double): Option[Double] =
    Parser.parse(src) match
      case Parser.Success(e, _) =>
        e.eval(new Environment(variables = Map("x" -> _Number(at)))) match
          case Right(_Number(d)) if !d.isNaN && !d.isInfinite => Some(d)
          case _                                              => None
      case _ => None

  // ── the principal part ─────────────────────────────────────────────────────

  "1/(x-1) about 1" should "be its own Laurent series" in
  {
    // A simple pole: the whole series is the single term 1/(x-1).
    val r = run("laurent(1 / (x - 1), x, 1, 1, 2)")
    assert(!r.startsWith("laurent("), s"should expand, got $r")
    // Agrees with the original away from the pole.
    for p <- Vector(1.5, 2.0, 3.0) do
      val got = valueAt("laurent(1 / (x - 1), x, 1, 1, 2)", p)
      assert(got.exists(d => math.abs(d - 1.0 / (p - 1.0)) < 1e-9),
             s"at x=$p expected ${1.0 / (p - 1.0)}, got $got")
  }

  "1/(x^2 - 1) about 1" should "match the function near the pole" in
  {
    val src = "laurent(1 / (x^2 - 1), x, 1, 1, 3)"
    assert(!staysSymbolic(src))
    for p <- Vector(1.1, 1.2, 0.9) do
      val expected = 1.0 / (p * p - 1.0)
      val got      = valueAt(src, p)
      assert(got.exists(d => math.abs(d - expected) < 1e-3),
             s"at x=$p expected ~$expected, got $got")
  }

  "sin(x)/x^2 about 0" should "expand through the cancelling factor" in
  {
    // g = x^2 * sin(x)/x^2 = sin(x) only after simplification -- the case that proves
    // multiplying is not the same as cancelling.
    val src = "laurent(sin(x) / x^2, x, 0, 1, 3)"
    assert(!staysSymbolic(src), s"should expand, got ${run(src)}")
    for p <- Vector(0.3, 0.5, -0.4) do
      val expected = math.sin(p) / (p * p)
      val got      = valueAt(src, p)
      assert(got.exists(d => math.abs(d - expected) < 1e-2),
             s"at x=$p expected ~$expected, got $got")
  }

  // ── auto-detected pole order (3.3 slice D) ─────────────────────────────────

  "the four-argument form" should "detect the pole order itself" in
  {
    val auto  = run("laurent(1 / (x - 1), x, 1, 2)")
    val stated = run("laurent(1 / (x - 1), x, 1, 1, 2)")
    assert(auto == stated, s"auto-detection should match the stated order:\n  $auto\n  $stated")
  }

  it should "detect an order-2 pole" in
  {
    val auto  = run("laurent(1 / (x - 1)^2, x, 1, 2)")
    val stated = run("laurent(1 / (x - 1)^2, x, 1, 2, 2)")
    assert(auto == stated, s"auto:\n  $auto\nstated:\n  $stated")
  }

  // ── degenerate and refused cases ───────────────────────────────────────────

  "a removable singularity" should "give an ordinary Taylor series rather than an error" in
  {
    // m = 0 means no principal part; the caller asked a well-formed question.
    val laurent = run("laurent(x^2 + 1, x, 0, 0, 3)")
    val taylor  = run("taylor(x^2 + 1, x, 0, 3)")
    assert(laurent == taylor, s"laurent=$laurent taylor=$taylor")
  }

  "a point that is not a singularity" should "fall through to Taylor" in
  {
    val auto   = run("laurent(x^2 + 1, x, 0, 3)")
    val taylor = run("taylor(x^2 + 1, x, 0, 3)")
    assert(auto == taylor, s"auto=$auto taylor=$taylor")
  }

  "an essential singularity" should "be refused, not truncated" in
  {
    // exp(1/x) at 0 has an INFINITE principal part. A truncation carries a different
    // contract from the polynomial tiers: the discarded terms blow up near the point
    // instead of becoming small.
    assert(staysSymbolic("laurent(exp(1 / x), x, 0, 3)"))
  }

  "an order beyond the cap" should "stay symbolic" in
  {
    assert(staysSymbolic("laurent(1 / (x - 1), x, 1, 5, 30)"))
  }

  "a negative order" should "stay symbolic" in
  {
    assert(staysSymbolic("laurent(1 / (x - 1), x, 1, -1, 3)"))
  }

  // ── node contract ──────────────────────────────────────────────────────────

  "the node" should "keep v out of children so substitute cannot rewrite it" in
  {
    val n = _Laurent(Ratio(_Number(1), Sum(x, _Number(-1))), x, _Number(1), Some(_Number(1)), _Number(2))
    assert(!n.children.contains(x), s"v must not be a child: ${n.children}")
  }

  it should "round-trip through toString and the parser" in
  {
    val src = "laurent(1 / (x - 1), x, 1, 1, 2)"
    Parser.parse(src) match
      case Parser.Success(e, _) =>
        Parser.parse(e.toString) match
          case Parser.Success(e2, _) => assert(e.toString == e2.toString)
          case other                 => fail(s"'${e.toString}' did not re-parse: $other")
      case other => fail(s"parse failed: $other")
  }

  it should "print the four-argument form when the order was omitted" in
  {
    Parser.parse("laurent(1 / (x - 1), x, 1, 2)") match
      case Parser.Success(e, _) =>
        assert(e.toString.startsWith("laurent(") && e.toString.endsWith(", x, 1.0, 2.0)"),
               s"expected the four-argument form, got ${e.toString}")
      case other => fail(s"parse failed: $other")
  }

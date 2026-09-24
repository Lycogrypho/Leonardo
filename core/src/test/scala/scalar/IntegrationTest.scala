package it.grypho.scala.leonardo
package scalar

import core.*
import scalar.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


class IntegrationTest extends AnyFlatSpec:

  val x = _Variable("x")
  def env: Environment = new Environment()

  def evalDefInt(integrand: _Expression, v: _Variable, a: Double, b: Double): Double =
    _DefIntegral(integrand, v, _Number(a), _Number(b)).eval(env) match
      case Right(_Number(y)) => y
      case other             => fail(s"expected a numeric result but got: $other")

  /** Parses and evaluates a source line, failing with the parser's own message. */
  def evalOf(src: String): Either[_Expression, _Value] =
    val p = Parser.parse(src)
    assert(p.successful, s"'$src' must parse: $p")
    p.get.eval(env)

  // --- the limits are ordinary expressions (F_0040) -------------------------------------

  "a definite integral's limits" should "accept an expression, not just a signed atom" in
  {
    // Reported 2026-09-24: `integral(sin(x), x, -100*pi, 100*pi)` was a PARSE error at the
    // `*`, because the limit positions took `signedValue` -- an optional sign and one atom --
    // while every other bounded construct (`ode`, `taylor`, `laurent`, `tabulate`, `sum`)
    // took a full expression. `0` to `2*pi` is the commonest definite integral there is.
    evalOf("integral(sin(x), x, -100*pi, 100*pi)") match
      case Right(_Number(d)) => assert(!d.isNaN && !d.isInfinite, d)
      case other             => fail(s"expected a number, got: $other")
    // Exact over a full period, and Simpson is accurate on a smooth periodic integrand.
    evalOf("integral(sin(x), x, 0, 2*pi)") match
      case Right(_Number(d)) => assert(math.abs(d) < 1e-4, d)
      case other             => fail(s"expected a number, got: $other")
    // An arithmetic bound: the upper limit is 2, so the answer is 2.
    evalOf("integral(x, x, 0, 1+1)") match
      case Right(_Number(d)) => assert(math.abs(d - 2.0) < 1e-4, d)
      case other             => fail(s"expected a number, got: $other")
  }

  it should "still carry an outer variable, so iterated integration keeps working" in
  {
    // The limits are `children`, not binders, so an inner limit naming the outer variable is
    // an ordinary free occurrence -- and it may now be an expression in it.
    evalOf("integral(integral(1, y, 0, 2*x), x, 0, 1)") match
      case Right(_Number(d)) => assert(math.abs(d - 1.0) < 1e-4, d)
      case other             => fail(s"expected a number, got: $other")
  }

  it should "round-trip, since toString prints the limits as expressions" in
  {
    val src = "integral(sin(x), x, 0, 2*pi)"
    val e   = Parser.parse(src)
    assert(e.successful, src)
    assert(Parser.parse(e.get.toString).successful, s"round-trip of ${e.get}")
  }

  "the indefinite form" should "still be reached when no limits follow" in
  {
    // The 4-argument arm is tried first and must fail cleanly back to the 2-argument one.
    assert(Parser.parse("integral(sin(x), x)").get.isInstanceOf[_Integral])
  }

  "∫1 dx from 0 to 5" should "equal 5.0" in
  {
    assert(math.abs(evalDefInt(_Number(1), x, 0, 5) - 5.0) < 1e-4)
  }

  "∫x dx from 0 to 1" should "equal 0.5" in
  {
    assert(math.abs(evalDefInt(x, x, 0, 1) - 0.5) < 1e-4)
  }

  "∫x² dx from 0 to 1" should "equal 1/3" in
  {
    assert(math.abs(evalDefInt(Power(x, _Number(2)), x, 0, 1) - 1.0 / 3.0) < 1e-4)
  }

  "∫sin(x) dx from 0 to π" should "equal 2.0" in
  {
    assert(math.abs(evalDefInt(Sin(x), x, 0, math.Pi) - 2.0) < 1e-4)
  }

  "∫cos(x) dx from 0 to π/2" should "equal 1.0" in
  {
    assert(math.abs(evalDefInt(Cos(x), x, 0, math.Pi / 2) - 1.0) < 1e-4)
  }

  "∫exp(x) dx from 0 to 1" should "equal e - 1" in
  {
    assert(math.abs(evalDefInt(Exp(x), x, 0, 1) - (math.E - 1)) < 1e-4)
  }

  "∫x dx from 1 to 3" should "equal 4.0" in
  {
    assert(math.abs(evalDefInt(x, x, 1, 3) - 4.0) < 1e-4)
  }

  "∫(x + y) dx from 0 to 1 with y unbound" should "stay symbolic" in
  {
    val y = _Variable("y")
    _DefIntegral(Sum(x, y), x, _Number(0), _Number(1)).eval(env) match
      case Left(_)  => succeed
      case Right(v) => fail(s"expected symbolic but got $v")
  }

  // Indefinite integration is now implemented (see IndefiniteIntegrationTest); with
  // x unbound the antiderivative x²/2 cannot fold to a number, so it stays symbolic —
  // but for a reason different from the old "not implemented" stub.
  "indefinite ∫x dx with x unbound" should "stay symbolic (antiderivative x²/2)" in
  {
    _Integral(x, x).eval(env) match
      case Left(_)  => succeed
      case Right(v) => fail(s"expected symbolic but got $v")
  }

  "indefinite ∫x dx with x bound to 4" should "reduce to 8.0 (x²/2)" in
  {
    val bound = new Environment().withBinding("x", _Number(4))
    _Integral(x, x).eval(bound) match
      case Right(_Number(y)) => assert(math.abs(y - 8.0) < 1e-4)
      case other             => fail(s"expected 8.0 but got: $other")
  }

  "parse+eval of \"integral(x, x, 0, 1)\"" should "equal 0.5" in
  {
    val result = Parser.parse("integral(x, x, 0, 1)")
    assert(result.successful, s"parse failed: $result")
    result.get.eval(env) match
      case Right(_Number(y)) => assert(math.abs(y - 0.5) < 1e-4)
      case other             => fail(s"expected a numeric result but got: $other")
  }

  // --- issue 2: step count scales with env.precision ---

  "∫x dx from 0 to 1 with precision=10" should "equal 0.5 with tight tolerance" in
  {
    val preciseEnv = new Environment(10)
    _DefIntegral(x, x, _Number(0), _Number(1)).eval(preciseEnv) match
      case Right(_Number(y)) => assert(math.abs(y - 0.5) < 1e-9)
      case other             => fail(s"expected numeric result but got: $other")
  }

  "∫x dx from 0 to 1 with precision=1" should "still approximate 0.5" in
  {
    val coarseEnv = new Environment(1)
    _DefIntegral(x, x, _Number(0), _Number(1)).eval(coarseEnv) match
      case Right(_Number(y)) => assert(math.abs(y - 0.5) < 1e-2)
      case other             => fail(s"expected numeric result but got: $other")
  }

  // --- NaN/Infinite guard: unbounded integrands must stay symbolic ---

  // Fast path (compiled closure): 1/x is compilable; at x=0 produces Infinity.
  // The existing fast-path guard catches this — regression test.
  "∫ 1/x dx from 0 to 1 (fast path, unbounded at 0)" should "stay symbolic" in
  {
    _DefIntegral(Ratio(_Number(1), x), x, _Number(0), _Number(1)).eval(env) match
      case Left(_)  => succeed
      case Right(v) => fail(s"expected symbolic but got $v")
  }

  // Fallback path (non-compiled): _Derivative nodes are not compilable, so the
  // tree-eval loop is used. d/dx(exp(1000x)) = 1000*exp(1000x); at x=1 this is
  // Infinity in double precision, accumulating to Infinity in the sum.
  // Before the fix the fallback returned Right(_Number(Infinity)); now Left(this).
  "∫ d/dx(exp(1000x)) dx from 0 to 1 (fallback path, overflows to Infinity)" should "stay symbolic" in
  {
    val integrand = _Derivative(Exp(Product(_Number(1000.0), x)), x)
    _DefIntegral(integrand, x, _Number(0.0), _Number(1.0)).eval(env) match
      case Left(_)  => succeed
      case Right(v) => fail(s"expected symbolic but got $v")
  }

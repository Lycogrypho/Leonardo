package it.grypho.scala.leonardo
package scalar

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** Issue 3.9 — hyperbolic and reciprocal-trigonometric functions.
 *
 *  Covers the twelve new nodes (`sinh`/`cosh`/`tanh`/`asinh`/`acosh`/`atanh`,
 *  `sec`/`csc`/`cot`/`sech`/`csch`/`coth`): evaluation, domain handling, derivatives,
 *  the ratio→node normalisation, integration, round-trip and complex closure.
 */
class HyperbolicTest extends AnyFlatSpec:

  private val env = new Environment()
  private val x   = _Variable("x")

  private def parse(src: String): _Expression =
    Parser.parse(src) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for '$src': $other")

  /** Numeric value at `x = p` (default: no free variable). */
  private def num(src: String, p: Double = 0.0): Option[Double] =
    parse(src).eval(new Environment(variables = Map("x" -> _Number(p)))) match
      case Right(_Number(d)) if !d.isNaN && !d.isInfinite => Some(d)
      case _                                              => None

  private def symbolic(src: String): Boolean = parse(src).eval(env).isLeft

  private def valueOf(src: String): _Value = parse(src).eval(env) match
    case Right(v) => v
    case Left(_)  => fail(s"'$src' stayed symbolic")

  private def close(a: Double, b: Double): Boolean = math.abs(a - b) < 1e-9

  // ── evaluation ───────────────────────────────────────────────────────────

  "the hyperbolic functions" should "evaluate at a real point" in
  {
    assert(num("sinh(1)").exists(close(_, math.sinh(1))))
    assert(num("cosh(1)").exists(close(_, math.cosh(1))))
    assert(num("tanh(1)").exists(close(_, math.tanh(1))))
    assert(num("asinh(2)").exists(close(_, math.log(2 + math.sqrt(5)))))
    assert(num("acosh(2)").exists(close(_, math.log(2 + math.sqrt(3)))))
    assert(num("atanh(0.5)").exists(close(_, 0.5 * math.log(3))))
  }

  "the reciprocal functions" should "evaluate at a real point" in
  {
    assert(num("sec(1)").exists(close(_, 1 / math.cos(1))))
    assert(num("csc(1)").exists(close(_, 1 / math.sin(1))))
    assert(num("cot(1)").exists(close(_, math.cos(1) / math.sin(1))))
    assert(num("sech(1)").exists(close(_, 1 / math.cosh(1))))
    assert(num("csch(1)").exists(close(_, 1 / math.sinh(1))))
    assert(num("coth(1)").exists(close(_, math.cosh(1) / math.sinh(1))))
  }

  "out-of-domain arguments" should "stay symbolic" in
  {
    assert(symbolic("acosh(0.5)"))   // acosh real only for x >= 1
    assert(symbolic("atanh(2)"))     // atanh real only for |x| < 1
    assert(symbolic("csch(0)"))      // pole at 0
    assert(symbolic("coth(0)"))
  }

  // ── identifiers merely starting with a keyword stay variables ──────────────

  "names extending a reserved word" should "stay variables" in
  {
    assert(parse("cosine") == _Variable("cosine"))
    assert(parse("sechx")  == _Variable("sechx"))
    assert(parse("cotangent") == _Variable("cotangent"))
  }

  // ── derivatives ────────────────────────────────────────────────────────────

  "derivatives" should "follow the standard rules" in
  {
    assert(derive(parse("sinh(x)"), x) == Cosh(x))
    assert(derive(parse("cosh(x)"), x) == Sinh(x))
    assert(derive(parse("tanh(x)"), x) == Power(Sech(x), _Number(2)))
  }

  it should "be numerically correct for the reciprocal and inverse families" in
  {
    // d/dx sec(x) = sec(x)tan(x); d/dx asinh(x) = 1/sqrt(x^2+1); d/dx atanh(x) = 1/(1-x^2)
    val p = 0.6
    assert(num(derive(parse("sec(x)"), x).toString, p)
             .exists(close(_, (1 / math.cos(p)) * math.tan(p))))
    assert(num(derive(parse("asinh(x)"), x).toString, p)
             .exists(close(_, 1 / math.sqrt(p * p + 1))))
    assert(num(derive(parse("atanh(x)"), x).toString, p)
             .exists(close(_, 1 / (1 - p * p))))
  }

  // ── simplification ──────────────────────────────────────────────────────────

  "inverse-function pairs" should "cancel where always valid" in
  {
    assert(simplifyFully(parse("sinh(asinh(x))")) == x)
    assert(simplifyFully(parse("asinh(sinh(x))")) == x)
    assert(simplifyFully(parse("tanh(atanh(x))")) == x)
    // acosh(cosh(x)) = |x| is NOT x, so it must not cancel
    assert(simplifyFully(parse("acosh(cosh(x))")) == Acosh(Cosh(x)))
  }

  "known values at zero" should "fold" in
  {
    assert(simplifyFully(parse("sinh(0)")) == _Number(0))
    assert(simplifyFully(parse("cosh(0)")) == _Number(1))
    assert(simplifyFully(parse("sech(0)")) == _Number(1))
  }

  "ratio spellings" should "normalise into the reciprocal nodes" in
  {
    assert(simplifyFully(parse("1 / cos(x)")) == Sec(x))
    assert(simplifyFully(parse("1 / sin(x)")) == Csc(x))
    assert(simplifyFully(parse("cos(x) / sin(x)")) == Cot(x))
    assert(simplifyFully(parse("1 / cos(x)^2")) == Power(Sec(x), _Number(2)))
    assert(simplifyFully(parse("1 / cosh(x)")) == Sech(x))
    assert(simplifyFully(parse("cosh(x) / sinh(x)")) == Coth(x))
  }

  // ── round-trip ──────────────────────────────────────────────────────────────

  "every new function" should "round-trip through toString" in
  {
    for name <- List("sinh", "cosh", "tanh", "asinh", "acosh", "atanh",
                     "sec", "csc", "cot", "sech", "csch", "coth") do
      val src = s"$name(x)"
      assert(parse(src).toString == src, s"round-trip failed for $src")
  }

  // ── integration (differentiate the antiderivative back) ──────────────────────

  private def verify(integrand: String, points: Double*): Unit =
    val f    = parse(integrand)
    val anti = integrate(f, x)
    assert(!anti.toString.startsWith("integral("), s"∫ $integrand should close, got $anti")
    val back = derive(anti, x)
    for p <- points do
      (num(back.toString, p), num(integrand, p)) match
        case (Some(got), Some(want)) =>
          assert(math.abs(got - want) < 1e-7 * math.max(1.0, math.abs(want)),
                 s"d/dx of ∫ $integrand at x=$p: got $got, want $want (anti=$anti)")
        case (g, w) => fail(s"could not evaluate at x=$p ($g, $w); anti=$anti")

  "the hyperbolic integrals" should "close and differentiate back" in
  {
    verify("sinh(x)", 0.3, 0.7)
    verify("cosh(x)", 0.3, 0.7)
    verify("tanh(x)", 0.3, 0.7)
    verify("1 / sinh(x)^2", 0.4, 0.8)   // csch^2 -> -coth
    verify("1 / cosh(x)^2", 0.3, 0.7)   // sech^2 -> tanh
  }

  "the reciprocal-trig integrals" should "close via node or ratio spelling" in
  {
    verify("sec(x) * tan(x)", 0.3, 0.7)
    verify("cot(x)", 0.5, 1.0)
    verify("sec(x)", 0.3, 0.7)
  }

  it should "carry the linear-argument chain rule" in
  {
    verify("sinh(2 * x)", 0.3, 0.6)
    verify("sec(3 * x)", 0.1, 0.2)
  }

  // ── complex closure ─────────────────────────────────────────────────────────

  "complex closure" should "evaluate a hyperbolic at a complex point" in
  {
    // sinh(1 + i) = sinh(1)cos(1) + i·cosh(1)sin(1)
    _Complex.parts(valueOf("sinh(1 + i)")) match
      case Some((re, im)) =>
        assert(close(re, math.sinh(1) * math.cos(1)))
        assert(close(im, math.cosh(1) * math.sin(1)))
      case None => fail("sinh(1 + i) should be complex")
    // cosh(i*pi) = cos(pi) = -1 (imaginary part vanishes exactly, since sinh(0) = 0)
    assert(num("cosh(i * pi)").exists(close(_, -1.0)))
  }

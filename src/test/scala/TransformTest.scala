package it.grypho.scala.leonardo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.BeforeAndAfter
import core.*
import scalar.*
import transform.*
import parser.Parser


class TransformTest extends AnyFlatSpec with BeforeAndAfter:

  private val emptyEnv = new Environment()

  private def parse(s: String): _Expression = Parser.parse(s).get

  private def evalAt(expr: String, bindings: (String, Double)*): Either[_Expression, _Value] =
    val env = new Environment(5, bindings.map((k, v) => k -> _Number(v)).toMap)
    parse(expr).eval(env)

  private def approxAt(expr: String, expected: Double, tol: Double, bindings: (String, Double)*): Unit =
    evalAt(expr, bindings*).toExpression match
      case _Number(d) => assert(math.abs(d - expected) <= tol, s"$expr: expected ≈ $expected but got $d")
      case other => fail(s"$expr: expected _Number ≈ $expected, got $other")

  // ─────────────────────────── parser round-trips ──────────────────────────────

  "laplace parser" should "produce a _Laplace node" in:
    val e = parse("laplace(sin(t), t, s)")
    assert(e.isInstanceOf[_Laplace])
    val l = e.asInstanceOf[_Laplace]
    assert(l.t.variable == "t")
    assert(l.s.variable == "s")

  it should "round-trip through toString and re-parse" in:
    val e   = parse("laplace(sin(2*t), t, s)")
    val str = e.toString
    val e2  = parse(str)
    assert(e == e2)

  "fourier parser" should "produce a _Fourier node" in:
    val e = parse("fourier(exp(-t), t, w)")
    assert(e.isInstanceOf[_Fourier])
    val f = e.asInstanceOf[_Fourier]
    assert(f.t.variable == "t")
    assert(f.w.variable == "w")

  it should "round-trip through toString and re-parse" in:
    val e   = parse("fourier(exp(-2*t), t, w)")
    val str = e.toString
    val e2  = parse(str)
    assert(e == e2)

  // ─────────────────────────── Laplace: basic pairs ────────────────────────────

  "laplace transform" should "evaluate L{1} = 1/s (s=2 → 0.5)" in:
    approxAt("laplace(1, t, s)", 0.5, 1e-6, "s" -> 2.0)

  it should "evaluate L{3} = 3/s (s=2 → 1.5)" in:
    approxAt("laplace(3, t, s)", 1.5, 1e-6, "s" -> 2.0)

  it should "evaluate L{t} = 1/s^2 (s=2 → 0.25)" in:
    approxAt("laplace(t, t, s)", 0.25, 1e-6, "s" -> 2.0)

  it should "evaluate L{t^2} = 2/s^3 (s=2 → 0.25)" in:
    approxAt("laplace(t^2, t, s)", 0.25, 1e-6, "s" -> 2.0)   // 2/8

  it should "evaluate L{t^3} = 6/s^4 (s=2 → 0.375)" in:
    approxAt("laplace(t^3, t, s)", 0.375, 1e-6, "s" -> 2.0)  // 6/16

  it should "evaluate L{e^{3t}} = 1/(s-3) (s=5 → 0.5)" in:
    approxAt("laplace(exp(3*t), t, s)", 0.5, 1e-6, "s" -> 5.0)

  it should "evaluate L{e^{-t}} = 1/(s+1) (s=3 → 0.25)" in:
    approxAt("laplace(exp(-t), t, s)", 0.25, 1e-6, "s" -> 3.0)

  it should "evaluate L{sin(2t)} = 2/(s^2+4) (s=3 → 2/13)" in:
    approxAt("laplace(sin(2*t), t, s)", 2.0 / 13.0, 1e-5, "s" -> 3.0)

  it should "evaluate L{cos(2t)} = s/(s^2+4) (s=3 → 3/13)" in:
    approxAt("laplace(cos(2*t), t, s)", 3.0 / 13.0, 1e-5, "s" -> 3.0)

  it should "evaluate L{sin(t)} = 1/(s^2+1) (s=2 → 0.2)" in:
    approxAt("laplace(sin(t), t, s)", 0.2, 1e-5, "s" -> 2.0)  // 1/5

  // ─────────────────────────── Laplace: linearity ──────────────────────────────

  it should "apply linearity: L{2*sin(t) + cos(t)} (s=3 → 0.5)" in:
    // L{2*sin(t)} = 2/(9+1) = 0.2; L{cos(t)} = 3/(9+1) = 0.3; sum = 0.5
    approxAt("laplace(2*sin(t) + cos(t), t, s)", 0.5, 1e-5, "s" -> 3.0)

  it should "apply constant multiple: L{5*t^2} (s=2 → 1.25)" in:
    approxAt("laplace(5*t^2, t, s)", 1.25, 1e-5, "s" -> 2.0)  // 5 * 2/8 = 10/8 = 1.25

  // ─────────────────────────── Laplace: first-shift theorem ────────────────────

  it should "apply first-shift: L{e^{2t}*sin(3t)} = 3/((s-2)^2+9) (s=5 → 1/6)" in:
    approxAt("laplace(exp(2*t)*sin(3*t), t, s)", 1.0 / 6.0, 1e-5, "s" -> 5.0)

  it should "apply first-shift (commuted): L{sin(3t)*e^{2t}} (s=5 → 1/6)" in:
    approxAt("laplace(sin(3*t)*exp(2*t), t, s)", 1.0 / 6.0, 1e-5, "s" -> 5.0)

  it should "apply first-shift: L{e^{2t}*cos(3t)} = (s-2)/((s-2)^2+9) (s=7 → 5/34)" in:
    approxAt("laplace(exp(2*t)*cos(3*t), t, s)", 5.0 / 34.0, 1e-5, "s" -> 7.0)

  it should "apply first-shift: L{t*e^{-t}} = 1/(s+1)^2 (s=3 → 1/16)" in:
    approxAt("laplace(t*exp(-t), t, s)", 0.0625, 1e-5, "s" -> 3.0)

  it should "apply first-shift: L{t^2*e^{3t}} = 2/(s-3)^3 (s=5 → 0.25)" in:
    approxAt("laplace(t^2*exp(3*t), t, s)", 0.25, 1e-5, "s" -> 5.0)  // 2/8

  it should "apply first-shift: L{e^{-t}*cos(2t)} = (s+1)/((s+1)^2+4) (s=2 → 3/13)" in:
    // (s+1)/((s+1)^2+4) at s=2: 3/(9+4) = 3/13
    approxAt("laplace(exp(-t)*cos(2*t), t, s)", 3.0 / 13.0, 1e-5, "s" -> 2.0)

  // ─────────────────────────── Laplace: symbolic fallback ──────────────────────

  it should "stay symbolic for unrecognized form sin(t^2)" in:
    val r = parse("laplace(sin(t^2), t, s)").eval(emptyEnv).toExpression
    assert(r.isInstanceOf[_Laplace], s"expected symbolic _Laplace, got $r")

  it should "stay symbolic when exponential has non-linear exponent" in:
    val r = parse("laplace(exp(t^2), t, s)").eval(emptyEnv).toExpression
    assert(r.isInstanceOf[_Laplace], s"expected symbolic _Laplace, got $r")

  it should "stay symbolic for L{t^n} when n > 20 (cap matches Expand/Normalize)" in:
    val r = parse("laplace(t^21, t, s)").eval(emptyEnv).toExpression
    assert(r.isInstanceOf[_Laplace], s"expected symbolic _Laplace, got $r")

  it should "stay symbolic for L{t^n} with very large n (would overflow Double or stack)" in:
    val r = parse("laplace(t^200, t, s)").eval(emptyEnv).toExpression
    assert(r.isInstanceOf[_Laplace], s"expected symbolic _Laplace, got $r")

  // ─────────────────────────── Fourier: rule table ─────────────────────────────

  "fourier transform" should "evaluate F{1} = 1/(iw) — pure imaginary at w=2 → im=-0.5" in:
    evalAt("fourier(1, t, w)", "w" -> 2.0).toExpression match
      case c: _Complex =>
        assert(math.abs(c.re) < 1e-5, s"expected zero real part, got ${c.re}")
        assert(math.abs(c.im + 0.5) < 1e-5, s"expected im=-0.5, got ${c.im}")
      case other => fail(s"expected _Complex, got $other")

  it should "evaluate F{e^{-2t}} = 1/(2+iw) — real at w=0 → 0.5" in:
    evalAt("fourier(exp(-2*t), t, w)", "w" -> 0.0).toExpression match
      case _Number(d)  => assert(math.abs(d - 0.5) < 1e-5)
      case c: _Complex => assert(math.abs(c.re - 0.5) < 1e-5 && math.abs(c.im) < 1e-5)
      case other       => fail(s"expected numeric ≈ 0.5, got $other")

  it should "evaluate F{e^{-t}*sin(t)} at w=1 to 0.2 - 0.4i" in:
    // F{e^{-t}sin(t)} = L{sin(t)}(s+1) at s=iw: 1/((s+1)^2+1) at s=i:
    // (i+1)^2 = 2i; denominator = 2i+1; 1/(1+2i) = (1-2i)/5 = 0.2 - 0.4i
    evalAt("fourier(exp(-t)*sin(t), t, w)", "w" -> 1.0).toExpression match
      case c: _Complex =>
        assert(math.abs(c.re - 0.2) < 1e-4, s"expected real≈0.2, got ${c.re}")
        assert(math.abs(c.im + 0.4) < 1e-4, s"expected im≈-0.4, got ${c.im}")
      case other => fail(s"expected _Complex, got $other")

  it should "stay symbolic for unrecognized form" in:
    val r = parse("fourier(sin(t^2), t, w)").eval(emptyEnv).toExpression
    assert(r.isInstanceOf[_Fourier], s"expected symbolic _Fourier, got $r")

  it should "not corrupt a free variable whose name matches the old hard-coded sentinel" in:
    // _Variable("__lt_s__") is constructed via the API (the parser rejects names starting
    // with '_'). fourierOf must pick a different internal variable, so __lt_s__ survives
    // in the result: F{c} = c/(i*w), i.e. the result contains __lt_s__ unsubstituted.
    val e      = _Variable("__lt_s__")
    val result = fourierOf(e, _Variable("t"), _Variable("w"))
    assert(result.toString.contains("__lt_s__"), s"__lt_s__ was corrupted: $result")

  // ─────────────────────────── reserved words ───────────────────────────────────

  "laplace and fourier" should "be reserved words (not parseable as variables)" in:
    assertThrows[Exception]:
      parse("laplace + 1")
    assertThrows[Exception]:
      parse("fourier + 1")

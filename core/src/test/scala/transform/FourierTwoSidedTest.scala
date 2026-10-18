package it.grypho.scala.leonardo
package transform

import org.scalatest.flatspec.AnyFlatSpec
import core.*
import scalar.*
import parser.Parser


/** F_0141: `fourier` is the two-sided transform and `fourierCausal` the transform of `x(t)·u(t)`;
 *  both decline unless every pole of each Laplace part lies strictly left of the imaginary axis. */
class FourierTwoSidedTest extends AnyFlatSpec:

  private def parse(s: String): _Expression = Parser.parse(s).get

  private def at(expr: String, w: Double): (Double, Double) =
    parse(expr).eval(new Environment(5, Map("w" -> _Number(w)))) match
      case Right(v) => _Complex.parts(v).getOrElse(fail(s"$expr at $w: not a number: $v"))
      case Left(e)  => fail(s"$expr at $w: unevaluated: $e")

  private def near(expr: String, w: Double, re: Double, im: Double, tol: Double = 1e-9): Unit =
    val (a, b) = at(expr, w)
    val scale  = math.max(math.hypot(re, im), 1.0)
    assert(math.hypot(a - re, b - im) <= tol * scale, s"$expr at $w: ($a, $b), wanted ($re, $im)")

  private def declines(expr: String): Unit =
    parse(expr).eval(new Environment()) match
      case Left(_: _Fourier)       => ()
      case Left(_: _FourierCausal) => ()
      case other                   => fail(s"$expr should stay unevaluated, got $other")

  private def hasComplex(e: _Expression): Boolean = e match
    case _: _Complex => true
    case other       => other.children.exists(hasComplex)

  "fourier" should "answer e^(−|t|) as the real rational 2/(1 + ω²)" in:
    near("fourier(exp(-abs(t)), t, w)", 0.0, 2.0, 0.0)
    near("fourier(exp(-abs(t)), t, w)", 2.0, 0.4, 0.0)
    parse("fourier(exp(-abs(t)), t, w)").eval(new Environment()) match
      case Left(r) =>
        assert(!r.toString.contains("fourier"), s"declined: $r")
        assert(!hasComplex(r), s"an even signal's transform should be real: $r")
      case Right(v) => fail(s"not a function of w: $v")

  it should "answer an odd signal as a purely imaginary transform" in:
    // t·e^(−|t|): 1/(1 + iω)² − 1/(1 − iω)² = −4iω/(1 + ω²)², so −i at ω = 1.
    near("fourier(t*exp(-abs(t)), t, w)", 1.0, 0.0, -1.0)

  it should "answer the centred pulse as 2·sin(ω)/ω, its pole at 0 removable" in:
    near("fourier(step(t + 1) - step(t - 1), t, w)", 1.0, 2 * math.sin(1.0), 0.0)
    near("fourier(step(t + 1) - step(t - 1), t, w)", 2.0, math.sin(2.0), 0.0)

  it should "answer a signal written as causal like fourierCausal does" in:
    near("fourier(step(t)*exp(-t), t, w)", 1.0, 0.5, -0.5)
    near("fourierCausal(exp(-t), t, w)", 1.0, 0.5, -0.5)

  it should "decline a signal that diverges as t → −∞" in:
    declines("fourier(exp(-t), t, w)")

  it should "decline 1 and cos(t), whose transforms are Dirac deltas, in both forms" in:
    declines("fourier(1, t, w)")
    declines("fourier(cos(t), t, w)")
    declines("fourierCausal(1, t, w)")
    declines("fourierCausal(cos(t), t, w)")
    declines("fourierCausal(step(t), t, w)")

  it should "decline a pole whose sign depends on a free parameter" in:
    declines("fourier(exp(-a*abs(t)), t, w)")
    declines("fourierCausal(exp(-a*t), t, w)")

  it should "decline a signal the Laplace table cannot read, as before" in:
    declines("fourier(exp(-t^2), t, w)")

  it should "not move with the units of time (F_0141 conditioning)" in:
    // e^(−10⁻⁶|t|): 2·10⁻⁶/(10⁻¹² + ω²) = 10⁶ at ω = 10⁻⁶.
    near("fourier(exp(-0.000001*abs(t)), t, w)", 1e-6, 1e6, 0.0)
    // a pole at −10⁻¹² is strictly left of the axis, however close in absolute terms
    near("fourierCausal(exp(-0.000000000001*t), t, w)", 0.0, 1e12, 0.0)

  "fourierCausal" should "keep a symbolic numerator, since only a pole decides convergence" in:
    parse("fourierCausal(k*exp(-2*t), t, w)").eval(new Environment(5, Map("w" -> _Number(0.0)))) match
      case Left(r)  => assert(r.toString.contains("k") && !r.toString.contains("fourier"), s"$r")
      case Right(v) => fail(s"k is free: $v")

  it should "round-trip through toString and re-parse" in:
    val e = parse("fourierCausal(exp(-2*t), t, w)")
    assert(e.isInstanceOf[_FourierCausal])
    assert(parse(e.toString) == e)

  it should "be a reserved word" in:
    assert(Parser.ReservedWords.contains("fourierCausal"))

  "fourierOf" should "return the node unevaluated when it declines" in:
    val (t, w) = (_Variable("t"), _Variable("w"))
    assert(fourierOf(_Number(1), t, w) == _Fourier(_Number(1), t, w))
    assert(causalFourierOf(_Number(1), t, w) == _FourierCausal(_Number(1), t, w))

  "fourierDecline" should "name a pole on the axis" in:
    val (t, w) = (_Variable("t"), _Variable("w"))
    val why = fourierDecline(Cos(t), t, w, causal = true)
    assert(why.exists(_.contains("imaginary axis")), s"$why")
    assert(fourierDecline(Exp(Product(_Number(-1), Abs(t))), t, w, causal = false).isEmpty)

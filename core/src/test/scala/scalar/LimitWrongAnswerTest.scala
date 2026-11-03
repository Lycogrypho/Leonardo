package it.grypho.scala.leonardo
package scalar

import org.scalatest.flatspec.AnyFlatSpec
import core.*
import parser.Parser


/** F_0276 (LT L_0062): limits that printed a wrong value as an answer, with no note.
 *
 *  Each case must answer the true limit or stay unevaluated: a declined limit is the accepted
 *  price, a wrong one is the defect.  Every case runs in both parse modes, since the exact tier
 *  reads `pi` as a rational at the working precision and the default one as a `Double`, and the
 *  two went wrong in different places.
 */
class LimitWrongAnswerTest extends AnyFlatSpec:

  private val env = new Environment()

  /** The limit's value, or `None` when it stays unevaluated. */
  private def limitOf(src: String, exact: Boolean): Option[Double] =
    val parsed = Parser.parse(src, if exact then Some(30) else None)
    assert(parsed.successful, s"parse failed: $src")
    parsed.get.eval(env).toExpression match
      case _Number(d)  => Some(d)
      case _: _Limit   => None
      case other       => fail(s"$src: neither a number nor an unevaluated limit: $other")

  /** `src` answers `truth` (within a relative `1e-6`, or the same infinity) or stays unevaluated. */
  private def rightOrDeclined(src: String, truth: Double): Unit =
    for exact <- Seq(false, true) do
      limitOf(src, exact) match
        case None => ()
        case Some(d) if d.isNaN => fail(s"$src (exact = $exact): NaN printed as an answer")
        case Some(d) if truth.isInfinite =>
          assert(d == truth, s"$src (exact = $exact): answered $d, the limit is $truth")
        case Some(d) =>
          assert(math.abs(d - truth) <= 1e-6 * math.max(1.0, math.abs(truth)),
            s"$src (exact = $exact): answered $d, the limit is $truth")

  /** `src` has no limit (it diverges, or its sides differ): it must stay unevaluated. */
  private def declined(src: String): Unit =
    for exact <- Seq(false, true) do
      limitOf(src, exact).foreach(d => fail(s"$src (exact = $exact): answered $d, there is no limit"))

  "a cancellation read at a scale where it loses every digit" should "not answer 0 (F_0276)" in {
    rightOrDeclined("limit(x^(-2) - (x^(-2) - 1), x, 0)", 1.0)
  }

  "a quotient whose L'Hopital step meets an infinite numerator" should "not answer 0 (F_0276)" in {
    rightOrDeclined("limit(h^(1/3)/h, h, 0, +)", Double.PositiveInfinity)
    rightOrDeclined("limit(h^(1/2)/h, h, 0, +)", Double.PositiveInfinity)
  }

  "the 1^inf and 0/0 forms that converge slowly" should "not answer the value of the base (F_0276)" in {
    rightOrDeclined("limit(ln(sin(x)/x)/x^2, x, 0)", -1.0 / 6)
    rightOrDeclined("limit((sin(x)/x)^(1/x^2), x, 0)", math.exp(-1.0 / 6))
    rightOrDeclined("limit(cos(x)^(1/x^2), x, 0)", math.exp(-0.5))
  }

  "a denominator that is a reading of zero" should "not be divided by (F_0276)" in {
    rightOrDeclined("limit((x^3 - exp(x - 1))/sin(pi*x), x, 1)", -2 / math.Pi)
    rightOrDeclined("limit(sec(x) - tan(x), x, pi/2, -)", 0.0)
    rightOrDeclined("limit(1/sin(x), x, pi, -)", Double.PositiveInfinity)
    rightOrDeclined("limit(1/sin(x), x, pi, +)", Double.NegativeInfinity)
    rightOrDeclined("limit(tan(x), x, pi/2, -)", Double.PositiveInfinity)
    rightOrDeclined("limit(tan(x), x, pi/2, +)", Double.NegativeInfinity)
    declined("limit(1/sin(x), x, pi)")
    declined("limit(tan(x), x, pi/2)")
  }

  "an oscillation at infinity" should "have no limit (F_0276)" in {
    declined("limit((-1)^(n - 1), n, inf)")
  }

  "a double pole" should "answer its infinity, never NaN (F_0276)" in {
    // (r + 5)^2 is positive on both sides and r is -5 there: -inf from both sides.
    rightOrDeclined("limit(r/(r^2 + 10*r + 25), r, -5)", Double.NegativeInfinity)
  }

  "a declined limit" should "be the call as written, not the form the tiers stopped on (F_0276)" in {
    // The decline echoed L'Hopital's differentiated quotient, and its point as `0.0` in exact mode.
    for exact <- Seq(None, Some(30)) do
      val call = Parser.parse("limit(h^(1/3)/h, h, 0, +)", exact).get
      assert(call.eval(env) == Left(call), s"exact = $exact")
  }

  "the limits these fixes sit beside" should "still answer" in {
    for exact <- Seq(false, true) do
      def is(src: String, want: Double): Unit =
        limitOf(src, exact) match
          case Some(d) if want.isInfinite => assert(d == want, s"$src: $d")
          case Some(d) => assert(math.abs(d - want) <= 1e-6 * math.max(1.0, math.abs(want)), s"$src: $d")
          case None    => fail(s"$src (exact = $exact): declined")
      is("limit(sin(x)/x, x, 0)", 1.0)
      is("limit((1 - cos(x))/x^2, x, 0)", 0.5)
      is("limit(x^2, x, 3)", 9.0)
      is("limit(1/x, x, 0, +)", Double.PositiveInfinity)
      is("limit(1/x^2, x, 0)", Double.PositiveInfinity)
      is("limit((x^2 - 1)/(x - 1), x, 1)", 2.0)
      is("limit(sin(x), x, pi)", 0.0)
  }

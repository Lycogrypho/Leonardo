package it.grypho.scala.leonardo
package statistics

import core.*
import probability.{_Distribution, DistKind}
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** The F distribution, the two-variance F-test and one-way ANOVA (F_0131). */
class AnovaTest extends AnyFlatSpec:

  private val env = new Environment()
  private def ev(s: String): Either[_Expression, _Value] = Parser.parse(s).get.eval(env)
  private def num(s: String): Double = ev(s) match
    case Right(_Number(d)) => d
    case other             => fail(s"$s gave $other")

  private def close(a: Double, b: Double, rel: Double = 1e-10): Boolean =
    math.abs(a - b) <= rel * math.max(1e-300, math.max(math.abs(a), math.abs(b)))

  private def tCdf(t: Double, df: Double): Double =
    _Distribution.of(DistKind.StudentT, Vector(df)).flatMap(_.cdf(t)).getOrElse(fail("t cdf"))

  "fdist(d1, d2)" should "be the square of a t variable when d1 = 1" in {
    // P(F <= t^2) = P(|T| <= t) = 1 - 2 P(T <= -t)
    val (t, d) = (1.7, 9.0)
    assert(close(num(s"cdf(fdist(1, 9), ${t * t})"), 1 - 2 * tCdf(-t, d)))
  }

  it should "have a density that integrates to its cdf" in {
    val viaIntegral = num("integral(pdf(fdist(4, 12), x), x, 0, 2)")
    assert(math.abs(viaIntegral - num("cdf(fdist(4, 12), 2)")) < 1e-6)
  }

  it should "have mean d2/(d2 - 2) and the textbook variance, where they exist" in {
    assert(close(num("expect(fdist(5, 10))"), 10.0 / 8))
    assert(close(num("variance(fdist(5, 10))"), 2 * 100.0 * 13 / (5 * 64.0 * 6)))
    assert(ev("expect(fdist(5, 2))").isLeft)
  }

  it should "invert its cdf" in {
    val q = num("quantile(fdist(5, 10), 0.95)")
    assert(close(num(s"cdf(fdist(5, 10), $q)"), 0.95, 1e-9))
  }

  it should "stay symbolic for non-positive degrees of freedom" in {
    assert(ev("fdist(0, 3)").isLeft && ev("fdist(3, -1)").isLeft)
  }

  private val (x, y) = ("[[19, 22, 16, 29, 24]]", "[[20, 11, 17, 12]]")
  private val xs = Vector(19.0, 22.0, 16.0, 29.0, 24.0)
  private val ys = Vector(20.0, 11.0, 17.0, 12.0)
  private def svar(v: Vector[Double]) = { val m = v.sum / v.size; v.map(a => (a - m) * (a - m)).sum / (v.size - 1) }

  "vartest(x, y)" should "take the upper tail of s1^2/s2^2 for greater, the two tails summing to one" in {
    val f = svar(xs) / svar(ys)
    val upper = 1 - num(s"cdf(fdist(4, 3), $f)")
    assert(close(num(s"vartest($x, $y, greater)"), upper, 1e-9))
    assert(close(num(s"vartest($x, $y, greater)") + num(s"vartest($x, $y, less)"), 1.0, 1e-12))
    assert(close(num(s"vartest($x, $y)"), 2 * math.min(upper, 1 - upper), 1e-9))
  }

  "anova(g1, g2, ...)" should "agree exactly with the pooled two-sample t-test on two groups" in {
    assert(close(num(s"anova($x, $y)"), num(s"ttest($x, $y, pooled)"), 1e-9))
  }

  it should "answer the upper tail of MSB/MSW on (k - 1, N - k)" in {
    // Means 2, 5, 8 around 5: SSB = 3*(9 + 0 + 9) = 54, SSW = 3 * 2 = 6, F = (54/2)/(6/6) = 27.
    val upper = 1 - num("cdf(fdist(2, 6), 27)")
    assert(close(num("anova([[1, 2, 3]], [[4, 5, 6]], [[7, 8, 9]])"), upper, 1e-8))
  }

  it should "refuse one group at the parser, the maximum(...) precedent, and stay unevaluated with no spread" in {
    assert(!Parser.parse("anova([[1, 2, 3]])").successful)
    assert(ev("anova([[1, 1]], [[2, 2]])").isLeft)
  }

  "the new forms" should "round-trip and reserve their words" in {
    for s <- List("fdist(3, 7)", "vartest(x, y)", "vartest(x, y, less)", "anova(a, b, c)") do
      val e = Parser.parse(s).get
      assert(Parser.parse(e.toString).get == e, s)
    List("fdist", "vartest", "anova").foreach(w => assert(Parser.ReservedWords.contains(w), w))
  }

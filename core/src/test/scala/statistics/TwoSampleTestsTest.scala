package it.grypho.scala.leonardo
package statistics

import core.*
import probability.{_Distribution, DistKind}
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** Two-sample and proportion inference (F_0130): the Welch and pooled t-tests, one-sided
 *  alternatives, proportion tests and the contingency-table chi-squared test, each checked against
 *  its statistic computed by hand and the distribution's own tail. */
class TwoSampleTestsTest extends AnyFlatSpec:

  private val env = new Environment()
  private def num(s: String): Double = Parser.parse(s).get.eval(env) match
    case Right(_Number(d)) => d
    case other             => fail(s"$s gave $other")

  private def close(a: Double, b: Double, rel: Double = 1e-10): Boolean =
    math.abs(a - b) <= rel * math.max(1e-300, math.max(math.abs(a), math.abs(b)))

  private def tCdf(t: Double, df: Double): Double =
    _Distribution.of(DistKind.StudentT, Vector(df)).flatMap(_.cdf(t)).getOrElse(fail("t cdf"))
  private def zCdf(z: Double): Double =
    _Distribution.of(DistKind.Normal, Vector(0.0, 1.0)).flatMap(_.cdf(z)).getOrElse(fail("normal cdf"))
  private def chiCdf(x: Double, df: Double): Double =
    _Distribution.of(DistKind.ChiSquared, Vector(df)).flatMap(_.cdf(x)).getOrElse(fail("chi cdf"))

  private val xs = Vector(19.0, 22.0, 16.0, 29.0, 24.0)

  "ttest with pooled and a null mean" should "stay unevaluated: there is nothing to pool (F_0150)" in {
    val sample = xs.mkString("[[", ", ", "]]")
    assert(Parser.parse(s"ttest($sample, 20, pooled)").get.eval(env).isLeft)
    assert(Parser.parse(s"ttest($sample, 20)").get.eval(env).isRight)
  }
  private val ys = Vector(20.0, 11.0, 17.0, 12.0)
  private val (x, y) = ("[[19, 22, 16, 29, 24]]", "[[20, 11, 17, 12]]")

  private def mean(v: Vector[Double]) = v.sum / v.size
  private def svar(v: Vector[Double]) = { val m = mean(v); v.map(a => (a - m) * (a - m)).sum / (v.size - 1) }

  "ttest(x, y)" should "be Welch's test, with the Welch-Satterthwaite degrees of freedom" in {
    val (a, b) = (svar(xs) / xs.size, svar(ys) / ys.size)
    val t  = (mean(xs) - mean(ys)) / math.sqrt(a + b)
    val df = (a + b) * (a + b) / (a * a / (xs.size - 1) + b * b / (ys.size - 1))
    assert(close(num(s"ttest($x, $y)"), 2 * tCdf(-math.abs(t), df)))
  }

  "ttest(x, y, pooled)" should "pool the variances on n1 + n2 - 2 degrees of freedom" in {
    val (n1, n2) = (xs.size, ys.size)
    val sp2 = ((n1 - 1) * svar(xs) + (n2 - 1) * svar(ys)) / (n1 + n2 - 2)
    val t   = (mean(xs) - mean(ys)) / math.sqrt(sp2 * (1.0 / n1 + 1.0 / n2))
    assert(close(num(s"ttest($x, $y, pooled)"), 2 * tCdf(-math.abs(t), n1 + n2 - 2)))
  }

  "a one-sided alternative" should "take one tail, the two tails summing to one" in {
    for (args, two) <- List((s"$x, $y", s"ttest($x, $y)"), (s"$x, $y, pooled", s"ttest($x, $y, pooled)"), (s"$x, 20", s"ttest($x, 20)")) do
      val (g, l) = (num(s"ttest($args, greater)"), num(s"ttest($args, less)"))
      assert(close(g + l, 1.0, 1e-12), args)
      assert(close(num(two), 2 * math.min(g, l)), args)
  }

  it should "answer the upper tail for greater when the sample mean is above mu0" in {
    val t = (mean(xs) - 20) / math.sqrt(svar(xs) / xs.size)
    assert(close(num(s"ttest($x, 20, greater)"), tCdf(-t, xs.size - 1)))
  }

  "proptest(x, n, p0)" should "be the one-proportion z-test" in {
    val z = (0.3 - 0.25) / math.sqrt(0.25 * 0.75 / 100)
    assert(close(num("proptest(30, 100, 0.25)"), 2 * zCdf(-math.abs(z))))
    assert(close(num("proptest(30, 100, 0.25, greater)"), zCdf(-z)))
  }

  "proptest(x1, n1, x2, n2)" should "be the two-proportion z-test with the pooled proportion" in {
    val (p1, p2, p) = (30.0 / 100, 45.0 / 120, 75.0 / 220)
    val z = (p1 - p2) / math.sqrt(p * (1 - p) * (1.0 / 100 + 1.0 / 120))
    assert(close(num("proptest(30, 100, 45, 120)"), 2 * zCdf(-math.abs(z))))
    assert(close(num("proptest(30, 100, 45, 120, less)"), zCdf(z)))
  }

  it should "stay unevaluated outside its domain" in {
    for s <- List("proptest(30, 100, 1.5)", "proptest(130, 100, 0.5)", "proptest(2.5, 10, 0.5)", "proptest(0, 10, 0, 10)") do
      assert(Parser.parse(s).get.eval(env).isLeft, s)
  }

  "chisqtest(table)" should "test independence from the margins on (r - 1)(c - 1) degrees of freedom" in {
    val obs = Vector(Vector(10.0, 20.0, 30.0), Vector(25.0, 15.0, 20.0))
    val (rows, cols, total) = (obs.map(_.sum), obs.transpose.map(_.sum), obs.flatten.sum)
    val stat = (for i <- 0 until 2; j <- 0 until 3 yield
      val e = rows(i) * cols(j) / total
      (obs(i)(j) - e) * (obs(i)(j) - e) / e).sum
    assert(close(num("chisqtest([[10, 20, 30], [25, 15, 20]])"), 1 - chiCdf(stat, 2), 1e-9))
  }

  it should "stay unevaluated for a single row, or a zero margin" in {
    assert(Parser.parse("chisqtest([[10, 20, 30]])").get.eval(env).isLeft)
    assert(Parser.parse("chisqtest([[0, 0], [5, 6]])").get.eval(env).isLeft)
  }

  "the goodness-of-fit p-value" should "keep its digits far in the tail, where 1 - cdf cancels to zero" in {
    // df = 1: P(X > s) = erfc(sqrt(s/2)), about 1e-219 at s = 1000.
    val p = num("chisqtest([[1000, 0]], [[500, 500]])")
    assert(p > 0.0)
    assert(close(p, 2 * zCdf(-math.sqrt(1000.0)), 1e-8))
  }

  "confint" should "read its critical value from the t distribution's own quantile" in {
    val half = num("quantile(studentt(4), 0.975)") * math.sqrt(svar(xs) / xs.size)
    Parser.parse(s"confint($x, 0.95)").get.eval(env) match
      case Right(m: _MatrixValue) =>
        assert(close(m(0, 0), mean(xs) - half, 1e-12) && close(m(0, 1), mean(xs) + half, 1e-12))
      case other => fail(s"got $other")
  }

  "the new forms" should "round-trip through toString" in {
    for s <- List("ttest(x, y, pooled, greater)", "ttest(x, 3, less)", "ttest(x, y, pooled)", "proptest(3, 10, 0.5)",
                  "proptest(3, 10, 4, 12, greater)", "chisqtest(t)") do
      val e = Parser.parse(s).get
      assert(Parser.parse(e.toString).get == e, s)
  }

  "the slot words" should "stay free variable names, and proptest be reserved" in {
    List("less", "greater", "pooled").foreach(w => assert(!Parser.ReservedWords.contains(w), w))
    assert(Parser.ReservedWords.contains("proptest"))
  }

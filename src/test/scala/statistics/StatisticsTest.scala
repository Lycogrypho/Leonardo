package it.grypho.scala.leonardo
package statistics

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** The statistics domain — issue 4.Q, all three slices. */
class StatisticsTest extends AnyFlatSpec:

  private val env = new Environment()

  private def parse(s: String): _Expression =
    Parser.parse(s) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for \"$s\": $other")

  private def num(s: String, e: Environment = env): Double =
    parse(s).eval(e) match
      case Right(_Number(d)) => d
      case other             => fail(s"\"$s\" did not reduce to a number: $other")

  private def matrix(s: String): Vector[Double] =
    parse(s).eval(env) match
      case Right(m: _MatrixValue) => m.toVector
      case other                  => fail(s"\"$s\" is not a dense matrix: $other")

  // --- slice A: descriptive ---

  "mean" should "average a sample regardless of its shape" in
  {
    assert(math.abs(num("mean([[1, 2, 3, 4]])") - 2.5) < 1e-15)
    // A column vector is the same sample: entries are read row-major, so shape is irrelevant.
    assert(math.abs(num("mean([[1], [2], [3], [4]])") - 2.5) < 1e-15)
    assert(math.abs(num("mean([[1, 2], [3, 4]])") - 2.5) < 1e-15)
  }

  "variance" should "be the unbiased n-1 estimator for a sample" in
  {
    // Sample [2,4,4,4,5,5,7,9]: mean 5, sum of squared deviations 32.
    // n-1 = 7 -> 32/7; n = 8 -> 4.
    assert(math.abs(num("variance([[2,4,4,4,5,5,7,9]])") - 32.0 / 7.0) < 1e-12)
    assert(math.abs(num("pvariance([[2,4,4,4,5,5,7,9]])") - 4.0) < 1e-12)
    assert(math.abs(num("stddev([[2,4,4,4,5,5,7,9]])") - math.sqrt(32.0 / 7.0)) < 1e-12)
    assert(math.abs(num("pstddev([[2,4,4,4,5,5,7,9]])") - 2.0) < 1e-12)
  }

  it should "still mean the population variance for a DISTRIBUTION" in
  {
    // A distribution is not a sample, so there is no n to correct for. Same word, different
    // object -- not an inconsistency, and the one-argument form dispatches on the argument.
    assert(math.abs(num("variance(normal(3, 2))") - 4.0) < 1e-15)
    assert(math.abs(num("mean(normal(3, 2))") - 3.0) < 1e-15)
    assert(math.abs(num("stddev(normal(3, 2))") - 2.0) < 1e-15)
    assert(math.abs(num("variance(poisson(4))") - 4.0) < 1e-15)
  }

  it should "match the two-pass formula where the one-pass form would collapse" in
  {
    // A large mean with a small spread is where E[X^2] - E[X]^2 loses its digits: the two
    // squares agree to ~16 significant figures and their difference keeps almost none.
    val sample = "[[1000000001, 1000000002, 1000000003, 1000000004, 1000000005]]"
    assert(math.abs(num(s"variance($sample)") - 2.5) < 1e-9,
           s"got ${num(s"variance($sample)")}, expected 2.5")
    // The one-pass form, computed here to show what was avoided.
    val xs      = Vector(1000000001.0, 1000000002.0, 1000000003.0, 1000000004.0, 1000000005.0)
    val onePass = (xs.map(x => x * x).sum / 5) - math.pow(xs.sum / 5, 2)
    assert(math.abs(onePass * 5.0 / 4.0 - 2.5) > 1e-9, "the one-pass form really is worse here")
  }

  it should "have no unbiased value for a single observation" in
  {
    assert(parse("variance([[5]])").eval(env).isLeft, "n - 1 = 0")
    assert(math.abs(num("pvariance([[5]])")) < 1e-15, "...but the population variance is 0")
  }

  "covariance and correlation" should "match known values" in
  {
    // Deviations (-1.5,-0.5,0.5,1.5) against (-3,-1,1,3) sum to 10, over n-1 = 3.
    assert(math.abs(num("covariance([[1,2,3,4]], [[2,4,6,8]])") - 10.0 / 3.0) < 1e-12)
    assert(math.abs(num("correlation([[1,2,3,4]], [[2,4,6,8]])") - 1.0) < 1e-12)
    assert(math.abs(num("correlation([[1,2,3,4]], [[8,6,4,2]])") + 1.0) < 1e-12)
    assert(math.abs(num("correlation([[1,2,3,4]], [[1,2,3,4]])") - 1.0) < 1e-15)
  }

  it should "stay inside [-1, 1] and refuse a constant sample" in
  {
    // The division can land a hair outside the range on a perfect fit, and a correlation of
    // 1.0000000000000002 breaks any caller that tests it.
    assert(num("correlation([[1,2,3,4]], [[1,2,3,4]])") <= 1.0)
    assert(parse("correlation([[1,1,1]], [[1,2,3]])").eval(env).isLeft, "no spread, no correlation")
  }

  it should "refuse mismatched sample lengths" in
  {
    assert(parse("covariance([[1,2,3]], [[1,2]])").eval(env).isLeft)
  }

  // --- exactness ---

  "an exactly-written sample" should "give an exact mean and variance" in
  {
    // Mean and variance are pure field operations, so they are closed over the rationals --
    // which is precisely where a floating-point variance is least trustworthy.
    def exact(s: String): Either[_Expression, _Value] =
      Parser.parse(s, Some(30)) match
        case Parser.Success(e, _) => e.eval(new Environment(workingPrecision = 30))
        case other                => fail(s"parse failed: $other")

    exact("mean([[1/3, 1/3, 1/3]])") match
      case Right(r: _Rational) => assert(r == _Rational.of(BigInt(1), BigInt(3)).get)
      case other               => fail(s"expected an exact mean, got $other")
    exact("variance([[1, 2, 3, 4]])") match
      case Right(r: _Rational) => assert(r.exact == "5/3", s"got ${r.exact}")
      case other               => fail(s"expected an exact variance, got $other")
    // A square root is not closed over the rationals, so stddev is only working-precision.
    assert(exact("stddev([[1, 2, 3, 4]])").isRight)
  }

  // --- slice B: regression ---

  "regress" should "recover the coefficients of a noiseless fit" in
  {
    // y = 3 + 2x exactly, with an explicit intercept column -- regress adds none.
    val beta = matrix("regress([[1,1],[1,2],[1,3],[1,4]], [[5],[7],[9],[11]])")
    assert(beta.size == 2)
    assert(math.abs(beta(0) - 3.0) < 1e-9, s"intercept = ${beta(0)}")
    assert(math.abs(beta(1) - 2.0) < 1e-9, s"slope = ${beta(1)}")
  }

  it should "accept the response either way up" in
  {
    val col = matrix("regress([[1,1],[1,2],[1,3],[1,4]], [[5],[7],[9],[11]])")
    val row = matrix("regress([[1,1],[1,2],[1,3],[1,4]], [[5,7,9,11]])")
    assert(col.zip(row).forall((a, b) => math.abs(a - b) < 1e-12))
  }

  it should "match a worked least-squares example" in
  {
    // x = 1..5, y = 2,4,5,4,5.  Textbook result: intercept 2.2, slope 0.6.
    val beta = matrix("regress([[1,1],[1,2],[1,3],[1,4],[1,5]], [[2],[4],[5],[4],[5]])")
    assert(math.abs(beta(0) - 2.2) < 1e-9, s"intercept = ${beta(0)}")
    assert(math.abs(beta(1) - 0.6) < 1e-9, s"slope = ${beta(1)}")
  }

  it should "refuse a rank-deficient design rather than produce nonsense" in
  {
    // Two identical columns: the normal equations would invert a singular XtX and return
    // confident rubbish; qrDecompose refuses it instead.
    assert(parse("regress([[1,1],[2,2],[3,3]], [[1],[2],[3]])").eval(env).isLeft)
    // More parameters than observations is equally refused.
    assert(parse("regress([[1,2,3]], [[1]])").eval(env).isLeft)
  }

  // --- slice C: the two new distributions ---

  "the Student-t distribution" should "match reference values" in
  {
    // A t with large nu approaches the standard normal.
    assert(math.abs(num("cdf(studentt(1000), 1.96)") - 0.975) < 1e-3)
    // Symmetry about zero.
    assert(math.abs(num("cdf(studentt(5), 0)") - 0.5) < 1e-12)
    assert(math.abs(num("cdf(studentt(5), -1.5)") - (1.0 - num("cdf(studentt(5), 1.5)"))) < 1e-12)
    // t(10) at 2.228 is the two-sided 5% critical value: cdf = 0.975.
    assert(math.abs(num("cdf(studentt(10), 2.228)") - 0.975) < 1e-4)
    assert(math.abs(num("pdf(studentt(5), 0)") - 0.3796066898224944) < 1e-10)
  }

  "the chi-squared distribution" should "match reference values" in
  {
    // chi-squared with k = 2 is Exponential(1/2): cdf = 1 - e^(-x/2).
    for x <- List(0.5, 1.0, 3.0) do
      assert(math.abs(num(s"cdf(chisq(2), $x)") - (1.0 - math.exp(-x / 2.0))) < 1e-12, s"at $x")
    // The 95th percentile of chi-squared with 1 df is 3.841.
    assert(math.abs(num("cdf(chisq(1), 3.841458821)") - 0.95) < 1e-8)
    assert(math.abs(num("mean(chisq(7))") - 7.0) < 1e-15)
    assert(math.abs(num("variance(chisq(7))") - 14.0) < 1e-15)
  }

  it should "report the t moments as undefined where they are" in
  {
    // The t mean does not exist for nu <= 1 and the variance for nu <= 2 -- facts about the
    // distribution, not computational give-ups.
    assert(parse("mean(studentt(1))").eval(env).isLeft)
    assert(parse("variance(studentt(2))").eval(env).isLeft)
    assert(math.abs(num("mean(studentt(3))")) < 1e-15)
    assert(math.abs(num("variance(studentt(4))") - 2.0) < 1e-12)
  }

  // --- slice C: the tests ---

  "the one-sample t-test" should "reproduce a textbook p-value" in
  {
    // Sample [5,6,7,8,9] (mean 7, sd 1.5811), null mu = 5.  t = 2/(1.5811/sqrt 5) = 2.8284
    // on 4 df, two-sided p = 0.0473.
    val p = num("ttest([[5,6,7,8,9]], 5)")
    assert(math.abs(p - 0.0474206556) < 1e-8, s"got $p")
    // Cross-checked against the t TABLE rather than against another of my own sums -- the
    // first version of this assertion carried a hand-computed constant that was simply
    // wrong.  t(4) two-sided is 2.7764 at 5% and 3.7469 at 2%, and the statistic here is
    // 2.8284, so the p-value must fall strictly inside (0.02, 0.05).
    assert(p > 0.02 && p < 0.05, s"p must lie between the 2% and 5% critical values, got $p")
    // A sample centred on the null gives p = 1.
    assert(math.abs(num("ttest([[5,6,7,8,9]], 7)") - 1.0) < 1e-12)
  }

  it should "return a probability, and now compose with the comparison operators" in
  {
    // The reason 4.Q waited for 4.R: this is how a significance test is actually written.
    assert(parse("ttest([[5,6,7,8,9]], 5) < 0.05").eval(env) == Right(_Bool(true)))
    assert(parse("ttest([[5,6,7,8,9]], 7) < 0.05").eval(env) == Right(_Bool(false)))
  }

  "a confidence interval" should "bracket the mean and match the t critical value" in
  {
    val ci = matrix("confint([[5,6,7,8,9]], 0.95)")
    assert(ci.size == 2)
    assert(ci(0) < 7.0 && ci(1) > 7.0, s"must bracket the mean: ${ci.mkString(", ")}")
    // mean 7 +/- t(4, 0.975) * 1.5811/sqrt(5) = 7 +/- 1.9634
    assert(math.abs(ci(0) - 5.036757) < 1e-4, s"lo = ${ci(0)}")
    assert(math.abs(ci(1) - 8.963243) < 1e-4, s"hi = ${ci(1)}")
    // A wider level gives a wider interval.
    val wide = matrix("confint([[5,6,7,8,9]], 0.99)")
    assert(wide(0) < ci(0) && wide(1) > ci(1))
  }

  it should "refuse a level outside (0, 1)" in
  {
    for lvl <- List("0", "1", "95", "-0.5") do
      assert(parse(s"confint([[5,6,7,8,9]], $lvl)").eval(env).isLeft, s"level $lvl")
  }

  "the chi-squared goodness-of-fit test" should "reproduce a textbook p-value" in
  {
    // Observed [10,20,30,40] against expected [25,25,25,25]:
    //   X^2 = (225 + 25 + 25 + 225) / 25 = 20 on 3 df, so p = 1.6974e-4.
    val p = num("chisqtest([[10,20,30,40]], [[25,25,25,25]])")
    assert(math.abs(p - 1.6974243555e-4) < 1e-12, s"got $p")
    // ...which is exactly the upper tail of that statistic, reached independently.
    assert(math.abs(p - (1.0 - num("cdf(chisq(3), 20)"))) < 1e-15)
    // A perfect fit gives p = 1.
    assert(math.abs(num("chisqtest([[25,25,25,25]], [[25,25,25,25]])") - 1.0) < 1e-12)
  }

  it should "refuse a zero expected count" in
  {
    // The statistic divides by the expected count, so a zero makes it undefined rather
    // than infinite.
    assert(parse("chisqtest([[1,2]], [[0,3]])").eval(env).isLeft)
  }

  // --- grammar hygiene ---

  "the statistics grammar" should "round-trip through toString" in
  {
    for s <- List("mean(x)", "variance(x)", "pvariance(x)", "stddev(x)", "pstddev(x)",
                  "covariance(x, y)", "correlation(x, y)", "regress(x, y)",
                  "ttest(x, y)", "confint(x, y)", "chisqtest(x, y)",
                  "studentt(x)", "chisq(x)") do
      assert(parse(parse(s).toString) == parse(s), s"round-trip failed for $s")
  }

  it should "reserve its words" in
  {
    for w <- List("mean", "pvariance", "stddev", "pstddev", "covariance", "correlation",
                  "regress", "ttest", "confint", "chisqtest", "studentt", "chisq") do
      assert(Parser.ReservedWords.contains(w), s"'$w' must be reserved")
  }

  it should "stay symbolic on a non-sample argument" in
  {
    for s <- List("mean(3)", "variance(x)", "covariance(3, 4)", "regress(3, 4)") do
      assert(parse(s).eval(env).isLeft, s"$s must stay symbolic")
  }

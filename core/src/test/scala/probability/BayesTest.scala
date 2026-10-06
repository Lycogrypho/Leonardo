package it.grypho.scala.leonardo
package probability

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

import scala.math.{abs, exp}


/** F_0050 — Bayesian inference: the two conjugate families, `bayes` over a finite set of
 *  hypotheses, and `posterior` for the conjugate pairs.
 *
 *  Closed forms are compared numerically, never through `toString`; the exact `bayes` case is
 *  checked at value level, on the cells' types and values.
 */
class BayesTest extends AnyFlatSpec:

  private val env = new Environment()

  private def parse(s: String): _Expression =
    Parser.parse(s) match
      case Parser.Success(e, _) => e
      case other                => fail(s"parse failed for \"$s\": $other")

  private def num(s: String, e: Environment = env): Double =
    parse(s).eval(e) match
      case Right(_Number(d)) => d
      case other             => fail(s"\"$s\" did not reduce to a number: $other")

  private def dist(s: String, e: Environment = env): _Distribution =
    parse(s).eval(e) match
      case Right(d: _Distribution) => d
      case other                   => fail(s"\"$s\" is not a distribution: $other")

  private def column(r: Either[_Expression, _Value]): (Int, Int, Vector[Double]) = r match
    case Right(m: _MatrixValue) => (m.rows, m.cols, m.toVector)
    case other                  => fail(s"expected a dense vector, got $other")

  // --- the families ---

  "betadist and gammadist" should "validate their parameters" in
  {
    for bad <- List("betadist(0, 1)", "betadist(1, -2)", "gammadist(0, 1)", "gammadist(2, 0)") do
      assert(parse(bad).eval(env).isLeft, s"$bad must stay symbolic")
  }

  it should "have the textbook moments, density and cdf" in
  {
    // Beta(2, 3): density 12 x (1-x)^2, mean 2/5, variance 1/25, cdf(1/2) = 11/16.
    assert(abs(num("expect(betadist(2, 3))") - 0.4) < 1e-15)
    assert(abs(num("variance(betadist(2, 3))") - 0.04) < 1e-15)
    assert(abs(num("pdf(betadist(2, 3), 0.5)") - 1.5) < 1e-12)
    assert(abs(num("cdf(betadist(2, 3), 0.5)") - 0.6875) < 1e-12)
    // Gamma(shape 2, rate 3): density 9 x e^(-3x), mean 2/3, variance 2/9,
    // cdf(1) = 1 - e^(-3)(1 + 3).  RATE, not scale: the scale reading would give mean 6.
    assert(abs(num("expect(gammadist(2, 3))") - 2.0 / 3.0) < 1e-15)
    assert(abs(num("variance(gammadist(2, 3))") - 2.0 / 9.0) < 1e-15)
    assert(abs(num("pdf(gammadist(2, 3), 1)") - 9.0 * exp(-3.0)) < 1e-12)
    assert(abs(num("cdf(gammadist(2, 3), 1)") - (1.0 - 4.0 * exp(-3.0))) < 1e-12)
  }

  it should "decide the endpoints of the support explicitly" in
  {
    assert(abs(num("pdf(betadist(1, 1), 0)") - 1.0) < 1e-15)       // the uniform
    assert(abs(num("pdf(betadist(2, 3), 0)")) < 1e-15)             // vanishes above a = 1
    assert(parse("pdf(betadist(0.5, 0.5), 0)").eval(env).isLeft)   // infinite: no number
    assert(abs(num("pdf(gammadist(1, 2), 0)") - 2.0) < 1e-15)      // the exponential at 0
    assert(abs(num("pdf(betadist(2, 3), 1.5)")) < 1e-15)
    assert(abs(num("cdf(betadist(2, 3), 2)") - 1.0) < 1e-15)
  }

  it should "have monotone cdfs that saturate" in
  {
    val cases = List(("betadist(2, 3)", (-10 to 30).map(_ * 0.05)),
                     ("gammadist(2, 3)", (0 to 60).map(_ * 0.25)))
    for (d, xs) <- cases do
      val cs = xs.map(x => num(s"cdf($d, $x)"))
      for (a, b) <- cs.zip(cs.tail) do assert(b >= a - 1e-12, s"$d cdf decreased")
      assert(cs.head < 1e-9 && cs.last > 1.0 - 1e-6, s"$d must span 0..1")
  }

  it should "be inverted by the existing quantile" in
  {
    for (d, p) <- List(("betadist(2, 3)", 0.3), ("gammadist(2, 3)", 0.9)) do
      val q = num(s"quantile($d, $p)")
      assert(abs(num(s"cdf($d, $q)") - p) < 1e-8, s"quantile($d, $p) = $q did not invert")
  }

  it should "round-trip and reserve their words" in
  {
    for s <- List("betadist(2.0, 3.0)", "gammadist(2.0, 3.0)") do
      val d = dist(s)
      assert(parse(d.toString).eval(env) == Right(d), s"round-trip failed for $s")
    for w <- List("betadist", "gammadist", "bayes", "posterior") do
      assert(Parser.ReservedWords.contains(w), s"'$w' must be reserved")
  }

  // --- bayes over hypotheses ---

  "bayes" should "normalise prior times likelihood" in
  {
    val (r, c, cells) = column(parse("bayes([[0.5, 0.5]], [[0.8, 0.2]])").eval(env))
    assert((r, c) == (1, 2))
    assert(abs(cells(0) - 0.8) < 1e-15 && abs(cells(1) - 0.2) < 1e-15)
    // The textbook screening problem: 1% prevalence, 90% sensitivity, 10% false positives.
    val (_, _, post) = column(parse("bayes([[0.01, 0.99]], [[0.9, 0.1]])").eval(env))
    assert(abs(post(0) - 0.009 / 0.108) < 1e-15, s"got ${post(0)}")
  }

  it should "keep the shape of its operands" in
  {
    val (r, c, cells) = column(parse("bayes([[1], [3]], [[1], [1]])").eval(env))
    assert((r, c) == (2, 1))
    assert(abs(cells(0) - 0.25) < 1e-15 && abs(cells(1) - 0.75) < 1e-15)
  }

  it should "stay exact on exact input" in
  {
    Parser.parse("bayes([[1/2, 1/2]], [[4/5, 1/5]])", Some(30)) match
      case Parser.Success(e, _) =>
        e.eval(env) match
          case Left(m: _MatrixShaped) =>
            assert((m.rows, m.cols) == (1, 2))
            val cells = m.children.collect { case r: _Rational => r }
            assert(cells.size == 2, s"every cell must be exact: ${m.children}")
            assert(abs(cells(0).toDouble - 0.8) < 1e-15 && abs(cells(1).toDouble - 0.2) < 1e-15)
          case other => fail(s"expected an exact symbolic row, got $other")
      case other => fail(s"parse failed: $other")
  }

  it should "demote to Double when one entry is inexact" in
  {
    // `a` is bound to a Double, so the likelihood row is mixed: float contagion wins and
    // the result is dense, never a rational dressed up as exact.
    val bound = env.withBinding("a", _Number(0.8))
    Parser.parse("bayes([[1/2, 1/2]], [[a, 1/5]])", Some(30)) match
      case Parser.Success(e, _) =>
        val (_, _, cells) = column(e.eval(bound))
        assert(abs(cells(0) - 0.8) < 1e-15)
      case other => fail(s"parse failed: $other")
  }

  it should "refuse a mismatch, a non-vector, a negative entry and zero evidence" in
  {
    for bad <- List("bayes([[0.5, 0.5]], [[1, 2, 3]])",            // length mismatch
                    "bayes([[0.5, 0.5]], [[1], [2]])",             // row against column
                    "bayes([[1, 2], [3, 4]], [[1, 2], [3, 4]])",   // not a vector
                    "bayes([[0.5, 0.5]], [[-1, 2]])",              // a negative likelihood
                    "bayes([[1, 0]], [[0, 1]])",                   // zero evidence
                    "bayes([[0.5, 0.5]], [[x, 1]])") do            // not numeric
      assert(parse(bad).eval(env).isLeft, s"$bad must stay symbolic")
    Parser.parse("bayes([[1, 0]], [[0, 1]])", Some(30)) match     // exact zero evidence
      case Parser.Success(e, _) => assert(e.eval(env).isLeft)
      case other                => fail(s"parse failed: $other")
  }

  it should "round-trip through toString" in
  {
    val e = parse("bayes([[0.5, 0.5]], [[0.8, 0.2]])")
    assert(parse(e.toString) == e, s"round-trip failed: ${e.toString}")
  }

  // --- conjugate posteriors ---

  "posterior" should "update a Beta prior on binomial data" in
  {
    val d = dist("posterior(betadist(2, 2), binomial(10, p), 7)")
    assert(d.kind == DistKind.BetaDist && d.params == Vector(9.0, 5.0))
    assert(abs(num("expect(posterior(betadist(2, 2), binomial(10, p), 7))") - 9.0 / 14.0) < 1e-15)
  }

  it should "update a Gamma prior on Poisson counts, additively in the rate" in
  {
    val d = dist("posterior(gammadist(2, 1), poisson(l), [[3, 5, 4]])")
    assert(d.kind == DistKind.GammaDist && d.params == Vector(14.0, 4.0))
    // A column is the same sample, and a single number is a sample of one.
    assert(dist("posterior(gammadist(2, 1), poisson(l), [[3], [5], [4]])").params == Vector(14.0, 4.0))
    assert(dist("posterior(gammadist(2, 1), poisson(l), 3)").params == Vector(5.0, 2.0))
  }

  it should "update a Normal prior on Normal data with known sigma" in
  {
    // tau0 = 1/4, tau = 1, n = 2, sum = 4: mu = 4 / 2.25, sigma = 1 / sqrt(2.25).
    val d = dist("posterior(normal(0, 2), normal(m, 1), [[1, 3]])")
    assert(d.kind == DistKind.Normal)
    assert(abs(d.params(0) - 4.0 / 2.25) < 1e-15 && abs(d.params(1) - 2.0 / 3.0) < 1e-15)
  }

  it should "feed the existing queries unchanged" in
  {
    val q = num("quantile(posterior(betadist(2, 2), binomial(10, p), 7), 0.5)")
    assert(q > 0.0 && q < 1.0)
    assert(abs(num("cdf(posterior(betadist(2, 2), binomial(10, p), 7), 0.5)") -
               num("cdf(betadist(9, 5), 0.5)")) < 1e-15)
  }

  it should "stay symbolic outside the table and on invalid data" in
  {
    for bad <- List(
      "posterior(normal(0, 2), normal(m, s), [[1, 3]])",        // unknown sigma (decision E)
      "posterior(betadist(2, 2), binomial(n, p), 7)",           // two free names
      "posterior(betadist(2, 2), binomial(10, 0.5), 7)",        // nothing to learn
      "posterior(betadist(2, 2), binomial(n, 0.5), 7)",         // the wrong slot is free
      "posterior(betadist(2, 2), binomial(10, p), 11)",         // k > n
      "posterior(betadist(2, 2), binomial(10, p), 2.5)",        // not a count
      "posterior(gammadist(2, 1), poisson(l), [[3, -1]])",      // a negative count
      "posterior(gammadist(2, 1), poisson(l), [[1.5]])",        // not a count
      "posterior(normal(0, 1), binomial(10, p), 3)",            // pair outside the table
      "posterior(betadist(2, 2), poisson(l), [[1]])",           // pair outside the table
      "posterior(7, binomial(10, p), 3)")                       // the prior is not a distribution
    do assert(parse(bad).eval(env).isLeft, s"$bad must stay symbolic")
  }

  it should "treat a bound name as not free" in
  {
    val bound = env.withBinding("p", _Number(0.3))
    assert(parse("posterior(betadist(2, 2), binomial(10, p), 7)").eval(bound).isLeft)
  }

  it should "round-trip through toString" in
  {
    val e = parse("posterior(betadist(2, 2), binomial(10, p), 7)")
    assert(parse(e.toString) == e, s"round-trip failed: ${e.toString}")
  }

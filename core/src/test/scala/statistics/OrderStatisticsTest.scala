package it.grypho.scala.leonardo
package statistics

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** Order statistics of a sample (F_0129): `median`, `quantile(x, p[, method])` with type 7 by
 *  default and type 6 on request (Decision 3), and `sort`. */
class OrderStatisticsTest extends AnyFlatSpec:

  private val env = new Environment()
  private def ev(s: String): Either[_Expression, _Value] = Parser.parse(s).get.eval(env)
  private def evExact(s: String): Either[_Expression, _Value] = Parser.parse(s, Some(30)).get.eval(env)

  private def num(s: String): Double = ev(s) match
    case Right(_Number(d)) => d
    case other             => fail(s"$s gave $other")

  private val tenToOne = "[[10, 9, 8, 7, 6, 5, 4, 3, 2, 1]]"

  "median(x)" should "pick the middle of an odd sample and average the middle pair of an even one" in {
    assert(num("median([[3, 1, 2]])") == 2.0)
    assert(num("median([[4, 1, 3, 2]])") == 2.5)
  }

  it should "stay exact on an exact sample" in {
    assert(evExact("median([[1, 2, 3, 4]])") == Right(_Rational(BigInt(5)).divide(_Rational(2)).getOrElse(fail())))
  }

  it should "be the distribution's 0.5 quantile when given a distribution" in {
    assert(math.abs(num("median(normal(3, 2))") - 3.0) < 1e-9)
  }

  "quantile(x, p)" should "interpolate between order statistics by type 7, R's and numpy's default" in {
    assert(math.abs(num(s"quantile($tenToOne, 0.25)") - 3.25) < 1e-12)
    assert(math.abs(num(s"quantile($tenToOne, 0.9)") - 9.1) < 1e-12)
  }

  it should "locate at (n + 1)p by type 6 on request, the book's percentile formula" in {
    assert(math.abs(num(s"quantile($tenToOne, 0.25, 6)") - 2.75) < 1e-12)
    assert(num(s"quantile($tenToOne, 0.05, 6)") == 1.0)     // (n + 1)p below 1: the minimum
    assert(num(s"quantile($tenToOne, 0.95, 6)") == 10.0)    // (n + 1)p past n: the maximum
  }

  it should "give the extremes at p = 0 and p = 1 under both types" in {
    for m <- List("", ", 6") do
      assert(num(s"quantile($tenToOne, 0$m)") == 1.0)
      assert(num(s"quantile($tenToOne, 1$m)") == 10.0)
  }

  it should "stay exact on an exact sample and an exact p" in {
    assert(evExact("quantile([[1, 2, 3, 4]], 1/3)") == Right(_Rational(2)))
  }

  it should "still answer a distribution's quantile as before" in {
    assert(math.abs(num("quantile(normal(0, 1), 0.975)") - 1.959963984540054) < 1e-9)
  }

  it should "stay unevaluated for a p outside [0, 1], an unknown type, or a method given with a distribution" in {
    assert(ev(s"quantile($tenToOne, 1.5)").isLeft)
    assert(ev(s"quantile($tenToOne, 0.5, 5)").isLeft)
    assert(ev("quantile(normal(0, 1), 0.5, 6)").isLeft)
  }

  "sort(x)" should "order a row as a row and a column as a column" in {
    assert(ev("sort([[3, 1, 2]])") == ev("[[1, 2, 3]]"))
    assert(ev("sort([[3], [1], [2]])") == ev("[[1], [2], [3]]"))
  }

  it should "answer a column of every entry for a general matrix, and keep an exact sample exact" in {
    assert(ev("sort([[4, 1], [3, 2]])") == ev("[[1], [2], [3], [4]]"))
    assert(evExact("sort([[1/2, 1/3]])") == evExact("[[1/3, 1/2]]"))
  }

  "the nodes" should "round-trip through toString" in {
    for s <- List("median(x)", "quantile(x, 0.5)", "quantile(x, 0.5, 6)", "sort(x)") do
      val e = Parser.parse(s).get
      assert(Parser.parse(e.toString).get == e, s)
  }

  "the words" should "be reserved, since productions need them" in {
    assert(Parser.ReservedWords.contains("median") && Parser.ReservedWords.contains("sort"))
  }

package it.grypho.scala.leonardo
package probability

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** The geometric and hypergeometric families (F_0132): every query the other families answer. */
class DiscreteFamiliesTest extends AnyFlatSpec:

  private def ev(s: String): Either[_Expression, _Value] = Parser.parse(s).get.eval(new Environment())

  private def num(s: String): Double = ev(s) match
    case Right(_Number(d)) => d
    case other             => fail(s"$s gave $other")

  private def close(a: Double, b: Double, rel: Double = 1e-12): Boolean =
    math.abs(a - b) <= rel * math.max(math.abs(a), math.abs(b))

  private def choose(n: Int, k: Int): BigInt =
    if k < 0 || k > n then BigInt(0) else (1 to k).foldLeft(BigInt(1))((acc, i) => acc * (n - k + i) / i)

  // --- geometric: trials up to and including the first success, support 1, 2, ... ---

  "geometric(p)" should "count trials to the first success, from 1" in {
    assert(num("pdf(geometric(0.25), 1)") == 0.25)
    assert(close(num("pdf(geometric(0.25), 3)"), 0.75 * 0.75 * 0.25))
    assert(num("pdf(geometric(0.25), 0)") == 0.0)
    assert(num("pdf(geometric(0.25), 2.5)") == 0.0)
  }

  it should "have the cdf 1 - (1 - p)^k" in {
    assert(close(num("cdf(geometric(0.25), 3)"), 1 - math.pow(0.75, 3)))
    assert(num("cdf(geometric(0.25), 0.5)") == 0.0)
  }

  it should "keep the cdf's digits for a small p, where 1 - (1 - p)^k cancels" in {
    // P(X <= 1000) for p = 1e-12 is k*p - k(k-1)p^2/2 + ..., about 1e-9.
    val (p, k) = (1e-12, 1000.0)
    val expected = k * p - k * (k - 1) * p * p / 2
    assert(close(num("cdf(geometric(1e-12), 1000)"), expected))
  }

  it should "have mean 1/p and variance (1 - p)/p^2" in {
    assert(close(num("expect(geometric(0.25))"), 4.0))
    assert(close(num("variance(geometric(0.25))"), 12.0))
  }

  it should "invert its cdf at the smallest integer reaching p" in {
    assert(num("quantile(geometric(0.5), 0.75)") == 2.0)
  }

  it should "include the lower endpoint in prob(d, lo, hi)" in {
    assert(close(num("prob(geometric(0.5), 2, 3)"), 0.25 + 0.125))
  }

  it should "draw whole numbers from 1 up, reproducibly" in {
    ev("draw(geometric(0.3), 50, 7)") match
      case Right(m: _MatrixValue) =>
        assert(m.toVector.forall(x => x >= 1 && x == math.floor(x)))
        assert(ev("draw(geometric(0.3), 50, 7)") == Right(m))
      case other => fail(s"expected a column, got $other")
  }

  it should "stay symbolic outside 0 < p <= 1" in {
    assert(ev("geometric(0)").isLeft)
    assert(ev("geometric(1.5)").isLeft)
  }

  it should "print the call that rebuilds it" in {
    assert(ev("geometric(0.25)").map(_.toString) == Right("geometric(0.25)"))
  }

  // --- hypergeometric(N, K, n): n draws without replacement from N items, K of them successes ---

  "hypergeometric(N, K, n)" should "have the mass C(K, k) C(N - K, n - k) / C(N, n)" in {
    val expected = (choose(7, 4) * choose(13, 8)).toDouble / choose(20, 12).toDouble
    assert(close(num("pdf(hypergeometric(20, 7, 12), 4)"), expected))
    assert(num("pdf(hypergeometric(20, 7, 12), 8)") == 0.0)
  }

  it should "sum to one over its support, and accumulate it in the cdf" in {
    val masses = (0 to 7).map(k => num(s"pdf(hypergeometric(20, 7, 12), $k)"))
    assert(close(masses.sum, 1.0))
    assert(close(num("cdf(hypergeometric(20, 7, 12), 4)"), masses.take(5).sum))
    assert(num("cdf(hypergeometric(20, 7, 12), 7)") == 1.0)
  }

  it should "start its support at max(0, n + K - N)" in {
    // 15 draws from 20 with 12 successes: at least 7 successes.
    assert(num("pdf(hypergeometric(20, 12, 15), 6)") == 0.0)
    assert(num("cdf(hypergeometric(20, 12, 15), 6)") == 0.0)
    assert(num("pdf(hypergeometric(20, 12, 15), 7)") > 0.0)
  }

  it should "have mean nK/N and variance n(K/N)(1 - K/N)(N - n)/(N - 1)" in {
    assert(close(num("expect(hypergeometric(20, 7, 12))"), 12 * 7 / 20.0))
    assert(close(num("variance(hypergeometric(20, 7, 12))"), 12 * (7 / 20.0) * (13 / 20.0) * 8 / 19.0))
  }

  it should "agree with the binomial in the large-population limit" in {
    assert(close(num("pdf(hypergeometric(1000000, 300000, 10), 3)"), num("pdf(binomial(10, 0.3), 3)"), 1e-4))
  }

  it should "stay symbolic for parameters outside its domain" in {
    assert(ev("hypergeometric(10, 11, 3)").isLeft)
    assert(ev("hypergeometric(10, 3, 11)").isLeft)
    assert(ev("hypergeometric(10, 3, 2.5)").isLeft)
  }

  it should "decline a support too large to tabulate rather than run unbounded" in {
    assert(ev("hypergeometric(100000000, 50000000, 50000000)").isLeft)
  }

  it should "print the call that rebuilds it" in {
    assert(ev("hypergeometric(20, 7, 12)").map(_.toString) == Right("hypergeometric(20.0, 7.0, 12.0)"))
  }

  it should "answer a quantile at the support cap within a few seconds, on every platform" in {
    // A support of 999 999 points, one short of MaxHypergeometricSupport: the table is built
    // once and the quantile's bisection reads it.
    val start = System.nanoTime()
    val q = num("quantile(hypergeometric(2000000, 999998, 999998), 0.5)")
    val seconds = (System.nanoTime() - start) / 1e9
    assert(math.abs(q - 499999) <= 2, q)
    assert(seconds < 5.0, s"took $seconds s")
  }

  "the family words" should "be reserved, since productions need them" in {
    assert(Parser.ReservedWords.contains("geometric") && Parser.ReservedWords.contains("hypergeometric"))
  }

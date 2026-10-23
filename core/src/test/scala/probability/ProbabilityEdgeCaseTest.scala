package it.grypho.scala.leonardo
package probability

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** Regressions from the audit of 2026-10-23 in the distributions: each case failed before its fix. */
class ProbabilityEdgeCaseTest extends AnyFlatSpec:

  private val env = new Environment()
  private def number(s: String, in: Environment = env): Double = Parser.parse(s).get.eval(in) match
    case Right(_Number(d)) => d
    case other             => fail(s"$s gave $other")

  "an upper-tail probability" should "be defined for every family whose cdf is (F_0195)" in {
    val withX = env.withBinding("X", _Distribution.of(DistKind.Poisson, Vector(2.0)).getOrElse(fail("poisson")))
    assert(math.abs(number("prob(X >= 3)", withX) - 0.3233235838169366) < 1e-12)
    assert(number("prob(chisq(4), 3, inf)") > 0.0)
    assert(number("prob(gammadist(2, 1), 3, inf)") > 0.0)
    assert(number("prob(fdist(3, 5), 2, inf)") > 0.0)
  }

  it should "be read from the tail, not as 1 - cdf (F_0195)" in {
    val withY = env.withBinding("Y", _Distribution.of(DistKind.Normal, Vector(0.0, 1.0)).getOrElse(fail("normal")))
    val p = number("prob(Y > 10)", withY)
    assert(math.abs(p - 7.619853024160527e-24) <= 1e-6 * 7.62e-24, p)
    assert(number("prob(normal(0, 1), 9, 10)") > 1e-19)
    assert(math.abs(number("prob(exponential(1), 40, inf)") - math.exp(-40)) <= 1e-9 * math.exp(-40))
  }

  "the exponential cdf" should "keep a tiny argument (F_0195)" in {
    assert(math.abs(number("cdf(exponential(1), 1e-20)") - 1e-20) <= 1e-30)
  }

  "quantile" should "reach a heavy tail (F_0195)" in {
    val q = number("quantile(studentt(1), 1e-9)")
    assert(math.abs(q - -3.1830988618e8) <= 1e-6 * 3.18e8, q)
  }

  "bayes" should "not read underflowing products as zero evidence (F_0196)" in {
    Parser.parse("bayes([[1e-30, 1e-30]], [[1e-300, 2e-300]])").get.eval(env) match
      case Right(m: _MatrixValue) => assert(math.abs(m.toVector(0) - 1.0 / 3) < 1e-12)
      case other                  => fail(s"got $other")
  }

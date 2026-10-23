package it.grypho.scala.leonardo
package statistics

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** Regressions from the audit of 2026-10-23 in statistics: each case failed before its fix. */
class StatisticsEdgeCaseTest extends AnyFlatSpec:

  private val env = new Environment()
  private def ev(s: String): Either[_Expression, _Value] = Parser.parse(s).get.eval(env)

  "a test on a constant sample" should "be refused although rounding leaves a spread (F_0196)" in {
    assert(ev("ttest([[0.1, 0.1, 0.1]], 0.1)").isLeft)
    assert(ev("ttest([[0.1, 0.1, 0.1]], [[0.2, 0.2, 0.2]])").isLeft)
    assert(ev("vartest([[0.1, 0.1, 0.1]], [[1, 2, 4]])").isLeft)
    assert(ev("anova([[0.1, 0.1, 0.1]], [[0.3, 0.3, 0.3]])").isLeft)
    assert(ev("correlation([[0.1, 0.1, 0.1]], [[1, 2, 4]])").isLeft)
  }

  it should "still answer a genuine small spread at a large mean (F_0196)" in {
    assert(ev("ttest([[1000000.000001, 1000000.000002, 1000000.000004]], 1000000)").isRight)
  }

  "chisqtest" should "refuse a negative count (F_0196)" in {
    assert(ev("chisqtest([[5, -1], [3, 4]])").isLeft)
    assert(ev("chisqtest([[5, 1], [3, 4]])").isRight)
  }

  "rls" should "refuse a non-real cell rather than misalign the operands (F_0196)" in {
    assert(ev("rls([[1 + 2i], [0]], eye(2), [[1], [1]], 1, 1)").isLeft)
  }

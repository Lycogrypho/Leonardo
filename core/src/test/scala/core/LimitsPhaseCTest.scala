package it.grypho.scala.leonardo
package core

import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec

/** F_0124 phase C: the budgets of `scalar`, `logic`, `equation` and the exact tier are `Limit`s
 *  read from the environment, or from a `Limits` overload where a published function takes none.
 *  The defaults are the old values, so every other suite is the evidence the default path is
 *  unchanged; each case here lowers (or raises) one budget and watches the answer change. */
class LimitsPhaseCTest extends AnyFlatSpec:

  private val env = new Environment()
  private def parse(s: String): _Expression = Parser.parse(s).get
  private def exact(s: String): _Expression = Parser.parse(s, Some(30)).get
  private def with1(l: Limit, v: Long): Environment = env.withLimit(l, v)
  private def value(e: _Expression, en: Environment): _Expression = e.eval(en).fold(identity, identity)
  private val (a, b, x) = (_Variable("a"), _Variable("b"), _Variable("x"))

  "the phase C limits" should "default to the published constants" in {
    assert(Limits.Default(Limit.MaxTruthTableVars) == logic.MaxTruthTableVars)
    assert(Limits.Default(Limit.MaxKleeneTableVars) == logic.MaxKleeneTableVars)
    assert(Limits.Default(Limit.MaxTabulateTerms) == scalar.MaxTabulateTerms)
    assert(Limits.Default(Limit.MaxTaylorOrder) == scalar.MaxTaylorOrder)
    assert(Limits.Default(Limit.MaxFourierOrder) == scalar.MaxFourierOrder)
    assert(Limits.Default(Limit.MaxDerivativeNodes) == scalar.MaxDerivativeNodes)
    assert(Limits.Default(Limit.MaxExactFactorial) == scalar.MaxExactFactorial)
    assert(Limits.Default(Limit.MaxExactSequenceIndex) == scalar.MaxExactSequenceIndex)
    assert(Limits.Default(Limit.MaxExactPowerBits) == _Rational.MaxExactPowerBits)
    assert(Limits.Default(Limit.MaxExpandWork) == scalar.MaxExpandWork)
    assert(Limits.Default(Limit.MaxExpandPower) == scalar.MaxExpandPower)
  }

  "truth tables" should "read their variable caps from the environment" in {
    val e = parse("a and b")
    assert(logic.truthTable(e, List(a, b), env).size == 4)
    assert(logic.truthTable(e, List(a, b), with1(Limit.MaxTruthTableVars, 1)).isEmpty)
    assert(logic.kleeneTable(e, List(a, b), env).size == 9)
    assert(logic.kleeneTable(e, List(a, b), with1(Limit.MaxKleeneTableVars, 1)).isEmpty)
  }

  "the normal forms" should "read the clause cap through the Limits overload" in {
    val e = parse("(a and b) or (c and d)")
    assert(logic.toCNF(e, false, Limits.Default) == logic.toCNF(e))
    assert(logic.toCNF(e) != e)
    assert(logic.toCNF(e, false, Limits.Default.withLimit(Limit.MaxNormalFormClauses, 2)) == e)
    assert(logic.toDNF(parse("(a or b) and (c or d)"), false, Limits.Default.withLimit(Limit.MaxNormalFormClauses, 2)) ==
           parse("(a or b) and (c or d)"))
  }

  "a limit" should "take no more L'Hôpital steps than the environment allows" in {
    // three steps to cos(x)/6; stopped after one, the remaining (1 - cos x)/(3x^2) is left to
    // the perturbation tier, whose cancellation cannot land on 1/6 exactly
    val e = parse("(x - sin(x))/x^3")
    assert(scalar.evalLimit(e, x, _Number(0), scalar.LimitDir.Both, env) == _Number(1.0 / 6))
    assert(scalar.evalLimit(e, x, _Number(0), scalar.LimitDir.Both, with1(Limit.MaxHopitalSteps, 1)) != _Number(1.0 / 6))
  }

  "solve's numeric tier" should "keep no more roots than the environment allows" in {
    val eq = parse("sin(x) = 0") match
      case q: equation._Equation => q
      case other                 => fail(s"$other")
    assert(equation.solve(eq, x, env).size == 8)
    assert(equation.solve(eq, x, with1(Limit.MaxNumericRoots, 3)).size == 3)
  }

  "the vectorised matrix solver" should "read its dimension cap from the environment" in {
    val e = parse("solve([[1, 0], [0, 2]] * X + X * [[3, 0], [0, 4]] = [[4, 5], [5, 6]], X)")
    val solved = value(e, env)
    assert(solved.isInstanceOf[equation._Equation], solved)
    assert(!value(e, with1(Limit.MaxVectorizedSize, 3)).isInstanceOf[equation._Equation])
  }

  "tabulate, sum and product" should "read the term cap from the environment, both ways" in {
    assert(value(parse("tabulate(k, k, 1, 5)"), with1(Limit.MaxTabulateTerms, 4)).isInstanceOf[scalar._Tabulate])
    assert(value(parse("sum(k, k, 1, 5)"), with1(Limit.MaxTabulateTerms, 4)).isInstanceOf[scalar._Reduction])
    assert(value(parse("sum(k, k, 1, 1001)"), env).isInstanceOf[scalar._Reduction])
    assert(value(parse("sum(k, k, 1, 1001)"), with1(Limit.MaxTabulateTerms, 2000)) == _Number(501501))
    assert(scalar.rangeRefusal(_Number(1), _Number(5), with1(Limit.MaxTabulateTerms, 4))
             .contains("5 terms exceed the limit of 4"))
  }

  "an interval power" should "read the integer-exponent cap from the environment" in {
    val e = parse("[-2, 1]^3")
    assert(e.eval(env).isRight)
    assert(e.eval(with1(Limit.MaxIntervalPower, 2)).isLeft)
  }

  "the series" should "read their order caps and the derivative budget from the environment" in {
    val t = parse("taylor(exp(x), x, 0, 25)")
    assert(value(t, env).isInstanceOf[scalar._Taylor])
    assert(!value(t, with1(Limit.MaxTaylorOrder, 30)).isInstanceOf[scalar._Taylor])
    assert(scalar.taylorSeries(parse("exp(x)"), x, _Number(0), 25).isEmpty)
    assert(scalar.taylorSeries(parse("exp(x)"), x, _Number(0), 25,
                               Limits.Default.withLimit(Limit.MaxTaylorOrder, 30)).isDefined)
    // erf is outside the power-series engine, so its coefficients take the derivative path
    val erf = parse("taylor(erf(x), x, 0, 3)")
    assert(!value(erf, env).isInstanceOf[scalar._Taylor])
    assert(value(erf, with1(Limit.MaxDerivativeNodes, 3)).isInstanceOf[scalar._Taylor])
    val f = parse("fourierSeries(x, x, 2*pi, 4)")
    assert(!value(f, env).isInstanceOf[scalar._FourierSeries])
    assert(value(f, with1(Limit.MaxFourierOrder, 3)).isInstanceOf[scalar._FourierSeries])
    val p = parse("pade(exp(x), x, 2, 2)")
    assert(!value(p, env).isInstanceOf[scalar._Pade])
    assert(value(p, with1(Limit.MaxTaylorOrder, 3)).isInstanceOf[scalar._Pade])
  }

  "the exact kernels" should "read their caps from the environment" in {
    assert(exact("fact(20)").eval(env).isRight)
    assert(exact("fact(20)").eval(with1(Limit.MaxExactFactorial, 10)).isLeft)
    assert(exact("Gamma(21)").eval(with1(Limit.MaxExactFactorial, 10)).isLeft)
    assert(exact("mfact(20, 2)").eval(with1(Limit.MaxExactFactorial, 10)).isLeft)
    assert(exact("fib(100)").eval(env).isRight)
    assert(exact("fib(100)").eval(with1(Limit.MaxExactSequenceIndex, 50)).isLeft)
    assert(parse("fib(100)").eval(with1(Limit.MaxExactSequenceIndex, 50)).isLeft)
    assert(exact("binom(30, 15)").eval(with1(Limit.MaxExactSequenceIndex, 10)).isLeft)
    assert(exact("harmonic(20)").eval(with1(Limit.MaxExactSequenceIndex, 10)).isLeft)
    assert(exact("catalan(20)").eval(with1(Limit.MaxExactSequenceIndex, 10)).isLeft)
    assert(exact("(3/7)^100").eval(env).isRight)
    assert(exact("(3/7)^100").eval(with1(Limit.MaxExactPowerBits, 10)).isLeft)
    assert(exact("[1/3, 1/2]^100").eval(with1(Limit.MaxExactPowerBits, 10)).isLeft)
  }

  "expand" should "read its power and work caps through the Limits overload" in {
    val cube = parse("(a + b)^3")
    assert(scalar.expand(cube) != cube)
    assert(scalar.expand(cube, Limits.Default.withLimit(Limit.MaxExpandPower, 2)).toString == cube.toString)
    val (r, why) = scalar.expandReporting(parse("(a + b)*(c + d)"), Limits.Default.withLimit(Limit.MaxExpandWork, 3))
    assert(why.exists(_.contains("limit of 3 term pairs")), (r, why))
  }

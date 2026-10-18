package it.grypho.scala.leonardo
package logic

import core.*
import parser.Parser
import org.scalatest.flatspec.AnyFlatSpec


/** `defuzz(e, v, lo, hi, method)` (F_0136): the three defuzzification methods at the prompt,
 *  each answering what its library function answers. */
class DefuzzMethodTest extends AnyFlatSpec:

  private def parsed(s: String): _Expression = Parser.parse(s).get

  private val x     = _Variable("x")
  private val curve = parsed("trapmf(x, 0, 2, 4, 10)")
  private val env   = new Environment()

  "defuzz with a method word" should "answer the mean of maxima for mom" in {
    val expected = meanOfMaxima(curve, x, 0.0, 10.0, DefaultDefuzzSamples, env)
    assert(expected.isDefined)
    assert(parsed("defuzz(trapmf(x, 0, 2, 4, 10), x, 0, 10, mom)").eval(env) == Right(_Number(expected.getOrElse(Double.NaN))))
  }

  it should "answer the bisector for bisector" in {
    val expected = bisector(curve, x, 0.0, 10.0, DefaultDefuzzSamples, env)
    assert(expected.isDefined)
    assert(parsed("defuzz(trapmf(x, 0, 2, 4, 10), x, 0, 10, bisector)").eval(env) == Right(_Number(expected.getOrElse(Double.NaN))))
  }

  it should "answer what the four-argument form answers for centroid" in {
    assert(parsed("defuzz(trapmf(x, 0, 2, 4, 10), x, 0, 10, centroid)").eval(env) ==
           parsed("defuzz(trapmf(x, 0, 2, 4, 10), x, 0, 10)").eval(env))
  }

  it should "tell the three methods apart on a skewed curve" in {
    val answers = List("centroid", "mom", "bisector").map { m =>
      parsed(s"defuzz(trapmf(x, 0, 2, 4, 10), x, 0, 10, $m)").eval(env)
    }
    assert(answers.distinct.size == 3, answers)
  }

  it should "round-trip through toString" in {
    val e = parsed("defuzz(trapmf(x, 0, 2, 4, 10), x, 0, 10, mom)")
    assert(parsed(e.toString) == e)
  }

  it should "stay unevaluated while a bound is symbolic" in {
    assert(parsed("defuzz(trapmf(x, 0, 2, 4, 10), x, 0, h, bisector)").eval(env).isLeft)
  }

  "the method words" should "not be reserved: they are slot words" in {
    List("centroid", "mom", "bisector").foreach(w => assert(!Parser.ReservedWords.contains(w), w))
    assert(parsed("mom + 1").freeVars == Set("mom"))
  }

  "an unknown method word" should "not parse" in {
    assert(!Parser.parse("defuzz(trapmf(x, 0, 2, 4, 10), x, 0, 10, median)").successful)
  }

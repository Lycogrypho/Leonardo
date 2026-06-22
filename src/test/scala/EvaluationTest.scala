package it.grypho.scala.leonardo

import parser.Environment
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.BeforeAndAfter

import expr._


class EvaluationTest extends AnyFlatSpec with BeforeAndAfter:
  implicit val env: Environment = new Environment()

  before { env.reset() }

  val x_var = _Variable("x")

  val evaluationTests: List[(_Expression, Any)] = List(
    (_Number(10),                                   10.0                       : Any),
    (_Number(3.1234567890),                         3.12346                    : Any),
    (Sum(_Number(2), _Number(3)),                   5.0                        : Any),
    (Product(_Number(7.0), _Number(3.1)),           21.7                       : Any),
    (Ratio(_Number(1), _Number(4)),                 0.25                       : Any),
    (Sum(_Number(1), Sum(_Number(4), _Number(5))),  10.0                       : Any),
    (Ratio(Product(_Number(3), _Number(8)), _Number(4)), 6.0                   : Any),
    (Sin(_Number(scala.math.Pi / 2)),               1.0                        : Any),
    (x_var,                                         x_var                      : Any),
    (Sum(Sum(_Number(1), _Number(4)), x_var),       Sum(_Number(5), x_var)     : Any)
  )

  for s <- evaluationTests do
    s"expression \"${s._1}\" " should s"be evaluated to \"${s._2}\" " in
    {
      s._1.eval(env) match
        case Right(x) => assert(x == s._2)
        case Left(e)  => assert(e == s._2)
    }

  // --- special value handling ---

  "division by zero" should "evaluate to Infinity, not a corrupt large number" in
  {
    Ratio(_Number(1), _Number(0)).eval(env) match
      case Right(x)  => assert(x.isInfinite && x > 0)
      case Left(sym) => fail(s"expected Infinity but got symbolic: $sym")
  }

  "negative division by zero" should "evaluate to -Infinity" in
  {
    Ratio(_Number(-1), _Number(0)).eval(env) match
      case Right(x)  => assert(x.isInfinite && x < 0)
      case Left(sym) => fail(s"expected -Infinity but got symbolic: $sym")
  }

  "log of a negative number" should "evaluate to NaN, not 0.0" in
  {
    Log(_Number(-1)).eval(env) match
      case Right(x)  => assert(x.isNaN)
      case Left(sym) => fail(s"expected NaN but got symbolic: $sym")
  }

  "log of zero" should "evaluate to -Infinity, not a corrupt large number" in
  {
    Log(_Number(0)).eval(env) match
      case Right(x)  => assert(x.isInfinite && x < 0)
      case Left(sym) => fail(s"expected -Infinity but got symbolic: $sym")
  }

  "a very large number" should "evaluate without Long overflow" in
  {
    val large = 1e15
    _Number(large).eval(env) match
      case Right(x)  => assert(x == large)
      case Left(sym) => fail(s"expected $large but got symbolic: $sym")
  }

  // --- variable binding ---

  val a = _Variable("a")
  val b = _Variable("b")

  "an assigned variable" should "be evaluated to a numeric value" in
  {
    a.set(_Number(3))
    assert(Sum(a, b).eval(env) == Sum(_Number(3), _Variable("b")).eval(env))

    b.set(_Number(7))
    assert(Sum(a, b).eval(env) == _Number(10).eval(env))
  }

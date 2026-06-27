package it.grypho.scala.leonardo

import parser.Environment
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.BeforeAndAfter

import expr._


class EvaluationTest extends AnyFlatSpec with BeforeAndAfter:
  implicit val env: Environment = new Environment()

  before { env.reset() }

  val x_var = _Variable("x")

  // Expected result is an _Expression: a _Number for rows that reduce to a value
  // (matched on the Right branch), a symbolic expression for rows that stay symbolic.
  val evaluationTests: List[(_Expression, _Expression)] = List(
    (_Number(10),                                   _Number(10.0)),
    (_Number(3.1234567890),                         _Number(3.12346)),
    (Sum(_Number(2), _Number(3)),                   _Number(5.0)),
    (Product(_Number(7.0), _Number(3.1)),           _Number(21.7)),
    (Ratio(_Number(1), _Number(4)),                 _Number(0.25)),
    (Sum(_Number(1), Sum(_Number(4), _Number(5))),  _Number(10.0)),
    (Ratio(Product(_Number(3), _Number(8)), _Number(4)), _Number(6.0)),
    (Sin(_Number(scala.math.Pi / 2)),               _Number(1.0)),
    (x_var,                                         x_var),
    (Sum(Sum(_Number(1), _Number(4)), x_var),       Sum(_Number(5), x_var))
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
      case Right(_Number(x)) => assert(x.isInfinite && x > 0)
      case other             => fail(s"expected Infinity but got: $other")
  }

  "negative division by zero" should "evaluate to -Infinity" in
  {
    Ratio(_Number(-1), _Number(0)).eval(env) match
      case Right(_Number(x)) => assert(x.isInfinite && x < 0)
      case other             => fail(s"expected -Infinity but got: $other")
  }

  "log of a negative number" should "evaluate to NaN, not 0.0" in
  {
    Log(_Number(-1)).eval(env) match
      case Right(_Number(x)) => assert(x.isNaN)
      case other             => fail(s"expected NaN but got: $other")
  }

  "log of zero" should "evaluate to -Infinity, not a corrupt large number" in
  {
    Log(_Number(0)).eval(env) match
      case Right(_Number(x)) => assert(x.isInfinite && x < 0)
      case other             => fail(s"expected -Infinity but got: $other")
  }

  "a very large number" should "evaluate without Long overflow" in
  {
    val large = 1e15
    _Number(large).eval(env) match
      case Right(_Number(x)) => assert(x == large)
      case other             => fail(s"expected $large but got: $other")
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

package it.grypho.scala.leonardo

import it.grypho.scala.leonardo.parser.Environment
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.BeforeAndAfter

import expr._


class EvaluationTest extends AnyFlatSpec with BeforeAndAfter:
  implicit val env: Environment = new Environment()

  val x_var = _Variable("x")

  val evaluationTests = List(
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

  val a = _Variable("a")
  val b = _Variable("b")

  "an assigned variable" should "be evaluated to a numeric value" in
  {
    a.set(_Number(3))
    assert(Sum(a, b).eval(env) == Sum(_Number(3), _Variable("b")).eval(env))

    b.set(_Number(7))
    assert(Sum(a, b).eval(env) == _Number(10).eval(env))
  }

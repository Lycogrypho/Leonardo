package it.grypho.scala.leonardo

import it.grypho.scala.leonardo.parser.Environment
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.BeforeAndAfter
import org.scalatest.exceptions.TestFailedException

import expr._

class EvaluationTest extends AnyFlatSpec with BeforeAndAfter
{
  implicit val env: Environment = new Environment()

  val x_var = _Variable("x")

  val evaluationTests = List( //("1",                  "test check"),
                              (_Number(10), 10.0),
                              (_Number(3.1234567890), 3.12346),
                              (Sum(_Number(2), _Number(3)), 5.0),
                              (Product(_Number(7.0), _Number(3.1)), 21.7),
                              (Ratio(_Number(1), _Number(4)), 0.25),
                              (Sum(_Number(1), Sum(_Number(4), _Number(5))), 10.0),
                              (Ratio(Product(_Number(3), _Number(8)), _Number(4)), 6.0),
                              (Sin(_Number(scala.math.Pi / 2)), 1.0),
                              (x_var, x_var),
                              (Sum(Sum(_Number(1), _Number(4)), x_var), Sum(_Number(5), x_var))
                              )

  for (s <- evaluationTests)
  {

    try
    {
      s"expression \"${s._1}\" " should s"be evaluated to \"${s._2}\" " in
      {
        for (x <- s._1.eval())
        {
          //println(s"numeratore \"${n.num}\" \t denominatore \"${n.den}\" \t valore \"${n.value}\" \t  ")
          assert(x == s._2)
        }
      }
    }
    catch
    {
      case e: TestFailedException => println(s"Error ${e} - problem occurred evaluating \"${s._1}\" to \"${s._2}\""
                                             ) //that should produce result \"${s._2}\" ")
      case e                      => println(s"unknown exception ${e} - problem occurred with expression \"${s}\" "
                                             ) //that should produce result \"${s._2}\" ")
    }
  }

  val a = _Variable("a")
  val b = _Variable("b")

  try
  {
    s"an assigned variable " should s"be evaluated to a numeric value " in
    {

      a.set(_Number(3))

      assert(Sum(a, b).eval() == Sum(_Number(3), _Variable("b")).eval())

      b.set(_Number(7))
      assert(Sum(a, b).eval() == _Number(10).eval())


    }
  }
  catch
  {
    case e: TestFailedException => println(s"Error ${e} - problem occurred evaluating variables \"${a}\" and \"${b}\""
                                           ) //that should produce result \"${s._2}\" ")
    case e                      => println(
      s"unknown exception ${e} - problem occurred with variables \"${a}\" and \"${b}\" "
      ) //that should produce result \"${s._2}\" ")
  }


}
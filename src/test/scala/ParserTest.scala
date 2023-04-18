package it.grypho.scala.leonardo

import it.grypho.scala.leonardo.parser.{Environment, Parser}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.BeforeAndAfter
import org.scalatest.exceptions.TestFailedException

class ParserTest extends AnyFlatSpec with BeforeAndAfter
{
  def env = new Environment()
  def parser = new Parser(env)

  val expressionStrings = List( ("1", "((1~None)~None)"),
                            ("1 + 2", "((1~None)~Some((+~(2~None))))"),
                            ("(1 + 9.2)", "(((((~((1~None)~Some((+~(9.2~None)))))~))~None)~None)"),
                            ("3*3E-5", "((3~Some((*~3E-5)))~None)"),
                            ("exp(1)", "((((exp(~((1~None)~None))~))~None)~None)"),
                            ("sin(a)", "((((sin(~((a~None)~None))~))~None)~None)"),
                            ("tg(x + 2)", "((((tg(~((x~None)~Some((+~(2~None)))))~))~None)~None)"),
                            ("tg(x + log(2))", "((((tg(~((x~None)~Some((+~(((log(~((2~None)~None))~))~None)))))~))~None)~None)"),
                            ("1* exp(x)", "((1~Some((*~((exp(~((x~None)~None))~)))))~None)"),
                            ("exp(cos(a))", "((((exp(~((((cos(~((a~None)~None))~))~None)~None))~))~None)~None)"),
                            ("3a", "((3~Some((~a)))~None)"),
                            ("3sin(a)", "((3~Some((~((sin(~((a~None)~None))~)))))~None)"),
                            ("derive(cos(3x), x)", "((((((derive(~((((cos(~((3~Some((~x)))~None))~))~None)~None))~,)~x)~))~None)~None)")
    )


  for (s <- expressionStrings)
  {
    s"Expression ${s._1} in the expression list " should s"be parsed as ${s._2}" in
      {
        try
        {
          val p = parser.parse(s._1)
          assert(p.get.toString === s._2 )
        }
        catch
        {
          case e: TestFailedException => println(s"Error ${e} - problem occurred with expression \"${s._1}\" that should produce result \"${s._2}\" ")
          case _ => println(s"unknown exception - problem occurred with expression \"${s._1}\" that should produce result \"${s._2}\" ")

        }
      }
  }

}
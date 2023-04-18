package it.grypho.scala.leonardo

import it.grypho.scala.leonardo.parser.{Environment, Parser}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.BeforeAndAfter
import org.scalatest.exceptions.TestFailedException

class ParserTest extends AnyFlatSpec with BeforeAndAfter
{
  def env = new Environment()
  def parser = new Parser(env)

  val expressionStrings = List( //("1",                  "test check"),
    ("1",                  """((None~(1~None))~None)"""),
    ("1 + 2",              """((None~(1~None))~Some((+~(2~None))))"""),
    ("(1 + 9.2)",          """((None~((((~((None~(1~None))~Some((+~(9.2~None)))))~))~None))~None)"""),
    ("3*3E-5",             """((None~(3~Some((*~3E-5))))~None)"""),
    ("exp(1)",             """((None~(((exp(~((None~(1~None))~None))~))~None))~None)"""),
    ("sin(a)",             """((None~(((sin(~((None~(a~None))~None))~))~None))~None)"""),
    ("tg(x + 2)",          """((None~(((tg(~((None~(x~None))~Some((+~(2~None)))))~))~None))~None)"""),
    ("tg(x + log(2))",     """((None~(((tg(~((None~(x~None))~Some((+~(((log(~((None~(2~None))~None))~))~None)))))~))~None))~None)"""),
    ("1* exp(x)",          """((None~(1~Some((*~((exp(~((None~(x~None))~None))~))))))~None)"""),
    ("exp(cos(a))",        """((None~(((exp(~((None~(((cos(~((None~(a~None))~None))~))~None))~None))~))~None))~None)"""),
    ("3a",                 """((None~(3~Some((~a))))~None)"""),
    ("3sin(a)",            """((None~(3~Some((~((sin(~((None~(a~None))~None))~))))))~None)"""),
    ("derive(cos(3x), x)", """((None~(((((derive(~((None~(((cos(~((None~(3~Some((~x))))~None))~))~None))~None))~,)~x)~))~None))~None)"""),
    ("-2",                 """((Some(-)~(2~None))~None)"""),
    ("+3sin(-a)",          """((Some(+)~(3~Some((~((sin(~((Some(-)~(a~None))~None))~))))))~None)""")
  )


  for (s <- expressionStrings)
  {
        try
        {
          s"Expression ${s._1} in the expression list " should s"be parsed as ${s._2}" in
            {
              val p = parser.parse(s._1)
              assert(p.get.toString == s._2 )
            }
        }
        catch
        {
          case e: TestFailedException => println(s"Error ${e} - problem occurred with expression \"${s._1}\" that should produce result \"${s._2}\" ")
          case _ => println(s"unknown exception - problem occurred with expression \"${s._1}\" that should produce result \"${s._2}\" ")
        }
  }

}
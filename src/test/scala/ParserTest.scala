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
    ("1",                  """1.0"""),
    ("1 + 2",              """(1.0 + 2.0)"""),
    ("(1 + 9.2)",          """(1.0 + 9.2)"""),
    ("3*3E-5",             """(3.0 * 3.0E-5)"""),
    ("exp(1)",             """exp(1.0)"""),
    ("sin(a)",             """sin(a)"""),
    ("tg(x + 2)",          """tg((x + 2.0))"""),
    ("tg(x + log(2))",     """tg((x + log(2.0)))"""),
    ("1* exp(x)",          """(1.0 * exp(x))"""),
    ("exp(cos(a))",        """exp(cos(a))"""),
    ("3a",                 """(3.0 * a)"""),
    ("3sin(a)",            """(3.0 * sin(a))"""),
    //("derive(cos(3x), x)", """"""),
    ("-2",                 """(-1.0 * 2.0)"""),
    ("+3sin(-a)",          """(3.0 * sin((-1.0 * a)))"""),
    ("-3k",                """(-1.0 * (3.0 * k))"""),
    ("sin(a)cos(b)",       """(sin(a) * cos(b))""")
  )
  //List( //("1",                  "test check"),
  //    ("1",                  """((None~(1~None))~None)"""),
  //    ("1 + 2",              """((None~(1~None))~Some((+~(2~None))))"""),
  //    ("(1 + 9.2)",          """((None~((((~((None~(1~None))~Some((+~(9.2~None)))))~))~None))~None)"""),
  //    ("3*3E-5",             """((None~(3~Some((*~3E-5))))~None)"""),
  //    ("exp(1)",             """((None~((((exp~()~((None~(1~None))~None))~))~None))~None)"""),
  //    ("sin(a)",             """((None~((((sin~()~((None~(a~None))~None))~))~None))~None)"""),
  //    ("tg(x + 2)",          """((None~((((tg~()~((None~(x~None))~Some((+~(2~None)))))~))~None))~None)"""),
  //    ("tg(x + log(2))",     """((None~((((tg~()~((None~(x~None))~Some((+~((((log~()~((None~(2~None))~None))~))~None)))))~))~None))~None)"""),
  //    ("1* exp(x)",          """((None~(1~Some((*~(((exp~()~((None~(x~None))~None))~))))))~None)"""),
  //    ("exp(cos(a))",        """((None~((((exp~()~((None~((((cos~()~((None~(a~None))~None))~))~None))~None))~))~None))~None)"""),
  //    ("3a",                 """((None~(3~Some((~a))))~None)"""),
  //    ("3sin(a)",            """((None~(3~Some((~(((sin~()~((None~(a~None))~None))~))))))~None)"""),
  //    ("derive(cos(3x), x)", """((None~(((((derive(~((None~((((cos~()~((None~(3~Some((~x))))~None))~))~None))~None))~,)~x)~))~None))~None)"""),
  //    ("-2",                 """((Some(-)~(2~None))~None)"""),
  //    ("+3sin(-a)",          """((Some(+)~(3~Some((~(((sin~()~((Some(-)~(a~None))~None))~))))))~None)"""),
  //    ("-3k",                """((Some(-)~(3~Some((~k))))~None)"""),
  //    ("sin(a)cos(b)",       """((None~((((sin~()~((None~(a~None))~None))~))~Some((~(((cos~()~((None~(b~None))~None))~))))))~None)""")
  //  )


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
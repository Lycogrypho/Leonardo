package it.grypho.scala.leonardo

import it.grypho.scala.leonardo.parser.{Environment, Parser}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.BeforeAndAfter

import expr._


class ParserTest extends AnyFlatSpec with BeforeAndAfter:
  val parser = new Parser

  val expressionStrings = List(
    ("1",                  """1.0""",                         _Number(1)),
    ("1 + 2",              """(1.0 + 2.0)""",                 Sum(_Number(1), _Number(2))),
    ("(1 + 9.2)",          """(1.0 + 9.2)""",                 Sum(_Number(1), _Number(9.2))),
    ("3*3E-5",             """(3.0 * 3.0E-5)""",              Product(_Number(3), _Number(3.0E-5))),
    ("exp(1)",             """exp(1.0)""",                    Exp(_Number(1.0))),
    ("sin(a)",             """sin(a)""",                      Sin(_Variable("a"))),
    ("tg(x + 2)",          """tg((x + 2.0))""",               Tg(Sum(_Variable("x"), _Number(2.0)))),
    ("tg(x + log(2))",     """tg((x + log(2.0)))""",          Tg(Sum(_Variable("x"), Log(_Number(2.0))))),
    ("1* exp(x)",          """(1.0 * exp(x))""",              Product(_Number(1), Exp(_Variable("x")))),
    ("exp(cos(a))",        """exp(cos(a))""",                 Exp(Cos(_Variable("a")))),
    ("3a",                 """(3.0 * a)""",                   Product(_Number(3.0), _Variable("a"))),
    ("3sin(a)",            """(3.0 * sin(a))""",              Product(_Number(3.0), Sin(_Variable("a")))),
    ("derive(cos(3x), x)", """deriv(cos((3.0 * x)) in x)""", _Derivative(Cos(Product(_Number(3.0), _Variable("x"))), _Variable("x"))),
    ("-2",                 """(-1.0 * 2.0)""",                Product(_Number(-1), _Number(2))),
    ("+3sin(-a)",          """(3.0 * sin((-1.0 * a)))""",     Product(_Number(3), Sin(Product(_Number(-1), _Variable("a"))))),
    ("-3k",                """(-1.0 * (3.0 * k))""",          Product(_Number(-1), Product(_Number(3.0), _Variable("k")))),
    ("sin(a)cos(b)",       """(sin(a) * cos(b))""",           Product(Sin(_Variable("a")), Cos(_Variable("b")))),
    ("pow(2, 10)",         """(2.0 ^ 10.0)""",               Power(_Number(2), _Number(10)))
  )

  for s <- expressionStrings do
    s"Expression ${s._1} in the expression list " should s"be parsed as ${s._3}" in
    {
      assert(parser.parse(s._1).get == s._3)
    }

  for s <- expressionStrings do
    s"Internal representation of Expression ${s._1} " should s"be converted into string ${s._2}" in
    {
      assert(s._3.toString == s._2)
    }

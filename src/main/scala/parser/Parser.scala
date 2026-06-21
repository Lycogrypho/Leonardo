package it.grypho.scala.leonardo
package parser

import scala.util.parsing.combinator.JavaTokenParsers
import it.grypho.scala.leonardo.expr._


/**
 * Grammar:
 * value      ::= Number | Variable
 * operator   ::= "+" | "-" | "*" | "/"
 * expr       ::= value | function "(" expr ")" | expr (operator expr)?
 * */

class Parser extends JavaTokenParsers:

  def expr: Parser[_Expression] = opt("+" | "-") ~ simpleExpr ^^
    {
      case Some("-") ~ e => Product(_Number(-1), e)
      case _         ~ e => e
    }

  def simpleExpr: Parser[_Expression] = term ~ rep(("+" | "-") ~ term) ^^
    {
      case left ~ rights => rights.foldLeft(left)
        {
          case (x, "+" ~ y) => Sum(x, y)
          case (x, "-" ~ y) => Sum(x, Product(_Number(-1), y))
        }
    }

  def term: Parser[_Expression] = factor ~ rep(("*" | "/" | "") ~ factor) ^^
    {
      case left ~ rights => rights.foldLeft(left)
        {
          case (x, "*" ~ y) => Product(x, y)
          case (x, "/" ~ y) => Ratio(x, y)
          case (x, ""  ~ y) => Product(x, y)
        }
    }

  def factor: Parser[_Expression] = function | functional | value | "(" ~> expr <~ ")"

  def function: Parser[_Expression] =
    "exp(" ~> expr <~ ")"                              ^^ Exp.apply       |
    "log(" ~> expr <~ ")"                              ^^ Log.apply       |
    "sin(" ~> expr <~ ")"                              ^^ Sin.apply       |
    "cos(" ~> expr <~ ")"                              ^^ Cos.apply       |
    "tg("  ~> expr <~ ")"                              ^^ Tg.apply        |
    "pow(" ~> expr ~ "," ~ expr <~ ")"                 ^^ { case b ~ _ ~ e => Power(b, e) }

  def functional: Parser[_Expression] =
    "derive("   ~> expr ~ "," ~ variable <~ ")"                                        ^^ { case e ~ _ ~ v         => _Derivative(e, v)            } |
    "integral(" ~> expr ~ "," ~ variable ~ "," ~ value ~ "," ~ value <~ ")"           ^^ { case e ~ _ ~ v ~ _ ~ l ~ _ ~ u => _DefIntegral(e, v, l, u) } |
    "integral(" ~> expr ~ "," ~ variable <~ ")"                                        ^^ { case e ~ _ ~ v         => _Integral(e, v)              }

  def value:    Parser[_Value]     = number | variable
  def number:   Parser[_Number]    = (floatingPointNumber | decimalNumber | wholeNumber) ^^ { s => _Number(s.toDouble) }
  def variable: Parser[_Variable]  = """[a-zA-Z]""".r ^^ { s => _Variable(s) }

  def parse(str: String): ParseResult[Any] = parseAll(expr, str)

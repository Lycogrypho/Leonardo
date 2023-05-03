package it.grypho.scala.leonardo
package parser

import scala.util.parsing.combinator.{ImplicitConversions, JavaTokenParsers}
import it.grypho.scala.leonardo.expr._
import it.grypho.scala.leonardo.expr._Number
import it.grypho.scala.leonardo.expr._Operation
//import it.grypho.scala.leonardo.expr


//import org.scalactic.Prettifier.default


/**
 * Grammar:
 * value ::= Number | Variable
 * operator ::= "+" | "-" | "*" | "/"
 * expr ::= value | function "(" expr ")" | expr (operator expr)?
 *
 **/

class Parser(env: Environment) extends JavaTokenParsers
{
  def expr      : Parser[_Expression] = opt("+" | "-" | "") ~ simpleExpr ^^
                                        {
                                          case Some("-") ~ e => Product(_Number(-1)(env), e)
                                          case _ ~ e         => e //Product(_Number(+1)(env), e)
                                        }

  def simpleExpr: Parser[_Expression] = term ~ rep(("+" | "-") ~ term) ^^
                                        {
                                          case left ~ rights => rights.foldLeft(left)
                                            {
                                              case (x, "+" ~ y) => Sum(x, y)
                                              case (x, "-" ~ y) => Sum(x, Product(_Number(-1)(env), y))

                                            }
                                        }

  def term      : Parser[_Expression] = factor ~ rep(("*"|"/"|"") ~ factor) ^^
                                        {
                                          case left ~ rights =>  rights.foldLeft(left)
                                            {
                                              case (x, "*" ~ y) => Product(x, y)
                                              case (x, "/" ~ y) => Ratio(x, y)
                                              case (x, "" ~ y)  => Product(x, y)

                                            }
                                        }
  //def factor    : Parser[_Expression] = function | functinoal | value | "(" ~> expr <~ ")"
  def factor    : Parser[_Expression] = function | value | "(" ~> expr <~ ")"
  def function  : Parser[_Expression] = "exp(" ~> expr <~ ")" ^^ Exp.apply |
                                        "log(" ~> expr <~ ")" ^^ Log.apply |
                                        "sin(" ~> expr <~ ")" ^^ Sin.apply |
                                        "cos(" ~> expr <~ ")" ^^ Cos.apply |
                                        "tg("  ~> expr <~ ")" ^^ Tg.apply

 /* def functional: Parser[_Expression] = "derive(" ~> expr <~ "," ~> variable <~ ")" ^^ { case e ~ v => _Derivative(e, v) } |
                                        "integral(" ~> expr <~ "," ~> variable <~ ")" ^^ { case e ~ v  => _Integral(e, v) }|
                                        "integral(" ~> expr <~ "," ~> variable <~  "," ~> value <~  "," ~> value <~ ")" ^^ { case e ~ v ~ l ~ u => _DefIntegral(e, v, l, u) }
*/
  def value     : Parser[_Value] = number | variable
  def number    : Parser[_Number] = (floatingPointNumber | decimalNumber | wholeNumber) ^^  {(a: String) => _Number(a.toDouble)(env)}
  def variable  :Parser[_Variable] = """[a-zA-Z]""".r ^^  {(a: String) => _Variable(a)(env)}

  def parse(str:String): ParseResult[Any] = parseAll(expr, str)
}


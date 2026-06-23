package it.grypho.scala.leonardo
package parser

import scala.util.parsing.combinator.JavaTokenParsers
import expr._


/**
 * Recursive descent parser for mathematical expressions.
 *
 * Grammar:
 *   expr       ::= ["+" | "-"] simpleExpr
 *   simpleExpr ::= term (("+"|"-") term)*
 *   term       ::= power (("*"|"/"|"") power)*      -- "" enables implicit multiplication
 *   power      ::= factor ["^" power]               -- right-associative; binds tighter than * /
 *   factor     ::= function | functional | value | "(" expr ")"
 *   function   ::= "exp(" expr ")" | "log(" expr ")" | "sin(" expr ")"
 *                | "cos(" expr ")" | "tg(" expr ")"
 *                | "pow(" expr "," expr ")"
 *   functional ::= "derive(" expr "," variable ")"
 *                | "integral(" expr "," variable ")"
 *                | "integral(" expr "," variable "," value "," value ")"
 *   value      ::= number | variable
 *   number     ::= floatingPointNumber | decimalNumber | wholeNumber
 *   variable   ::= [a-zA-Z]    -- single character only
 */
object Parser extends JavaTokenParsers:

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

  def term: Parser[_Expression] = power ~ rep(("*" | "/" | "") ~ power) ^^
    {
      case left ~ rights => rights.foldLeft(left)
        {
          case (x, "*" ~ y) => Product(x, y)
          case (x, "/" ~ y) => Ratio(x, y)
          case (x, ""  ~ y) => Product(x, y)
        }
    }

  // Right-associative exponentiation: 2 ^ 3 ^ 2 parses as 2 ^ (3 ^ 2).
  // Binds tighter than * and /; use parentheses for a compound base or exponent.
  def power: Parser[_Expression] = factor ~ opt("^" ~> power) ^^
    {
      case b ~ Some(e) => Power(b, e)
      case b ~ None    => b
    }

  def factor: Parser[_Expression] = function | functional | value | "(" ~> expr <~ ")"

  def function: Parser[_Expression] =
    "exp(" ~> expr <~ ")"             ^^ Exp.apply                              |
    "log(" ~> expr <~ ")"             ^^ Log.apply                              |
    "sin(" ~> expr <~ ")"             ^^ Sin.apply                              |
    "cos(" ~> expr <~ ")"             ^^ Cos.apply                              |
    "tg("  ~> expr <~ ")"             ^^ Tg.apply                               |
    "pow(" ~> expr ~ "," ~ expr <~ ")" ^^ { case b ~ _ ~ e => Power(b, e) }

  def functional: Parser[_Expression] =
    "derive("   ~> expr ~ "," ~ variable <~ ")"                                ^^ { case e ~ _ ~ v             => _Derivative(e, v)            } |
    "integral(" ~> expr ~ "," ~ variable ~ "," ~ value ~ "," ~ value <~ ")"   ^^ { case e ~ _ ~ v ~ _ ~ l ~ _ ~ u => _DefIntegral(e, v, l, u) } |
    "integral(" ~> expr ~ "," ~ variable <~ ")"                                ^^ { case e ~ _ ~ v             => _Integral(e, v)              }

  def value:    Parser[_Value]    = number | variable
  def number:   Parser[_Number]   = (floatingPointNumber | decimalNumber | wholeNumber) ^^ { s => _Number(s.toDouble) }
  def variable: Parser[_Variable] = """[a-zA-Z]""".r ^^ { s => _Variable(s) }

  def parse(str: String): ParseResult[_Expression] = parseAll(expr, str)

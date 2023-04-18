package it.grypho.scala.leonardo
package parser

import scala.util.parsing.combinator.{ImplicitConversions, JavaTokenParsers}
import it.grypho.scala.leonardo.expr._

/**
 * Grammar:
 * value ::= Number | Variable
 * operator ::= "+" | "-" | "*" | "/"
 * expr ::= value | function "(" expr ")" | expr (operator expr)?
 *
 **/

class Parser(env: Environment) extends JavaTokenParsers
{
  def expr      : Parser[Any] = term ~ opt("+" ~ term | "-" ~ term) // | function ~ expr ~ ")"
  def term      : Parser[Any] = factor ~ opt("*" ~ factor | "/" ~ factor| "" ~ factor)
  def factor    : Parser[Any] = "(" ~ expr ~ ")" | function ~ expr ~ ")" |  derivative | value
  def function  : Parser[Any] = "exp(" | "log(" | "sin(" | "cos(" | "tg("
  def derivative: Parser[Any] = "derive(" ~ expr ~ "," ~ variable ~ ")"
  def value     : Parser[Any] = number | variable
  def number    : Parser[Any] = floatingPointNumber | decimalNumber | wholeNumber
  def variable  :Parser[Any] = "a" | "x" //"""[a-zA-Z]""".r

  def parse(str:String): ParseResult[Any] = parseAll(expr, str)
}


package it.grypho.scala.leonardo
package parser

import scala.util.parsing.combinator.JavaTokenParsers
import core.*
import scalar.*


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
 *                | "cos(" expr ")" | "tan(" expr ")" | "tg(" expr ")" | "asin(" expr ")" | "acos(" expr ")" | "atan(" expr ")"
 *                | "pow(" expr "," expr ")"
 *   functional ::= "derive(" expr "," variable ")"
 *                | "integral(" expr "," variable ")"
 *                | "integral(" expr "," variable "," value "," value ")"
 *   value      ::= number | constant | variable
 *   number     ::= floatingPointNumber
 *   constant   ::= "pi" | "e"             -- built-in numeric literals (word-boundary guarded)
 *   variable   ::= [a-zA-Z][a-zA-Z0-9]*
 */
object Parser extends JavaTokenParsers:

  private val MaxDepth = 500
  private val depth = new ThreadLocal[Int]:
    override def initialValue(): Int = 0

  private def guardedExpr: Parser[_Expression] = Parser { in =>
    val d = depth.get()
    if d >= MaxDepth then Failure(s"expression exceeds maximum nesting depth of $MaxDepth", in)
    else
      depth.set(d + 1)
      try expr(in)
      finally depth.set(d)
  }

  def expr: Parser[_Expression] = opt("+" | "-") ~ simpleExpr ^^
    {
      // A negated literal folds to a negative _Number, so "-2".toString is "-2.0"
      // (a re-parsable fixpoint) rather than "(-1.0 * 2.0)".
      case Some("-") ~ _Number(n) => _Number(-n)
      case Some("-") ~ e          => Product(_Number(-1), e)
      case _         ~ e          => e
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

  def factor: Parser[_Expression] = function | functional | value | "(" ~> guardedExpr <~ ")"

  def function: Parser[_Expression] =
    "exp(" ~> guardedExpr <~ ")"                          ^^ Exp.apply                         |
    "log(" ~> guardedExpr <~ ")"                          ^^ Log.apply                         |
    "sin(" ~> guardedExpr <~ ")"                          ^^ Sin.apply                         |
    "cos(" ~> guardedExpr <~ ")"                          ^^ Cos.apply                         |
    "tan(" ~> guardedExpr <~ ")"                          ^^ Tg.apply                         |
    "tg("  ~> guardedExpr <~ ")"                          ^^ Tg.apply                          |
    "asin(" ~> guardedExpr <~ ")"                         ^^ Asin.apply                        |
    "acos(" ~> guardedExpr <~ ")"                         ^^ Acos.apply                        |
    "atan(" ~> guardedExpr <~ ")"                         ^^ Atan.apply                        |
    "pow(" ~> guardedExpr ~ "," ~ guardedExpr <~ ")"      ^^ { case b ~ _ ~ e => Power(b, e) }

  def functional: Parser[_Expression] =
    "derive("   ~> guardedExpr ~ "," ~ variable <~ ")"                                           ^^ { case e ~ _ ~ v             => _Derivative(e, v)            } |
    "integral(" ~> guardedExpr ~ "," ~ variable ~ "," ~ value ~ "," ~ value <~ ")"              ^^ { case e ~ _ ~ v ~ _ ~ l ~ _ ~ u => _DefIntegral(e, v, l, u) } |
    "integral(" ~> guardedExpr ~ "," ~ variable <~ ")"                                           ^^ { case e ~ _ ~ v             => _Integral(e, v)              }

  def value:    Parser[_Expression] = number | constant | variable
  def number:   Parser[_Number]    = floatingPointNumber ^^ { s => _Number(s.toDouble) }
  // Negative lookahead prevents "pine" from matching as pi + ne, or "exp" as e + xp.
  def constant: Parser[_Number]    =
    """pi(?![a-zA-Z0-9])""".r ^^^ _Number(math.Pi) |
    """e(?![a-zA-Z0-9])""".r  ^^^ _Number(math.E)
  def variable: Parser[_Variable]  = """[a-zA-Z][a-zA-Z0-9]*""".r ^^ { s => _Variable(s) }

  def parse(str: String): ParseResult[_Expression] = parseAll(expr, str)

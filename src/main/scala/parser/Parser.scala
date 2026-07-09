package it.grypho.scala.leonardo
package parser

import scala.util.parsing.combinator.JavaTokenParsers
import core.*
import scalar.*


/**
 * Recursive descent parser for mathematical expressions.
 *
 * Grammar:
 *   expr        ::= ["+" | "-"] simpleExpr
 *   simpleExpr  ::= term (("+"|"-") term)*
 *   term        ::= signedPower (("*"|"/") signedPower | "" power)*
 *                                                    -- "" enables implicit multiplication;
 *                                                    -- its operand is UNSIGNED (see below)
 *   signedPower ::= ["+" | "-"] power                -- signed operand after an explicit operator
 *   power       ::= factor ["^" signedPower]         -- right-associative; binds tighter than * /
 *   factor      ::= function | functional | value | "(" expr ")"
 *   function    ::= "exp(" expr ")" | "log(" expr ")" | "sin(" expr ")"
 *                 | "cos(" expr ")" | "tan(" expr ")" | "tg(" expr ")" | "asin(" expr ")" | "acos(" expr ")" | "atan(" expr ")"
 *                 | "pow(" expr "," expr ")"
 *   functional  ::= "derive(" expr "," variable ")"
 *                 | "integral(" expr "," variable ")"
 *                 | "integral(" expr "," variable "," signedValue "," signedValue ")"
 *   signedValue ::= ["+" | "-"] value
 *   value       ::= number | constant | variable
 *   number      ::= unsigned floating literal       -- a '-' is ALWAYS an operator, never
 *                                                    -- part of the token; scientific-notation
 *                                                    -- exponent signs ("3E-5") are unaffected
 *   constant    ::= "pi" | "e"            -- built-in numeric literals (word-boundary guarded)
 *   variable    ::= [a-zA-Z][a-zA-Z0-9]*
 *
 * Sign handling: a signed literal token would let implicit multiplication swallow
 * "3-2" as 3 * (-2) instead of subtraction (and "e-3" as e * (-3)). So the number
 * token is unsigned, and the sign is grammar: allowed where an explicit operator
 * precedes (start of expression, after "+ - * / ^" and in integral limits), and
 * deliberately NOT allowed as the operand of implicit multiplication.
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

  // A negated literal folds to a negative _Number, so "-2".toString is "-2.0"
  // (a re-parsable fixpoint) rather than "(-1.0 * 2.0)".
  private def applySign(sign: Option[String], e: _Expression): _Expression = (sign, e) match
    case (Some("-"), _Number(n)) => _Number(-n)
    case (Some("-"), _)          => Product(_Number(-1), e)
    case _                       => e

  def expr: Parser[_Expression] = opt("+" | "-") ~ simpleExpr ^^
    {
      case sign ~ e => applySign(sign, e)
    }

  def simpleExpr: Parser[_Expression] = term ~ rep(("+" | "-") ~ term) ^^
    {
      case left ~ rights => rights.foldLeft(left)
        {
          case (x, "+" ~ y) => Sum(x, y)
          case (x, "-" ~ y) => Sum(x, Product(_Number(-1), y))
        }
    }

  // Explicit * and / take a signed right operand (3 * -x); implicit multiplication
  // takes an unsigned one, so that "3-2" binds as subtraction, never as 3 * (-2).
  def term: Parser[_Expression] = signedPower ~ rep(("*" | "/") ~ signedPower | "" ~ power) ^^
    {
      case left ~ rights => rights.foldLeft(left)
        {
          case (x, "*" ~ y) => Product(x, y)
          case (x, "/" ~ y) => Ratio(x, y)
          case (x, ""  ~ y) => Product(x, y)
        }
    }

  // Signed operand for explicit-operator positions: start of a term, after * / ^
  // and after binary + -. Enables 3 * -x, 3 + -x, 2^-x, 3 - -2.
  def signedPower: Parser[_Expression] = opt("+" | "-") ~ power ^^
    {
      case sign ~ e => applySign(sign, e)
    }

  // Right-associative exponentiation: 2 ^ 3 ^ 2 parses as 2 ^ (3 ^ 2).
  // Binds tighter than * and /; use parentheses for a compound base or exponent.
  def power: Parser[_Expression] = factor ~ opt("^" ~> signedPower) ^^
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
    "integral(" ~> guardedExpr ~ "," ~ variable ~ "," ~ signedValue ~ "," ~ signedValue <~ ")"  ^^ { case e ~ _ ~ v ~ _ ~ l ~ _ ~ u => _DefIntegral(e, v, l, u) } |
    "integral(" ~> guardedExpr ~ "," ~ variable <~ ")"                                           ^^ { case e ~ _ ~ v             => _Integral(e, v)              }

  // Integral limits accept a sign ("integral(x, x, -1, 1)", lower limit "-pi")
  // without opening the limit position to full sub-expressions.
  def signedValue: Parser[_Expression] = opt("+" | "-") ~ value ^^
    {
      case sign ~ e => applySign(sign, e)
    }

  def value:    Parser[_Expression] = number | constant | variable
  // Unsigned by design — see the sign-handling note in the header. The [eE][+-]?
  // exponent keeps scientific notation ("3E-5", "2e-3") intact.
  def number:   Parser[_Number]    = """(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?""".r ^^ { s => _Number(s.toDouble) }
  // Negative lookahead prevents "pine" from matching as pi + ne, or "exp" as e + xp.
  def constant: Parser[_Number]    =
    """pi(?![a-zA-Z0-9])""".r ^^^ _Number(math.Pi) |
    """e(?![a-zA-Z0-9])""".r  ^^^ _Number(math.E)
  def variable: Parser[_Variable]  = """[a-zA-Z][a-zA-Z0-9]*""".r ^^ { s => _Variable(s) }

  def parse(str: String): ParseResult[_Expression] = parseAll(expr, str)

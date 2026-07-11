package it.grypho.scala.leonardo
package parser

import scala.util.parsing.combinator.JavaTokenParsers
import core.*
import scalar.*
import matrix.*
import equation.*


/**
 * Recursive descent parser for mathematical expressions.
 *
 * Grammar:
 *   topLevel    ::= expr ["=" expr]              -- "=" makes an _Equation; top level only,
 *                                                -- so equations never nest
 *   expr        ::= ["+" | "-"] simpleExpr
 *   simpleExpr  ::= term (("+"|"-") term)*
 *   term        ::= signedPower (("*"|"/") signedPower | "" power)*
 *                                                    -- "" enables implicit multiplication;
 *                                                    -- its operand is UNSIGNED (see below)
 *   signedPower ::= ["+" | "-"] power                -- signed operand after an explicit operator
 *   power       ::= factor ["^" signedPower]         -- right-associative; binds tighter than * /
 *   factor      ::= function | functional | matrix | value | "(" expr ")"
 *   matrix      ::= "[" matrixRow ("," matrixRow)* "]"   -- rows must be equally long
 *   matrixRow   ::= "[" expr ("," expr)* "]"
 *   function    ::= "exp(" expr ")" | "log(" expr ")" | "sin(" expr ")"
 *                 | "cos(" expr ")" | "tan(" expr ")" | "tg(" expr ")" | "asin(" expr ")" | "acos(" expr ")" | "atan(" expr ")"
 *                 | "transpose(" expr ")" | "pow(" expr "," expr ")"
 *   functional  ::= "derive(" expr "," variable ")"
 *                 | "integral(" expr "," variable ")"
 *                 | "integral(" expr "," variable "," signedValue "," signedValue ")"
 *   signedValue ::= ["+" | "-"] value
 *   value       ::= number | constant | variable
 *   number      ::= unsigned floating literal       -- a '-' is ALWAYS an operator, never
 *                                                    -- part of the token; scientific-notation
 *                                                    -- exponent signs ("3E-5") are unaffected
 *   constant    ::= "pi" | "e"            -- built-in numeric literals (word-boundary guarded)
 *   variable    ::= [a-zA-Z][a-zA-Z0-9]*  -- except ReservedWords (functions, functionals,
 *                                         -- constants, REPL commands); exact match only
 *
 * Sign handling: a signed literal token would let implicit multiplication swallow
 * "3-2" as 3 * (-2) instead of subtraction (and "e-3" as e * (-3)). So the number
 * token is unsigned, and the sign is grammar: allowed where an explicit operator
 * precedes (start of expression, after "+ - * / ^" and in integral limits), and
 * deliberately NOT allowed as the operand of implicit multiplication.
 *
 * Matrix dispatch: operand types are unknown at parse time, so + and * normally
 * build the scalar Sum/Product nodes (whose eval also computes CONCRETE matrix
 * values — see scalar._Operation). But when a matrix is syntactically visible
 * (a matrix literal, transpose(...), or a node built from one), the fold dispatches
 * structurally: + → MatSum, matrix*matrix → MatProduct, scalar*matrix → MatScale,
 * -M → MatScale(-1, M). This keeps the round-trip invariant: the matrix nodes print
 * as "(a + b)" / "(a * b)", and re-parsing recovers the same node type from the
 * shape of the operands.
 */
object Parser extends JavaTokenParsers:

  // Words that can never be parsed as a variable name: the function/functional
  // vocabulary and constants of the grammar itself, plus the REPL command words —
  // reserved here too so a session binding can never shadow (or be shadowed by)
  // a command. Bare "sin" or "simplify" is a parse error, not a variable; names
  // merely starting with a reserved word ("sina", "evalx") stay legal.
  val ReservedWords: Set[String] = Set(
    "exp", "log", "sin", "cos", "tan", "tg", "asin", "acos", "atan",
    "pow", "transpose",                                   // functions
    "derive", "integral",                                 // functionals
    "pi", "e",                                            // constants
    "simplify", "expand", "eval", "env", "vars",
    "precision", "unset", "help", "quit", "exit"          // REPL commands
  )

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

  // A node is matrix-shaped when a matrix is syntactically visible in it: a matrix
  // literal or one of the matrix operation nodes (MatSum, MatProduct, MatScale,
  // Transpose). Drives the structural dispatch of + - * and unary minus.
  private def isMatrixShaped(e: _Expression): Boolean =
    e.isInstanceOf[_Matrix] || e.isInstanceOf[_MatrixOperation]

  private def mkSum(x: _Expression, y: _Expression): _Expression =
    if isMatrixShaped(x) || isMatrixShaped(y) then MatSum(x, y) else Sum(x, y)

  private def mkMul(x: _Expression, y: _Expression): _Expression =
    (isMatrixShaped(x), isMatrixShaped(y)) match
      case (true, true)   => MatProduct(x, y)
      case (false, true)  => MatScale(x, y)
      case (true, false)  => MatScale(y, x)
      case (false, false) => Product(x, y)

  private def mkNeg(e: _Expression): _Expression =
    if isMatrixShaped(e) then MatScale(_Number(-1), e) else Product(_Number(-1), e)

  // A negated literal folds to a negative _Number, so "-2".toString is "-2.0"
  // (a re-parsable fixpoint) rather than "(-1.0 * 2.0)". Likewise the sign folds
  // into an existing leading numeric coefficient — "-3k" is (-3.0 * k), and
  // "(-1.0 * M)" re-parses to MatScale(-1, M) rather than a doubly-wrapped
  // MatScale(-1, MatScale(1, M)) — keeping negated products round-trip stable.
  private def applySign(sign: Option[String], e: _Expression): _Expression = (sign, e) match
    case (Some("-"), _Number(n))                => _Number(-n)
    case (Some("-"), Product(_Number(k), rest)) => Product(_Number(-k), rest)
    case (Some("-"), MatScale(_Number(k), m))   => MatScale(_Number(-k), m)
    case (Some("-"), _)                         => mkNeg(e)
    case _                                      => e

  def expr: Parser[_Expression] = opt("+" | "-") ~ simpleExpr ^^
    {
      case sign ~ e => applySign(sign, e)
    }

  def simpleExpr: Parser[_Expression] = term ~ rep(("+" | "-") ~ term) ^^
    {
      case left ~ rights => rights.foldLeft(left)
        {
          case (x, "+" ~ y) => mkSum(x, y)
          case (x, "-" ~ y) => mkSum(x, mkNeg(y))
        }
    }

  // Explicit * and / take a signed right operand (3 * -x); implicit multiplication
  // takes an unsigned one, so that "3-2" binds as subtraction, never as 3 * (-2).
  def term: Parser[_Expression] = signedPower ~ rep(("*" | "/") ~ signedPower | "" ~ power) ^^
    {
      case left ~ rights => rights.foldLeft(left)
        {
          case (x, "*" ~ y) => mkMul(x, y)
          case (x, "/" ~ y) => Ratio(x, y)
          case (x, ""  ~ y) => mkMul(x, y)
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

  def factor: Parser[_Expression] = function | functional | matrixLiteral | value | "(" ~> guardedExpr <~ ")"

  // Matrix literal: [[a, b], [c, d]] — rows of full expressions, all equally long
  // (a row vector is [[1, 2]]). Ragged rows are a parse error, not an exception.
  def matrixLiteral: Parser[_Expression] =
    "[" ~> rep1sep(matrixRow, ",") <~ "]" ^? (
      { case rows if rows.forall(_.size == rows.head.size) =>
          _Matrix(rows.size, rows.head.size, rows.flatten.toVector) },
      _ => "matrix rows must all have the same length"
    )

  def matrixRow: Parser[List[_Expression]] = "[" ~> rep1sep(guardedExpr, ",") <~ "]"

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
    "transpose(" ~> guardedExpr <~ ")"                    ^^ Transpose.apply                   |
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
  // Reserved words are rejected wholesale — the regex is greedy, so "simplify"
  // cannot fall back to variable "simplif" times variable "y".
  def variable: Parser[_Variable]  = """[a-zA-Z][a-zA-Z0-9]*""".r ^? (
    { case s if !ReservedWords.contains(s) => _Variable(s) },
    s => s"'$s' is a reserved word and cannot be used as a variable"
  )

  // Equations exist only at the top level: "a = b" is an _Equation, "a = b = c" and
  // "(a = b)" are parse errors (parenthesized positions contain plain expressions).
  def topLevel: Parser[_Expression] = expr ~ opt("=" ~> expr) ^^
    {
      case l ~ Some(r) => _Equation(l, r)
      case l ~ None    => l
    }

  def parse(str: String): ParseResult[_Expression] = parseAll(topLevel, str)

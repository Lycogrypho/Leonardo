package it.grypho.scala.leonardo
package parser

import scala.util.parsing.combinator.JavaTokenParsers
import core.*
import scalar.*
import matrix.*
import equation.*
import transform.*


/**
 * Recursive descent parser for mathematical expressions.
 *
 * Grammar:
 *   topLevel      ::= equationExpr
 *   equationExpr  ::= expr ["=" expr | "==" expr]  -- non-associative; "=" → _Equation,
 *                                                   -- "==" → _EqualityCheck (always _Bool).
 *                                                   -- "(a = b)" is now valid; "a = b = c" is not.
 *   expr          ::= ["+" | "-"] simpleExpr
 *   simpleExpr  ::= term (("+"|"-") term)*
 *   term        ::= signedPower (("*"|"/") signedPower | "" power)*
 *                                                    -- "" enables implicit multiplication;
 *                                                    -- its operand is UNSIGNED (see below)
 *   signedPower ::= ["+" | "-"] power                -- signed operand after an explicit operator
 *   power       ::= factor ["^" signedPower]         -- right-associative; binds tighter than * /
 *   factor      ::= function | functional | matrix | value | "(" equationExpr ")"
 *   matrix      ::= "[" matrixRow ("," matrixRow)* "]"   -- rows must be equally long
 *   matrixRow   ::= "[" equationExpr ("," equationExpr)* "]"
 *   function    ::= "exp(" expr ")" | "log(" expr ")" | "ln(" expr ")" | "sin(" expr ")"
 *                 | "cos(" expr ")" | "tan(" expr ")" | "tg(" expr ")" | "asin(" expr ")" | "acos(" expr ")" | "atan(" expr ")"
 *                 | "transpose(" expr ")" | "pow(" expr "," expr ")"
 *   functional  ::= "derive(" expr "," variable ")"
 *                 | "integral(" expr "," variable ")"
 *                 | "integral(" expr "," variable "," signedValue "," signedValue ")"
 *                 | "solve(" equationExpr "," variable ")"   -- equationExpr may be an inline
 *                                                            -- "lhs = rhs" or a named variable
 *                 | "solveSystem(" equationExpr "," variable ("," variable)* ")"
 *                                                            -- equationExpr is a matrix of
 *                                                            -- equations [[eq1, eq2, …]]
 *   signedValue ::= ["+" | "-"] value
 *   value       ::= number | constant | variable
 *   number      ::= unsigned floating literal       -- a '-' is ALWAYS an operator, never
 *                                                    -- part of the token; scientific-notation
 *                                                    -- exponent signs ("3E-5") are unaffected
 *   constant    ::= "pi" | "e" | "i"      -- built-in literals (word-boundary guarded);
 *                                         -- "i" is the imaginary unit → _Complex(0, 1)
 *   variable    ::= [a-zA-Z][a-zA-Z0-9_]*  -- except ReservedWords (functions, functionals,
 *                                         -- constants, REPL commands); exact match only;
 *                                         -- underscore allowed after the first char (x_1, alpha_hat)
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
 *
 * Performance: all grammar productions are `lazy val` so the combinator graph and
 * compiled regexes are built once on first access and reused for every subsequent
 * `parse` call. Only `guardedExpr` and `guardedSignedPower` remain `def` because
 * they capture per-call depth state via a ThreadLocal counter.
 */
object Parser extends JavaTokenParsers:

  // Words that can never be parsed as a variable name: the function/functional
  // vocabulary and constants of the grammar itself, plus the REPL command words —
  // reserved here too so a session binding can never shadow (or be shadowed by)
  // a command. Bare "sin" or "simplify" is a parse error, not a variable; names
  // merely starting with a reserved word ("sina", "evalx") stay legal.
  val ReservedWords: Set[String] = Set(
    "exp", "log", "ln", "sin", "cos", "tan", "tg", "asin", "acos", "atan",
    "pow", "transpose", "at",                             // functions
    "derive", "integral", "solve", "solveSystem", "limit", "laplace", "fourier", "invlaplace", // functionals
    "pi", "e", "i", "inf",                               // constants (inf = +∞)
    "simplify", "expand", "eval", "env", "vars",
    "precision", "unset", "samples", "help", "quit", "exit" // REPL commands
  )

  private val MaxDepth = 500
  private val depth = new ThreadLocal[Int]:
    override def initialValue(): Int = 0

  private def guardedExpr: Parser[_Expression] = Parser { in =>
    val d = depth.get()
    if d >= MaxDepth then Failure(s"expression exceeds maximum nesting depth of $MaxDepth", in)
    else
      depth.set(d + 1)
      try equationExpr(in)
      finally depth.set(d)
  }

  // The power → signedPower → power right-recursion never passes through guardedExpr
  // (which is only entered via explicit parentheses and function argument lists), so
  // 2^-2^-2^-… bypasses the MaxDepth check and blows the JVM stack. This wrapper
  // increments the depth counter at every ^ so the same cap applies to both paths.
  private def guardedSignedPower: Parser[_Expression] = Parser { in =>
    val d = depth.get()
    if d >= MaxDepth then Failure(s"expression exceeds maximum nesting depth of $MaxDepth", in)
    else
      depth.set(d + 1)
      try signedPower(in)
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

  // "==" is tried before "=" so that the two-character operator is not shadowed by the
  // one-character one. Non-associative: only one optional relation per expression, so
  // "a = b = c" is a parse error (the trailing "= c" is rejected by parseAll / the
  // surrounding rule). "(a = b)" is now a valid sub-expression — use it to bind a
  // named equation: "h := x = 5", then "solve(h, x)".
  lazy val equationExpr: Parser[_Expression] = expr ~ opt(("==" | "=") ~ expr) ^^
    {
      case l ~ Some("==" ~ r) => _EqualityCheck(l, r)
      case l ~ Some(_ ~ r)    => _Equation(l, r)
      case l ~ None           => l
    }

  lazy val expr: Parser[_Expression] = opt("+" | "-") ~ simpleExpr ^^
    {
      case sign ~ e => applySign(sign, e)
    }

  lazy val simpleExpr: Parser[_Expression] = term ~ rep(("+" | "-") ~ term) ^^
    {
      case left ~ rights => rights.foldLeft(left)
        {
          case (x, "+" ~ y) => mkSum(x, y)
          case (x, "-" ~ y) => mkSum(x, mkNeg(y))
        }
    }

  // Explicit * and / take a signed right operand (3 * -x); implicit multiplication
  // takes an unsigned one, so that "3-2" binds as subtraction, never as 3 * (-2).
  lazy val term: Parser[_Expression] = signedPower ~ rep(("*" | "/") ~ signedPower | "" ~ power) ^^
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
  lazy val signedPower: Parser[_Expression] = opt("+" | "-") ~ power ^^
    {
      case sign ~ e => applySign(sign, e)
    }

  // Right-associative exponentiation: 2 ^ 3 ^ 2 parses as 2 ^ (3 ^ 2).
  // Binds tighter than * and /; use parentheses for a compound base or exponent.
  // The exponent uses guardedSignedPower so that deep ^ chains are caught by the same
  // MaxDepth cap as deeply parenthesised expressions.
  lazy val power: Parser[_Expression] = factor ~ opt("^" ~> guardedSignedPower) ^^
    {
      case b ~ Some(e) => Power(b, e)
      case b ~ None    => b
    }

  lazy val factor: Parser[_Expression] = function | functional | matrixLiteral | value | "(" ~> guardedExpr <~ ")"

  // Matrix literal: [[a, b], [c, d]] — rows of full expressions, all equally long
  // (a row vector is [[1, 2]]). Ragged rows are a parse error, not an exception.
  lazy val matrixLiteral: Parser[_Expression] =
    "[" ~> rep1sep(matrixRow, ",") <~ "]" ^? (
      { case rows if rows.forall(_.size == rows.head.size) =>
          _Matrix(rows.size, rows.head.size, rows.flatten.toVector) },
      _ => "matrix rows must all have the same length"
    )

  lazy val matrixRow: Parser[List[_Expression]] = "[" ~> rep1sep(guardedExpr, ",") <~ "]"

  lazy val function: Parser[_Expression] =
    "exp(" ~> guardedExpr <~ ")"                                              ^^ Exp.apply      |
    "ln("  ~> guardedExpr <~ ")"                                              ^^ Ln.apply       |
    "log(" ~> guardedExpr ~ opt("," ~> guardedExpr) <~ ")" ^^ {
      case e ~ None    => LogBase(e, _Number(10))
      case e ~ Some(b) => LogBase(e, b)
    }                                                                                           |
    "sin(" ~> guardedExpr <~ ")"                                              ^^ Sin.apply      |
    "cos(" ~> guardedExpr <~ ")"                          ^^ Cos.apply                         |
    "tan(" ~> guardedExpr <~ ")"                          ^^ Tg.apply                         |
    "tg("  ~> guardedExpr <~ ")"                          ^^ Tg.apply                          |
    "asin(" ~> guardedExpr <~ ")"                         ^^ Asin.apply                        |
    "acos(" ~> guardedExpr <~ ")"                         ^^ Acos.apply                        |
    "atan(" ~> guardedExpr <~ ")"                         ^^ Atan.apply                        |
    "transpose(" ~> guardedExpr <~ ")"                    ^^ Transpose.apply                   |
    "pow(" ~> guardedExpr ~ "," ~ guardedExpr <~ ")"      ^^ { case b ~ _ ~ e => Power(b, e) }            |
    "at("  ~> guardedExpr ~ "," ~ guardedExpr ~ "," ~ guardedExpr <~ ")" ^^ { case m ~ _ ~ r ~ _ ~ c => _MatrixIndex(m, r, c) }

  // Direction token for limit(expr, var, point, +/-): consumed after the point comma.
  private lazy val limitDir: Parser[LimitDir] = ("+" | "-") ^^ {
    case "+" => LimitDir.FromRight
    case "-" => LimitDir.FromLeft
  }

  lazy val functional: Parser[_Expression] =
    "limit("  ~> guardedExpr ~ "," ~ variable ~ "," ~ guardedExpr ~ opt("," ~> limitDir) <~ ")" ^^ {
      case e ~ _ ~ v ~ _ ~ pt ~ None      => _Limit(e, v, pt, LimitDir.Both)
      case e ~ _ ~ v ~ _ ~ pt ~ Some(dir) => _Limit(e, v, pt, dir)
    }                                                                                             |
    "laplace(" ~> guardedExpr ~ "," ~ variable ~ "," ~ variable <~ ")" ^^ {
      case e ~ _ ~ t ~ _ ~ s              => _Laplace(e, t, s)
    }                                                                                             |
    "fourier(" ~> guardedExpr ~ "," ~ variable ~ "," ~ variable <~ ")" ^^ {
      case e ~ _ ~ t ~ _ ~ w              => _Fourier(e, t, w)
    }                                                                                             |
    "invlaplace(" ~> guardedExpr ~ "," ~ variable ~ "," ~ variable <~ ")" ^^ {
      case f ~ _ ~ s ~ _ ~ t              => _InverseLaplace(f, s, t)
    }                                                                                             |
    "derive("   ~> guardedExpr ~ "," ~ variable <~ ")"                                           ^^ { case e ~ _ ~ v             => _Derivative(e, v)            } |
    "integral(" ~> guardedExpr ~ "," ~ variable ~ "," ~ signedValue ~ "," ~ signedValue <~ ")"  ^^ { case e ~ _ ~ v ~ _ ~ l ~ _ ~ u => _DefIntegral(e, v, l, u) } |
    "integral(" ~> guardedExpr ~ "," ~ variable <~ ")"                                           ^^ { case e ~ _ ~ v             => _Integral(e, v)              } |
    // guardedExpr here calls equationExpr, so "solve(x = 5, x)" and "solve(h, x)" both work
    "solve("    ~> guardedExpr ~ "," ~ variable <~ ")"                                            ^^ { case e ~ _ ~ v             => _Solve(e, v)                 } |
    // equations is a matrix of _Equation nodes; variables are listed after the first comma
    "solveSystem(" ~> guardedExpr ~ "," ~ rep1sep(variable, ",") <~ ")"                          ^^ { case eqs ~ _ ~ vars         => _SolveSystem(eqs, vars)      }

  // Integral limits accept a sign ("integral(x, x, -1, 1)", lower limit "-pi")
  // without opening the limit position to full sub-expressions.
  lazy val signedValue: Parser[_Expression] = opt("+" | "-") ~ value ^^
    {
      case sign ~ e => applySign(sign, e)
    }

  lazy val value:    Parser[_Expression] = number | constant | variable
  // Unsigned by design — see the sign-handling note in the header. The [eE][+-]?
  // exponent keeps scientific notation ("3E-5", "2e-3") intact.
  lazy val number:   Parser[_Number]    = """(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?""".r ^^ { s => _Number(s.toDouble) }
  // Negative lookahead prevents "pine" from matching as pi + ne, or "exp" as e + xp.
  // "i" is the imaginary unit; "3i" is implicit multiplication (3 * i) → _Complex(0, 3),
  // and "im"/"i1" stay ordinary variables (guarded like "e"/"pi").
  lazy val constant: Parser[_Value]     =
    """pi(?![a-zA-Z0-9])""".r  ^^^ _Number(math.Pi)                |
    """e(?![a-zA-Z0-9])""".r   ^^^ _Number(math.E)                 |
    """i(?![a-zA-Z0-9])""".r   ^^^ _Complex.of(0, 1)               |
    """inf(?![a-zA-Z0-9])""".r ^^^ _Number(Double.PositiveInfinity)
  // Reserved words are rejected wholesale — the regex is greedy, so "simplify"
  // cannot fall back to variable "simplif" times variable "y".
  lazy val variable: Parser[_Variable]  = """[a-zA-Z][a-zA-Z0-9_]*""".r ^? (
    { case s if !ReservedWords.contains(s) => _Variable(s) },
    s => s"'$s' is a reserved word and cannot be used as a variable"
  )

  lazy val topLevel: Parser[_Expression] = equationExpr

  def parse(str: String): ParseResult[_Expression] = parseAll(topLevel, str)

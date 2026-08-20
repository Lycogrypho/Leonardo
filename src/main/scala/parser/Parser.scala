package it.grypho.scala.leonardo
package parser

import scala.util.parsing.combinator.JavaTokenParsers
import core.*
import scalar.*
import matrix.*
import equation.*
import transform.*
import ode.*
import logic.*


/** Recursive-descent parser for Leonardo mathematical expressions.
 *
 *  All grammar productions are `lazy val` so the combinator graph and compiled
 *  regexes are built once on first access and reused for every subsequent
 *  `parse` call.  Only `guardedExpr` and `guardedSignedPower` remain `def`
 *  because they capture per-call depth state via a `ThreadLocal` counter.
 *
 *  Grammar summary:
 *  {{{
 *  topLevel     ::= logicExpr
 *  logicExpr    ::= implExpr                    -- boolean connectives bind loosest
 *  implExpr     ::= orExpr ["implies" implExpr] -- right-associative, loosest connective
 *  orExpr       ::= xorExpr ("or" xorExpr)*
 *  xorExpr      ::= andExpr ("xor" andExpr)*
 *  andExpr      ::= notExpr ("and" notExpr)*
 *  notExpr      ::= "not" notExpr | equationExpr | "(" logicExpr ")"
 *  equationExpr ::= expr [("==" | "=") expr]   -- "==" -> _EqualityCheck; "=" -> _Equation
 *  expr         ::= ["+" | "-"] simpleExpr
 *  simpleExpr   ::= term (("+" | "-") term)*
 *  term         ::= signedPower (("*" | "/") signedPower | "" power)*
 *  signedPower  ::= ["+" | "-"] power
 *  power        ::= factor ["^" signedPower]    -- right-associative
 *  factor       ::= function | functional | matrix | value | "(" equationExpr ")"
 *  matrix       ::= "[" matrixRow ("," matrixRow)* "]"
 *  matrixRow    ::= "[" equationExpr ("," equationExpr)* "]"
 *  }}}
 *
 *  Sign handling: the number token is unsigned so that implicit multiplication
 *  does not swallow `3-2` as `3 * (-2)`.  A sign is allowed where an explicit
 *  operator precedes (start of expression, after `+`, `-`, `*`, `/`, `^`, and
 *  in `integral` limits) but deliberately not in the operand of implicit `*`.
 *
 *  Matrix dispatch: when a syntactically visible matrix operand appears, `+`/`-`/`*`
 *  build `MatSum`/`MatProduct`/`MatScale` rather than `Sum`/`Product`.  `M * y`
 *  with a non-literal `y` builds `MatProduct(M, y)` (not `MatScale(y, M)`) because
 *  `y` may be a variable bound to a matrix at eval time, and swapping would silently
 *  commute the product.  Only a literal `_Number` right operand is known to commute.
 *
 *  Nesting guard: a `ThreadLocal` depth counter is incremented at every parenthesised
 *  sub-expression and at every `^` exponent; the parser returns a failure rather than
 *  blowing the JVM stack when the depth reaches `MaxDepth`.
 */
object Parser extends JavaTokenParsers:

  /** Words that can never be used as a variable name.
   *
   *  Includes every function and functional keyword of the grammar, the built-in
   *  constants (`pi`, `e`, `i`, `inf`), and the REPL command words, so a session
   *  binding can never shadow or be shadowed by a command.  Bare `sin` or `simplify`
   *  is a parse error, not a variable; names merely starting with a reserved word
   *  (`sina`, `evalx`) stay legal.
   */
  val ReservedWords: Set[String] = Set(
    "exp", "log", "ln", "sin", "cos", "tan", "tg", "asin", "acos", "atan",
    "pow", "transpose", "at", "det", "inv", "eye", "zeros", "lu", "qr", "eigen", "eig", "jordan", "step",  // functions
    "derive", "integral", "solve", "solveSystem", "limit", "laplace", "fourier", "invlaplace", "ode", // functionals
    "and", "or", "not", "implies", "xor",                // logic connectives
    "pi", "e", "i", "inf", "true", "false",              // constants (inf = +inf; true/false = _Bool)
    "simplify", "expand", "eval", "env", "vars", "precision",
    "unset", "samples", "colors", "pretty", "truth", "help", "quit", "exit" // REPL commands
  )

  private val MaxDepth = 500
  private val depth = new ThreadLocal[Int]:
    override def initialValue(): Int = 0

  /** Wraps a production with the shared `ThreadLocal` depth guard so recursive
   *  positions fail cleanly instead of blowing the JVM stack.
   *  @param p the production to guard (by-name: productions are `lazy val`s)
   */
  private def depthGuarded(p: => Parser[_Expression]): Parser[_Expression] = Parser { in =>
    val d = depth.get()
    if d >= MaxDepth then Failure(s"expression exceeds maximum nesting depth of $MaxDepth", in)
    else
      depth.set(d + 1)
      try p(in)
      finally depth.set(d)
  }

  /** Guards `equationExpr` against unbounded parenthesis nesting. */
  private def guardedExpr: Parser[_Expression] = depthGuarded(equationExpr)

  /** Guards `signedPower` against unbounded `^` chaining.
   *
   *  The `power -> signedPower -> power` right-recursion bypasses `guardedExpr`
   *  (which is only entered via explicit parentheses and function argument lists),
   *  so `2^-2^-2^-...` would blow the JVM stack without this wrapper.
   */
  private def guardedSignedPower: Parser[_Expression] = depthGuarded(signedPower)

  /** Guards the parenthesised-logic branch of `notExpr` against unbounded nesting. */
  private def guardedLogicExpr: Parser[_Expression] = depthGuarded(logicExpr)

  /** Guards the `not` right-recursion (`not not not ...`) against unbounded chaining. */
  private def guardedNotExpr: Parser[_Expression] = depthGuarded(notExpr)

  /** Guards the `implies` right-recursion against unbounded chaining. */
  private def guardedImplExpr: Parser[_Expression] = depthGuarded(implExpr)

  /** Returns `true` when `e` is syntactically a matrix (literal or matrix operation). */
  private def isMatrixShaped(e: _Expression): Boolean =
    e.isInstanceOf[_Matrix] || e.isInstanceOf[_MatrixOperation]

  /** Builds a sum node, choosing `MatSum` when either operand is matrix-shaped. */
  private def mkSum(x: _Expression, y: _Expression): _Expression =
    if isMatrixShaped(x) || isMatrixShaped(y) then MatSum(x, y) else Sum(x, y)

  /** Builds a product node, dispatching to `MatProduct` / `MatScale` / `Product`.
   *
   *  `M * y` with a non-literal `y` builds `MatProduct(M, y)` rather than
   *  `MatScale(y, M)`: `y` may be a variable bound to a matrix at eval time,
   *  and swapping the operands would silently commute the product.
   */
  private def mkMul(x: _Expression, y: _Expression): _Expression =
    (isMatrixShaped(x), isMatrixShaped(y)) match
      case (true, true)   => MatProduct(x, y)
      case (false, true)  => MatScale(x, y)
      case (true, false)  => y match
        case _Number(_) => MatScale(y, x)
        case _          => MatProduct(x, y)
      case (false, false) => Product(x, y)

  /** Negates `e`, choosing `MatScale(-1, e)` when `e` is matrix-shaped. */
  private def mkNeg(e: _Expression): _Expression =
    if isMatrixShaped(e) then MatScale(_Number(-1), e) else Product(_Number(-1), e)

  /** Applies `sign` to `e`, folding negation into leading numeric coefficients.
   *
   *  A negated literal folds to a negative `_Number` (`"-2"` -> `_Number(-2.0)`),
   *  and the sign folds into an existing leading coefficient (`"-3k"` -> `(-3.0 * k)`),
   *  keeping negated products round-trip stable.
   */
  private def applySign(sign: Option[String], e: _Expression): _Expression = (sign, e) match
    case (Some("-"), _Number(n))                => _Number(-n)
    case (Some("-"), Product(_Number(k), rest)) => Product(_Number(-k), rest)
    case (Some("-"), MatScale(_Number(k), m))   => MatScale(_Number(-k), m)
    case (Some("-"), _)                         => mkNeg(e)
    case _                                      => e

  /** A word-boundary-guarded keyword: matches `word` only when not followed by an
   *  identifier character, so `and` never captures the prefix of a variable `andrew`.
   *  @param word the keyword text
   */
  private def kw(word: String): Parser[String] = (word + """(?![a-zA-Z0-9_])""").r

  /** A full logic expression -- the boolean connectives bind loosest, below `=`/`==`.
   *
   *  Precedence (tightest to loosest): `not` > `and` > `xor` > `or` > `implies`;
   *  `implies` is right-associative, the others left-fold.  So
   *  `a or b and c` is `a or (b and c)` and `x = 1 and y = 2` is `(x = 1) and (y = 2)`.
   */
  lazy val logicExpr: Parser[_Expression] = implExpr

  /** Implication level: right-associative and the loosest connective. */
  lazy val implExpr: Parser[_Expression] = orExpr ~ opt(kw("implies") ~> guardedImplExpr) ^^
    {
      case l ~ Some(r) => Implies(l, r)
      case l ~ None    => l
    }

  /** Disjunction level: left-folding `or`. */
  lazy val orExpr: Parser[_Expression] = xorExpr ~ rep(kw("or") ~> xorExpr) ^^
    {
      case l ~ rs => rs.foldLeft(l)(Or.apply)
    }

  /** Exclusive-disjunction level: left-folding `xor`. */
  lazy val xorExpr: Parser[_Expression] = andExpr ~ rep(kw("xor") ~> andExpr) ^^
    {
      case l ~ rs => rs.foldLeft(l)(Xor.apply)
    }

  /** Conjunction level: left-folding `and`. */
  lazy val andExpr: Parser[_Expression] = notExpr ~ rep(kw("and") ~> notExpr) ^^
    {
      case l ~ rs => rs.foldLeft(l)(And.apply)
    }

  /** Negation level and the descent into the arithmetic grammar.
   *
   *  `equationExpr` is tried before the parenthesised-logic branch so ordinary
   *  arithmetic parentheses (`(x + 1) * 2`) keep parsing through `factor`; only when
   *  the arithmetic parse fails (`(a and b)`) is the group re-read as logic.
   *  `guardedExpr` stays bound to `equationExpr`, so `2 * (x = 1)` remains a parse
   *  error and equations stay top-level only.
   */
  lazy val notExpr: Parser[_Expression] =
    kw("not") ~> guardedNotExpr ^^ Not.apply |
    equationExpr                             |
    "(" ~> guardedLogicExpr <~ ")"

  /** Top-level grammar: an optional equation or equality relation.
   *
   *  `==` is tried before `=` so the two-character operator is not shadowed.
   *  Non-associative: only one optional relation per expression, so `a = b = c`
   *  is a parse error.  `(a = b)` is valid as a sub-expression so a named
   *  equation can be bound: `h := x = 5`, then `solve(h, x)`.
   */
  lazy val equationExpr: Parser[_Expression] = expr ~ opt(("==" | "=") ~ expr) ^^
    {
      case l ~ Some("==" ~ r) => _EqualityCheck(l, r)
      case l ~ Some(_ ~ r)    => _Equation(l, r)
      case l ~ None           => l
    }

  /** An expression with an optional leading sign. */
  lazy val expr: Parser[_Expression] = opt("+" | "-") ~ simpleExpr ^^
    {
      case sign ~ e => applySign(sign, e)
    }

  /** A sequence of additive terms. */
  lazy val simpleExpr: Parser[_Expression] = term ~ rep(("+" | "-") ~ term) ^^
    {
      case left ~ rights => rights.foldLeft(left)
        {
          case (x, "+" ~ y) => mkSum(x, y)
          case (x, "-" ~ y) => mkSum(x, mkNeg(y))
        }
    }

  /** Explicit `*` and `/` take a signed right operand; implicit multiplication takes an unsigned one.
   *
   *  This keeps `3-2` binding as subtraction rather than `3 * (-2)`.
   */
  lazy val term: Parser[_Expression] = signedPower ~ rep(("*" | "/") ~ signedPower | "" ~ power) ^^
    {
      case left ~ rights => rights.foldLeft(left)
        {
          case (x, "*" ~ y) => mkMul(x, y)
          case (x, "/" ~ y) => Ratio(x, y)
          case (x, ""  ~ y) => mkMul(x, y)
        }
    }

  /** A signed operand for positions after an explicit operator (`3 * -x`, `2^-x`). */
  lazy val signedPower: Parser[_Expression] = opt("+" | "-") ~ power ^^
    {
      case sign ~ e => applySign(sign, e)
    }

  /** Right-associative exponentiation: `2 ^ 3 ^ 2` parses as `2 ^ (3 ^ 2)`. */
  lazy val power: Parser[_Expression] = factor ~ opt("^" ~> guardedSignedPower) ^^
    {
      case b ~ Some(e) => Power(b, e)
      case b ~ None    => b
    }

  /** A grammar factor: function, functional, matrix literal, value, or parenthesised expression. */
  lazy val factor: Parser[_Expression] = function | functional | matrixLiteral | value | "(" ~> guardedExpr <~ ")"

  /** A matrix literal: `[[a, b], [c, d]]`.  All rows must have the same length. */
  lazy val matrixLiteral: Parser[_Expression] =
    "[" ~> rep1sep(matrixRow, ",") <~ "]" ^? (
      { case rows if rows.forall(_.size == rows.head.size) =>
          _Matrix(rows.size, rows.head.size, rows.flatten.toVector) },
      _ => "matrix rows must all have the same length"
    )

  /** A single row of a matrix literal. */
  lazy val matrixRow: Parser[List[_Expression]] = "[" ~> rep1sep(guardedExpr, ",") <~ "]"

  /** All mathematical function keywords and their AST mappings. */
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
    "det(" ~> guardedExpr <~ ")"                          ^^ Determinant.apply                  |
    "inv(" ~> guardedExpr <~ ")"                          ^^ Inverse.apply                      |
    "pow(" ~> guardedExpr ~ "," ~ guardedExpr <~ ")"      ^^ { case b ~ _ ~ e => Power(b, e) }            |
    "at("  ~> guardedExpr ~ "," ~ guardedExpr ~ "," ~ guardedExpr <~ ")" ^^ { case m ~ _ ~ r ~ _ ~ c => _MatrixIndex(m, r, c) } |
    "eye("   ~> guardedExpr <~ ")"                                        ^^ IdentityMatrix.apply          |
    "zeros(" ~> guardedExpr ~ opt("," ~> guardedExpr) <~ ")" ^^ {
      case n ~ None    => ZeroMatrix(n, n)
      case r ~ Some(c) => ZeroMatrix(r, c)
    }                                                                                                       |
    "lu("     ~> guardedExpr <~ ")"                                       ^^ _LUDecomposition.apply        |
    "qr("     ~> guardedExpr <~ ")"                                       ^^ _QRDecomposition.apply        |
    "eigen("  ~> guardedExpr <~ ")"                                       ^^ _EigenDecomposition.apply     |
    "eig("    ~> guardedExpr <~ ")"                                       ^^ _EigDecomposition.apply       |
    "jordan(" ~> guardedExpr <~ ")"                                       ^^ _JordanDecomposition.apply    |
    "step("   ~> guardedExpr <~ ")"                                       ^^ _Heaviside.apply

  /** Direction token for `limit(expr, var, point, +/-)`. */
  private lazy val limitDir: Parser[LimitDir] = ("+" | "-") ^^ {
    case "+" => LimitDir.FromRight
    case "-" => LimitDir.FromLeft
  }

  /** All functional keywords (operators over bound variables) and their AST mappings. */
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
    // ode(rhs, depVar, indepVar, t0, y0, target): first-order IVP y' = rhs, y(t0) = y0,
    // evaluated at target.  depVar/indepVar are variables; rhs/t0/y0/target are expressions.
    "ode(" ~> guardedExpr ~ "," ~ variable ~ "," ~ variable ~ "," ~ guardedExpr ~ "," ~ guardedExpr ~ "," ~ guardedExpr <~ ")" ^^ {
      case rhs ~ _ ~ y ~ _ ~ t ~ _ ~ t0 ~ _ ~ y0 ~ _ ~ tgt => _ODE(rhs, y, t, t0, y0, tgt)
    }                                                                                             |
    "derive("   ~> guardedExpr ~ "," ~ variable <~ ")"                                           ^^ { case e ~ _ ~ v             => _Derivative(e, v)            } |
    "integral(" ~> guardedExpr ~ "," ~ variable ~ "," ~ signedValue ~ "," ~ signedValue <~ ")"  ^^ { case e ~ _ ~ v ~ _ ~ l ~ _ ~ u => _DefIntegral(e, v, l, u) } |
    "integral(" ~> guardedExpr ~ "," ~ variable <~ ")"                                           ^^ { case e ~ _ ~ v             => _Integral(e, v)              } |
    // guardedExpr calls equationExpr, so "solve(x = 5, x)" and "solve(h, x)" both work.
    "solve("    ~> guardedExpr ~ "," ~ variable <~ ")"                                            ^^ { case e ~ _ ~ v             => _Solve(e, v)                 } |
    // equations is a matrix of _Equation nodes; variables are listed after the first comma.
    "solveSystem(" ~> guardedExpr ~ "," ~ rep1sep(variable, ",") <~ ")"                          ^^ { case eqs ~ _ ~ vars         => _SolveSystem(eqs, vars)      }

  /** A signed value for integral-limit positions (`integral(x, x, -1, 1)`). */
  lazy val signedValue: Parser[_Expression] = opt("+" | "-") ~ value ^^
    {
      case sign ~ e => applySign(sign, e)
    }

  /** A bare value: number, constant, or variable. */
  lazy val value:    Parser[_Expression] = number | constant | variable

  /** An unsigned floating-point literal.
   *
   *  Unsigned by design — a `-` is always a grammar operator, never part of the token.
   *  Scientific notation exponent signs (`3E-5`) are handled by the regex and are unaffected.
   */
  lazy val number:   Parser[_Number]    = """(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?""".r ^^ { s => _Number(s.toDouble) }

  /** The built-in constants `pi`, `e`, `i`, `inf`, `true`, and `false`, all
   *  word-boundary guarded.
   *
   *  `i` is the imaginary unit (`_Complex(0, 1)`); `3i` is implicit multiplication
   *  yielding `_Complex(0, 3)`, while `im` or `i1` stay ordinary variables.
   *  `true`/`false` are the boolean literals (`_Bool`); the guard keeps `truex` an
   *  ordinary variable.
   */
  lazy val constant: Parser[_Value]     =
    """pi(?![a-zA-Z0-9])""".r  ^^^ _Number(math.Pi)                 |
    """e(?![a-zA-Z0-9])""".r   ^^^ _Number(math.E)                  |
    """i(?![a-zA-Z0-9])""".r   ^^^ _Complex.of(0, 1)                |
    """inf(?![a-zA-Z0-9])""".r ^^^ _Number(Double.PositiveInfinity) |
    """true(?![a-zA-Z0-9_])""".r  ^^^ _Bool(true)                   |
    """false(?![a-zA-Z0-9_])""".r ^^^ _Bool(false)

  /** A user-defined variable name.
   *
   *  Matches `[a-zA-Z][a-zA-Z0-9_]*` and rejects any string in `ReservedWords`.
   *  The regex is greedy so `simplify` cannot fall back to variable `simplif` times `y`.
   */
  lazy val variable: Parser[_Variable]  = """[a-zA-Z][a-zA-Z0-9_]*""".r ^? (
    { case s if !ReservedWords.contains(s) => _Variable(s) },
    s => s"'$s' is a reserved word and cannot be used as a variable"
  )

  /** The top-level production: a full logic expression (connectives bind loosest;
   *  a plain arithmetic or equation expression passes through unchanged).
   */
  lazy val topLevel: Parser[_Expression] = logicExpr

  /** Parses `str` and returns a `ParseResult` containing the AST or an error message.
   *
   *  @param str the input string to parse
   *  @return `Success(expr)` on success, or `Failure`/`Error` with a description
   */
  def parse(str: String): ParseResult[_Expression] = parseAll(topLevel, str)

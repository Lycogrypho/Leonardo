package it.grypho.scala.leonardo
package parser

import scala.util.parsing.combinator.JavaTokenParsers
import core.*
import scalar.*
import matrix.*
import equation.*
import transform.*
import ode.*
import probability.*
import statistics.*
import logic.*
import domain.*
import vector.*


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
    "sinh", "cosh", "tanh", "asinh", "acosh", "atanh",   // hyperbolic (3.9)
    "sec", "csc", "cot", "sech", "csch", "coth",          // reciprocal trig / hyperbolic (3.9)
    "pow", "transpose", "at", "det", "inv", "eye", "zeros", "lu", "qr", "eigen", "eig", "jordan", "step",  // functions
    "derive", "integral", "solve", "solveSystem", "limit", "laplace", "fourier", "invlaplace", "ode", // functionals
    "domain", "differentiable", "singularities",         // domain analysis (3.3)
    "taylor", "maclaurin", "fourierSeries", "pade", "laurent",
    "tobase", "balanced",                                // base conversion (3.5)      // series expansions (4.J)
    "and", "or", "not", "implies", "xor",                // logic connectives
    "truth", "very", "somewhat", "trimf", "trapmf", "gaussmf", "sigmf", "defuzz", // fuzzy tier
    "fact", "dfact", "mfact", "lgamma", "Gamma", "Beta",  // special functions (4.I);
    "normal", "uniform", "exponential", "binomial", "poisson",  // 4.P distributions
    "pdf", "cdf", "prob", "quantile", "expect", "variance",     // 4.P queries + moments
    "studentt", "chisq",                                       // 4.Q distributions
    "mean", "pvariance", "stddev", "pstddev", "covariance", "correlation",  // 4.Q descriptive
    "regress", "ttest", "confint", "chisqtest",                // 4.Q regression + inference
    "erf", "erfc", "digamma", "gammaP", "gammaQ", "betaI",  // 4.O; all lowercase-safe --
                                                         // none is a plausible variable
                                                         // name, unlike gamma/beta
                                                         // Gamma/Beta are capitalised so the
                                                         // lowercase names stay free as variables
    "Si", "Ci", "Ei", "li", "fresnelS", "fresnelC",      // special integral functions (3.15);
                                                         // Si/Ci/Ei capitalised and reserved
                                                         // (plausible variable names, the
                                                         // Gamma/Beta reasoning); li keeps the
                                                         // C spelling; Fresnel spelled out --
                                                         // bare S/C would be homoglyph traps
    "grad", "div", "curl", "laplacian", "jacobian", "hessian",  // vector calculus (6.24)
    "pi", "e", "i", "inf", "true", "false", "unknown",   // constants (inf = +inf; true/false/unknown = truth values)
    "simplify", "expand", "eval", "env", "vars", "precision",
    "unset", "samples", "colors", "pretty", "exact", "truth3", "logic", "help", "quit", "exit" // REPL commands
  )

  private val MaxDepth = 500
  private val depth = new ThreadLocal[Int]:
    override def initialValue(): Int = 0

  /** Working precision in force for the current parse, or `None` for the `Double` path.
   *
   *  A `ThreadLocal` for the same reason `depth` above is one: the grammar is a tree of
   *  `lazy val` productions on a singleton, so per-parse state cannot be a constructor
   *  parameter without rebuilding the whole grammar per call, and it must not be a plain
   *  `var` on a shared object.  Set and cleared by [[parse]].
   *
   *  `Option[Int]` rather than a boolean beside an `Int`: the mode and the precision are
   *  one fact, and "exact off but precision 30" should not be representable.
   */
  private val exactPrecision = new ThreadLocal[Option[Int]]:
    override def initialValue(): Option[Int] = None

  /** Renders an irrational constant for the mode in force.
   *
   *  In exact mode `pi` and `e` need no symbolic-atom redesign: they become *requests for
   *  a rational approximation at the working precision*, which is all the exact tier ever
   *  promises about an irrational.  Outside exact mode they stay the `Double` they were.
   *
   *  @param d the constant's `Double` value
   *  @return a `_Rational` approximation in exact mode, the `_Number` otherwise
   */
  private def irrational(d: Double, exactly: Int => Option[_Rational]): _Value =
    exactPrecision.get() match
      case Some(digits) => exactly(digits).getOrElse(_Number(d))
      case None         => _Number(d)

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

  /** A membership-expression argument: a full logic expression, not just arithmetic.
   *
   *  Membership degrees are truth values, so aggregating curves with the connectives
   *  (`defuzz(cold or warm, t, 0, 40)` -- the usual fuzzy-inference step) has to parse.
   *  Only the positions that genuinely take a membership expression use this; ordinary
   *  function arguments stay on `guardedExpr`, which keeps equations top-level only.
   */
  private def guardedMembership: Parser[_Expression] = guardedLogicExpr

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

  /** The literal `-1` in whichever arithmetic tier is in force.
   *
   *  Subtraction is built as `a + (-1)·b`, so getting this wrong would quietly route
   *  *every* subtraction and negation through the promotion lattice's float contagion:
   *  `3 - 5` would leave the exact tier before it ever entered it.  The matrix form below
   *  stays a `_Number` deliberately — matrix entries are `Double` until slice B.
   */
  private def negOne: _Value = literalInt(-1)

  /** An integer literal synthesised by the grammar, in whichever tier is in force.
   *
   *  Any constant the *parser* invents — the `-1` of a negation, the `2` of `dfact`, the
   *  `10` of bare `log` — has to be built through here.  A hard-coded `_Number` would mix
   *  tiers and quietly pull the surrounding expression out of the exact one.
   *
   *  @param n the integer value
   *  @return a `_Rational` in exact mode, a `_Number` otherwise
   */
  private def literalInt(n: Int): _Value =
    exactPrecision.get() match
      case Some(_) => _Rational(n)
      case None    => _Number(n)

  /** Negates `e`, choosing `MatScale(-1, e)` when `e` is matrix-shaped. */
  private def mkNeg(e: _Expression): _Expression =
    if isMatrixShaped(e) then MatScale(_Number(-1), e) else Product(negOne, e)

  /** Applies `sign` to `e`, folding negation into leading numeric coefficients.
   *
   *  A negated literal folds to a negative `_Number` (`"-2"` -> `_Number(-2.0)`),
   *  and the sign folds into an existing leading coefficient (`"-3k"` -> `(-3.0 * k)`),
   *  keeping negated products round-trip stable.
   */
  private def applySign(sign: Option[String], e: _Expression): _Expression = (sign, e) match
    // Exact literals first: `_Number` is a widening extractor, so without these two a
    // negated exact literal would fold into an inexact one at parse time (issue 4.L).
    case (Some("-"), r: _Rational)                => r.negate
    case (Some("-"), Product(r: _Rational, rest)) => Product(r.negate, rest)
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
   *  Operators are tried longest-first (`>=` before `>`, `==` before `=`) so a
   *  two-character operator is never shadowed by its prefix.
   *  Non-associative: only one optional relation per expression, so `a = b = c` and
   *  `0 < x < 1` are both parse errors.  Chained comparisons are a separate feature.  `(a = b)` is valid as a sub-expression so a named
   *  equation can be bound: `h := x = 5`, then `solve(h, x)`.
   */
  lazy val equationExpr: Parser[_Expression] =
    // ORDER IS LOAD-BEARING: longest operator first, or `a >= b` would match `>` and then
    // fail on the stray `=`.  Same rule that already put `==` ahead of `=`.
    expr ~ opt((">=" | "<=" | "!=" | "==" | ">" | "<" | "=") ~ expr) ^^
    {
      case l ~ Some("==" ~ r) => _EqualityCheck(l, r)
      case l ~ Some(">=" ~ r) => _Comparison(l, CompareOp.Ge, r)
      case l ~ Some("<=" ~ r) => _Comparison(l, CompareOp.Le, r)
      case l ~ Some("!=" ~ r) => _Comparison(l, CompareOp.Ne, r)
      case l ~ Some(">"  ~ r) => _Comparison(l, CompareOp.Gt, r)
      case l ~ Some("<"  ~ r) => _Comparison(l, CompareOp.Lt, r)
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
      case e ~ None    => LogBase(e, literalInt(10))
      case e ~ Some(b) => LogBase(e, b)
    }                                                                                           |
    "sin(" ~> guardedExpr <~ ")"                                              ^^ Sin.apply      |
    "cos(" ~> guardedExpr <~ ")"                          ^^ Cos.apply                         |
    "tan(" ~> guardedExpr <~ ")"                          ^^ Tg.apply                         |
    "tg("  ~> guardedExpr <~ ")"                          ^^ Tg.apply                          |
    "asin(" ~> guardedExpr <~ ")"                         ^^ Asin.apply                        |
    "acos(" ~> guardedExpr <~ ")"                         ^^ Acos.apply                        |
    "atan(" ~> guardedExpr <~ ")"                         ^^ Atan.apply                        |
    "sinh("  ~> guardedExpr <~ ")"                        ^^ Sinh.apply                        |
    "cosh("  ~> guardedExpr <~ ")"                        ^^ Cosh.apply                        |
    "tanh("  ~> guardedExpr <~ ")"                        ^^ Tanh.apply                        |
    "asinh(" ~> guardedExpr <~ ")"                        ^^ Asinh.apply                       |
    "acosh(" ~> guardedExpr <~ ")"                        ^^ Acosh.apply                       |
    "atanh(" ~> guardedExpr <~ ")"                        ^^ Atanh.apply                       |
    "sech("  ~> guardedExpr <~ ")"                        ^^ Sech.apply                        |
    "csch("  ~> guardedExpr <~ ")"                        ^^ Csch.apply                        |
    "coth("  ~> guardedExpr <~ ")"                        ^^ Coth.apply                        |
    "sec("   ~> guardedExpr <~ ")"                        ^^ Sec.apply                         |
    "csc("   ~> guardedExpr <~ ")"                        ^^ Csc.apply                         |
    "cot("   ~> guardedExpr <~ ")"                        ^^ Cot.apply                         |
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
    "step("   ~> guardedExpr <~ ")"                                       ^^ _Heaviside.apply           |
    // Special functions. dfact(n) is sugar for mfact(n, 2), the same relationship
    // log(x) has with LogBase(x, 10).
    "fact("   ~> guardedExpr <~ ")"                                       ^^ Factorial.apply               |
    // The step is built in the tier currently in force: an inexact 2 here would make
    // dfact(n) mix tiers and fall out of the exact path that mfact(n, 2) takes.
    "dfact("  ~> guardedExpr <~ ")"       ^^ { n => MultiFactorial(n, literalInt(2)) }                     |
    "mfact("  ~> guardedExpr ~ "," ~ guardedExpr <~ ")" ^^ { case n ~ _ ~ k => MultiFactorial(n, k) }      |
    "lgamma(" ~> guardedExpr <~ ")"                                       ^^ LogGamma.apply                |
    // Special functions, 4.O tier.
    // Probability, 4.P.  `expect`/`variance` take a one- OR two-argument form, the same
    // shape `log(x)` / `log(x, b)` already established; the two-argument form names the
    // random variable, which is a BINDER and so must be parsed as a variable, not an expr.
    "normal("      ~> guardedExpr ~ "," ~ guardedExpr <~ ")" ^^ { case a ~ _ ~ b => _DistributionOf(DistKind.Normal, List(a, b)) }   |
    "uniform("     ~> guardedExpr ~ "," ~ guardedExpr <~ ")" ^^ { case a ~ _ ~ b => _DistributionOf(DistKind.Uniform, List(a, b)) }  |
    "binomial("    ~> guardedExpr ~ "," ~ guardedExpr <~ ")" ^^ { case a ~ _ ~ b => _DistributionOf(DistKind.Binomial, List(a, b)) } |
    "exponential(" ~> guardedExpr <~ ")"       ^^ { a => _DistributionOf(DistKind.Exponential, List(a)) }                            |
    "poisson("     ~> guardedExpr <~ ")"       ^^ { a => _DistributionOf(DistKind.Poisson, List(a)) }                                |
    // Statistics, 4.Q.  `mean` and `variance` accept a sample OR a distribution.
    "mean("        ~> guardedExpr <~ ")" ^^ { e => _Statistic(StatKind.Mean, e) }          |
    "pvariance("   ~> guardedExpr <~ ")" ^^ { e => _Statistic(StatKind.PVariance, e) }     |
    "stddev("      ~> guardedExpr <~ ")" ^^ { e => _Statistic(StatKind.StdDev, e) }        |
    "pstddev("     ~> guardedExpr <~ ")" ^^ { e => _Statistic(StatKind.PStdDev, e) }       |
    "covariance("  ~> guardedExpr ~ "," ~ guardedExpr <~ ")" ^^ { case a ~ _ ~ b => _PairStatistic(PairStatKind.Covariance, a, b) }  |
    "correlation(" ~> guardedExpr ~ "," ~ guardedExpr <~ ")" ^^ { case a ~ _ ~ b => _PairStatistic(PairStatKind.Correlation, a, b) } |
    "regress("     ~> guardedExpr ~ "," ~ guardedExpr <~ ")" ^^ { case a ~ _ ~ b => _Regress(a, b) }            |
    "ttest("       ~> guardedExpr ~ "," ~ guardedExpr <~ ")" ^^ { case a ~ _ ~ b => _Test(TestKind.TTest, a, b) }     |
    "confint("     ~> guardedExpr ~ "," ~ guardedExpr <~ ")" ^^ { case a ~ _ ~ b => _Test(TestKind.ConfInt, a, b) }   |
    "chisqtest("   ~> guardedExpr ~ "," ~ guardedExpr <~ ")" ^^ { case a ~ _ ~ b => _Test(TestKind.ChiSqTest, a, b) } |
    // The two distribution families 4.Q needed and 4.P had not shipped.
    "studentt("    ~> guardedExpr <~ ")" ^^ { a => _DistributionOf(DistKind.StudentT, List(a)) }   |
    "chisq("       ~> guardedExpr <~ ")" ^^ { a => _DistributionOf(DistKind.ChiSquared, List(a)) } |    "pdf("      ~> guardedExpr ~ "," ~ guardedExpr <~ ")" ^^ { case d ~ _ ~ x => _DistributionQuery(Query.Pdf, d, List(x)) }      |
    "cdf("      ~> guardedExpr ~ "," ~ guardedExpr <~ ")" ^^ { case d ~ _ ~ x => _DistributionQuery(Query.Cdf, d, List(x)) }      |
    "quantile(" ~> guardedExpr ~ "," ~ guardedExpr <~ ")" ^^ { case d ~ _ ~ x => _DistributionQuery(Query.Quantile, d, List(x)) } |
    "prob("     ~> guardedExpr ~ "," ~ guardedExpr ~ "," ~ guardedExpr <~ ")" ^^ {
      case d ~ _ ~ lo ~ _ ~ hi => _DistributionQuery(Query.Prob, d, List(lo, hi)) }                                               |
    // The predicate form (4.R): `prob(X < 2)`, `prob(0 < X and X < 1)`.  Takes a full LOGIC
    // expression so `and` is available, and is tried after the three-argument form so the
    // interval spelling is not shadowed.
    "prob("     ~> guardedLogicExpr <~ ")" ^^ { p => _DistributionQuery(Query.ProbOf, p, Nil) } |
    "expect("   ~> guardedExpr ~ "," ~ variable <~ ")" ^^ { case e ~ _ ~ v => _Expectation(e, Some(v)) }                          |
    "expect("   ~> guardedExpr <~ ")"                  ^^ { e => _Expectation(e, None) }                                          |
    "variance(" ~> guardedExpr ~ "," ~ variable <~ ")" ^^ { case e ~ _ ~ v => _Variance(e, Some(v)) }                             |
    // The ONE-argument form belongs to `statistics`, which dispatches on the argument:
    // a distribution delegates back to `probability.Moments`, a matrix is a sample.  The
    // two-argument form above stays the linearity rule table and is untouched.
    "variance(" ~> guardedExpr <~ ")"        ^^ { e => _Statistic(StatKind.Variance, e) }        |    "erf("     ~> guardedExpr <~ ")"                                     ^^ Erf.apply                     |
    "erfc("    ~> guardedExpr <~ ")"                                     ^^ Erfc.apply                    |
    "digamma(" ~> guardedExpr <~ ")"                                     ^^ Digamma.apply                 |
    "gammaP("  ~> guardedExpr ~ "," ~ guardedExpr <~ ")" ^^ { case a ~ _ ~ x => GammaP(a, x) }            |
    "gammaQ("  ~> guardedExpr ~ "," ~ guardedExpr <~ ")" ^^ { case a ~ _ ~ x => GammaQ(a, x) }            |
    "betaI("   ~> guardedExpr ~ "," ~ guardedExpr ~ "," ~ guardedExpr <~ ")" ^^ {
      case x ~ _ ~ a ~ _ ~ b => BetaI(x, a, b) }                                                          |
    // Special integral functions (3.15). "Si("/"Ci("/"Ei(" are case-sensitive literals, so
    // they cannot shadow "sin(" and friends; "fresnelS(" is listed before "fresnelC(" only
    // for tidiness -- the two do not prefix each other.
    "Si("       ~> guardedExpr <~ ")"                                    ^^ Si.apply                      |
    "Ci("       ~> guardedExpr <~ ")"                                    ^^ Ci.apply                      |
    "Ei("       ~> guardedExpr <~ ")"                                    ^^ Ei.apply                      |
    "li("       ~> guardedExpr <~ ")"                                    ^^ Li.apply                      |
    "fresnelS(" ~> guardedExpr <~ ")"                                    ^^ FresnelS.apply                |
    "fresnelC(" ~> guardedExpr <~ ")"                                    ^^ FresnelC.apply                |
    "Gamma("  ~> guardedExpr <~ ")"                                       ^^ Gamma.apply                   |
    // Greek aliases (4.K). No ReservedWords entry is needed: the variable regex is
    // ASCII-only, so no Greek letter can ever be a variable name and there is nothing to
    // collide with. toString keeps emitting the ASCII spelling, so :save scripts stay
    // portable to terminals that cannot render or type these.
    // Capital Beta is NOT offered: U+0392 is a homoglyph of Latin B, so BETA(2,3) and
    // B(2,3) would be indistinguishable on screen; lowercase beta is visually distinct.
    "Γ("  ~> guardedExpr <~ ")"                                          ^^ Gamma.apply                   |
    "β("   ~> guardedExpr ~ "," ~ guardedExpr <~ ")" ^^ { case x ~ _ ~ y => Beta(x, y) }                |
    "Beta("   ~> guardedExpr ~ "," ~ guardedExpr <~ ")" ^^ { case x ~ _ ~ y => Beta(x, y) }                |
    // Fuzzy tier: truth(x) converts a scalar degree into a truth value (and is the
    // printed form of a graded _Truth); the hedges and curves produce degrees directly.
    "truth("    ~> guardedExpr <~ ")"                                     ^^ _TruthOf.apply                |
    "very("     ~> guardedMembership <~ ")"                               ^^ Very.apply                    |
    "somewhat(" ~> guardedMembership <~ ")"                               ^^ Somewhat.apply                |
    "trimf("  ~> guardedExpr ~ "," ~ guardedExpr ~ "," ~ guardedExpr ~ "," ~ guardedExpr <~ ")" ^^ {
      case x ~ _ ~ a ~ _ ~ b ~ _ ~ c => TriMF(x, a, b, c)
    }                                                                                                      |
    "trapmf(" ~> guardedExpr ~ "," ~ guardedExpr ~ "," ~ guardedExpr ~ "," ~ guardedExpr ~ "," ~ guardedExpr <~ ")" ^^ {
      case x ~ _ ~ a ~ _ ~ b ~ _ ~ c ~ _ ~ d => TrapMF(x, a, b, c, d)
    }                                                                                                      |
    "gaussmf(" ~> guardedExpr ~ "," ~ guardedExpr ~ "," ~ guardedExpr <~ ")" ^^ {
      case x ~ _ ~ m ~ _ ~ s => GaussMF(x, m, s)
    }                                                                                                      |
    "sigmf(" ~> guardedExpr ~ "," ~ guardedExpr ~ "," ~ guardedExpr <~ ")" ^^ {
      case x ~ _ ~ a ~ _ ~ c => SigMF(x, a, c)
    }

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
    // fourierSeries(...) is listed before fourier(...) so the longer keyword wins; the
    // two are different operations -- a series in t versus a transform into w.
    "fourierSeries(" ~> guardedExpr ~ "," ~ variable ~ "," ~ guardedExpr ~ "," ~ guardedExpr <~ ")" ^^ {
      case e ~ _ ~ v ~ _ ~ p ~ _ ~ n      => _FourierSeries(e, v, p, n)
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
    // defuzz(e, v, lo, hi): centre-of-gravity defuzzification over [lo, hi].
    "defuzz(" ~> guardedMembership ~ "," ~ variable ~ "," ~ signedValue ~ "," ~ signedValue <~ ")" ^^ {
      case e ~ _ ~ v ~ _ ~ l ~ _ ~ h => _Defuzzify(e, v, l, h)
    }                                                                                             |
    // taylor(e, v, point, n); maclaurin(e, v, n) is sugar for point = 0, the same
    // relationship log(x) has with LogBase(x, 10).
    "taylor(" ~> guardedExpr ~ "," ~ variable ~ "," ~ guardedExpr ~ "," ~ guardedExpr <~ ")" ^^ {
      case e ~ _ ~ v ~ _ ~ p ~ _ ~ n => _Taylor(e, v, p, n)
    }                                                                                             |
    "maclaurin(" ~> guardedExpr ~ "," ~ variable ~ "," ~ guardedExpr <~ ")" ^^ {
      case e ~ _ ~ v ~ _ ~ n => _Taylor(e, v, literalInt(0), n)
    }                                                                                             |
    // pade(e, v, m, n): the [m/n] rational approximant about zero.
    "pade(" ~> guardedExpr ~ "," ~ variable ~ "," ~ guardedExpr ~ "," ~ guardedExpr <~ ")" ^^ {
      case e ~ _ ~ v ~ _ ~ m ~ _ ~ n => _Pade(e, v, m, n)
    }                                                                                             |
    "derive("   ~> guardedExpr ~ "," ~ variable <~ ")"                                           ^^ { case e ~ _ ~ v             => _Derivative(e, v)            } |
    "integral(" ~> guardedExpr ~ "," ~ variable ~ "," ~ signedValue ~ "," ~ signedValue <~ ")"  ^^ { case e ~ _ ~ v ~ _ ~ l ~ _ ~ u => _DefIntegral(e, v, l, u) } |
    "integral(" ~> guardedExpr ~ "," ~ variable <~ ")"                                           ^^ { case e ~ _ ~ v             => _Integral(e, v)              } |
    // Base conversion (3.5).  `tobase` covers any radix 2..36; `balanced` is ternary with
    // the {-1, 0, 1} digit set that 4.G's symmetric logic already uses.
    "tobase(" ~> guardedExpr ~ "," ~ guardedExpr <~ ")" ^^ { case e ~ _ ~ b => _ToBase(e, b) } |
    "balanced(" ~> guardedExpr <~ ")"                   ^^ { e              => _Balanced(e)  } |    // Laurent series (3.4).  The five-argument form states the pole order; the four-argument
    // form omits it and lets `singularitiesOf` detect it.  Longest first, or the 4-arg rule
    // would match and then choke on the extra comma.
    "laurent(" ~> guardedExpr ~ "," ~ variable ~ "," ~ guardedExpr ~ "," ~ guardedExpr ~ "," ~ guardedExpr <~ ")" ^^ { case e ~ _ ~ v ~ _ ~ a ~ _ ~ m ~ _ ~ n => _Laurent(e, v, a, Some(m), n) } |
    "laurent(" ~> guardedExpr ~ "," ~ variable ~ "," ~ guardedExpr ~ "," ~ guardedExpr <~ ")"                     ^^ { case e ~ _ ~ v ~ _ ~ a ~ _ ~ n         => _Laurent(e, v, a, None, n)    } |    // Domain analysis (3.3).  The optional third argument selects the number system; it is
    // matched as a bare literal only in this position, so `real` and `complex` stay usable
    // as ordinary variable names everywhere else and need no `ReservedWords` entry.
    "domain(" ~> guardedExpr ~ "," ~ variable ~ opt("," ~> ("real" | "complex")) <~ ")"        ^^ { case e ~ _ ~ v ~ k          => _Domain(e, v, domainKind(k))          } |
    "differentiable(" ~> guardedExpr ~ "," ~ variable ~ opt("," ~> ("real" | "complex")) <~ ")" ^^ { case e ~ _ ~ v ~ k         => _Differentiable(e, v, domainKind(k))  } |
    "singularities(" ~> guardedExpr ~ "," ~ variable <~ ")"                                     ^^ { case e ~ _ ~ v             => _Singularities(e, v)                  } |
    // guardedLogicExpr, not guardedExpr: `logicExpr` sits ABOVE `equationExpr`, so this
    // accepts everything the narrower rule did -- "solve(x = 5, x)", "solve(h, x)",
    // "solve(x^2 - 4 > 0, x)" -- and additionally the connectives, which issue 3.2 needs for
    // "solve(2x > 2 and x < 5, x)".  With guardedExpr the argument stopped below `and`/`or`
    // and those spelled out as "'and' is a reserved word and cannot be used as a variable".
    "solve("    ~> guardedLogicExpr ~ "," ~ variable <~ ")"                                       ^^ { case e ~ _ ~ v             => _Solve(e, v)                 } |
    // equations is a matrix of _Equation nodes; variables are listed after the first comma.
    "solveSystem(" ~> guardedExpr ~ "," ~ rep1sep(variable, ",") <~ ")"                          ^^ { case eqs ~ _ ~ vars         => _SolveSystem(eqs, vars)      } |
    // Vector calculus (6.24): field/scalar, then the ORDERED coordinate tuple -- the same
    // shape as solveSystem, because the coordinate list is exactly as position-significant
    // as a solve-variable list and must never be inferred from freeVars.
    "grad("      ~> guardedExpr ~ "," ~ rep1sep(variable, ",") <~ ")" ^^ { case f ~ _ ~ vs => _Grad(f, vs.toVector)      } |
    "div("       ~> guardedExpr ~ "," ~ rep1sep(variable, ",") <~ ")" ^^ { case f ~ _ ~ vs => _Div(f, vs.toVector)       } |
    "curl("      ~> guardedExpr ~ "," ~ rep1sep(variable, ",") <~ ")" ^^ { case f ~ _ ~ vs => _Curl(f, vs.toVector)      } |
    "laplacian(" ~> guardedExpr ~ "," ~ rep1sep(variable, ",") <~ ")" ^^ { case f ~ _ ~ vs => _Laplacian(f, vs.toVector) } |
    "jacobian("  ~> guardedExpr ~ "," ~ rep1sep(variable, ",") <~ ")" ^^ { case f ~ _ ~ vs => _Jacobian(f, vs.toVector)  } |
    "hessian("   ~> guardedExpr ~ "," ~ rep1sep(variable, ",") <~ ")" ^^ { case f ~ _ ~ vs => _Hessian(f, vs.toVector)   }

  /** The domain-analysis number system, defaulting to the reals when unstated. */
  private def domainKind(k: Option[String]): DomainKind =
    if k.contains("complex") then DomainKind.Complex else DomainKind.Real

  /** A signed value for integral-limit positions (`integral(x, x, -1, 1)`). */
  lazy val signedValue: Parser[_Expression] = opt("+" | "-") ~ value ^^
    {
      case sign ~ e => applySign(sign, e)
    }

  /** A bare value: number, constant, or variable. */
  lazy val value:    Parser[_Expression] = basedLiteral | number | constant | variable

  /** A based integer literal: `0b1011`, `0o17`, `0xff`, and `0t1T0` for balanced ternary.
   *
   *  **Listed before `number` in `value`**, and it has to be: the ordinary numeric regex
   *  happily matches the leading `0` of `0xff` and leaves `xff` behind to fail as a stray
   *  variable, so a later alternative never gets the chance.
   *
   *  The digits are word-boundary guarded so `0b1011x` is a parse error rather than a
   *  literal followed by a variable.
   */
  lazy val basedLiteral: Parser[_Value] =
    """0[bB][01]+(?![a-zA-Z0-9])""".r        ^^ { s => based(s.drop(2), 2)  } |
    """0[oO][0-7]+(?![a-zA-Z0-9])""".r       ^^ { s => based(s.drop(2), 8)  } |
    """0[xX][0-9a-fA-F]+(?![a-zA-Z0-9])""".r ^^ { s => based(s.drop(2), 16) } |
    """0[tT][01Tt]+(?![a-zA-Z0-9])""".r      ^^ { s =>
      _Based.parseBalanced(s.drop(2)).fold[_Value](_Number(Double.NaN))(n =>
        _Based.of(n.toDouble, 3, balanced = true))
    }

  /** Builds a based literal, or NaN when the digits do not parse (unreachable: the regex
   *  already restricts the digit set for each prefix).
   */
  private def based(digits: String, base: Int): _Value =
    _Based.parseDigits(digits, base).fold[_Value](_Number(Double.NaN))(n =>
      _Based.of(n.toDouble, base))
  /** An unsigned floating-point literal.
   *
   *  Unsigned by design — a `-` is always a grammar operator, never part of the token.
   *  Scientific notation exponent signs (`3E-5`) are handled by the regex and are unaffected.
   */
  lazy val number:   Parser[_Value]     = """(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?""".r ^^ { s =>
    // Exact mode reads the literal AS WRITTEN, which is the whole reason the mode has to
    // exist at parse time: "0.1".toDouble is already the nearest dyadic, and no later stage
    // can recover the tenth that was meant.  _Rational.fromDecimalString keeps it a tenth.
    exactPrecision.get() match
      case Some(_) => _Rational.fromDecimalString(s).getOrElse(_Number(s.toDouble))
      case None    => _Number(s.toDouble)
  }

  /** The built-in constants `pi`, `e`, `i`, `inf`, `true`, `false`, and `unknown`, all
   *  word-boundary guarded.
   *
   *  `i` is the imaginary unit (`_Complex(0, 1)`); `3i` is implicit multiplication
   *  yielding `_Complex(0, 3)`, while `im` or `i1` stay ordinary variables.
   *  `true`/`false` are the boolean literals (`_Bool`) and `unknown` is the third Kleene
   *  truth value (`_Truth.Unknown`); the guard keeps `truex` an ordinary variable.
   */
  lazy val constant: Parser[_Value]     =
    """pi(?![a-zA-Z0-9])""".r  ^^ { _ => irrational(math.Pi, piAt) } |
    """e(?![a-zA-Z0-9])""".r   ^^ { _ => irrational(math.E, eAt)   } |
    """i(?![a-zA-Z0-9])""".r   ^^^ _Complex.of(0, 1)                |
    """inf(?![a-zA-Z0-9])""".r ^^^ _Number(Double.PositiveInfinity) |
    """true(?![a-zA-Z0-9_])""".r    ^^^ _Bool(true)                 |
    """false(?![a-zA-Z0-9_])""".r   ^^^ _Bool(false)                |
    """unknown(?![a-zA-Z0-9_])""".r ^^^ _Truth.Unknown

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
   *  @param str   the input string to parse
   *  @param exact `Some(digits)` builds exact literals (`core._Rational`) and approximates
   *               the irrational constants to that working precision; `None`, the default,
   *               is the unchanged `Double` path.  Passed explicitly rather than read from
   *               an `Environment`, mirroring `logic.simplifyLogic`'s `symmetric` and
   *               `semantics` parameters — the parser predates any environment and has no
   *               business depending on one.
   *  @return `Success(expr)` on success, or `Failure`/`Error` with a description
   */
  def parse(str: String, exact: Option[Int] = None): ParseResult[_Expression] =
    val previous = exactPrecision.get()
    exactPrecision.set(exact)
    try parseAll(topLevel, str)
    finally exactPrecision.set(previous)

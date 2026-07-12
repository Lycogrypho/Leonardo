package it.grypho.scala.leonardo
package cli

import core.*
import scalar.*
import matrix.*
import parser.Parser


/**
 * Interactive command-line session over the Leonardo library.
 *
 * Session state lives outside the library's Environment because the two kinds of
 * assignment differ: a constant right-hand side becomes a numeric binding (a _Value
 * the Environment can hold), while a right-hand side with free variables becomes a
 * named definition (a symbolic _Expression, which the Environment deliberately
 * cannot hold). Definitions are late-bound: they are substituted into input at use
 * time, so redefining f also changes any g defined in terms of f.
 *
 * Assignment uses ":=" (CAS convention): bare "=" always parses as an _Equation,
 * so "x = 2*x + 1" is a relation to evaluate, never a binding. Session scripts
 * (:save) emit ":=" and old "="-style scripts are NOT accepted — re-create them.
 *
 * Commands:
 *   x := 3.001           bind a value (constant right-hand side)
 *   f := sin(x) + x      define a function (right-hand side with free variables)
 *   h := lhs = rhs       bind a named equation (can be passed to solve(h, x))
 *   lhs = rhs            equation: true/false when concrete; solvable via solve()
 *   lhs == rhs           equality check: evaluates to bool but not solvable
 *   <expression>         evaluate, e.g.  f + 1  or  derive(f, x)
 *   simplify <expr>      structural simplification, no numeric evaluation
 *   expand <expr>        distribute products over sums
 *   precision <n>        set decimal precision
 *   env                  list precision, bindings, and definitions
 *   unset <name>         remove a binding or definition
 *   :load <file>         run a session script (file IO handled by the read loop)
 *   :save <file>         write current state to a replayable script (read loop)
 *   help                 this summary
 *   quit | exit          leave (handled by the read loop)
 */
final class Session:
  private var precision: Int = Environment.DefaultPrecision
  private var bindings: Map[String, _Value] = Map()
  private var definitions: Map[String, _Expression] = Map()

  // Rebuilt per command from the immutable Environment constructor.
  private def env: Environment = new Environment(precision, bindings)

  private val emptyEnv = new Environment()

  private val assignment   = """([a-zA-Z][a-zA-Z0-9]*)\s*:=(.+)""".r
  // Lazy first group lets the regex engine find the shortest expression that still
  // leaves valid tokens for <var> <lo> <hi> and an optional <n> at the tail.
  private val samplesRegex = """^(.+?)\s+([a-zA-Z]\w*)\s+(-?[\d.]+(?:[eE][+-]?\d+)?)\s+(-?[\d.]+(?:[eE][+-]?\d+)?)(?:\s+(\d+))?$""".r

  def execute(line: String): String = line.trim match
    case ""                     => ""
    case "help" | "?"           => Session.help
    case s"help $rest" => Session.helpTopic(rest.trim)
    case s"? $rest"    => Session.helpTopic(rest.trim)
    case "env" | "vars"         => state
    // Reserved-name assignments are rejected BEFORE the command patterns: an input
    // like "simplify := 3" would otherwise be captured by the "simplify <expr>"
    // command and fail with a confusing parse error. Constants get their own
    // message ("e" always parses to _Number(math.E), so a binding would be
    // silently unreachable); the rest of the reserved vocabulary (function names,
    // functionals, command words) can never be parsed as a variable either.
    case assignment(name, _) if Session.ReservedConstants.contains(name) =>
      s"cannot assign to '$name': it is a built-in constant"
    case assignment(name, _) if Parser.ReservedWords.contains(name) =>
      s"cannot assign to '$name': it is a reserved word"
    case s"precision $n"        => setPrecision(n.trim)
    case s"unset $name"         => unset(name.trim)
    case s"simplify $rest"      => withParsed(rest)(e => simplify(resolveMatrixOps(substitute(e, definitions))).toString)
    case s"expand $rest"        => withParsed(rest)(e => expand(resolveMatrixOps(substitute(e, definitions))).toString)
    case s"eval $rest"          => withParsed(rest)(evaluate)
    case s"samples $rest"       => doSamples(rest)
    case s":$_"                 => ":load and :save are only available at the interactive prompt"
    case assignment(name, rhs)  => withParsed(rhs)(assign(name, _))
    case expression             => withParsed(expression)(evaluate)

  /**
   * Current session state serialized as a replayable script — one command per line,
   * precision first, then bindings and definitions in name order. Feeding this back
   * through `load` (or line by line through `execute`) reconstructs the session.
   * Pure: this is what the REPL writes to a `:save` file.
   */
  def script: String =
    val lines =
      List(s"precision $precision") ++
      bindings.toList.sortBy(_._1).map((k, v) => s"$k := $v") ++
      definitions.toList.sortBy(_._1).map((k, e) => s"$k := $e")
    lines.mkString("\n")

  /**
   * Execute a whole script body (e.g. the contents of a `:load` file), returning the
   * newline-joined non-empty outputs of its commands. Blank lines and `#` comments are
   * skipped. IO-free — the caller supplies the text, so this is unit-testable; the REPL
   * loop is the only place that actually reads the file.
   */
  def load(text: String): String =
    text.linesIterator
      .map(_.trim)
      .filter(l => l.nonEmpty && !l.startsWith("#"))
      .map(execute)
      .filter(_.nonEmpty)
      .mkString("\n")

  private def withParsed(input: String)(f: _Expression => String): String =
    val result =
      try Parser.parse(input)
      catch case e: Exception => return s"parse error: ${e.getMessage}"
    if result.successful then f(result.get)
    else
      val base = s"parse error: ${result.toString.linesIterator.next()}"
      // "g(x)"-style call syntax on a defined name is a common spelling of issue
      // 1.1's derive(g(x), f(x)); the grammar has bare variables only, so hint.
      definitions.keys.find(n => input.matches(s".*\\b$n\\s*\\(.*")) match
        case Some(n) => s"$base\nnote: function-call syntax '$n(...)' is not supported; use the bare name '$n'"
        case None    => base

  private def evaluate(e: _Expression): String =
    resolveDerivativeBinders(e) match
      case Left(message) => message
      case Right(resolved) =>
        substitute(resolved, definitions).eval(env).toExpression match
          case n: _Number  => n.display(precision)
          case c: _Complex => c.display(precision)
          case other       => other.toString

  /**
   * Carry out matrix algebra before simplify/expand: scalar Sum/Product/Ratio nodes
   * whose operands are matrix-shaped — a matrix literal, a matrix operation node, or
   * a variable bound to a matrix value — are re-typed to the matrix operations and
   * reduced, so "C = A * B" then "simplify C" executes the multiplication and hands
   * simplify a _Matrix whose elements it simplifies one by one (_ElementWise).
   * Purely scalar sub-expressions are untouched: "simplify x + 0" keeps ignoring
   * numeric bindings. Matrix operands, by contrast, must be resolved through the
   * session bindings — executing A * B is impossible without knowing A and B.
   */
  private def resolveMatrixOps(e: _Expression): _Expression =
    def isMatrixish(x: _Expression): Boolean = x match
      case _: _Matrix | _: _MatrixOperation | _: _MatrixValue => true
      case v: _Variable => env.get(v.variable).exists(_.isInstanceOf[_MatrixValue])
      case _            => false

    val rec = e.rebuild(e.children.map(resolveMatrixOps))
    rec match
      case Sum(a, b) if isMatrixish(a) || isMatrixish(b)     => MatSum(a, b).eval(env).toExpression
      case Product(a, b) if isMatrixish(a) && isMatrixish(b) => MatProduct(a, b).eval(env).toExpression
      case Product(a, b) if isMatrixish(b)                   => MatScale(a, b).eval(env).toExpression
      case Product(a, b) if isMatrixish(a)                   => MatScale(b, a).eval(env).toExpression
      case Ratio(a, b) if isMatrixish(a) && !isMatrixish(b)  => MatScale(Ratio(_Number(1), b), a).eval(env).toExpression
      case m: _MatrixOperation                               => m.eval(env).toExpression
      case other                                             => other

  /**
   * d/df where f names a *definition*: the binder is (correctly) never substituted,
   * so left alone the derivative would be taken with respect to a variable that no
   * longer occurs in the substituted body — a silent 0. Rewrite via the chain rule
   * instead: dg/df = (dg/dx) / (df/dx) over the definition's single free variable x.
   * Definitions of zero or several free variables have no unambiguous chain-rule
   * variable; those are rejected with a message (Left).
   */
  private def resolveDerivativeBinders(e: _Expression): Either[String, _Expression] = e match
    case _Derivative(body, v) if definitions.contains(v.variable) =>
      resolveDerivativeBinders(body).flatMap { b =>
        val fbody = substitute(definitions(v.variable), definitions)
        fbody.freeVars.toList.sorted match
          case x :: Nil => Right(Ratio(_Derivative(b, _Variable(x)), _Derivative(fbody, _Variable(x))))
          case Nil      => Left(s"cannot derive with respect to '${v.variable}': its definition has no free variables")
          case vars     => Left(s"cannot derive with respect to '${v.variable}': its definition has several free variables ${vars.mkString("(", ", ", ")")}")
      }
    // Change-of-variable: ∫ g df = ∫ g · (df/dx) dx over f's single free variable x.
    // The derivative df/dx is evaluated eagerly so constant slopes fold immediately
    // (e.g. f := 2*x → df/dx = 2, and simplify(g * 2) folds away the trivial product).
    // _DefIntegral with a definition binder is rejected because its bounds are stated
    // in terms of f and would need to be back-solved via the definition, which is out of scope.
    case _Integral(body, v) if definitions.contains(v.variable) =>
      resolveDerivativeBinders(body).flatMap { b =>
        val fbody = substitute(definitions(v.variable), definitions)
        fbody.freeVars.toList.sorted match
          case x :: Nil =>
            val deriv = _Derivative(fbody, _Variable(x)).eval(emptyEnv).toExpression
            Right(_Integral(simplify(Product(b, deriv)), _Variable(x)))
          case Nil  => Left(s"cannot integrate with respect to '${v.variable}': its definition has no free variables")
          case vars => Left(s"cannot integrate with respect to '${v.variable}': its definition has several free variables ${vars.mkString("(", ", ", ")")}")
      }
    case _DefIntegral(_, v, _, _) if definitions.contains(v.variable) =>
      Left(s"cannot compute a definite integral with respect to '${v.variable}': it is a definition; use the underlying variable directly")
    case other =>
      val results = other.children.map(resolveDerivativeBinders)
      results.collectFirst { case Left(message) => message } match
        case Some(message) => Left(message)
        case None          => Right(other.rebuild(results.map(_.toOption.get)))

  /**
   * The numeric-vs-definition decision must see the same expression bare evaluation
   * would: resolve derivative binders and substitute definitions BEFORE folding, so
   * "q := derive(p, x)" (p a definition) differentiates the substituted body instead
   * of treating p as an unknown constant and collapsing to 0. The RAW rhs is still
   * what gets stored for a definition, keeping it late-bound (redefining p updates q).
   */
  private def assign(name: String, rhs: _Expression): String =
    resolveDerivativeBinders(rhs) match
      case Left(message) => message
      case Right(resolved) =>
        substitute(resolved, definitions).eval(emptyEnv) match
          case Right(value) =>
            bindings = bindings + (name -> value)
            definitions = definitions - name
            s"$name := $value"
          case Left(_) =>
            definitions = definitions + (name -> rhs)
            bindings = bindings - name
            s"$name := $rhs"

  private def doSamples(rest: String): String = rest.trim match
    case samplesRegex(exprStr, varStr, loStr, hiStr, nStr) =>
      val lo = loStr.toDouble
      val hi = hiStr.toDouble
      if lo >= hi then "samples: lo must be strictly less than hi"
      else
        val n = Option(nStr).flatMap(_.toIntOption).getOrElse(200)
        withParsed(exprStr.trim) { e =>
          val v      = _Variable(varStr)
          val points = sample(substitute(e, definitions), v, lo, hi, n, env)
          if points.isEmpty then "(no finite values in range)"
          else
            points.map((x, y) =>
              s"${_Number(x).display(precision)}\t${_Number(y).display(precision)}"
            ).mkString("\n")
        }
    case _ => "usage: samples <expr> <var> <lo> <hi> [<n>]"

  private def setPrecision(text: String): String =
    text.toIntOption match
      case Some(n) if n >= 0 => precision = n; s"precision = $n"
      case _                 => s"precision expects a non-negative integer, got: $text"

  private def unset(name: String): String =
    if bindings.contains(name) || definitions.contains(name) then
      bindings = bindings - name
      definitions = definitions - name
      s"$name unset"
    else s"$name is not set"

  private def state: String =
    val lines =
      List(s"precision = $precision") ++
      bindings.toList.sortBy(_._1).map((k, v) => s"$k := $v") ++
      definitions.toList.sortBy(_._1).map((k, e) => s"$k := $e")
    lines.mkString("\n")

object Session:
  // Names the parser always resolves as constants; assignment to them is rejected.
  val ReservedConstants: Set[String] = Set("pi", "e")

  /** Per-command help text, keyed by the command token (":=", "simplify", …).
   *  Returned by `help <topic>`; bare `help` still shows the full `help` listing. */
  val helpTopics: Map[String, String] = Map(
    ":=" ->
      """|Bind a name or define a function.
         |Constant RHS → numeric value; RHS with free variables → late-bound definition.
         |  x := 3.001          bind a numeric value
         |  f := sin(x) + x     define a function (late-bound: redefining f updates g := f^2)
         |  h := x^2 = 4        bind a named equation (pass to solve(h, x))""".stripMargin,
    "=" ->
      """|Equation relation: true/false when both sides are concrete, symbolic otherwise.
         |Solvable via solve().  Use ":=" for assignment — "=" is never a binding.
         |  10*x = 2*x + 1      evaluates to false when x = 3
         |  solve(10*x = 2*x + 1, x)   → x = 0.125""".stripMargin,
    "==" ->
      """|Equality check: same semantics as "=" but not accepted by solve().
         |Useful when you want a boolean result without accidentally creating a solvable equation.
         |  i == 0 - i*i*i      evaluates true""".stripMargin,
    "simplify" ->
      """|Structural simplification: remove identities, fold constants, cancel inverses.
         |Matrix algebra is carried out first, then each element simplified.
         |Numeric bindings are NOT applied (use bare evaluation for that).
         |  simplify x + 0      → x
         |  simplify C          (C := A * B) executes the multiplication, simplifies each cell""".stripMargin,
    "expand" ->
      """|Distribute products over sums; expand integer powers via the binomial theorem.
         |Matrix algebra is carried out first.
         |  expand x * (y + z)  → ((x * y) + (x * z))
         |  expand (x + 1)^2    → (((x ^ 2.0) + (2.0 * x)) + 1.0)""".stripMargin,
    "eval" ->
      """|Evaluate an expression substituting current bindings and returning a numeric result.
         |  eval sin(pi/2)      → 1.0""".stripMargin,
    "precision" ->
      """|Set the decimal precision for display and numeric comparisons.
         |  precision 8         8 significant decimal digits
         |  precision 5         restore default""".stripMargin,
    "env" ->
      """|List current precision, numeric bindings, and symbolic definitions.
         |  env""".stripMargin,
    "unset" ->
      """|Remove a binding or definition by name.
         |  unset x""".stripMargin,
    ":load" ->
      """|Replay a session script from a file (interactive prompt only; not available via execute).
         |  :load session.txt""".stripMargin,
    ":save" ->
      """|Write the current session state to a replayable script (interactive prompt only).
         |  :save session.txt""".stripMargin,
    "help" ->
      """|Print the command summary, or topic-specific detail for one command.
         |  help               full listing
         |  help simplify      details for the simplify command
         |  ? :=               same as "help :=" """.stripMargin,
    "quit" ->
      """|Exit the REPL (interactive prompt only; "exit" is accepted too).
         |  quit""".stripMargin,
    "solve" ->
      """|Solve an equation for a variable.
         |Linear and quadratic forms are solved exactly; other forms use numeric bisection.
         |  solve(10*x = 2*x + 1, x)   → x = 0.125
         |  solve(x^2 = 4, x)          → [[x = -2.0, x = 2.0]]
         |  solve(h, x)                h is a named equation""".stripMargin,
    "derive" ->
      """|Differentiate an expression with respect to a variable or a defined function.
         |Chain rule applies when the binder is a definition.
         |  derive(sin(x), x)           → cos(x)
         |  derive(g, f)                chain rule when f := sin(x)""".stripMargin,
    "integral" ->
      """|Indefinite symbolic integration via a rule table.
         |  integral(x^2, x)            → ((1.0 / 3.0) * (x ^ 3.0))""".stripMargin,
    "samples" ->
      """|Sample a function over a uniform grid, returning tab-separated (x, f(x)) pairs.
         |Non-finite values (div-by-zero, domain errors) are silently skipped.
         |n defaults to 200; x values are in ascending order.
         |  samples sin(x) x -10 10
         |  samples f x 0 5 100    (f must be a defined function)""".stripMargin,
    "solveSystem" ->
      """|Solve a square system of n linear equations in n unknowns.
         |Gaussian elimination (dense) or symbolic row-reduction (symbolic coefficients).
         |  solveSystem([[2*x + y = 3, x - y = 0]], x, y)   → [[x = 1.0, y = 1.0]]
         |  Named equation matrices work: solveSystem(S, x, y)""".stripMargin,
  )

  def helpTopic(topic: String): String =
    if topic.isEmpty then help else helpTopics.getOrElse(topic, help)

  val help: String =
    """x := 3.001           bind a value (constant right-hand side)
      |f := sin(x) + x      define a function (right-hand side with free variables)
      |h := lhs = rhs       bind a named equation (use with solve(h, x))
      |lhs = rhs            equation: true/false once both sides are concrete; stays
      |                     symbolic with free variables; solvable via solve()
      |lhs == rhs           equality check: same as "=" but not accepted by solve()
      |<expression>         evaluate, e.g.  f + 1  or  derive(f, x)
      |simplify <expr>      structural simplification (matrix algebra is carried out,
      |                     then each element is simplified; scalars ignore bindings)
      |expand <expr>        distribute products over sums (matrix algebra as above)
      |precision <n>        set decimal precision
      |env                  list precision, bindings, and definitions
      |unset <name>         remove a binding or definition
      |:load <file>         run a session script (bindings/definitions/commands)
      |:save <file>         write the current session state to a replayable script
      |help                 this summary
      |quit | exit          leave""".stripMargin

  /** Read a :load file and replay it through the session. IO lives here, not in Session. */
  def loadFile(session: Session, path: String): String =
    scala.util.Using(scala.io.Source.fromFile(path))(_.mkString) match
      case scala.util.Success(text) => session.load(text)
      case scala.util.Failure(e)    => s"could not read $path: ${e.getMessage}"

  /** Write the session's replayable script to a :save file. IO lives here, not in Session. */
  def saveFile(session: Session, path: String): String =
    scala.util.Try:
      val w = new java.io.PrintWriter(path)
      try w.write(session.script) finally w.close()
    match
      case scala.util.Success(_) => s"saved to $path"
      case scala.util.Failure(e) => s"could not write $path: ${e.getMessage}"


@main def repl(): Unit =
  println("Leonardo CAS — type 'help' for commands, 'quit' to leave")
  val session = Session()
  var running = true
  while running do
    print("leonardo> ")
    Option(scala.io.StdIn.readLine()) match
      case None | Some("quit") | Some("exit") => running = false
      case Some(s":load $path")               => println(Session.loadFile(session, path.trim))
      case Some(s":save $path")               => println(Session.saveFile(session, path.trim))
      case Some(line)                         =>
        val out = session.execute(line)
        if out.nonEmpty then println(out)

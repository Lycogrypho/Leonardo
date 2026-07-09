package it.grypho.scala.leonardo
package cli

import core.*
import scalar.*
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
 * Commands:
 *   x = 3.001            bind a value (constant right-hand side)
 *   f = sin(x) + x       define a function (right-hand side with free variables)
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

  private val assignment = """([a-zA-Z][a-zA-Z0-9]*)\s*=(.+)""".r

  def execute(line: String): String = line.trim match
    case ""                     => ""
    case "help" | "?"           => Session.help
    case "env" | "vars"         => state
    case s"precision $n"        => setPrecision(n.trim)
    case s"unset $name"         => unset(name.trim)
    case s"simplify $rest"      => withParsed(rest)(e => simplify(substitute(e, definitions)).toString)
    case s"expand $rest"        => withParsed(rest)(e => expand(substitute(e, definitions)).toString)
    case s"eval $rest"          => withParsed(rest)(evaluate)
    case s":$_"                 => ":load and :save are only available at the interactive prompt"
    // Reserved constants are resolved at parse time, so a binding under their name
    // would be silently unreachable ("e" always parses to _Number(math.E)). Reject
    // before parsing the right-hand side; scripts replayed through load surface the
    // same message.
    case assignment(name, _) if Session.ReservedConstants.contains(name) =>
      s"cannot assign to '$name': it is a built-in constant"
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
      bindings.toList.sortBy(_._1).map((k, v) => s"$k = $v") ++
      definitions.toList.sortBy(_._1).map((k, e) => s"$k = $e")
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
    val result = Parser.parse(input)
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
          case n: _Number => n.display(precision)
          case other      => other.toString

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
    case other =>
      val results = other.children.map(resolveDerivativeBinders)
      results.collectFirst { case Left(message) => message } match
        case Some(message) => Left(message)
        case None          => Right(other.rebuild(results.map(_.toOption.get)))

  private def assign(name: String, rhs: _Expression): String =
    rhs.eval(emptyEnv) match
      case Right(value) =>
        bindings = bindings + (name -> value)
        definitions = definitions - name
        s"$name = $value"
      case Left(_) =>
        definitions = definitions + (name -> rhs)
        bindings = bindings - name
        s"$name = $rhs"

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
      bindings.toList.sortBy(_._1).map((k, v) => s"$k = $v") ++
      definitions.toList.sortBy(_._1).map((k, e) => s"$k = $e")
    lines.mkString("\n")

object Session:
  // Names the parser always resolves as constants; assignment to them is rejected.
  val ReservedConstants: Set[String] = Set("pi", "e")

  val help: String =
    """x = 3.001            bind a value (constant right-hand side)
      |f = sin(x) + x       define a function (right-hand side with free variables)
      |<expression>         evaluate, e.g.  f + 1  or  derive(f, x)
      |simplify <expr>      structural simplification, no numeric evaluation
      |expand <expr>        distribute products over sums
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

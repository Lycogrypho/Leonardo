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
    else s"parse error: ${result.toString.linesIterator.next()}"

  private def evaluate(e: _Expression): String =
    substitute(e, definitions).eval(env).toExpression match
      case n: _Number => n.display(precision)
      case other      => other.toString

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

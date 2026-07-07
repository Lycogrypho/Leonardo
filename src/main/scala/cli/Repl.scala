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
 *   help                 this summary
 *   quit | exit          leave (handled by the read loop)
 */
final class Session:
  private var precision: Int = Environment.DefaultPrecision
  private var bindings: Map[String, _Value] = Map()
  private var definitions: Map[String, _Expression] = Map()

  // Rebuilt per command: Environment cannot enumerate its bindings or change
  // precision after construction, and the maps here are tiny.
  private def env: Environment =
    val e = new Environment(precision)
    bindings.foreach((k, v) => e.assign(k, v))
    e

  private val emptyEnv = new Environment()

  private val assignment = """([a-zA-Z])\s*=(.+)""".r

  def execute(line: String): String = line.trim match
    case ""                     => ""
    case "help" | "?"           => Session.help
    case "env" | "vars"         => state
    case s"precision $n"        => setPrecision(n.trim)
    case s"unset $name"         => unset(name.trim)
    case s"simplify $rest"      => withParsed(rest)(e => simplify(substitute(e, definitions)).toString)
    case s"expand $rest"        => withParsed(rest)(e => expand(substitute(e, definitions)).toString)
    case s"eval $rest"          => withParsed(rest)(evaluate)
    case assignment(name, rhs)  => withParsed(rhs)(assign(name, _))
    case expression             => withParsed(expression)(evaluate)

  private def withParsed(input: String)(f: _Expression => String): String =
    val result = Parser.parse(input)
    if result.successful then f(result.get)
    else s"parse error: ${result.toString.linesIterator.next()}"

  private def evaluate(e: _Expression): String =
    substitute(e, definitions).eval(env).toExpression.toString

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
      |help                 this summary
      |quit | exit          leave""".stripMargin


@main def repl(): Unit =
  println("Leonardo CAS — type 'help' for commands, 'quit' to leave")
  val session = Session()
  var running = true
  while running do
    print("leonardo> ")
    scala.io.StdIn.readLine() match
      case null | "quit" | "exit" => running = false
      case line =>
        val out = session.execute(line)
        if out.nonEmpty then println(out)

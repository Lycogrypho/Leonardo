package it.grypho.scala.leonardo
package mcp


/** One string parameter of a tool (F_0126).
 *  @param name        the argument's name
 *  @param description what the client is told it is
 *  @param required    whether a call must give it */
final case class ToolParam(name: String, description: String, required: Boolean = true)

/** A tool's answer (F_0126).
 *  @param text    the REPL's printed answer, notes included, or the reason for an error
 *  @param latex   the LaTeX rendering of the same result, when the session offers one
 *  @param isError whether the request itself failed (bad arguments, a timeout), as opposed to an
 *                 answer that declines, which is content */
final case class ToolAnswer(text: String, latex: Option[String], isError: Boolean)

/** A tool as an MCP client sees it, and the one command line it becomes (F_0126 Decision C): the
 *  grammar stays the single definition of every operation.
 *  @param name        the tool's name
 *  @param description what it does, for the client
 *  @param params      its parameters, all strings
 *  @param line        the command line for the given arguments, or why they are refused */
final case class ToolDef(name: String, description: String, params: Vector[ToolParam],
                         line: Map[String, String] => Either[String, String])

/** The server's tools (F_0126): `evaluate` and the typed ones. */
object Tools:

  private val Identifier = "[a-zA-Z][a-zA-Z0-9_]*".r

  /** A required argument: present, non-blank, one line. */
  private def arg(args: Map[String, String], name: String): Either[String, String] =
    args.get(name).map(_.trim) match
      case None | Some("")                => Left(s"missing argument '$name'")
      case Some(v) if v.exists(c => c == '\n' || c == '\r') => Left(s"'$name' must be one line")
      case Some(v)                        => Right(v)

  /** An optional argument: absent, or as [[arg]]. */
  private def optional(args: Map[String, String], name: String): Either[String, Option[String]] =
    if args.get(name).forall(_.trim.isEmpty) then Right(None) else arg(args, name).map(Some(_))

  private def variable(args: Map[String, String], name: String = "variable"): Either[String, String] =
    arg(args, name).filterOrElse(Identifier.matches, s"'$name' must be a variable name")

  private val expressionParam = ToolParam("expression", "an expression in Leonardo's grammar (see the grammar resource)")
  private val variableParam   = ToolParam("variable", "the variable, a name such as x")

  /** `evaluate(line)`: any REPL line, on the session. */
  val evaluate: ToolDef = ToolDef("evaluate",
    "Runs one line of Leonardo's REPL on this connection's session and returns its answer: an " +
    "expression to evaluate or simplify, an assignment `name := expr` (kept for later lines), or a " +
    "command such as `simplify e`, `expand e`, `exact on`, `precision 10`. A computation that cannot " +
    "be done is answered with the expression unevaluated and a note saying why, never with a guess. " +
    "Read the grammar resource before writing a line.",
    Vector(ToolParam("line", "one REPL line")),
    args => arg(args, "line"))

  /** `simplify(expression)`. */
  val simplify: ToolDef = ToolDef("simplify",
    "Simplifies an expression: collects like terms, folds constants and evaluates what it can, " +
    "ignoring the session's variable bindings.",
    Vector(expressionParam),
    args => arg(args, "expression").map(e => s"simplify $e"))

  /** `derive(expression, variable)`. */
  val derive: ToolDef = ToolDef("derive",
    "The symbolic derivative of an expression with respect to one variable.",
    Vector(expressionParam, variableParam),
    args => for e <- arg(args, "expression"); v <- variable(args) yield s"derive($e, $v)")

  /** `integrate(expression, variable[, lower, upper])`. */
  val integrate: ToolDef = ToolDef("integrate",
    "The antiderivative of an expression, or with both bounds its definite integral (exact where an " +
    "antiderivative is smooth on the interval, otherwise certified numerical quadrature).",
    Vector(expressionParam, variableParam,
           ToolParam("lower", "the lower bound, an expression; give both bounds or neither", required = false),
           ToolParam("upper", "the upper bound, an expression", required = false)),
    args =>
      for
        e  <- arg(args, "expression")
        v  <- variable(args)
        lo <- optional(args, "lower")
        hi <- optional(args, "upper")
        l  <- (lo, hi) match
                case (Some(a), Some(b)) => Right(s"integral($e, $v, $a, $b)")
                case (None, None)       => Right(s"integral($e, $v)")
                case _                  => Left("give both bounds or neither")
      yield l)

  /** `solve(equation, variable)`, answered without binding the variable. */
  val solve: ToolDef = ToolDef("solve",
    "Solves an equation `lhs = rhs` or an inequality for one variable. Unlike a bare `solve` line, " +
    "it does not bind the variable in the session.",
    Vector(ToolParam("equation", "an equation or inequality, e.g. x^2 - 4 = 0 or x^2 > 4"), variableParam),
    // `simplify` answers a solve without binding it: only bare evaluation binds
    args => for q <- arg(args, "equation"); v <- variable(args) yield s"simplify solve($q, $v)")

  /** `limit(expression, variable, point[, direction])`. */
  val limit: ToolDef = ToolDef("limit",
    "The limit of an expression as a variable tends to a point (`inf` for infinity), two-sided unless " +
    "a direction `+` or `-` is given.",
    Vector(expressionParam, variableParam, ToolParam("point", "the point, an expression or inf"),
           ToolParam("direction", "`+` from above, `-` from below; omit for two-sided", required = false)),
    args =>
      for
        e <- arg(args, "expression")
        v <- variable(args)
        p <- arg(args, "point")
        d <- optional(args, "direction").filterOrElse(_.forall(Set("+", "-")), "direction must be + or -")
      yield d.fold(s"limit($e, $v, $p)")(s => s"limit($e, $v, $p, $s)"))

  /** `scala_value(line)`: the line's result as Scala source constructing it (Decision D). */
  val scalaValue: ToolDef = ToolDef("scala_value",
    "Runs one REPL line like `evaluate` and returns its result as Scala source that constructs the same " +
    "value with the Leonardo library (`it.grypho:leonardo`), to paste into a program.",
    Vector(ToolParam("line", "one REPL line that produces an expression")),
    args => arg(args, "line"))

  /** Every tool, in the order a client lists them. */
  val all: Vector[ToolDef] = Vector(evaluate, simplify, derive, integrate, solve, limit, scalaValue)

  /** Calls a tool on the worker's session.
   *  @param name   the tool
   *  @param args   its arguments
   *  @param worker the session behind its timeout
   *  @return the answer */
  def call(name: String, args: Map[String, String], worker: SessionWorker): ToolAnswer =
    all.find(_.name == name).toRight(s"no tool named '$name'").flatMap(_.line(args)) match
      case Left(why) => ToolAnswer(why, None, isError = true)
      case Right(line) if name == scalaValue.name =>
        worker.run(s => (s.execute(line), s.lastResult)) match
          case Left(why)                 => ToolAnswer(why, None, isError = true)
          case Right((text, None))       => ToolAnswer(s"the line produced no expression: $text", None, isError = true)
          case Right((_, Some(result))) => ToScala(result) match
            case Right(src) => ToolAnswer(s"${ToScala.Import}\n\n$src", None, isError = false)
            case Left(what) => ToolAnswer(s"the result holds $what, which has no Scala source form", None, isError = true)
      case Right(line) =>
        worker.run(s => (s.execute(line), s.lastLatex)) match
          case Left(why)          => ToolAnswer(why, None, isError = true)
          case Right((text, tex)) => ToolAnswer(text, tex, isError = false)

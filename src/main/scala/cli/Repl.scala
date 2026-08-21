package it.grypho.scala.leonardo
package cli

import core.*
import scalar.*
import matrix.*
import equation.{_Equation, _Solve}
import logic.{_Connective, asTruth, simplifyLogicFully, truthTable, kleeneTable,
                  MaxTruthTableVars, MaxKleeneTableVars}
import parser.Parser

import scala.util.control.NonFatal

import org.jline.reader.{EndOfFileException, LineReader, LineReaderBuilder, UserInterruptException}
import org.jline.terminal.TerminalBuilder


/** Interactive command-line session over the Leonardo library.
 *
 *  Session state lives outside the library's `Environment` because the two kinds
 *  of assignment differ: a constant right-hand side becomes a numeric binding (a
 *  `_Value` the `Environment` can hold), while a right-hand side with free
 *  variables becomes a named definition (a symbolic `_Expression`, which the
 *  `Environment` deliberately cannot hold).  Definitions are late-bound: they are
 *  substituted into input at use time, so redefining `f` also changes any `g`
 *  defined in terms of `f`.
 *
 *  Assignment uses `:=` (CAS convention): bare `=` always parses as an
 *  `_Equation`, so `x = 2*x + 1` is a relation to evaluate, never a binding.
 *  Session scripts (`:save`) emit `:=` and old `=`-style scripts are not accepted.
 *
 *  Commands:
 *  {{{
 *  x := 3.001           bind a value (constant right-hand side)
 *  f := sin(x) + x      define a function (right-hand side with free variables)
 *  h := lhs = rhs       bind a named equation (can be passed to solve(h, x))
 *  g := consolidate(e)  freeze the simplified+evaluated result of e into g (not late-bound)
 *  lhs = rhs            equation: true/false when concrete; solvable via solve()
 *  lhs == rhs           equality check: evaluates to bool but not solvable
 *  <expression>         evaluate, e.g.  f + 1  or  derive(f, x)
 *  simplify <expr>      structural simplification, no numeric evaluation
 *  expand <expr>        distribute products over sums
 *  precision <n>        set decimal precision
 *  pretty on | off      multi-line, column-aligned matrix display (default: off)
 *  logic symmetric on|off  spell truth values as -1 / 0 / 1 (default: off)
 *  logic minmax|product|lukasiewicz   fuzzy t-norm family (default: minmax)
 *  env                  list precision, bindings, and definitions
 *  unset <name>         remove a binding or definition
 *  :load <file>         run a session script (file IO handled by the read loop)
 *  :save <file>         write current state to a replayable script (read loop)
 *  help                 this summary
 *  quit | exit          leave (handled by the read loop)
 *  }}}
 */
final class Session:
  private val MaxPrecision = 15
  private var precision: Int = Environment.DefaultPrecision
  private var colorSchemeName: String = "dark"
  // Multi-line, column-aligned matrix display (issue 4.6). Off by default so the
  // single-line `[[…], […]]` form (which tests and :save scripts rely on) is unchanged;
  // `pretty on` opts in, mirroring the `colors` toggle.
  private var prettyMatrix: Boolean = false
  // Symmetric ternary display/parse encoding (issue 4.G): {-1, 0, 1} instead of
  // {false, unknown, true}. Off by default so the 4.F alphabet is unchanged; an
  // *encoding* toggle only -- the min-max rule table is the same either way.
  private var symmetricLogic: Boolean = false
  // Fuzzy t-norm family (issue 4.H). MinMax is the default and the only lattice of the
  // three, so the boolean and three-valued tiers are unaffected by it.
  private var semantics: LogicSemantics = LogicSemantics.MinMax
  private var bindings: Map[String, _Value] = Map()
  private var definitions: Map[String, _Expression] = Map()

  /** Returns the name of the active colour scheme (`"dark"`, `"light"`, or `"none"`). */
  def currentColorScheme: String = colorSchemeName

  /** Builds a fresh `Environment` from the current precision and numeric bindings. */
  private def env: Environment = new Environment(precision, bindings, symmetricLogic, semantics)

  private val emptyEnv = new Environment()

  private val assignment      = """([a-zA-Z][a-zA-Z0-9_]*)\s*:=(.+)""".r
  private val multiAssignment = """([a-zA-Z][a-zA-Z0-9_]*(?:\s*,\s*[a-zA-Z][a-zA-Z0-9_]*)+)\s*:=(.+)""".r
  // "name := consolidate(expr)" — freeze the simplified + evaluated result into `name`
  // (issue 4.9). consolidate(...) is NOT a grammar function; it is recognised here on the
  // whole RHS so the inner text is handed to the ordinary expression parser via withParsed.
  private val consolidation   = """([a-zA-Z][a-zA-Z0-9_]*)\s*:=\s*consolidate\((.+)\)""".r
  // Lazy first group lets the regex engine find the shortest expression that still
  // leaves valid tokens for <var> <lo> <hi> and an optional <n> at the tail.
  private val samplesRegex = """^(.+?)\s+([a-zA-Z][a-zA-Z0-9_]*)\s+(-?[\d.]+(?:[eE][+-]?\d+)?)\s+(-?[\d.]+(?:[eE][+-]?\d+)?)(?:\s+(\d+))?$""".r

  /** Executes one line of input and returns the display string.
   *
   *  Dispatches to the appropriate command handler or falls through to expression
   *  evaluation.  Returns an empty string for blank input.
   *
   *  @param line the raw input line (not yet trimmed)
   *  @return the result to print, or `""` for silent commands
   */
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
    case "colors"               => s"colors = $colorSchemeName"
    case s"colors $name"        => setColors(name.trim)
    case "pretty"               => s"pretty = ${if prettyMatrix then "on" else "off"}"
    case s"pretty $mode"        => setPretty(mode.trim)
    case "logic"                => logicState
    case "logic symmetric"      => s"logic symmetric = ${if symmetricLogic then "on" else "off"}"
    case s"logic symmetric $mode" => setSymmetricLogic(mode.trim)
    case s"logic $rest"         => setSemantics(rest.trim)
    // simplify renders through toString (it deliberately ignores session precision);
    // symmetricSpelling only overrides it when the result is itself a truth value.
    case s"simplify $rest"      => withParsed(rest) { e =>
      val r = simplifyPipeline(e)
      symmetricSpelling(r).getOrElse(r.toString)
    }
    case s"expand $rest"        => withParsed(rest)(e => expand(resolveMatrixOps(substitute(e, definitions))).toString)
    case s"eval $rest"          => withParsed(rest)(evaluate)
    case s"samples $rest"       => doSamples(rest)
    case "truth"                => "usage: truth <expr>"
    case "truth3"               => "usage: truth3 <expr>"
    case s"truth3 $rest"        => withParsed(rest)(doKleeneTable)
    case s"truth $rest"         => withParsed(rest)(doTruthTable)
    case s":$_"                 => ":load and :save are only available at the interactive prompt"
    // Precedes the generic assignment: the reserved-name guards above already matched the
    // broader `assignment` pattern for this input, so `name` is validated by the time we
    // get here. consolidate freezes; plain `:=` (below) stays late-bound.
    case consolidation(name, inner) => withParsed(inner)(consolidate(name, _))
    case multiAssignment(namesStr, rhs) =>
      withParsed(rhs)(multiAssign(namesStr.split(",").map(_.trim).toList, _))
    case assignment(name, rhs)  => withParsed(rhs)(assign(name, _))
    case expression             => withParsed(expression)(evaluate)

  /** Serialises a `_Value` for a `:save` script so that it round-trips exactly.
   *
   *  `_Number` uses the raw `Double` (`d.toString`), not the display-rounded value, so
   *  no digits are lost regardless of session precision.  `_Bool` is written as the
   *  `true`/`false` literal and `_Truth.Unknown` as `unknown`, which the grammar parses
   *  back (word-boundary guarded constants, like `pi`/`e`).
   */
  private def serializeValue(v: _Value): String = v match
    case _Number(d) => d.toString
    case _Bool(b)   => if b then "true" else "false"
    case other      => other.toString

  /** Shared line-builder for `script` (replayable) and `state` (human-readable). */
  private def buildLines(headers: List[String], bindFmt: _Value => String): String =
    (headers ++
     bindings.toList.sortBy(_._1).map((k, v) => s"$k := ${bindFmt(v)}") ++
     definitions.toList.sortBy(_._1).map((k, e) => s"$k := $e")
    ).mkString("\n")

  /** Current session state serialized as a replayable script — one command per line,
   *  precision first, then bindings and definitions in name order. Feeding this back
   *  through `load` (or line by line through `execute`) reconstructs the session.
   *  Pure: this is what the REPL writes to a `:save` file.
   */
  def script: String = buildLines(
    List(s"precision $precision", s"colors $colorSchemeName", s"pretty ${if prettyMatrix then "on" else "off"}",
         s"logic ${semanticsName(semantics)}",
         s"logic symmetric ${if symmetricLogic then "on" else "off"}"),
    serializeValue)

  /** Execute a whole script body (e.g. the contents of a `:load` file), returning the
   *  newline-joined non-empty outputs of its commands. Blank lines and `#` comments are
   *  skipped. IO-free — the caller supplies the text, so this is unit-testable; the REPL
   *  loop is the only place that actually reads the file.
   *
   *  @param text the script body (newline-separated commands)
   *  @return the concatenated non-empty output lines
   */
  def load(text: String): String =
    text.linesIterator
      .map(_.trim)
      .filter(l => l.nonEmpty && !l.startsWith("#"))
      .map(execute)
      .filter(_.nonEmpty)
      .mkString("\n")

  /** Parses `input`, then calls `f` on the resulting expression inside a `NonFatal` guard.
   *
   *  Parse errors and non-fatal evaluation exceptions are turned into readable messages so
   *  the REPL loop stays alive.  Fatal errors (`OutOfMemoryError`, `StackOverflowError`)
   *  propagate — the dimension caps in the matrix constructors prevent allocation failures
   *  up front, and catching a fatal error would leave the JVM in an unknown state.
   */
  private def withParsed(input: String)(f: _Expression => String): String =
    scala.util.Try(Parser.parse(input)).fold(
      e => s"parse error: ${e.getMessage}",
      result =>
        if result.successful then
          try f(result.get)
          catch case NonFatal(e) => s"evaluation error: ${Option(e.getMessage).getOrElse(e.getClass.getSimpleName)}"
        else
          val base = s"parse error: ${result.toString.linesIterator.next()}"
          // "g(x)"-style call syntax on a defined name is a common spelling of issue
          // 1.1's derive(g(x), f(x)); the grammar has bare variables only, so hint.
          definitions.keys.find(n => input.matches(s".*\\b$n\\s*\\(.*")) match
            case Some(n) => s"$base\nnote: function-call syntax '$n(...)' is not supported; use the bare name '$n'"
            case None    => base
    )

  /** Formats an eval result for display, applying session precision. */
  private def formatResult(result: Either[_Expression, _Value]): String =
    formatExpression(result.toExpression, prettyMatrix)

  /** The symmetric-ternary spelling of a truth value: `-1` / `0` / `1`.
   *
   *  `None` when the toggle is off or `e` is not a truth value, so every caller falls
   *  back to the default `false` / `unknown` / `true` alphabet.  Rendering only -- the
   *  stored value is the same either way, which is why `:save` keeps writing the word
   *  spelling and scripts stay portable across the toggle.
   */
  private def symmetricSpelling(e: _Expression): Option[String] =
    if !symmetricLogic then None
    else e match
      case v: _Value => _Truth.toSymmetric(v).map(symmetricDigit)
      case _         => None

  /** Renders a symmetric digit, dropping the decimal point for the three whole digits. */
  private def symmetricDigit(s: Double): String =
    if s == s.round.toDouble then s.round.toString else _Number(s).display(precision)

  /** Renders a value for a table cell or an assignment echo, honouring the symmetric
   *  toggle and the session precision.
   *
   *  A `_Number` goes through `display(precision)` rather than `toString`, which is fixed
   *  at `Environment.DefaultPrecision`.  Without this a small result renders as `0.0`
   *  however high the session precision is set -- which is exactly how the `solve`
   *  quadratic defect looked like a 100% error when it was in fact 25%.  Everything else
   *  keeps `toString`, so matrix echoes are unchanged.
   */
  private def truthCell(v: _Value): String = symmetricSpelling(v).getOrElse(v match
    case n: _Number => n.display(precision)
    case other      => other.toString)

  /** Formats `e` for display, applying session precision recursively.
   *
   *  The `pretty` flag is forced off when recursing into a matrix's cells, so a
   *  matrix-of-matrices (a decomposition result) keeps its inner matrices single-line
   *  and only the outermost matrix is stacked.
   */
  private def formatExpression(e: _Expression, pretty: Boolean): String =
    symmetricSpelling(e).getOrElse(formatDefault(e, pretty))

  /** [[formatExpression]] in the default truth alphabet. */
  private def formatDefault(e: _Expression, pretty: Boolean): String = e match
    case n: _Number      => n.display(precision)
    case c: _Complex     => c.display(precision)
    case t: _Truth       => t.display(precision)
    case m: _MatrixValue =>
      renderMatrix(Vector.tabulate(m.rows, m.cols)((i, j) => _Number(m(i, j)).display(precision)), pretty)
    case m: _Matrix      =>
      renderMatrix(Vector.tabulate(m.rows, m.cols)((i, j) => formatExpression(m(i, j), pretty = false)), pretty)
    case other           => other.toString

  /** Renders a grid of already-formatted cell strings.
   *
   *  Single-line `[[...], [...]]` by default; column-aligned multi-line when `pretty`
   *  is true and the matrix has at least two rows.
   */
  private def renderMatrix(cells: Vector[Vector[String]], pretty: Boolean): String =
    if pretty && cells.sizeIs >= 2 then prettyMatrixString(cells)
    else cells.map(_.mkString("[", ", ", "]")).mkString("[", ", ", "]")

  /** Multi-line matrix: right-aligned columns, rows stacked with balanced brackets. */
  private def prettyMatrixString(cells: Vector[Vector[String]]): String =
    val widths = cells.head.indices.map(j => cells.map(_(j).length).max)
    val rows   = cells.map(row =>
      row.zip(widths).map((c, w) => " " * (w - c.length) + c).mkString("[", ", ", "]"))
    rows.zipWithIndex.map { (r, i) =>
      val open  = if i == 0 then "[" else " "
      val close = if i == rows.size - 1 then "]" else ""
      s"$open$r$close"
    }.mkString("\n")

  /** Returns `true` when the expression tree contains a `_Solve` functional node. */
  private def containsSolve(e: _Expression): Boolean = e match
    case _: _Solve => true
    case _         => e.children.exists(containsSolve)

  /** Returns `true` when the expression tree contains a boolean connective node. */
  private def containsConnective(e: _Expression): Boolean = e match
    case _: _Connective => true
    case _              => e.children.exists(containsConnective)

  /** The `simplify` command pipeline: matrix algebra first, then the scalar structural
   *  pass, then -- only when connectives are present -- the logic pass, with
   *  `scalar.simplifyFully` injected as its leaf pass so scalar bodies nested inside
   *  connectives are simplified too.  The connective gate keeps purely scalar input
   *  byte-identical to the scalar-only pipeline.
   */
  private def simplifyPipeline(e: _Expression): _Expression =
    val prepared = simplify(resolveMatrixOps(substitute(e, definitions)))
    if containsConnective(prepared) then simplifyLogicFully(prepared, simplifyFully, symmetricLogic, semantics)
    else prepared

  /** Handles the `truth <expr>` command: tabulates the expression over its free
   *  variables (definitions substituted first; session numeric bindings are ignored --
   *  every free variable is enumerated as a boolean).  Rows that do not reduce to a
   *  boolean show `?` in the result column -- including rows whose value is the graded
   *  `unknown`, which is what `truth3` is for.
   */
  private def doTruthTable(e: _Expression): String =
    tabulate(e, "truth", MaxTruthTableVars, if symmetricLogic then 2 else 5) { (body, vars) =>
      truthTable(body, vars, env)
        .map((assignment, result) =>
          (assignment.view.mapValues(b => truthCell(_Bool(b))).toMap, result.map(b => truthCell(_Bool(b)))))
    }

  /** Handles the `truth3 <expr>` command: the three-valued (Kleene) counterpart of
   *  `truth`, enumerating each free variable over `false`/`unknown`/`true`.
   */
  private def doKleeneTable(e: _Expression): String =
    tabulate(e, "truth3", MaxKleeneTableVars, if symmetricLogic then 2 else 7) { (body, vars) =>
      kleeneTable(body, vars, env)
        .map((assignment, result) =>
          (assignment.view.mapValues(v => formatExpression(v, pretty = false)).toMap,
           result.map(v => formatExpression(v, pretty = false))))
    }

  /** Shared renderer for the `truth` / `truth3` commands.
   *
   *  Substitutes definitions, enumerates the body's free variables in name order, and
   *  lays the rows out as left-aligned columns with the expression itself as the result
   *  header.  A row whose result is `None` renders as `?`.
   *
   *  @param e        the parsed expression to tabulate
   *  @param command  the command name, used in the too-many-variables message
   *  @param maxVars  the variable cap for this table's arity
   *  @param minWidth minimum column width (the widest truth literal of this table)
   *  @param rows     builds the already-stringified rows for a body and its variables
   *  @return the rendered table, or an error message when the variable cap is exceeded
   */
  private def tabulate(e: _Expression, command: String, maxVars: Int, minWidth: Int)
                      (rows: (_Expression, List[_Variable]) => Vector[(Map[String, String], Option[String])]): String =
    val body = substitute(e, definitions)
    val vars = body.freeVars.toList.sorted.map(_Variable.apply)
    if vars.sizeIs > maxVars then
      s"$command: too many variables (${vars.size}); the limit is $maxVars"
    else
      val names  = vars.map(_.variable)
      val widths = names.map(n => math.max(n.length, minWidth))
      def row(cells: List[String], result: String): String =
        val vals = cells.zip(widths).map((c, w) => c.padTo(w, ' ')).mkString(" ")
        if vals.isEmpty then s"| $result" else s"$vals | $result"
      val header = row(names, body.toString)
      val lines  = rows(body, vars).map { (assignment, result) =>
        row(names.map(assignment), result.getOrElse("?"))
      }
      (header +: lines).mkString("\n")

  /** Auto-binds the solved variable when `solve` produces a result at the REPL top level.
   *
   *  Single solution `v = rhs` -> bind `v`.
   *  Multiple solutions -> bind `v_1`, `v_2`, ... leaving `v` itself unbound.
   *  No solution -> `None` (falls back to `formatResult`).
   */
  private def tryAutoBindSolve(result: Either[_Expression, _Value]): Option[String] =
    result match
      case Left(_Equation(v: _Variable, rhs))
          if !Session.ReservedConstants.contains(v.variable)
          && !Parser.ReservedWords.contains(v.variable) =>
        Some(assign(v.variable, rhs))
      case Left(_Matrix(1, _, elems)) =>
        val equations = elems.collect { case _Equation(v: _Variable, rhs) => (v.variable, rhs) }
        if equations.size == elems.size && equations.map(_._1).distinct.size == 1 then
          val name = equations.head._1
          val lines = equations.zipWithIndex.map { case ((_, rhs), i) => assign(s"${name}_${i + 1}", rhs) }
          Some(lines.mkString("\n"))
        else None
      case _ => None

  /** Evaluates `e` with current definitions and bindings, auto-binding solve results. */
  private def evaluate(e: _Expression): String =
    resolveDerivativeBinders(e) match
      case Left(message) => message
      case Right(resolved) =>
        val result = substitute(resolved, definitions).eval(env)
        if containsSolve(e) then tryAutoBindSolve(result).getOrElse(formatResult(result))
        else formatResult(result)

  /** Carry out matrix algebra before simplify/expand: scalar Sum/Product/Ratio nodes
   *  whose operands are matrix-shaped — a matrix literal, a matrix operation node, or
   *  a variable bound to a matrix value — are re-typed to the matrix operations and
   *  reduced, so `C = A * B` then `simplify C` executes the multiplication and hands
   *  simplify a `_Matrix` whose elements it simplifies one by one (`_ElementWise`).
   *  Purely scalar sub-expressions are untouched: `simplify x + 0` keeps ignoring
   *  numeric bindings. Matrix operands, by contrast, must be resolved through the
   *  session bindings — executing `A * B` is impossible without knowing `A` and `B`.
   */
  private def resolveMatrixOps(e: _Expression): _Expression =
    val localEnv = env
    def isMatrixish(x: _Expression): Boolean = x match
      case _: _Matrix | _: _MatrixOperation | _: _MatrixValue => true
      case v: _Variable => localEnv.get(v.variable).exists(_.isInstanceOf[_MatrixValue])
      case _            => false

    val rec = e.rebuild(e.children.map(resolveMatrixOps))
    rec match
      case Sum(a, b) if isMatrixish(a) || isMatrixish(b)     => MatSum(a, b).eval(localEnv).toExpression
      case Product(a, b) if isMatrixish(a) && isMatrixish(b) => MatProduct(a, b).eval(localEnv).toExpression
      case Product(a, b) if isMatrixish(b)                   => MatScale(a, b).eval(localEnv).toExpression
      case Product(a, b) if isMatrixish(a)                   => MatScale(b, a).eval(localEnv).toExpression
      case Ratio(a, b) if isMatrixish(a) && !isMatrixish(b)  => MatScale(Ratio(_Number(1), b), a).eval(localEnv).toExpression
      case Ratio(a, b) if isMatrixish(a) && isMatrixish(b)   => MatProduct(a, Inverse(b)).eval(localEnv).toExpression
      case Ratio(a, b) if isMatrixish(b)                     => MatScale(a, Inverse(b)).eval(localEnv).toExpression
      case d @ Determinant(_)                                => d.eval(localEnv).toExpression
      case m: _MatrixOperation                               => m.eval(localEnv).toExpression
      case other                                             => other

  /** Rewrites derivative/integral binders that name a *definition* via the chain rule.
   *
   *  `d/df` where `f` names a definition: the binder is never substituted, so left
   *  alone the derivative would be taken with respect to a variable that no longer
   *  occurs in the substituted body — a silent 0.  Rewrites via the chain rule:
   *  `dg/df = (dg/dx) / (df/dx)` over the definition's single free variable `x`.
   *  Definitions with zero or several free variables are rejected with a message
   *  (`Left`).  The same logic applies to `_Integral` binders (change-of-variable),
   *  while `_DefIntegral` with a definition binder is rejected outright.
   *
   *  @param e the expression to rewrite
   *  @return `Right(rewritten)` on success; `Left(message)` when a binder cannot be resolved
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
    // Change-of-variable: integral g df = integral g * (df/dx) dx over f's single free variable x.
    // The derivative df/dx is evaluated eagerly so constant slopes fold immediately
    // (e.g. f := 2*x -> df/dx = 2, and simplify(g * 2) folds away the trivial product).
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
      other.children.map(resolveDerivativeBinders).partitionMap(identity) match
        case (message :: _, _) => Left(message)
        case (Nil, children)   => Right(other.rebuild(children))

  /** Binds `name` to `rhs`, choosing between a numeric binding and a late-bound definition.
   *
   *  Resolves derivative binders and substitutes definitions before folding, so
   *  `q := derive(p, x)` (where `p` is a definition) differentiates the substituted
   *  body rather than treating `p` as an unknown constant.  The raw `rhs` is stored
   *  for a definition, keeping it late-bound.
   *
   *  @param name the variable name to bind
   *  @param rhs  the right-hand side expression (raw, not yet substituted)
   *  @return the display string `"name := value"` or an error message
   */
  private def assign(name: String, rhs: _Expression): String =
    resolveDerivativeBinders(rhs) match
      case Left(message) => message
      case Right(resolved) =>
        substitute(resolved, definitions).eval(emptyEnv) match
          case Right(value) =>
            bindings = bindings + (name -> value)
            definitions = definitions - name
            s"$name := ${truthCell(value)}"
          case Left(_) =>
            definitions = definitions + (name -> rhs)
            bindings = bindings - name
            s"$name := $rhs"

  /** Freezes the simplified+evaluated result of `rhs` into `name` (the `consolidate` command).
   *
   *  Unlike `assign`, which stores the raw body for late-binding, `consolidate`
   *  snapshots the simplified expression with the current bindings applied.  A fully
   *  numeric result becomes a value binding; a residual symbolic result is stored as a
   *  frozen definition — the simplified expression as it stands now, not the raw body.
   *  Because the stored body is already fully substituted, a later redefinition of a
   *  dependency cannot change it, and a `:save`/`:load` round-trip reproduces the
   *  identical definition entry.
   *
   *  @param name  the variable name to freeze into
   *  @param rhs   the right-hand side expression (inner of `consolidate(...)`)
   *  @return the display string `"name := value"` or an error message
   */
  private def consolidate(name: String, rhs: _Expression): String =
    resolveDerivativeBinders(rhs) match
      case Left(message) => message
      case Right(resolved) =>
        val prepared = simplify(resolveMatrixOps(substitute(resolved, definitions)))
        prepared.eval(env) match
          case Right(value) =>
            bindings = bindings + (name -> value)
            definitions = definitions - name
            s"$name := ${truthCell(value)}"
          case Left(expr) =>
            val frozen = simplify(expr)
            definitions = definitions + (name -> frozen)
            bindings = bindings - name
            s"$name := $frozen"

  /** Handles tuple assignment `L, U, P := lu(A)`: binds multiple names to a 1*n row result.
   *
   *  @param names the list of variable names (left-hand side of the tuple)
   *  @param rhs   the right-hand side expression (must evaluate to a 1*n `_Matrix`)
   *  @return newline-joined display strings for each binding, or an error message
   */
  private def multiAssign(names: List[String], rhs: _Expression): String =
    names.find(Session.ReservedConstants.contains) match
      case Some(bad) => s"cannot assign to '$bad': it is a built-in constant"
      case None =>
        names.find(Parser.ReservedWords.contains) match
          case Some(bad) => s"cannot assign to '$bad': it is a reserved word"
          case None =>
            resolveDerivativeBinders(rhs) match
              case Left(message) => message
              case Right(resolved) =>
                substitute(resolved, definitions).eval(env) match
                  case Left(_Matrix(1, n, elems)) if n == names.size =>
                    names.zip(elems).map((name, elem) => assign(name, elem)).mkString("\n")
                  case Left(_Matrix(1, n, _)) =>
                    s"tuple assignment: ${names.size} names on the left but $n elements on the right"
                  case _ =>
                    s"tuple assignment: right-hand side must evaluate to a 1xn row (use lu, qr, eig, jordan, eigen)"

  /** Handles the `samples <expr> <var> <lo> <hi> [<n>]` command. */
  private def doSamples(rest: String): String = rest.trim match
    case samplesRegex(exprStr, varStr, loStr, hiStr, nStr) =>
      // The samples regex admits malformed literals like "1..2" (its [\d.]+ class allows
      // several dots), so parse the bounds with toDoubleOption rather than toDouble, which
      // would throw a NumberFormatException and — this path runs outside withParsed — crash
      // the REPL loop.
      (loStr.toDoubleOption, hiStr.toDoubleOption) match
        case (Some(lo), Some(hi)) if lo >= hi => "samples: lo must be strictly less than hi"
        case (Some(lo), Some(hi)) =>
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
        case _ => "samples: <lo> and <hi> must be numbers"
    case _ => "usage: samples <expr> <var> <lo> <hi> [<n>]"

  /** Sets the display precision, rejecting out-of-range values. */
  private def setPrecision(text: String): String =
    text.toIntOption match
      case Some(n) if n >= 0 && n <= MaxPrecision => precision = n; s"precision = $n"
      case Some(n) if n > MaxPrecision =>
        s"precision expects a value between 0 and $MaxPrecision, got: $n"
      case _ => s"precision expects a non-negative integer, got: $text"

  /** Sets the active colour scheme by name, rejecting unknown names. */
  private def setColors(name: String): String =
    if ColorScheme.All.contains(name) then
      colorSchemeName = name
      s"colors = $name"
    else
      val available = ColorScheme.All.keys.toList.sorted.mkString(", ")
      s"unknown color scheme '$name'; available: $available"

  /** Both logic settings in one listing, for the bare `logic` command. */
  private def logicState: String =
    s"logic ${semanticsName(semantics)}\nlogic symmetric = ${if symmetricLogic then "on" else "off"}"

  /** The REPL spelling of a semantics: the enum case name in lower case. */
  private def semanticsName(s: LogicSemantics): String = s.toString.toLowerCase

  /** Selects the t-norm family by name (case-insensitive), rejecting unknown names. */
  private def setSemantics(text: String): String =
    LogicSemantics.values.find(_.toString.equalsIgnoreCase(text)) match
      case Some(s) => semantics = s; s"logic ${semanticsName(s)}"
      case None =>
        val available = LogicSemantics.values.map(semanticsName).mkString(", ")
        s"unknown logic setting '$text'; try: $available, or symmetric on | off"

  /** Sets the symmetric-ternary encoding flag from `"on"`/`"off"` (case-insensitive). */
  private def setSymmetricLogic(text: String): String = text.toLowerCase match
    case "on"  | "true"  => symmetricLogic = true;  "logic symmetric = on"
    case "off" | "false" => symmetricLogic = false; "logic symmetric = off"
    case _               => s"logic symmetric expects 'on' or 'off', got: $text"

  /** Sets the pretty-matrix flag from `"on"`/`"off"` (case-insensitive). */
  private def setPretty(text: String): String = text.toLowerCase match
    case "on"  | "true"  => prettyMatrix = true;  "pretty = on"
    case "off" | "false" => prettyMatrix = false; "pretty = off"
    case _               => s"pretty expects 'on' or 'off', got: $text"

  /** Removes a binding or definition by name, reporting whether it existed. */
  private def unset(name: String): String =
    if bindings.contains(name) || definitions.contains(name) then
      bindings = bindings - name
      definitions = definitions - name
      s"$name unset"
    else s"$name is not set"

  /** Returns the current session state in human-readable form. */
  private def state: String = buildLines(List(s"precision = $precision"), _.toString)

/** Companion object: constants and IO helpers shared across the REPL entry point. */
object Session:
  /** Names the parser always resolves as constants; assignment to them is rejected. */
  val ReservedConstants: Set[String] = Set("pi", "e")

  /** Per-command help text, keyed by the command token (`:=`, `simplify`, ...).
   *  Returned by `help <topic>`; bare `help` still shows the full help listing.
   */
  val helpTopics: Map[String, String] = Map(
    ":=" ->
      """|Bind a name or define a function.
         |Constant RHS -> numeric value; RHS with free variables -> late-bound definition.
         |  x := 3.001          bind a numeric value
         |  f := sin(x) + x     define a function (late-bound: redefining f updates g := f^2)
         |  h := x^2 = 4        bind a named equation (pass to solve(h, x))
         |  L, U, P := lu(A)    bind multiple names to elements of a 1xn result (any decomposition)""".stripMargin,
    "=" ->
      """|Equation relation: true/false when both sides are concrete, symbolic otherwise.
         |Solvable via solve().  Use ":=" for assignment -- "=" is never a binding.
         |  10*x = 2*x + 1      evaluates to false when x = 3
         |  solve(10*x = 2*x + 1, x)   -> x = 0.125""".stripMargin,
    "==" ->
      """|Equality check: same semantics as "=" but not accepted by solve().
         |Useful when you want a boolean result without accidentally creating a solvable equation.
         |  i == 0 - i*i*i      evaluates true""".stripMargin,
    "simplify" ->
      """|Structural simplification: remove identities, fold constants, cancel inverses.
         |Matrix algebra is carried out first, then each element simplified.
         |Numeric bindings are NOT applied (use bare evaluation for that).
         |  simplify x + 0      -> x
         |  simplify C          (C := A * B) executes the multiplication, simplifies each cell""".stripMargin,
    "expand" ->
      """|Distribute products over sums; expand integer powers via the binomial theorem.
         |Matrix algebra is carried out first.
         |  expand x * (y + z)  -> ((x * y) + (x * z))
         |  expand (x + 1)^2    -> (((x ^ 2.0) + (2.0 * x)) + 1.0)""".stripMargin,
    "eval" ->
      """|Evaluate an expression substituting current bindings and returning a numeric result.
         |  eval sin(pi/2)      -> 1.0""".stripMargin,
    "consolidate" ->
      """|Freeze the simplified + evaluated result of an expression into a new variable.
         |Unlike ":=", which keeps a definition late-bound, consolidate snapshots the value
         |NOW using the current bindings -- redefining a dependency later does not change it.
         |A fully numeric result becomes a value binding; a residual symbolic result is stored
         |as a frozen (already-simplified) definition.
         |  x := 2
         |  f := x + 1
         |  g := consolidate(f + f)   -> g := 6.0   (stays 6.0 even after x := 100)
         |  a := 3
         |  h := consolidate(a * y)   -> h := (3.0 * y)   (a folded in and frozen; h ignores later a := 9)""".stripMargin,
    "precision" ->
      """|Set the decimal precision for display and numeric comparisons.
         |  precision 8         8 significant decimal digits
         |  precision 5         restore default""".stripMargin,
    "colors" ->
      """|Switch the syntax-highlighting colour scheme for the interactive prompt.
         |The scheme takes effect immediately and is persisted by :save / :load.
         |  colors dark         bold yellow commands, cyan functions, magenta constants, green numbers (default)
         |  colors light        bold blue commands, green functions, magenta constants, red numbers
         |  colors none         disable highlighting
         |  colors              show the active scheme""".stripMargin,
    "pretty" ->
      """|Toggle multi-line, column-aligned display of matrices with 2+ rows.
         |Off by default; the setting is persisted by :save / :load.
         |  pretty on           stack rows on separate lines, right-align columns
         |  pretty off          single-line [[...], [...]] form (default)
         |  pretty              show the current setting""".stripMargin,
    "truth" ->
      """|Print the truth table of a boolean expression over its free variables.
         |Variables are enumerated as true/false (the first variable varies slowest);
         |rows that do not reduce to a boolean show "?".  Limit: 16 variables.
         |Connective precedence (tightest to loosest): not, and, xor, or, implies.
         |  truth a and (b or not a)
         |  truth (a or b) and not (a and b)     equivalent to a xor b
         |Use truth3 for the three-valued (Kleene) table.""".stripMargin,
    "truth3" ->
      """|Print the three-valued (Kleene) truth table over the free variables.
         |Each variable is enumerated as false / unknown / true (3^n rows, max 10 vars).
         |Semantics: and = min, or = max, not = 1 - a, so "unknown" is the fixpoint of
         |negation and the classical laws that fail there (a and not a) do not fold.
         |  truth3 a and not a          -> unknown at a = unknown, false otherwise
         |  truth3 a implies b""".stripMargin,
    "unknown" ->
      """|The third Kleene truth value: neither true nor false.
         |A first-class value like true/false -- bindable, printable, saved by :save.
         |  not unknown                 -> unknown   (unknown is the negation fixpoint)
         |  false and unknown           -> false     (0 annihilates min)
         |  true or unknown             -> true      (1 annihilates max)
         |  unknown and unknown         -> unknown
         |  u := unknown                bind it like any other value""".stripMargin,
    "defuzz" ->
      """|Defuzzify a membership curve over [lo, hi] to a single crisp value.
         |Uses the centre of gravity (centroid) over a 201-point grid.
         |  defuzz(trimf(x, 0, 5, 10), x, 0, 10)      -> 5.0   (apex of a symmetric triangle)
         |  defuzz(gaussmf(x, 3, 1), x, 0, 6)         -> 3.0
         |Membership curves: trimf(x,a,b,c), trapmf(x,a,b,c,d), gaussmf(x,mean,sigma),
         |sigmf(x,a,c).  Hedges: very(d) = d^2, somewhat(d) = sqrt(d).
         |truth(x) turns a scalar degree in [0,1] into a truth value (and is how a
         |graded degree prints, so it round-trips through :save).""".stripMargin,
    "logic" ->
      """|Switch the truth-value alphabet between the default and symmetric ternary.
         |Symmetric ternary spells the SAME three truth values with the digits -1, 0, 1
         |(false, unknown, true), related by the affine map t = (s + 1) / 2.  It is an
         |encoding, not a semantics: the min-max rule table is identical either way.
         |With the toggle on, -1 / 0 / 1 are also READ as truth values in connective
         |positions, so "1 and 0" is "true and unknown"; with it off a bare 0 stays a
         |plain number, so ordinary arithmetic is never reinterpreted.
         |The setting is persisted by :save / :load; :save always writes the word
         |spelling, so scripts stay portable across the toggle.
         |  logic symmetric on        -1 / 0 / 1
         |  logic symmetric off       false / unknown / true (default)
         |  logic symmetric           show the current setting
         |The t-norm family selects how graded degrees combine; all three agree with
         |classical logic on true/false, so the boolean and ternary tiers are unaffected.
         |  logic minmax              and = min, or = max (default; the only lattice)
         |  logic product             and = a*b, or = a+b-a*b
         |  logic lukasiewicz         and = max(0,a+b-1), or = min(1,a+b)
         |  logic                     show both settings""".stripMargin,
    "taylor" ->
      """|Expand an expression as a truncated Taylor polynomial about a point.
         |maclaurin(e, v, n) is sugar for taylor(e, v, 0, n).  Order is capped at 20.
         |  maclaurin(exp(x), x, 4)     -> 1 + x + x^2/2 + x^3/6 + x^4/24
         |  maclaurin(sin(x), x, 7)     odd powers only
         |  taylor(exp(x), x, 1, 4)     expanded about x = 1
         |  taylor(x^2, x, a, 2)        a symbolic centre is fine; the result is
         |                              a polynomial in (x - a)
         |The expansion variable stays FREE in the result, so binding it evaluates the
         |polynomial.  Stays symbolic when the order is not an integer in 0..20, or when
         |a coefficient cannot be differentiated -- maclaurin(Gamma(x), x, 3) needs the
         |digamma function and so returns unevaluated rather than a bogus series.""".stripMargin,
    "fact" ->
      """|Factorial and the gamma family.
         |  fact(5)             -> 120.0      exact for integers up to 170!
         |  fact(0.5)           -> 0.88623    analytic continuation: Gamma(1.5)
         |  dfact(7)            -> 105.0      double factorial 7!! = 7*5*3*1
         |  mfact(10, 3)        -> 280.0      multifactorial, step 3: 10*7*4*1
         |  Gamma(5)            -> 24.0       Gamma(n) = (n-1)!
         |  lgamma(1e5)                       ln|Gamma| -- finite where Gamma overflows
         |  Beta(1, 4)          -> 0.25       Beta(a,b) = Gamma(a)Gamma(b)/Gamma(a+b)
         |Gamma and Beta are capitalised so that lowercase "gamma" and "beta" stay
         |available as ordinary variable names.  Poles (0, -1, -2, ...), 171! and
         |beyond, and complex arguments all stay symbolic rather than returning
         |infinities.  Differentiating Gamma needs the digamma function, which is not
         |implemented, so derive(Gamma(x), x) stays symbolic.""".stripMargin,
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
         |A matrix equation is solved for a scalar unknown cell-by-cell (intersection),
         |or for a matrix unknown via the inverse / Kronecker vectorization (bind the
         |known matrices first; symbolic coefficients give a symbolic solution).
         |  solve(10*x = 2*x + 1, x)   -> x = 0.125
         |  solve(x^2 = 4, x)          -> [[x = -2.0, x = 2.0]]
         |  solve([[x, 2*x]] = [[3, 6]], x)   -> x = 3.0
         |  solve(A * X = B, X)        -> X = A-inverse * B     (X * A = B -> X = B * A-inverse)
         |  solve(A * X + C = B, X)    affine term: -> A*X = B - C
         |  solve(A * X * D = B, X)    two-sided: -> X = A-inverse * B * D-inverse
         |  solve(A * X + X * B = C, X)   Sylvester/Lyapunov, via vec/Kronecker
         |  solve(h, x)                h is a named equation""".stripMargin,
    "derive" ->
      """|Differentiate an expression with respect to a variable or a defined function.
         |Chain rule applies when the binder is a definition.
         |  derive(sin(x), x)           -> cos(x)
         |  derive(g, f)                chain rule when f := sin(x)""".stripMargin,
    "integral" ->
      """|Indefinite symbolic integration via a rule table.
         |  integral(x^2, x)            -> ((1.0 / 3.0) * (x ^ 3.0))""".stripMargin,
    "samples" ->
      """|Sample a function over a uniform grid, returning tab-separated (x, f(x)) pairs.
         |Non-finite values (div-by-zero, domain errors) are silently skipped.
         |n defaults to 200; x values are in ascending order.
         |  samples sin(x) x -10 10
         |  samples f x 0 5 100    (f must be a defined function)""".stripMargin,
    "limit" ->
      """|Compute lim_{v -> point} e. Optional direction: "+" (from right) or "-" (from left).
         |Uses L'Hopital's rule for 0/0 and inf/inf forms; "inf" / "-inf" as limit points.
         |  limit(sin(x)/x, x, 0)       -> 1.0 (L'Hopital)
         |  limit(1/x, x, 0, +)         -> inf
         |  limit(1/x, x, 0, -)         -> -inf
         |  limit(atan(x), x, inf)       -> 1.5708 (pi/2)
         |  limit(1/x, x, inf)           -> 0.0""".stripMargin,
    "laplace" ->
      """|Compute the Laplace transform L{e(t)} with output variable s.
         |Linearity, powers, exponentials, sin/cos, and the first-shift theorem e^{at}*g(t).
         |  laplace(1, t, s)             -> (1.0 / s)
         |  laplace(t^2, t, s)           -> (2.0 / (s ^ 3.0))
         |  laplace(sin(3*t), t, s)      -> 3/(s^2+9)
         |  laplace(exp(2*t)*cos(t), t, s) -> (s-2)/((s-2)^2+1) via first-shift
         |Stays symbolic when no rule applies.""".stripMargin,
    "fourier" ->
      """|Compute the unilateral Fourier transform F{e(t)} = L{e(t)}|_{s=i*w}.
         |Result is generally complex-valued (contains i, the imaginary unit).
         |  fourier(exp(-2*t), t, w)     -> 1/(2 + i*w)
         |  fourier(1, t, w)             -> 1/(i*w)
         |  fourier(exp(-t)*sin(t), t, w) -> 1/((i*w+1)^2+1) via first-shift
         |Stays symbolic when the Laplace transform is not in the table.""".stripMargin,
    "invlaplace" ->
      """|Compute the inverse Laplace transform L-1{f(s)} with output variable t.
         |Rational f(s) = N(s)/D(s) with degree of D <= 2: linear, repeated, and complex
         |poles (partial fractions / completing the square), plus linearity.
         |  invlaplace(1/s^2, s, t)          -> t
         |  invlaplace(1/(s-3), s, t)        -> exp(3*t)
         |  invlaplace(2/(s^2+4), s, t)      -> sin(2*t)
         |  invlaplace(3/((s-2)^2+9), s, t)  -> exp(2*t)*sin(3*t) via completing the square
         |Stays symbolic for deg D >= 3, symbolic coefficients, or non-rational input.""".stripMargin,
    "eigen" ->
      """|Compute the eigenvalues of a square matrix.
         |Returns a 1xn row [[l1, ..., ln]]; real eigenvalues are _Number, complex pairs are _Complex.
         |Uses QR iteration with Wilkinson shifts; stays symbolic if the operand is not dense.
         |  eigen([[4, 1], [1, 3]])          -> [[4.61803, 2.38197]]
         |  eigen([[0, -1], [1, 0]])         -> [[(0.0 + 1.0i), (0.0 - 1.0i)]]
         |Access element k (1-based): at(eigen(A), 1, k)""".stripMargin,
    "solveSystem" ->
      """|Solve a square system of n linear equations in n unknowns.
         |Gaussian elimination (dense) or symbolic row-reduction (symbolic coefficients).
         |  solveSystem([[2*x + y = 3, x - y = 0]], x, y)   -> [[x = 1.0, y = 1.0]]
         |  Named equation matrices work: solveSystem(S, x, y)""".stripMargin,
  )

  /** Returns the help text for a specific `topic`, or the full help listing when not found.
   *
   *  @param topic the command token to look up (e.g. `"simplify"`, `":="`)
   *  @return topic-specific text, or `help` when the topic is unknown
   */
  def helpTopic(topic: String): String =
    if topic.isEmpty then help else helpTopics.getOrElse(topic, help)

  /** The full command-summary listing printed by `help` with no argument. */
  val help: String =
    """x := 3.001           bind a value (constant right-hand side)
      |f := sin(x) + x      define a function (right-hand side with free variables)
      |h := lhs = rhs       bind a named equation (use with solve(h, x))
      |g := consolidate(e)  freeze the simplified+evaluated result of e into g (not late-bound)
      |L, U, P := lu(A)     bind multiple names to a decomposition result (1xn row)
      |lhs = rhs            equation: true/false once both sides are concrete; stays
      |                     symbolic with free variables; solvable via solve()
      |lhs == rhs           equality check: same as "=" but not accepted by solve()
      |<expression>         evaluate, e.g.  f + 1  or  limit(sin(x)/x, x, 0)
      |simplify <expr>      structural simplification (matrix algebra is carried out,
      |                     then each element is simplified; scalars ignore bindings)
      |expand <expr>        distribute products over sums (matrix algebra as above)
      |a and b, not a, ...  logic connectives (loosest first): implies, or, xor, and,
      |                     not; literals true/false/unknown (three-valued Kleene)
      |truth <expr>         print the truth table over the expression's free variables
      |truth3 <expr>        three-valued table: false / unknown / true per variable
      |fact(n), Gamma(z)    factorial and gamma family; see "help fact"
      |taylor(e,v,pt,n)     Taylor/Maclaurin expansion; see "help taylor"
      |precision <n>        set decimal precision
      |colors <scheme>      syntax highlighting: dark | light | none  (default: dark)
      |pretty on | off      multi-line, column-aligned matrix display (default: off)
      |logic symmetric on|off  spell truth values as -1 / 0 / 1 instead of
      |                     false / unknown / true (symmetric ternary; default: off)
      |logic <semantics>    fuzzy t-norm: minmax | product | lukasiewicz (default: minmax)
      |defuzz(e, v, lo, hi) defuzzify a membership curve to a crisp value (centroid)
      |env                  list precision, bindings, and definitions
      |unset <name>         remove a binding or definition
      |:load <file>         run a session script (bindings/definitions/commands)
      |:save <file>         write the current session state to a replayable script
      |help                 this summary
      |quit | exit          leave""".stripMargin

  /** Reads a `:load` file and replays it through the session.  IO lives here, not in `Session`. */
  def loadFile(session: Session, path: String): String =
    scala.util.Using(scala.io.Source.fromFile(path, "UTF-8"))(_.mkString) match
      case scala.util.Success(text) => session.load(text)
      case scala.util.Failure(e)    => s"could not read $path: ${e.getMessage}"

  /** Writes the session's replayable script to a `:save` file.  IO lives here, not in `Session`. */
  def saveFile(session: Session, path: String): String =
    scala.util.Try:
      val w = new java.io.PrintWriter(path, java.nio.charset.StandardCharsets.UTF_8)
      try w.write(session.script) finally w.close()
    match
      case scala.util.Success(_) => s"saved to $path"
      case scala.util.Failure(e) => s"could not write $path: ${e.getMessage}"

  /** Interprets one line read by the REPL loop, resolving the commands `execute` cannot:
   *  the file-IO `:load`/`:save` and the `quit`/`exit` sentinels.  A `None` line stands
   *  for end-of-input (Ctrl-D).  Returns `None` to stop the loop, or `Some(output)` to
   *  continue — `output` is the (possibly empty) text to print.  This keeps the loop's
   *  dispatch logic unit-testable; only the JLine plumbing in `repl()` is interactive-only.
   *
   *  @param session the active `Session` that accumulates state
   *  @param line    the input line, or `None` for end-of-input
   *  @return `None` to terminate the loop; `Some(output)` to continue
   */
  def step(session: Session, line: Option[String]): Option[String] =
    line match
      case None | Some("quit") | Some("exit") => None
      case Some(s":load $path")               => Some(loadFile(session, path.trim))
      case Some(s":save $path")               => Some(saveFile(session, path.trim))
      case Some(other)                        => Some(session.execute(other))


/** Entry point for the interactive Leonardo REPL. */
@main def repl(): Unit =
  val session = Session()
  // A system terminal enables arrow-key line editing; dumb(true) makes it fall back
  // gracefully (rather than throwing) when no interactive console is attached, e.g.
  // piped input or CI, where the loop still works line-by-line without editing.
  val terminal = TerminalBuilder.builder().system(true).dumb(true).build()
  // Command history persisted across sessions in the user's home directory; JLine
  // loads it on start and appends to it as lines are entered.
  val historyFile = java.nio.file.Paths.get(System.getProperty("user.home"), ".leonardo_history")
  val highlighter = LeonardoHighlighter(() => session.currentColorScheme)
  val reader = LineReaderBuilder.builder()
    .terminal(terminal)
    .variable(LineReader.HISTORY_FILE, historyFile)
    .highlighter(highlighter)
    .build()
  val out = terminal.writer()
  out.println("Leonardo CAS -- type 'help' for commands, 'quit' to leave")
  out.flush()
  try
    var running = true
    while running do
      // Ctrl-C abandons the current line but keeps the session (empty line = no-op);
      // Ctrl-D (end of input) ends the loop like `quit`.
      val line =
        try Some(reader.readLine("leonardo> "))
        catch
          case _: UserInterruptException => Some("")
          case _: EndOfFileException     => None
      Session.step(session, line) match
        case None         => running = false
        case Some(result) => if result.nonEmpty then out.println(result)
      out.flush()
  finally
    terminal.close()

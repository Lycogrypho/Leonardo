package it.grypho.scala.leonardo
package cli

import org.jline.reader.{Highlighter as JHighlighter, LineReader}
import org.jline.utils.{AttributedString, AttributedStringBuilder, AttributedStyle}

import java.util.regex.Pattern

/**
 * Colour mapping for one syntactic role in the Leonardo prompt.
 * `Plain` sets every field to `AttributedStyle.DEFAULT`, disabling all colouring.
 */
case class ColorScheme(
  command:  AttributedStyle,   // REPL command keywords (simplify, derive, colors, …)
  function: AttributedStyle,   // mathematical function names (sin, cos, exp, …)
  constant: AttributedStyle,   // named constants (pi, e, i)
  number:   AttributedStyle,   // numeric literals
  operator: AttributedStyle,   // arithmetic operators and :=
  equation: AttributedStyle,   // bare = (equation relation)
  paren:    AttributedStyle,   // parentheses and square brackets
  variable: AttributedStyle    // user identifiers and everything else
)

object ColorScheme:
  /** Optimised for dark / black terminal backgrounds. */
  val Dark: ColorScheme = ColorScheme(
    command  = AttributedStyle.DEFAULT.bold().foreground(AttributedStyle.YELLOW),
    function = AttributedStyle.DEFAULT.foreground(AttributedStyle.CYAN),
    constant = AttributedStyle.DEFAULT.foreground(AttributedStyle.MAGENTA),
    number   = AttributedStyle.DEFAULT.foreground(AttributedStyle.GREEN),
    operator = AttributedStyle.DEFAULT,
    equation = AttributedStyle.DEFAULT.bold(),
    paren    = AttributedStyle.DEFAULT,
    variable = AttributedStyle.DEFAULT
  )

  /** Optimised for light / white terminal backgrounds. */
  val Light: ColorScheme = ColorScheme(
    command  = AttributedStyle.DEFAULT.bold().foreground(AttributedStyle.BLUE),
    function = AttributedStyle.DEFAULT.foreground(AttributedStyle.GREEN),
    constant = AttributedStyle.DEFAULT.foreground(AttributedStyle.MAGENTA),
    number   = AttributedStyle.DEFAULT.foreground(AttributedStyle.RED),
    operator = AttributedStyle.DEFAULT,
    equation = AttributedStyle.DEFAULT.bold().foreground(AttributedStyle.RED),
    paren    = AttributedStyle.DEFAULT,
    variable = AttributedStyle.DEFAULT
  )

  /** No colouring — every token rendered in the terminal's default style. */
  val Plain: ColorScheme = ColorScheme(
    command  = AttributedStyle.DEFAULT,
    function = AttributedStyle.DEFAULT,
    constant = AttributedStyle.DEFAULT,
    number   = AttributedStyle.DEFAULT,
    operator = AttributedStyle.DEFAULT,
    equation = AttributedStyle.DEFAULT,
    paren    = AttributedStyle.DEFAULT,
    variable = AttributedStyle.DEFAULT
  )

  val All: Map[String, ColorScheme] = Map(
    "dark"  -> Dark,
    "light" -> Light,
    "none"  -> Plain
  )

  def named(name: String): ColorScheme = All.getOrElse(name, Dark)


/**
 * JLine `Highlighter` that token-colours the Leonardo prompt as the user types.
 *
 * A single left-to-right scan classifies each character run as one of: `:=` /
 * `:load` / `:save` (operators / pseudo-commands), numeric literals, word tokens
 * (command keyword, function, constant, or variable), and single-character operators
 * and brackets. The active scheme is fetched via `schemeName` on every keystroke, so
 * `colors dark` / `colors light` / `colors none` takes effect immediately.
 *
 * `reader` is never used; call `highlightBuffer` directly in tests and tooling.
 */
class LeonardoHighlighter(schemeName: () => String) extends JHighlighter:

  private val Commands  = Set("simplify", "expand", "eval", "precision", "env",
                               "unset", "help", "samples", "colors", "pretty", "quit", "exit")
  private val Functions = Set("sin", "cos", "tan", "tg", "asin", "acos", "atan",
                               "exp", "ln", "log", "transpose", "pow",
                               "integral", "derive", "solve", "solveSystem",
                               "limit", "laplace", "fourier", "invlaplace", "ode")
  private val Constants = Set("pi", "e", "i")

  private val NumPat  = raw"\d+(?:\.\d+)?(?:[eE][+-]?\d+)?".r
  private val WordPat = raw"[a-zA-Z][a-zA-Z0-9_]*".r

  def highlightBuffer(buffer: String): AttributedString =
    val cs         = ColorScheme.named(schemeName())
    val sb         = new AttributedStringBuilder()
    val firstNonWs = buffer.indexWhere(!_.isWhitespace)
    var pos        = 0

    def put(style: AttributedStyle, text: String): Unit =
      sb.style(style)
      sb.append(text)
      sb.style(AttributedStyle.DEFAULT)

    while pos < buffer.length do
      val rest = buffer.substring(pos)
      if rest.startsWith(":=") then
        put(cs.operator, ":=")
        pos += 2
      else if rest.startsWith(":") then
        // :load / :save pseudo-commands — colour the whole token up to whitespace
        val token = rest.takeWhile(!_.isWhitespace)
        put(cs.command, token)
        pos += token.length
      else NumPat.findPrefixMatchOf(rest) match
        case Some(m) =>
          put(cs.number, m.matched)
          pos += m.matched.length
        case None => WordPat.findPrefixMatchOf(rest) match
          case Some(m) =>
            val word  = m.matched
            val style =
              if pos == firstNonWs && Commands.contains(word) then cs.command
              else if Functions.contains(word)                then cs.function
              else if Constants.contains(word)               then cs.constant
              else                                                cs.variable
            put(style, word)
            pos += word.length
          case None =>
            val style = buffer(pos) match
              case '+' | '-' | '*' | '/' | '^' => cs.operator
              case '='                          => cs.equation
              case '(' | ')' | '[' | ']'       => cs.paren
              case _                            => cs.variable
            put(style, buffer(pos).toString)
            pos += 1

    sb.toAttributedString

  override def highlight(reader: LineReader, buffer: String): AttributedString =
    highlightBuffer(buffer)

  // JLine may invoke these on error-marking paths; we do not use error highlighting.
  override def setErrorPattern(errorPattern: Pattern): Unit = ()
  override def setErrorIndex(errorIndex: Int):         Unit = ()

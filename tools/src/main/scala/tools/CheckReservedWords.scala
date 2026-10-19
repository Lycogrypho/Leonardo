package it.grypho.scala.leonardo
package tools

import java.nio.file.{Path, Paths}

/** Fails if a word the grammar parses as a call is not in `Parser.ReservedWords` (F_0148).
 *
 *  Both lists are written by hand — about 230 `"word("` productions and the reserved set — and
 *  nothing compared them: `consolidate` was found unreserved by accident (F_0030).  A missing
 *  reservation is the costly direction, because the 2.13 rule makes it permanent: once a release
 *  has let a word be a variable, reserving it later breaks the saved scripts that use it.  So the
 *  check runs before a release can ship the gap.
 *
 *  **One direction only.**  A word reserved without a production is legitimate — a REPL command
 *  word, a constant, a coordinate system — so the reverse would need an allowlist longer than the
 *  check.  Slot words (`zoh`, `pooled`, `less`) never appear as `"word("`, so they need no
 *  exemption.  A word already released unreserved is listed in [[Released]] with its reason
 *  rather than reserved, for the same 2.13 rule.
 *
 *  The file is read as text, comments removed (they quote productions in prose), so the guard
 *  needs no dependency on the library.
 */
object CheckReservedWords:

  private val DefaultFile = Paths.get("core", "src", "main", "scala", "parser", "Parser.scala")

  /** Grammar words that are deliberately NOT reserved, each with the reason. */
  private val Released: Map[String, String] = Map.empty

  private val Production = """"([A-Za-z][A-Za-z0-9_]*)\("""".r
  private val Literal    = """"([^"\\]*)"""".r

  /** `text` with `//` and `/* */` comments blanked, string literals kept intact. */
  private def withoutComments(text: String): String =
    val out = new StringBuilder
    var i = 0
    var inString = false
    while i < text.length do
      val c = text(i)
      if inString then
        out += c
        if c == '\\' && i + 1 < text.length then { out += text(i + 1); i += 1 }
        else if c == '"' then inString = false
        i += 1
      else if c == '"' then { inString = true; out += c; i += 1 }
      else if text.startsWith("//", i) then
        while i < text.length && text(i) != '\n' do i += 1
      else if text.startsWith("/*", i) then
        val end = text.indexOf("*/", i + 2)
        i = if end < 0 then text.length else end + 2
      else { out += c; i += 1 }
    out.toString

  /** The `ReservedWords` literal block and the text outside it, or `None` when it is not found. */
  private def split(code: String): Option[(String, String)] =
    val start = code.indexOf("val ReservedWords")
    val open  = if start < 0 then -1 else code.indexOf('(', start)
    Option.when(open >= 0) {
      var depth = 0
      var j     = open
      var close = -1
      while close < 0 && j < code.length do
        code(j) match
          case '(' => depth += 1
          case ')' => depth -= 1; if depth == 0 then close = j
          case _   => ()
        j += 1
      (code.substring(open, math.max(close, open)), code.substring(0, start) + code.substring(math.max(close, open)))
    }

  /** Every grammar word that is called like a function but not reserved.
   *
   *  @param file the parser source
   *  @return one finding per word, empty when every production's word is reserved
   */
  def offences(file: Path): Vector[String] =
    split(withoutComments(Files.read(file))) match
      case None => Vector(s"$file: no `val ReservedWords` block found")
      case Some((block, grammar)) =>
        val reserved = Literal.findAllMatchIn(block).map(_.group(1)).toSet
        words(grammar).filterNot(w => reserved.contains(w) || Released.contains(w))
          .map(w => s"$file: \"$w(\" is a production, but `$w` is not in ReservedWords")

  /** The distinct words the grammar of `file` parses as calls, outside the reserved block — what
   *  the check compares, exposed so a test can see it is not comparing nothing.
   *  @param file the parser source
   *  @return the words, sorted */
  def productions(file: Path): Vector[String] =
    split(withoutComments(Files.read(file))).fold(Vector.empty)((_, grammar) => words(grammar))

  private def words(grammar: String): Vector[String] =
    Production.findAllMatchIn(grammar).map(_.group(1)).toVector.distinct.sorted

  /** The check, as an exit code, over the given file or the parser.
   *
   *  @param args one source file; empty means the parser
   *  @return 1 when a production's word is unreserved, 0 otherwise
   */
  def run(args: Seq[String]): Int =
    val file = args.headOption.map(Paths.get(_)).getOrElse(DefaultFile)
    Files.report(
      offences(file),
      n => s"\n$file: $n unreserved production word(s)",
      "A word the grammar parses as a call must be reserved, or a binding of that name is\n" +
      "unreachable (F_0148). Add it to ReservedWords before it ships: once released, a word\n" +
      "cannot be reclaimed (2.13); list it in CheckReservedWords.Released with its reason instead.")

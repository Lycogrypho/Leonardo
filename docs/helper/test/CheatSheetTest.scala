package it.grypho.scala.leonardo
package docs

import cli.Session
import org.scalatest.flatspec.AnyFlatSpec

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

/** Executes the cheat sheet's *Result* column (D_0274, the owner's Decision 1).
 *
 *  `docs/src/cheatsheet.md` is hand-written, and D_0262 found seven results the REPL does not
 *  give — every one of which running the row would have caught.  This suite reads every table
 *  whose header has a *Syntax* and a *Result* column, and for each row whose two cells are each
 *  one code span runs the syntax through a fresh [[cli.Session]] and compares the printed answer.
 *  A row whose result is prose, empty or several spans is not a claim this suite can check, and is
 *  left alone.
 *
 *  **Setup**: a table whose rows need state (exact mode, a binding) is preceded by an HTML comment
 *  `<!-- cheatsheet-setup: line; line -->`, invisible on the site, whose lines run before each row.
 */
class CheatSheetTest extends AnyFlatSpec:

  import CheatSheetTest.*

  private lazy val sheet: String = new String(Files.readAllBytes(locate()), StandardCharsets.UTF_8)

  "the cheat sheet" should "have executable rows to check" in {
    // A reader that silently found nothing would make the suite vacuous.
    assert(rows(sheet).size >= 100, s"only ${rows(sheet).size} executable rows found")
  }

  it should "show, in every executable row, the answer the REPL gives" in {
    val wrong = rows(sheet).flatMap { r =>
      val session = new Session()
      r.setup.foreach(session.execute)
      val answer = session.execute(r.syntax).trim
      Option.when(answer != r.result)(s"line ${r.line}: `${r.syntax}` shows `${r.result}`, the REPL gives `$answer`")
    }
    assert(wrong.isEmpty, wrong.mkString("\n", "\n", ""))
  }

  "the reader" should "split cells on unescaped bars and honour a setup comment" in {
    val text =
      """<!-- cheatsheet-setup: exact on; a := 2 -->
        || Syntax | Result | Notes |
        ||---|---|---|
        || `a \|\| b` | `x` | n |
        || `1 + 1` | two | prose result, skipped |
        || `2 + 2` | `4.0` | |
        |
        || Syntax | Result |
        ||---|---|
        || `3` | `3.0` |
        |""".stripMargin
    val found = rows(text)
    assert(found.map(_.syntax) == List("a || b", "2 + 2", "3"))
    assert(found.head.setup == List("exact on", "a := 2"))
    assert(found.last.setup.isEmpty, "a setup comment applies to the next table only")
  }


object CheatSheetTest:

  /** One executable row: its 1-based line, the setup lines, the syntax and the shown result. */
  final case class Row(line: Int, setup: List[String], syntax: String, result: String)

  private val SetupComment = """<!--\s*cheatsheet-setup:(.*?)-->""".r
  private val CodeSpan     = """`([^`]+)`""".r

  /** The cells of a table row, split on bars that are not escaped, `\|` read as `|`. */
  private def cells(line: String): List[String] =
    line.trim.stripPrefix("|").stripSuffix("|").split("""(?<!\\)\|""", -1).toList.map(_.trim.replace("\\|", "|"))

  /** The text of a cell that is exactly one code span. */
  private def onlySpan(cell: String): Option[String] = cell match
    case CodeSpan(s) => Some(s)
    case _           => None

  /** Every executable row of `text`, in order. */
  def rows(text: String): List[Row] =
    val lines = text.linesIterator.toVector
    // A table: consecutive lines starting with `|`; the header is its first line.
    val starts = lines.indices.filter(i => lines(i).trim.startsWith("|") && (i == 0 || !lines(i - 1).trim.startsWith("|")))
    starts.toList.flatMap { s =>
      val header = cells(lines(s))
      val (syntaxCol, resultCol) = (header.indexOf("Syntax"), header.indexOf("Result"))
      if syntaxCol < 0 || resultCol < 0 then Nil
      else
        val setup = lines.take(s).reverseIterator.map(_.trim).dropWhile(_.isEmpty).nextOption() match
          case Some(SetupComment(body)) => body.split(';').toList.map(_.trim).filter(_.nonEmpty)
          case _                        => Nil
        val body = lines.indices.drop(s + 2).takeWhile(i => lines(i).trim.startsWith("|"))
        body.toList.flatMap { i =>
          val c = cells(lines(i))
          for
            syntax <- c.lift(syntaxCol).flatMap(onlySpan)
            result <- c.lift(resultCol).flatMap(onlySpan)
          yield Row(i + 1, setup, syntax, result)
        }
    }

  /** `docs/src/cheatsheet.md`, found from the working directory upward, so the suite runs from
   *  sbt (the repository root) and from an IDE whose working directory is the module. */
  def locate(): Path =
    Iterator.iterate(Paths.get("").toAbsolutePath)(_.getParent).takeWhile(_ != null)
      .map(_.resolve("docs").resolve("src").resolve("cheatsheet.md"))
      .find(Files.isRegularFile(_))
      .getOrElse(throw new IllegalStateException("docs/src/cheatsheet.md not found above the working directory"))

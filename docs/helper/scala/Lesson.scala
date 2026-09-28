package it.grypho.scala.leonardo
package docs

import cli.Session

/** A worked REPL transcript for a lesson page, executed at build time (issue D_0003).
 *
 *  **Verification and presentation were competing, and this is what separates them.**
 *  `docs/src/repl.md` already proves a REPL example by running it inside an `mdoc` block, so the
 *  transcripts on this site are real output rather than typed-out examples — that is the
 *  build-time guarantee which decided the course lives here at all.  What it cannot do is *look*
 *  right for a lesson: mdoc renders the call and its result, so a reader meets
 *  `s.execute("x := 3")` and a `res: String`, which is Scala scaffolding around the one thing
 *  they are being taught to type.  Correct on a page about embedding a session; wrong on a page
 *  teaching the prompt, where it shows a surface the reader will never use.
 *
 *  So a lesson calls [[show]] from an `mdoc:passthrough` block, whose printed output is spliced
 *  into the page as raw markdown.  The session really runs; the reader sees only the prompt and
 *  the answer.
 *
 *  {{{
 *  ```scala mdoc:silent
 *  val lesson = Lesson()
 *  ```
 *
 *  ```scala mdoc:passthrough
 *  lesson.show("x := 3", "derive(sin(x), x)")
 *  ```
 *  }}}
 *
 *  **One instance per page, reused across blocks**, exactly as `repl.md` reuses its `Session`:
 *  a lesson builds up state, and a fresh session per block would silently un-define every name
 *  the previous paragraph introduced.
 *
 *  **Two things it deliberately does not do.**  It does not render a `plot` — that command is
 *  intercepted by the browser page and never reaches `Session`, so executing `plot …` here would
 *  quietly produce the echo of a free variable rather than a figure, and a plotting lesson must
 *  carry a run link and a static image instead.  And it does not yet emit that run link: the
 *  encoder it needs must agree with `web.ShareLink`'s `decodeURIComponent` and is filed
 *  separately, which costs nothing now because **the signature already absorbs it** — links will
 *  appear by changing this class, with no lesson rewritten.
 */
final class Lesson:

  /** The session every call shares, so a page reads as one continuous sitting. */
  private val session = Session()

  /** Executes each line and prints the transcript block for `mdoc:passthrough` to splice in.
   *
   *  @param lines the commands, exactly as a reader would type them at the prompt
   */
  def show(lines: String*): Unit = println(render(lines))

  /** Builds the fenced transcript for `lines`, running each through the shared session.
   *
   *  Fenced as `text` rather than `scala`: what it contains is the REPL's own command language,
   *  which is not Scala, and highlighting it as Scala would misinform the reader it is aimed at.
   *  A command whose answer is empty — a setting, for instance — contributes its prompt line and
   *  nothing else, since a blank line under it would read as an answer that happened to be blank.
   *
   *  @param lines the commands to run, in order
   *  @return the transcript, fenced and ready to splice into a page
   */
  private[docs] def render(lines: Seq[String]): String =
    val body = lines.flatMap(line => s"> $line" +: answerOf(line))
    (("```text" +: body) :+ "```").mkString("\n")

  /** The answer lines for one command: its output split into lines, or nothing when it is empty. */
  private def answerOf(line: String): Seq[String] =
    session.execute(line) match
      case ""     => Seq.empty
      case answer => answer.linesIterator.toSeq

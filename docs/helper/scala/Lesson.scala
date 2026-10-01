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
 *  **Each transcript carries a run link** (issue F_0045): the same executed session that printed
 *  the block also yields the URL that reproduces it, because `Session.script` percent-encoded *is*
 *  the fragment `web.ShareLink` reads.  The link therefore costs one call and reproduces the state
 *  **at that point in the lesson**, so a reader may join at any paragraph rather than only at the
 *  top.  The encoder is `cli.ShareEncoding`, shared with the page so the two cannot disagree; when
 *  it refuses — a session too long to carry — the link is simply omitted, since a truncated one
 *  would `:load` as a valid but different session.
 *
 *  **The link always points at the published app**, never at a local build.  A locally generated
 *  page therefore sends a reader to the live site, which is the right trade: the alternative is a
 *  link that works only on the machine that built it, and these pages exist to be published.
 *
 *  **One thing it deliberately does not do.**  It does not render a `plot` — that command is
 *  intercepted by the browser page and never reaches `Session`, so executing `plot …` here would
 *  quietly produce the echo of a free variable rather than a figure.  A plotting lesson pairs the
 *  run link above with a static image instead.
 */
final class Lesson:

  /** The session every call shares, so a page reads as one continuous sitting. */
  private val session = Session()

  /** Executes each line and prints the transcript block for `mdoc:passthrough` to splice in.
   *
   *  @param lines the commands, exactly as a reader would type them at the prompt
   */
  def show(lines: String*): Unit = println(page(lines))

  /** The transcript for `lines` followed by its run link, which is what [[show]] prints.
   *
   *  Separate from [[render]] so that each half is pinned on its own: `render`'s contract is the
   *  fenced block and nothing else, and a link that cannot be built must leave that block
   *  untouched rather than appended to with an apology.
   *
   *  @param lines the commands to run, in order
   *  @return the block, and the link beneath it when one could be built
   */
  private[docs] def page(lines: Seq[String]): String =
    val block = render(lines)
    runLink.fold(block)(link => s"$block\n\n$link")

  /** A link that opens the browser REPL on the session as it stands, or `None` if it is too big.
   *
   *  Read **after** the lines have run, so the link carries the state the reader has just been
   *  shown rather than the state before it.
   */
  private[docs] def runLink: Option[String] =
    cli.ShareEncoding.fragmentFor(session.script).toOption
      // Short, because a page carries one of these per transcript -- up to fourteen on the longer
      // lessons -- and a full sentence repeated that often reads as clutter rather than an
      // affordance. Under a block, three words say everything. Plain ASCII: a decorative glyph
      // would be one more thing for the charset guard to have to allow.
      .map(fragment => s"*[run this session](${Lesson.AppUrl}#$fragment)*")

  /** The session's own script, so a test can compare a reloaded session against this one. */
  private[docs] def sessionScript: String = session.script

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

  /** The answer lines for one command: its output split into lines, or nothing when it is empty.
   *
   *  Blank lines at either end are dropped.  They carry no information in a transcript — an
   *  answer is the text, not its surrounding whitespace — and at least one command emits a
   *  leading newline before a matrix, which would otherwise read as an empty first result.
   */
  private def answerOf(line: String): Seq[String] =
    session.execute(line) match
      case ""     => Seq.empty
      case answer => answer.linesIterator.toSeq.dropWhile(_.isBlank).reverse.dropWhile(_.isBlank).reverse


/** Where a lesson's run link points. */
object Lesson:

  /** The published browser REPL.
   *
   *  Absolute and hard-coded on purpose: `/app` is assembled *after* Jekyll runs, so a relative
   *  link to it fails mdoc's own link check — the same reason every `/api` reference on this site
   *  is absolute.
   */
  val AppUrl: String = "https://lycogrypho.github.io/Leonardo/app/"

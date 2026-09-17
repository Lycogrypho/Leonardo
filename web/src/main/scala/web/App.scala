package it.grypho.scala.leonardo
package web

import scala.scalajs.js
import scala.scalajs.js.Dynamic.global
import scala.scalajs.js.annotation.JSExportTopLevel

import cli.Session

/** The browser REPL (issue F_0003 phase 3): one page, one [[cli.Session]], no server.
 *
 *  **Almost nothing of Leonardo is here.**  `Session.step` already dispatches every command
 *  including `:save` and `:load`, on both platforms, so this file is a text box, a transcript
 *  and a history ring — the same finding that made phase 2 cheap, collected at the point where
 *  it pays off.  What is genuinely browser-only is exactly two things: the `plot` / `points`
 *  commands, which cannot exist on a terminal, and the shareable link.
 *
 *  **The DOM is reached through `js.Dynamic` rather than a typed binding, deliberately.**  The
 *  surface used is six calls wide, `cli.SessionIO` already established `js.Dynamic.global` as
 *  the house idiom for a browser API, and the alternative is a dependency whose code would ship
 *  to every reader of the Pages site.  Revisit if this file ever grows a real widget; it should
 *  not.
 */
object App:

  /** How many past lines ↑ can reach.  Bounded because the transcript is the real record; the
   *  ring is a convenience and an unbounded one would grow for a page that is never reloaded.
   */
  private val MaxHistory = 200

  /** Shown for a bare `plot` / `points`, and substituted into the sampler's own usage line so
   *  the message names the command the user actually typed.
   */
  private val PlotUsage = "usage: plot <expr> <var> <lo> <hi> [<n>]"

  private val session = new Session()
  private var history: Vector[String] = Vector.empty
  private var cursor: Int             = 0

  private def document = global.document
  private def byId(id: String): js.Dynamic = document.getElementById(id)

  /** Starts the page.  Called by a `<script>` tag once `main.js` has loaded.
   *
   *  Exported under a stable name because `index.html` names it: the linker would otherwise
   *  discard the whole object as unreachable, exactly as it discards a library with no entry
   *  point.
   */
  @JSExportTopLevel("leonardoStart")
  def start(): Unit =
    val input = byId("input")
    input.addEventListener("keydown", (e: js.Dynamic) => onKey(input, e))
    byId("share").addEventListener("click", (_: js.Dynamic) => share())
    byId("clear").addEventListener("click", (_: js.Dynamic) => clearTranscript())

    restoreFromLink()
    note("Type an expression, or `help`. Nothing you type leaves this tab.")
    input.focus()

  /** Enter submits; the arrow keys walk the history. */
  private def onKey(input: js.Dynamic, e: js.Dynamic): Unit =
    e.key.asInstanceOf[String] match
      case "Enter" =>
        val line = input.value.asInstanceOf[String]
        input.value = ""
        submit(line)
      case "ArrowUp"   => e.preventDefault(); recall(input, -1)
      case "ArrowDown" => e.preventDefault(); recall(input, +1)
      case _           => ()

  /** Moves through the history ring and puts the result in the input box. */
  private def recall(input: js.Dynamic, delta: Int): Unit =
    if history.nonEmpty then
      // Clamped to `history.size`, one past the last entry, which is the empty line the user
      // started from -- so pressing Down past the newest entry clears the box rather than
      // sticking on it.
      cursor = math.max(0, math.min(history.size, cursor + delta))
      input.value = if cursor == history.size then "" else history(cursor)

  /** Runs one line and shows what it produced. */
  private def submit(line: String): Unit =
    val trimmed = line.trim
    if trimmed.nonEmpty then
      history = (history :+ trimmed).takeRight(MaxHistory)
      cursor  = history.size
      echo(trimmed)
      // The two browser-only commands are intercepted here rather than added to `Session`,
      // which keeps `Session` platform-neutral: a `plot` command on a terminal REPL would be
      // a command that cannot do anything. This is how the JVM loop treats `:save` too.
      val out = trimmed match
        // The bare forms are matched FIRST and explicitly. Without them `plot` alone falls
        // through to the evaluator, where it is a perfectly good free variable and echoes
        // itself back -- a silently useless answer instead of the usage line. `plot` and
        // `points` are deliberately NOT added to Parser.ReservedWords: the commands exist
        // only in the browser, and reserving the names would tax the terminal REPL, where
        // they mean nothing, for a feature it does not have (the issue-2.13 rule).
        case "plot" | "points" => PlotUsage
        case s"plot $rest"     => plot(rest, geometric = false)
        case s"points $rest"   => plot(rest, geometric = true)
        case other =>
          Session.step(session, Some(other))
            .getOrElse("(session ended — reload the page to start another)")
      if out.nonEmpty then write(out, "out")

  /** Draws a figure, and answers with the message the transcript should carry.
   *
   *  @param geometric whether the axes must share a scale — true for coordinates in a plane,
   *                   false for `y = f(x)`; see [[PlotSpec]] for why that distinction is not
   *                   cosmetic
   */
  private def plot(rest: String, geometric: Boolean): String =
    session.samplePoints(rest) match
      // The sampler's messages name `samples`, which is the terminal command; re-point them
      // at the one that was actually typed rather than explaining a command this page lacks.
      case Left(why) if why.startsWith("usage:") => PlotUsage
      case Left(why) => why.replace("samples:", "plot:")
      case Right(r) if r.points.isEmpty => "(no finite values in range)"
      case Right(r) =>
        val spec =
          if geometric then PlotSpec.coordinates(r.points, r.expr)
          else PlotSpec.line(r.points, r.expr, r.variable)
        Plot.draw(spec, byId("figure")) match
          case Right(_)  => s"(plotted ${r.points.size} points)"
          case Left(why) => why

  /** Copies a link that restores this session. */
  private def share(): Unit =
    ShareLink.encode(session.script) match
      case Left(why) => note(why)
      case Right(fragment) =>
        val base = global.location.href.asInstanceOf[String].takeWhile(_ != '#')
        val url  = s"$base#$fragment"
        global.location.hash = fragment
        // The clipboard API is unavailable on an insecure origin and may be refused outright,
        // so the URL bar -- already updated above -- is the fallback that always works.
        if js.typeOf(global.navigator.clipboard) != "undefined" then
          global.navigator.clipboard.writeText(url)
          note("link copied to the clipboard")
        else note("link is in the address bar")

  /** Replays a session carried in the URL fragment, if there is one. */
  private def restoreFromLink(): Unit =
    ShareLink.decode(global.location.hash.asInstanceOf[String]).foreach { script =>
      val out = session.load(script)
      note("restored a shared session from this link")
      if out.nonEmpty then write(out, "out")
    }

  private def echo(line: String): Unit = write(s"> $line", "echo")
  private def note(line: String): Unit = write(line, "note")

  /** Appends one block to the transcript and scrolls it into view.
   *
   *  Written through `textContent`, never `innerHTML`: results contain `<`, `>` and `&` in the
   *  ordinary course of things (`x < 2`, `a and b`), and building markup out of them would
   *  both mangle the output and make every result an injection site.
   */
  private def write(text: String, kind: String): Unit =
    val transcript = byId("transcript")
    val block      = document.createElement("div")
    block.className   = kind
    block.textContent = text
    transcript.appendChild(block)
    transcript.scrollTop = transcript.scrollHeight

  private def clearTranscript(): Unit =
    byId("transcript").innerHTML = ""
    Plot.clear(byId("figure"))

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

  /** Shown for a bare `bode` / `nyquist`.  The bounds are a frequency BAND, not a range, and
   *  the message says so — sweeping from zero is the mistake a newcomer makes first, and a
   *  geometric grid cannot start there.
   */
  private val SweepUsage = "usage: bode <expr> <var> <wMin> <wMax> [<n>]   (wMin > 0)"

  /** A sweep that produced nothing is almost always a band starting at zero, so the message
   *  names that rather than leaving the reader to guess at an empty figure.
   */
  private val EmptySweep =
    "(no response over that band — the frequency grid is logarithmic, so wMin must be > 0)"

  private val session = new Session()
  private var history: Vector[String] = Vector.empty
  private var cursor: Int             = 0

  /** The settings panel's fields, paired with what they edit.  Built once and then only
   *  *written to* — rebuilding the row on every line would take the caret out of a number
   *  box while it was being typed in.
   */
  private var controls: Vector[(Settings.Control, js.Dynamic)] = Vector.empty

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
    val mathfield = byId("mathfield")
    byId("mathsubmit").addEventListener("click", (_: js.Dynamic) => submitMath(mathfield))
    buildSettings()

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

  /** Submits what the math field holds, or shows why it could not be read (issue F_0017).
   *
   *  **The conversion is two steps and only the first is MathLive's**: the field holds LaTeX,
   *  `convertLatexToAsciiMath` turns that into AsciiMath, and `cli.AsciiMath` turns *that* into
   *  grammar text — the half that can refuse, and the half that is unit-tested on the JVM.
   *
   *  **A refusal is shown, never submitted.**  The grammar has no unknown-identifier error, so
   *  handing it something unconvertible would print a confident product rather than a
   *  complaint; that is the whole reason the reader refuses on its own authority.  The field is
   *  left as the user wrote it so the mistake can be corrected rather than retyped.
   */
  private def submitMath(field: js.Dynamic): Unit =
    val error = byId("matherror")
    error.textContent = ""
    val latex = Option(field.value).map(_.asInstanceOf[String]).getOrElse("")
    if latex.trim.nonEmpty then
      toAsciiMath(latex) match
        case None => error.textContent = "the editor is unavailable; type the expression instead"
        case Some(asciiMath) =>
          cli.AsciiMath.toGrammar(asciiMath) match
            case Left(message) => error.textContent = message.trim
            case Right(text)   =>
              field.value = ""
              submit(text, typedLatex = Some(latex))

  /** MathLive's LaTeX-to-AsciiMath conversion, or `None` when the vendored build is missing.
   *
   *  `js.typeOf` rather than a null check, for the reason [[MathRender.available]] gives:
   *  reading an undeclared name raises `ReferenceError` before any comparison could run.
   */
  private def toAsciiMath(latex: String): Option[String] =
    Option.when(js.typeOf(global.leonardoLatexToAsciiMath) != "undefined")(
      global.leonardoLatexToAsciiMath(latex).asInstanceOf[String])

  /** Moves through the history ring and puts the result in the input box. */
  private def recall(input: js.Dynamic, delta: Int): Unit =
    if history.nonEmpty then
      // Clamped to `history.size`, one past the last entry, which is the empty line the user
      // started from -- so pressing Down past the newest entry clears the box rather than
      // sticking on it.
      cursor = math.max(0, math.min(history.size, cursor + delta))
      input.value = if cursor == history.size then "" else history(cursor)

  /** Runs one line and shows what it produced.
   *
   *  @param line       the grammar text to run — whatever its origin, this is what executes,
   *                    joins the history and would be `:save`d
   *  @param typedLatex the formula a math-field submission held, when there was one; echoed
   *                    typeset BESIDE the grammar echo (issue F_0032), because the formula
   *                    used to vanish the instant Enter was pressed, which read as the editor
   *                    being ignored. The grammar echo stays: it is the canonical spelling of
   *                    what was drawn, and hiding it would teach the reader nothing about how
   *                    to type the same thing at the prompt.
   */
  private def submit(line: String, typedLatex: Option[String] = None): Unit =
    val trimmed = line.trim
    if trimmed.nonEmpty then
      history = (history :+ trimmed).takeRight(MaxHistory)
      cursor  = history.size
      echo(trimmed)
      // Class "math echo", so it composes the two existing styles -- a formula, in the echo's
      // accent colour -- and stays OFF the plain "math" class the result channel uses.
      typedLatex.foreach(writeMath(_, kind = "math echo"))
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
        case "plot" | "points"   => PlotUsage
        case "bode" | "nyquist"  => SweepUsage
        case s"plot $rest"       => plot(rest, geometric = false)
        case s"points $rest"     => plot(rest, geometric = true)
        case s"bode $rest"       => bode(rest)
        case s"nyquist $rest"    => nyquist(rest)
        case other =>
          Session.step(session, Some(other))
            .getOrElse("(session ended — reload the page to start another)")
      if out.nonEmpty then
        // `latex on` fills the session's side channel with the LaTeX of this result -- and
        // only of THIS one, since it is cleared per command. Typesetting is attempted here
        // and nowhere else: if the renderer did not load, or the answer was not an
        // expression, the text is all that goes up.
        //
        // BOTH are shown, text first (F_0031). The formula used to REPLACE the text, which
        // made `latex on` silently swallow everything `pretty` did and left that setting
        // looking broken; and the text is the canonical grammar form -- what `:save` writes
        // and what a reader has to retype -- so it is the one thing that may not be hidden.
        // `latex on` turns `pretty` off in the session, so the text sitting beside the
        // formula is the compact form rather than the same layout twice.
        write(out, "out")
        session.lastLatex.foreach(l => writeMath(l))
      // Every line can change a setting -- `latex on` typed at the prompt must move the
      // checkbox, or the panel becomes a second, stale source of truth for state the command
      // language already owns. Refreshing unconditionally costs one pass over seven fields.
      refreshSettings()

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
        draw(spec, r.points.size)

  /** Draws a Bode diagram — gain and unwrapped phase over a logarithmic frequency axis. */
  private def bode(rest: String): String =
    session.bodePoints(rest) match
      case Left(why) if why.startsWith("usage:") => SweepUsage
      case Left(why)                             => why
      case Right(r) if r.points.isEmpty          => EmptySweep
      case Right(r) =>
        draw(PlotSpec.bode(r.points, s"${r.expr}   (${r.variable} → iω)"), r.points.size)

  /** Draws a Nyquist diagram — the same sweep in the complex plane, axes locked. */
  private def nyquist(rest: String): String =
    session.nyquistPoints(rest) match
      case Left(why) if why.startsWith("usage:") => SweepUsage
      case Left(why)                             => why
      case Right(r) if r.points.isEmpty          => EmptySweep
      case Right(r) =>
        draw(PlotSpec.coordinates(r.points, s"${r.expr}   (${r.variable} → iω)"), r.points.size)

  /** Hands a spec to the page and answers with the line the transcript should carry. */
  private def draw(spec: String, count: Int): String =
    Plot.draw(spec, byId("figure")) match
      case Right(_)  => s"(plotted $count points)"
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

  /** Appends one typeset block, answering whether it was typeset.
   *
   *  The block is appended **only on success**, so a renderer that did not load leaves no
   *  empty row behind for the caller's plain-text fallback to sit under.
   *
   *  @param latex math-mode source — the session's LaTeX channel, or a math-field echo
   *  @param kind  the block's class: `"math"` for a result, `"math echo"` for an input
   *               (issue F_0032); the distinction is what keeps a consumer reading results
   *               from picking up echoes
   *  @return whether the transcript gained a formula
   */
  private def writeMath(latex: String, kind: String = "math"): Boolean =
    val block = document.createElement("div")
    if !MathRender.render(latex, block) then false
    else
      block.className = kind
      val transcript = byId("transcript")
      transcript.appendChild(block)
      transcript.scrollTop = transcript.scrollHeight
      true

  private def clearTranscript(): Unit =
    byId("transcript").innerHTML = ""
    Plot.clear(byId("figure"))

  // ── the settings panel (F_0016 step 6) ───────────────────────────────────────
  //
  // Which settings exist, in what order, and with what values is the SESSION's to say -- see
  // Settings, where everything decidable without a DOM lives. What is left here is element
  // creation, and a changed control goes straight back through `submit`: the panel issues the
  // command a user would have typed rather than a second way of setting the same thing.

  /** Builds the panel once from the session's settings. */
  private def buildSettings(): Unit =
    val bar = byId("settings")
    controls = Settings.panel(session.settings).map { (control, value) =>
      val field = fieldFor(control, value)
      field.id = Settings.id(control.setting)
      field.addEventListener("change", (_: js.Dynamic) => changed(control, field))
      bar.appendChild(labelled(control.label, field))
      control -> field
    }.toVector

  /** One `<label>` carrying the setting's name and its field. */
  private def labelled(text: String, field: js.Dynamic): js.Dynamic =
    val label = document.createElement("label")
    label.className = "setting"
    val caption = document.createElement("span")
    caption.textContent = text
    label.appendChild(caption)
    label.appendChild(field)
    label

  /** The input element a control's widget calls for, already showing `value`. */
  private def fieldFor(control: Settings.Control, value: String): js.Dynamic =
    control.widget match
      case Settings.Widget.Toggle =>
        val box = document.createElement("input")
        box.`type` = "checkbox"
        box.checked = Settings.isOn(value)
        box
      case Settings.Widget.Number(min) =>
        val box = document.createElement("input")
        box.`type` = "number"
        box.min    = min.toString
        box.value  = value
        box
      case Settings.Widget.Choice(options) =>
        val select = document.createElement("select")
        options.foreach { name =>
          val option = document.createElement("option")
          option.value       = name
          option.textContent = name
          select.appendChild(option)
        }
        select.value = value
        select

  /** What a field currently stands for, as the setting's argument. */
  private def valueOf(control: Settings.Control, field: js.Dynamic): String =
    control.widget match
      case Settings.Widget.Toggle => Settings.toggleValue(field.checked.asInstanceOf[Boolean])
      case _                      => field.value.asInstanceOf[String]

  /** Runs the command a changed control stands for.
   *
   *  It goes through [[submit]], the prompt's own path, so the transcript records it and the
   *  refusal of a bad value reads exactly as it would if it had been typed — and `submit`
   *  refreshes the panel afterwards, which is what snaps a refused value back.
   */
  private def changed(control: Settings.Control, field: js.Dynamic): Unit =
    submit(Settings.command(control.setting, valueOf(control, field)))

  /** Writes the session's current settings into the fields. */
  private def refreshSettings(): Unit =
    val current = session.settings.toMap
    controls.foreach { (control, field) =>
      current.get(control.setting).foreach { value =>
        control.widget match
          case Settings.Widget.Toggle => field.checked = Settings.isOn(value)
          case _                      => field.value = value
      }
    }

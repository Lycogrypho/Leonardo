package it.grypho.scala.leonardo
package web

import scala.scalajs.js
import scala.scalajs.js.Dynamic.{global, literal}

/** A DOM small enough to drive [[App]] and nothing more (issue F_0027).
 *
 *  **There is no browser here, so this cannot prove the page looks right — it proves the part
 *  that actually breaks**: that every element id `App` reaches for exists in `index.html`,
 *  that the key handler runs, that a line round-trips through `cli.Session.step`, and that
 *  `plot` produces a document Plotly could read.  A typo in an id is otherwise invisible until
 *  someone opens the page.
 *
 *  **The ids below must be exactly the ones `index.html` defines**, and that agreement is what
 *  is under test: `getElementById` throws for anything else, so a rename on one side alone
 *  fails loudly here rather than silently in a browser.
 *
 *  Ported from a Node script by F_0027.  The one property the port cannot carry is that the
 *  **linked** bundle starts — a suite exercises the classes, not the artifact a browser
 *  downloads — so `.github/scripts/check-web-app.js` survives as a three-line smoke test for
 *  exactly that.
 */
object FakeDom:

  /** Every id `index.html` defines and `App` reaches for. */
  val Ids: Vector[String] = Vector("input", "transcript", "figure", "share", "clear", "settings")

  /** The last document handed to Plotly, so a test can read what would have been drawn. */
  var drawn: js.Dynamic = null

  /** Every LaTeX string handed to the typesetter, newest last. */
  var typeset: Vector[String] = Vector.empty

  private var elements: Map[String, js.Dynamic] = Map.empty

  /** One element: the properties `App` reads or writes, and nothing else.
   *
   *  `checked` and `type` are plain properties, which is all the settings panel needs — it
   *  creates inputs and a select, then reads and writes those two and `value`.
   */
  def element(id: String): js.Dynamic =
    val el = literal(
      id = id, value = "", className = "", textContent = "", innerHTML = "",
      `type` = "", checked = false, scrollTop = 0, scrollHeight = 0
    )
    el.children  = js.Array[js.Dynamic]()
    el.listeners = literal()
    el.addEventListener = { (kind: String, fn: js.Function1[js.Dynamic, Unit]) =>
      val bucket = el.listeners.selectDynamic(kind)
      if js.isUndefined(bucket) then el.listeners.updateDynamic(kind)(js.Array(fn))
      else bucket.asInstanceOf[js.Array[js.Function1[js.Dynamic, Unit]]].push(fn)
    }: js.Function2[String, js.Function1[js.Dynamic, Unit], Unit]
    el.appendChild = { (child: js.Dynamic) =>
      el.children.asInstanceOf[js.Array[js.Dynamic]].push(child)
      ()
    }: js.Function1[js.Dynamic, Unit]
    el.focus = (() => ()): js.Function0[Unit]
    el

  /** The global object, reached as an ordinary value.
   *
   *  **Not `js.Dynamic.global.document = …` directly**: Scala.js compiles a member of
   *  `js.Dynamic.global` into a *bare global reference*, so that assignment emits
   *  `document = …` — and the linker's output is strict-mode, where assigning to an undeclared
   *  name is a `ReferenceError` rather than a global quietly appearing.  Going through
   *  `globalThis`, which *is* declared, makes it an ordinary property assignment.  Reading is
   *  unaffected, which is why [[App]] and [[Plot]] need no change.
   */
  private def g: js.Dynamic = global.globalThis

  /** Installs the fake globals and starts the page.  Call once per suite. */
  def install(): Unit =
    elements = Ids.map(id => id -> element(id)).toMap
    drawn    = null
    typeset  = Vector.empty

    val doc = literal()
    doc.getElementById = { (id: String) =>
      elements.getOrElse(id,
        throw new NoSuchElementException(s"App reached for an element index.html does not define: #$id"))
    }: js.Function1[String, js.Dynamic]
    doc.createElement = ((_: String) => element("div")): js.Function1[String, js.Dynamic]
    g.document = doc

    g.location = literal(href = "https://example.invalid/app/", hash = "")
    // `navigator` is deliberately NOT stubbed: Node defines it read-only, so assigning throws
    // -- and nothing here needs it. The share path reads `navigator.clipboard`, finds it
    // undefined and falls back to the address bar, which is the branch a test would want
    // anyway; the clipboard one cannot be exercised without a browser.

    // Plotly is stubbed rather than loaded: the real one needs layout and measurement a fake
    // DOM cannot provide, and what matters here is the document handed to it.
    val plotly = literal()
    plotly.react = { (_: js.Dynamic, data: js.Dynamic, layout: js.Dynamic) =>
      drawn = literal(data = data, layout = layout)
    }: js.Function3[js.Dynamic, js.Dynamic, js.Dynamic, Unit]
    plotly.purge = ((_: js.Dynamic) => ()): js.Function1[js.Dynamic, Unit]
    g.Plotly = plotly

    // MathLive likewise, and the stub is deliberately recognisable: what is under test is that
    // the page CALLS it with the LaTeX the session produced and puts the result in the
    // transcript -- not that MathLive can typeset, which is its own problem.
    g.leonardoLatexToMarkup = { (latex: String) =>
      typeset = typeset :+ latex
      s"""<span class="ml">$latex</span>"""
    }: js.Function1[String, String]

    App.start()

  /** The element behind an id, for a test that needs to poke at it. */
  def byId(id: String): js.Dynamic = elements(id)

  private def fire(el: js.Dynamic, kind: String, event: js.Dynamic): Unit =
    val bucket = el.listeners.selectDynamic(kind)
    if !js.isUndefined(bucket) then
      bucket.asInstanceOf[js.Array[js.Function1[js.Dynamic, Unit]]].foreach(_(event))

  /** Types a line into the prompt and presses Enter. */
  def typeLine(line: String): Unit =
    val input = byId("input")
    input.value = line
    fire(input, "keydown", literal(key = "Enter", preventDefault = (() => ()): js.Function0[Unit]))

  /** Presses a key in the prompt without typing anything. */
  def press(key: String): Unit =
    fire(byId("input"), "keydown",
         literal(key = key, preventDefault = (() => ()): js.Function0[Unit]))

  /** Fires a `change` on a settings control, as a browser does when a value is edited. */
  def change(el: js.Dynamic): Unit = fire(el, "change", literal())

  private def blocks(kind: String): Vector[js.Dynamic] =
    byId("transcript").children.asInstanceOf[js.Array[js.Dynamic]].toVector
      .filter(_.className.asInstanceOf[String] == kind)

  /** The newest plain-text result in the transcript. */
  def lastOutput(): String =
    blocks("out").lastOption.fold("(nothing)")(_.textContent.asInstanceOf[String])

  /** The newest typeset block's markup. */
  def lastMath(): String =
    blocks("math").lastOption.fold("(nothing)")(_.innerHTML.asInstanceOf[String])

  /** A settings field by its id; the panel nests one field inside each label. */
  def field(id: String): Option[js.Dynamic] =
    byId("settings").children.asInstanceOf[js.Array[js.Dynamic]].toVector.flatMap { label =>
      label.children.asInstanceOf[js.Array[js.Dynamic]].toVector
    }.find(_.id.asInstanceOf[String] == id)

  /** The number of controls the panel built. */
  def controlCount: Int = byId("settings").children.asInstanceOf[js.Array[js.Dynamic]].length

package it.grypho.scala.leonardo
package web

import scala.scalajs.js
import scala.scalajs.js.Dynamic.global

/** Hands LaTeX to the vendored MathLive renderer — the impure half of the `latex` toggle
 *  (issue F_0016 step 5).
 *
 *  **Everything decidable without a browser is already decided**: `latex.ToLatex` produces the
 *  source and is unit-tested on both platforms, and `cli.Session` decides *when* there is any.
 *  What is left here is the call, which cannot be tested without a DOM — the same split
 *  [[Plot]] makes, and for the same reason.
 *
 *  **The vendored build is the full one since F_0017**, which carries the `<math-field>` editor
 *  as well as these conversion functions — one file replacing the SSR build rather than joining
 *  it.  Choosing MathLive at F_0016 is what made that a swap rather than a migration.  Its
 *  keyboard sounds are still not shipped, but now by decision (`soundsDirectory = null` in
 *  `index.html`) rather than by the SSR build's absence of a keyboard.
 */
object MathRender:

  /** Whether the vendored renderer is present.
   *
   *  **`js.typeOf`, not a null check**, the rule phase 2 paid for: reading an undeclared name
   *  raises `ReferenceError` before any comparison can run.
   *
   *  The ordering caveat this used to carry is **gone since F_0017**: the full build is UMD and
   *  loads as a classic script, so the global is defined before `main.js` runs and a render can
   *  no longer arrive too early.  The check stays because a *missing* vendor directory is still
   *  possible, and it degrades the same way — the transcript keeps its plain-text answer.
   */
  private def available: Boolean = js.typeOf(global.leonardoLatexToMarkup) != "undefined"

  /** Typesets `latex` into `block`.
   *
   *  **The only `innerHTML` in the page that WRITES CONTENT, and deliberately so**: markup is
   *  what a typesetter returns.  Two others exist and are not exceptions — `App.clearTranscript`
   *  and `Plot.clear` assign the **empty string** to empty a container, which carries nothing
   *  to escape; stating the rule as "the one `innerHTML`" was imprecise enough that a reader
   *  checking it found it false (issue D_0031).  The distinction that matters is content, not
   *  count.
   *
   *  The transcript's rule — `textContent`, never `innerHTML`, because a result is full of
   *  `<`, `>` and `&` — is not broken here: the string is not user text but MathLive's
   *  serialisation of a *parsed* formula, so every character has passed through
   *  [[latex.ToLatex]]'s escaper and then MathLive's tokenizer, and a `<` comes back as
   *  `&lt;` (checked, rather than assumed, before this call was written).
   *
   *  @param latex math-mode source, with no surrounding delimiters
   *  @param block the element to typeset into
   *  @return whether it was typeset; `false` leaves `block` untouched for the caller to fill
   */
  def render(latex: String, block: js.Dynamic): Boolean =
    if !available then false
    else
      try
        block.innerHTML = global.leonardoLatexToMarkup(latex).asInstanceOf[String]
        true
      catch case _: Throwable => false

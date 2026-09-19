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
 *  **Render-only.**  What is vendored is MathLive's SSR distribution: the conversion functions
 *  and nothing else, so no mathfield element, no virtual keyboard and none of its keyboard
 *  sounds are shipped.  The editor is a later entry (F_0017), and choosing MathLive now is
 *  what keeps that from being a migration.
 */
object MathRender:

  /** Whether the vendored renderer is present.
   *
   *  **`js.typeOf`, not a null check**, the rule phase 2 paid for: reading an undeclared name
   *  raises `ReferenceError` before any comparison can run.  It is `undefined` for a second
   *  reason here that `Plotly` does not have — the renderer is an ES module and so loads
   *  *after* the classic `main.js`, so a very early render can legitimately find it missing.
   *  Both cases degrade the same way: the transcript keeps its plain-text answer.
   */
  private def available: Boolean = js.typeOf(global.leonardoLatexToMarkup) != "undefined"

  /** Typesets `latex` into `block`.
   *
   *  **The one `innerHTML` in the page, and deliberately so**: markup is what a typesetter
   *  returns.  The transcript's rule — `textContent`, never `innerHTML`, because a result is
   *  full of `<`, `>` and `&` — is not broken by it: the string is not user text but
   *  MathLive's serialisation of a *parsed* formula, so every character has passed through
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

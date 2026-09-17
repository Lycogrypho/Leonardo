package it.grypho.scala.leonardo
package web

import scala.scalajs.js
import scala.scalajs.js.Dynamic.global

/** Hands a [[PlotSpec]] document to Plotly — the impure half of the plotting facade.
 *
 *  Everything that can be decided without a browser lives in [[PlotSpec]] and is unit-tested
 *  there; what is left here is the call itself, which cannot be tested without a DOM and so is
 *  kept as small as it can be.  That split is the reason the facade is testable at all.
 */
object Plot:

  /** Whether the vendored Plotly bundle is present.
   *
   *  **`js.typeOf` and not a null check**, the rule phase 2 paid for: an absent global is not
   *  an undefined property, and reading one raises `ReferenceError` before any comparison can
   *  run.  `typeof` is the one operator JavaScript guarantees will not throw on an undeclared
   *  name.  A page served without `vendor/plotly.min.js` therefore degrades to a REPL that
   *  explains itself, rather than one that dies on the first `plot`.
   */
  private def available: Boolean = js.typeOf(global.Plotly) != "undefined"

  /** Draws a spec into a container element.
   *
   *  @param spec      a `{data, layout}` document from [[PlotSpec]]
   *  @param container the element to draw into
   *  @return unit, or a message explaining why nothing was drawn
   */
  def draw(spec: String, container: js.Dynamic): Either[String, Unit] =
    if !available then
      Left("plotting is unavailable: vendor/plotly.min.js did not load")
    else
      try
        val doc = js.JSON.parse(spec)
        // `react` rather than `newPlot`: it creates the figure the first time and updates it
        // in place afterwards, so a session that plots repeatedly does not accumulate state
        // in the container.
        global.Plotly.react(container, doc.data, doc.layout,
                            js.Dynamic.literal(responsive = true, displaylogo = false))
        Right(())
      catch
        case e: Throwable =>
          Left(s"could not draw: ${Option(e.getMessage).getOrElse(e.getClass.getSimpleName)}")

  /** Empties a container, so a failed plot does not leave the previous figure standing beside
   *  a message describing a different one.
   */
  def clear(container: js.Dynamic): Unit =
    if available then
      try global.Plotly.purge(container)
      catch case _: Throwable => ()
    container.innerHTML = ""

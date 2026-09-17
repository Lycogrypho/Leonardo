package it.grypho.scala.leonardo
package web

import scala.scalajs.js.annotation.JSExportTopLevel

import cli.Session

/** The smallest entry point that reaches the whole library, used to size the bundle.
 *
 *  **Phase 1 could not measure the bundle and this is why**: `fullLinkJS` on a library with no
 *  entry point emits nothing at all, because dead-code elimination correctly removes
 *  everything unreachable.  A number only exists once something *calls* the library, so the
 *  measurement had to wait for a module that does.
 *
 *  `Session.execute` is deliberately the only thing called.  It is the REPL's single entry
 *  point and it reaches parsing, evaluation, simplification, the matrix and exact tiers and
 *  the display layer — so whatever the linker keeps for this is very close to what a real
 *  browser REPL will ship, without any UI code confusing the figure.
 */
object SizeProbe:

  /** Evaluates one line through a fresh session.
   *
   *  @param line the input, exactly as a user would type it at the prompt
   *  @return the REPL's own output for that line
   */
  @JSExportTopLevel("leonardoEval")
  def eval(line: String): String = new Session().execute(line)

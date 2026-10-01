package it.grypho.scala.leonardo
package web

import scala.scalajs.js
import scala.scalajs.js.Dynamic.global

/** Encodes a session into the page's URL fragment, and reads one back.
 *
 *  **A shareable link needed no new serialisation at all**: `Session.script` already emits a
 *  replayable script and `Session.load` already consumes one, because `:save`/`:load` are built
 *  on exactly that pair.  So a link that restores a worked example is the existing round trip
 *  plus a percent-encoder — which is the same reason phase 2's browser persistence was cheap.
 *
 *  **The fragment, deliberately, and not a query string.**  Everything after `#` is held by the
 *  browser and never sent to the server, so a shared session stays as private as the page's
 *  promise that no user data leaves the browser.  A query string would put the user's work in
 *  the Pages access log.
 */
object ShareLink:

  /** The fragment key, so the encoding can gain neighbours without becoming ambiguous. */
  private val Key = cli.ShareEncoding.Key

  /** Browsers do not agree on a URL length limit and the practical floor is around 64k, so a
   *  long session is refused rather than truncated: half a script would `:load` as a *valid*
   *  but different session, which is worse than a link that plainly says it is too big.
   */
  private[web] val MaxFragment = cli.ShareEncoding.MaxFragment

  /** Builds the fragment for a session script.
   *
   *  **The format lives in `cli.ShareEncoding`, not here** (issue F_0045): the documentation
   *  helper has to build the same fragment on the JVM, to put a run link beside an executed
   *  lesson transcript, and it cannot import a Scala.js project.  The cross-built `repl` module
   *  is the only place both can read, so the key, the cap and the encoder moved there and this
   *  is a wrapper.  Decoding stays below, because only a browser ever needs it.
   *
   *  @param script the session script, from `Session.script`
   *  @return the fragment text including its key, or a message if the script is too long
   */
  def encode(script: String): Either[String, String] = cli.ShareEncoding.fragmentFor(script)

  /** Reads a session script out of a fragment.
   *
   *  @param fragment the raw fragment, with or without its leading `#`
   *  @return the script, or `None` when the fragment carries no session
   */
  def decode(fragment: String): Option[String] =
    val body = fragment.stripPrefix("#")
    if !body.startsWith(Key) then None
    else
      // A hand-edited or truncated fragment makes decodeURIComponent throw a URIError; a
      // malformed link must leave the page usable rather than failing to start.
      try Option(decodeComponent(body.substring(Key.length))).filter(_.nonEmpty)
      catch case _: Throwable => None

  /** `decodeURIComponent`, which is a JavaScript global rather than anything in the JDK. It is
   *  the authority this module's encoder is checked against, and is deliberately not
   *  re-implemented: only a browser ever decodes a fragment.
   */
  private def decodeComponent(s: String): String =
    global.decodeURIComponent(s).asInstanceOf[String]

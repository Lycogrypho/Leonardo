package it.grypho.scala.leonardo
package cli

import scala.scalajs.js
import scala.scalajs.js.Dynamic.global

/** Where `:save` and `:load` put a session — **the Scala.js implementation** (F_0003 phase 2).
 *
 *  **`:save`/`:load` keep their exact spelling in the browser**, backed by `localStorage`
 *  instead of the file system.  That is deliberate: the alternative was a separate browser-only
 *  persistence API, which would have made the two builds speak different command languages for
 *  no gain.  A session saved as `:save work` is reloaded as `:load work` on either platform —
 *  only the medium differs.
 *
 *  It costs nothing extra because `Session.script` and `Session.load` were **already** pure
 *  string in / string out: the file system was the only JVM-bound part of `:save`, and it was
 *  never in `Session` itself.
 *
 *  Keys are prefixed so a session cannot collide with whatever else the hosting page keeps in
 *  `localStorage`.
 */
private[cli] object SessionIO:

  /** What a store is called in user-facing messages. */
  val what: String = "browser storage"

  private val Prefix = "leonardo.session."

  /** `localStorage` is absent in a non-browser host — a bare Node run, or a page with storage
   *  disabled — and it is reached through an Option per the house rule against unwrapped
   *  nullable platform APIs.
   *
   *  **The test must be `js.typeOf`, not a null or undefined check on the value.**  An absent
   *  global is not an undefined property: reading it raises `ReferenceError` before any
   *  comparison can run, which is precisely how the first Node test run failed.  `typeof` is
   *  the one operator JavaScript guarantees will not throw on an undeclared name.
   */
  private def storage: Option[js.Dynamic] =
    if js.typeOf(global.localStorage) == "undefined" then None
    else Option(global.localStorage).filterNot(s => js.isUndefined(s) || s == null)

  /** Reads a saved session.
   *  @param name the session name
   *  @return the script text, or a message explaining why it could not be read
   */
  def read(name: String): Either[String, String] =
    storage match
      case None => Left("browser storage is not available here")
      case Some(s) =>
        val v = s.getItem(Prefix + name)
        if js.isUndefined(v) || v == null then Left(s"no saved session named '$name'")
        else Right(v.asInstanceOf[String])

  /** Writes a session script.
   *  @param name the session name
   *  @param text the replayable script
   *  @return unit, or a message explaining why it could not be written
   */
  def write(name: String, text: String): Either[String, Unit] =
    storage match
      case None => Left("browser storage is not available here")
      case Some(s) =>
        // A quota failure is the realistic one: browsers cap localStorage at a few megabytes
        // per origin, and it surfaces as a thrown exception rather than a false return.
        try { s.setItem(Prefix + name, text); Right(()) }
        catch case e: Throwable => Left(Option(e.getMessage).getOrElse("storage write refused"))

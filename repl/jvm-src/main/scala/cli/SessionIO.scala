package it.grypho.scala.leonardo
package cli

/** Where `:save` and `:load` put a session — **the JVM implementation** (issue F_0003 phase 2).
 *
 *  A named session is a file path, which is what it has always been.
 */
private[cli] object SessionIO:

  /** What a store is called in user-facing messages, so `Session` need not know which one it
   *  is talking to.
   */
  val what: String = "file"

  /** Reads a saved session.
   *  @param name the path to read
   *  @return the script text, or a message explaining why it could not be read
   */
  def read(name: String): Either[String, String] =
    scala.util.Using(scala.io.Source.fromFile(name, "UTF-8"))(_.mkString) match
      case scala.util.Success(text) => Right(text)
      case scala.util.Failure(e)    => Left(e.getMessage)

  /** Writes a session script.
   *  @param name the path to write
   *  @param text the replayable script
   *  @return unit, or a message explaining why it could not be written
   */
  def write(name: String, text: String): Either[String, Unit] =
    scala.util.Try:
      val w = new java.io.PrintWriter(name, java.nio.charset.StandardCharsets.UTF_8)
      try w.write(text) finally w.close()
    match
      case scala.util.Success(_) => Right(())
      case scala.util.Failure(e) => Left(e.getMessage)

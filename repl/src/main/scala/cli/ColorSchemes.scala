package it.grypho.scala.leonardo
package cli

/** The syntax-highlighting scheme *names* the `colors` command accepts.
 *
 *  Split from `ColorScheme` itself when `repl` became a cross-build (issue F_0003 phase 2).
 *  The distinction is exactly the platform boundary: a scheme's **styles** are JLine
 *  `AttributedStyle` values and cannot exist in a browser, but the **names** are what
 *  `Session` needs — it only ever validates the argument to `colors` and lists the choices,
 *  and never touches a style.  Keeping the names here lets the whole `colors` command stay in
 *  the shared tree, where the browser front end can offer it too.
 *
 *  `ColorScheme.All` on the JVM must agree with this list; `ColorSchemeNamesTest` pins that,
 *  because two hand-maintained lists of the same thing are exactly how they drift apart.
 */
private[cli] object ColorSchemes:

  /** Every accepted scheme name, in the order the REPL offers them. */
  val Names: List[String] = List("dark", "light", "none")

  /** The scheme a fresh session starts in. */
  val Default: String = "dark"

  /** @param name a candidate scheme name
   *  @return whether `name` is one this REPL accepts
   */
  def isValid(name: String): Boolean = Names.contains(name)

  /** The choices as the REPL lists them in an error message. */
  def listed: String = Names.sorted.mkString(", ")

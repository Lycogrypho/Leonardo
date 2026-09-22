package it.grypho.scala.leonardo
package tools

import java.nio.charset.StandardCharsets
import java.nio.file.{Files as JFiles, Path, Paths}
import scala.jdk.StreamConverters.*

/** The file walking and reporting every guard shares (issue F_0027).
 *
 *  Each check is otherwise a rule and a regex; what they have in common is finding the files
 *  to apply it to and saying what they found in the same shape.  Kept in one place so three
 *  guards cannot disagree about which files they cover — a divergence invisible until the day
 *  one of them misses something.
 */
object Files:

  /** The text extensions the encoding guards cover.
   *
   *  Deliberately a list of what this repository *writes*, not everything readable: a vendored
   *  `.js` or a `.woff2` is not ours to judge, and walking them would cost a fast guard the
   *  one thing it has.
   */
  val TextExtensions: Set[String] = Set(".scala", ".md", ".txt", ".puml", ".sbt")

  /** Everything the encoding guards cover, written **once** (issue F_0028).
   *
   *  It used to be written three times — the `checks` alias, `FixMojibakeTest`'s repository
   *  scan and `CheckersTest`'s — and all three said `core/src repl/src web/src tools/src
   *  docs`, inherited from the Python originals and carried through the port verbatim. Two
   *  things were wrong with that. **It missed four real source trees**: `CrossType.Pure` puts
   *  the platform code in `core/jvm-src`, `core/js-src`, `repl/jvm-src` and `repl/js-src`, so
   *  `Terminal.scala` and `DoubleRender.scala` were never scanned at all — and the 2.8 damage
   *  spread by copy-paste, which makes an unscanned tree exactly where the next paste lands.
   *  And **three hand-maintained copies of one set is the drift `ColorSchemeNamesTest` exists
   *  to prevent elsewhere**; the remedy is the one [[CheckActionPins]] already uses — a
   *  default the callers fall back to, rather than a list each of them repeats.
   *
   *  Trees and single files mix freely because [[walk]] returns a named file as itself, which
   *  is how the extensionless `NOTICE` is reached at all, and answers an absent root with
   *  nothing, which is how the untracked local trio can be listed although no CI checkout
   *  has it.
   */
  val GuardedRoots: Vector[Path] = Vector(
    // The library and the REPL: shared sources and BOTH platform trees (F_0003).
    "core/src", "core/jvm-src", "core/js-src",
    "repl/src", "repl/jvm-src", "repl/js-src",
    // The browser front end, and the guards themselves — a guard must be clean of its own rules.
    "web/src", "tools/src",
    // The build definition. `project/` holds .sbt and .scala the compiler never sees as part
    // of a module, so nothing else would ever reach them.
    "build.sbt", "project",
    // The documentation site's input, and the documents a visitor to the repository meets
    // first. `LICENSE` is deliberately absent: it is a verbatim Apache-2.0 text nobody here may
    // edit, so a finding in it could only ever be a stuck build.
    "docs", "README.md", "CHANGELOG.md", "NOTICE",
    // Gitignored, so absent from every CI checkout and scanned on a LOCAL run only. Listed
    // anyway because this is where most of the project's prose is actually written, and a local
    // run is therefore the only place damage in them could ever be caught.
    "CLAUDE.md", "ToDo.md", "Done.md", "DesignNotes.md"
  ).map(Paths.get(_))

  /** The roots a guard was told to scan, or [[GuardedRoots]] when it was told none.
   *
   *  @param args the command line's path arguments
   *  @return those paths, or the repository's own list
   */
  def rootsOrDefault(args: Seq[String]): Vector[Path] =
    if args.isEmpty then GuardedRoots else args.toVector.map(Paths.get(_))

  /** Every file under `root` whose name ends in one of `extensions`, in a stable order.
   *
   *  A `root` that is itself a file is returned as one, so a caller may name either — which is
   *  what lets `--check <one file>` work while CI names whole trees.  A missing root yields
   *  nothing rather than failing: the callers pass several, and one absent tree is not a
   *  reason to refuse the others.
   *
   *  @param root       a file or directory
   *  @param extensions the suffixes to keep
   *  @return the matching paths, sorted
   */
  def walk(root: Path, extensions: Set[String]): Vector[Path] =
    if !JFiles.exists(root) then Vector.empty
    else if JFiles.isRegularFile(root) then Vector(root)
    else
      val stream = JFiles.walk(root)
      try
        stream.toScala(Vector)
          .filter(JFiles.isRegularFile(_))
          .filter(p => extensions.exists(p.getFileName.toString.endsWith))
          .sortBy(_.toString)
      finally stream.close()

  /** Reads a file as UTF-8, replacing anything undecodable rather than throwing.
   *
   *  **Replacing is the right failure mode here**: a guard that dies on the damage it exists
   *  to find reports nothing at all, and `U+FFFD` is itself on the forbidden list — so an
   *  unreadable byte becomes a finding instead of a stack trace.
   *
   *  @param path the file to read
   *  @return its contents, with undecodable bytes as `U+FFFD`
   */
  def read(path: Path): String = new String(JFiles.readAllBytes(path), StandardCharsets.UTF_8)

  /** Writes UTF-8 with `\n` endings, whatever the platform.
   *
   *  The repository is UTF-8 with Unix endings, and a repair that silently rewrote every line
   *  ending on Windows would turn a four-character fix into a whole-file diff.
   *
   *  @param path the file to write
   *  @param text its new contents
   */
  def write(path: Path, text: String): Unit =
    JFiles.write(path, text.replace("\r\n", "\n").getBytes(StandardCharsets.UTF_8))

  /** Prints `findings`, then a one-line count, and answers the exit code a guard should use.
   *
   *  One shape for every guard, so a reader of the CI log does not have to learn four.
   *
   *  @param findings  one line per offence, already formatted `path:line: detail`
   *  @param summary   the count line, given the number found
   *  @param remedy    printed only when there is something to remedy
   *  @return 1 when anything was found, 0 otherwise
   */
  def report(findings: Seq[String], summary: Int => String, remedy: String = ""): Int =
    findings.foreach(println)
    println(summary(findings.size))
    if findings.nonEmpty && remedy.nonEmpty then println(remedy)
    if findings.isEmpty then 0 else 1

package it.grypho.scala.leonardo
package tools

import java.nio.charset.StandardCharsets
import java.nio.file.{Files as JFiles, Path}
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

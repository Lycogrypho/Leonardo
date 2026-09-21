package it.grypho.scala.leonardo
package tools

import java.nio.file.{Path, Paths}

/** Refuses a hardcoded Scala version anywhere under `.github/` (issue F_0023).
 *
 *  The build output directory is named after the Scala version, so a path of the shape
 *  `web/target/scala-<version>/leonardo-web-opt/main.js` spells it.  That is a trap with a
 *  recorded cost: Scala Steward's version-bump branch edits every file carrying the literal,
 *  and when one of them is a **workflow**, pushing the branch is refused outright for a PAT
 *  without the `workflow` scope — failing the whole automation run rather than the one file.
 *  Widening the token would hand the automation power over the workflows that hold the
 *  signing key, so the literal goes instead.
 *
 *  `pages.yml` removed it in 2026-09 and wrote the incident above the glob that replaced it;
 *  `ci.yml` kept it regardless.  **That is the evidence this is a standing check and not a
 *  note**: the condition had already decayed once while a comment three files away explained
 *  why it must not.  It joins [[FixMojibake]], [[CheckCharset]] and [[CheckActionPins]],
 *  which exist for exactly the same reason.
 *
 *  The remedy is always a glob — `web/target/scala-*\/…` in a shell, a directory scan in a
 *  program — since a build produces exactly one such directory.
 */
object CheckScalaVersion:

  /** `scala-` followed by a digit: the output-directory form, and only that.
   *
   *  Comments stay free to write `scala-<version>` generically, which is what a reader needs
   *  and what `pages.yml` already does.
   */
  private val VersionedDir = """scala-\d""".r

  /** Text only; a workflow cannot hide a path in a binary. */
  private val Extensions = Set(".yml", ".yaml", ".scala", ".js", ".sh", ".md", ".py")

  private val DefaultRoot = Paths.get(".github")

  /** Every `path:line: text` under `roots` that spells a Scala version.
   *
   *  @param roots the directories to scan
   *  @return one finding per offending line
   */
  def offences(roots: Seq[Path]): Vector[String] =
    roots.toVector.flatMap(Files.walk(_, Extensions)).flatMap { path =>
      Files.read(path).linesIterator.zipWithIndex.collect {
        case (line, i) if VersionedDir.findFirstIn(line).isDefined =>
          s"$path:${i + 1}: ${line.trim}"
      }
    }

  /** The check, as an exit code, over the given directories or `.github`.
   *
   *  @param args directories to scan; empty means the default
   *  @return 1 when a version is spelled, 0 otherwise
   */
  def run(args: Seq[String]): Int =
    val roots = if args.isEmpty then Vector(DefaultRoot) else args.toVector.map(Paths.get(_))
    Files.report(
      offences(roots),
      n => s"$n hardcoded Scala version(s) under ${roots.mkString(", ")}",
      "use a glob (web/target/scala-*/...) so a Scala bump edits no file here")

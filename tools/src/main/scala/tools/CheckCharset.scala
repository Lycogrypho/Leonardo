package it.grypho.scala.leonardo
package tools

import java.nio.file.Path

/** Detects characters from scripts this codebase cannot legitimately contain (issue 2.8).
 *
 *  A safety net for the mojibake repair.  [[FixMojibake]] reverses a cp1252 round-trip, and in
 *  rare cases a *correct* sequence reverses into valid UTF-8 by coincidence: `×–`
 *  (U+00D7 U+2013) becomes bytes D7 96, which is valid UTF-8 for Hebrew ZAYIN.  A bad repair
 *  therefore shows up as a character from an unrelated script.
 *
 *  **This is a blacklist, not a whitelist, and deliberately so.**  The first attempt
 *  enumerated the allowed blocks and produced 222 false positives, because the legitimate
 *  mathematical notation here is far wider than it looks: modifier letters (`Aᵀ`, `aᵛ`, `eˣ`),
 *  combining marks (`r̂`), long arrows, box drawing in separator comments, subscript j
 *  (U+2C7C) — each a separate Unicode block.  Enumerating what mathematics is allowed to look
 *  like is a losing game; enumerating the handful of scripts it will never use is not.
 */
object CheckCharset:

  /** Scripts that cannot appear in this project, as inclusive code-point ranges.
   *
   *  A hit is either a bad mojibake repair or something else that wants a human's attention.
   *  Kept in step with [[FixMojibake.Forbidden]], which refuses a repair landing in one.
   */
  val Forbidden: Vector[(Int, Int, String)] = Vector(
    (0x0400, 0x04FF, "Cyrillic"),
    (0x0530, 0x058F, "Armenian"),
    (0x0590, 0x05FF, "Hebrew"),
    (0x0600, 0x06FF, "Arabic"),
    (0x0700, 0x074F, "Syriac"),
    (0x0900, 0x097F, "Devanagari"),
    (0x0E00, 0x0E7F, "Thai"),
    (0x3040, 0x30FF, "Kana"),
    (0x4E00, 0x9FFF, "CJK"),
    (0xAC00, 0xD7AF, "Hangul"),
    (0xFFFD, 0xFFFD, "replacement character"),
    // C1 controls: invisible, and a reliable fingerprint of a half-decoded byte sequence.
    (0x0080, 0x009F, "C1 control")
  )

  /** The script `ch` belongs to, when that script is one this codebase never uses. */
  def offending(ch: Char): Option[String] =
    Forbidden.collectFirst { case (lo, hi, name) if ch >= lo && ch <= hi => name }

  /** Every `path:line: detail` under `roots` carrying a forbidden character.
   *
   *  One finding per *line*, listing each distinct character once in code-point order — a
   *  damaged line usually carries several, and repeating the line per character would bury
   *  the file that needs looking at.
   *
   *  @param roots the files or directories to scan
   *  @return one finding per offending line
   */
  def offences(roots: Seq[Path]): Vector[String] =
    roots.toVector.flatMap(Files.walk(_, Files.TextExtensions)).flatMap { path =>
      Files.read(path).linesIterator.zipWithIndex.flatMap { (line, i) =>
        val hits = line.toVector.flatMap(c => offending(c).map(c -> _)).distinct.sortBy(_._1.toInt)
        Option.when(hits.nonEmpty) {
          val detail = hits.map { (c, script) =>
            val name = Option(Character.getName(c.toInt)).getOrElse("?")
            f"U+${c.toInt}%04X ($script, $name)"
          }.mkString(", ")
          s"$path:${i + 1}: $detail"
        }
      }
    }

  /** The check, as an exit code, over the given paths or [[Files.GuardedRoots]].
   *
   *  @param args the files or directories to scan; empty means the repository's own list
   *  @return 1 when anything is found, 0 otherwise
   */
  def run(args: Seq[String]): Int =
    Files.report(offences(Files.rootsOrDefault(args)), n => s"$n suspect line(s)")

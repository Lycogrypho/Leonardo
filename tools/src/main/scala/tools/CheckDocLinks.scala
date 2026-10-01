package it.grypho.scala.leonardo
package tools

import java.nio.file.{Files as JFiles, Path, Paths}

/** Refuses a relative link or asset under `docs/src` that does not resolve (issue D_0003).
 *
 *  **mdoc already reports these — as a WARNING, and the build stays green.**  Measured
 *  2026-09-28 by breaking a link on purpose: `docs/mdoc` prints
 *  `warning: Unknown link 'lessons/expressions.md', did you mean 'expressions.md'?` and exits
 *  successfully, so nothing goes red and no one is told.  That was survivable while the prose
 *  pages were flat and few; the lesson branches (D_0003) put ~20 pages one directory down, each
 *  carrying `../` links and a `../logo_bw.svg`, where a single rename rots many references at
 *  once.  A warning nobody reads is the condition [[CheckActionPins]] and [[CheckScalaVersion]]
 *  both exist to replace — a one-time tidy-up decays, a standing check does not.
 *
 *  **Two offences, and the second is the one a local run cannot otherwise catch.**  A target
 *  that does not exist is the obvious case.  A target whose *case* differs from the file on disk
 *  is the subtle one: Windows and macOS resolve it happily, Linux does not, and GitHub Pages is
 *  Linux — so the link works for the author and 404s for every reader.  That is the `@main`
 *  case-collision trap of [[Entrypoints]] in a second form, and it is caught here by comparing
 *  against the real name on disk rather than by asking whether the path exists.
 *
 *  What it deliberately ignores: absolute URLs (the published `/app` and `/api` links must be
 *  absolute, precisely because those directories are produced *after* mdoc runs), `mailto:`,
 *  bare anchors, mdoc's `@VARIABLE@` substitutions, and anything inside a fenced code block —
 *  a sample is not a link, and flagging one would make the guard something to switch off.
 */
object CheckDocLinks:

  /** Markdown `](target)` and HTML `src="target"` / `href="target"`, which is every way a page
   *  here names another file.  Reference-style links are not used in this documentation.
   */
  private val Targets = """\]\(([^)\s]+)\)|(?:src|href)="([^"\s]+)"""".r

  /** A fence opens or closes on ``` or ~~~ at the start of a line, whatever follows it. */
  private val Fence = """^\s*(?:```|~~~)""".r

  /** Anything with a scheme, a protocol-relative prefix, a bare fragment, or an mdoc variable:
   *  none of them name a file in this tree, so none of them can be resolved against one.
   */
  private def external(target: String): Boolean =
    target.startsWith("#") || target.startsWith("//") || target.startsWith("@") ||
      target.matches("""^[a-zA-Z][a-zA-Z0-9+.-]*:.*""")

  /** The link targets of one file's text, with fenced code skipped.
   *
   *  @param text the markdown source
   *  @return each target with the 1-based line it was written on
   */
  private[tools] def targetsOf(text: String): Vector[(String, Int)] =
    val (found, _) = text.linesIterator.zipWithIndex.foldLeft((Vector.empty[(String, Int)], false)) {
      case ((acc, inFence), (line, i)) =>
        if Fence.findFirstIn(line).isDefined then (acc, !inFence)
        else if inFence then (acc, inFence)
        else
          val hits = Targets.findAllMatchIn(line).map(m => Option(m.group(1)).getOrElse(m.group(2)))
          (acc ++ hits.filterNot(external).map(t => (t, i + 1)), inFence)
    }
    found

  /** Strips a fragment and a query, leaving the path a target names. */
  private def pathPart(target: String): String =
    target.takeWhile(c => c != '#' && c != '?')

  /** Every unresolvable or wrongly-cased target under `roots`.
   *
   *  @param roots the directories to scan
   *  @return one finding per offending link, and how many files were read
   */
  def offences(roots: Seq[Path]): (Vector[String], Int) =
    val files = roots.toVector.flatMap(Files.walk(_, Set(".md")))
    val found = files.flatMap { file =>
      targetsOf(Files.read(file)).flatMap { (target, line) =>
        val relative = pathPart(target)
        if relative.isEmpty then None
        else
          val resolved = file.toAbsolutePath.getParent.resolve(relative).normalize
          if !JFiles.exists(resolved) then
            Some(s"$file:$line: broken link '$target'")
          else
            // Exists is not enough: Windows and macOS match case-insensitively where the Linux
            // runner that publishes the site does not.
            val real = resolved.toRealPath()
            val asked = resolved.getFileName.toString
            if real.getFileName.toString != asked then
              Some(s"$file:$line: wrong case '$target' — on disk it is '${real.getFileName}'")
            else None
      }
    }
    (found, files.size)

  /** The check, as an exit code, over the given directories or `docs/src`.
   *
   *  @param args directories to scan; empty means the default
   *  @return 1 when a link does not resolve, 0 otherwise
   */
  def run(args: Seq[String]): Int =
    val roots = if args.isEmpty then Vector(Paths.get("docs/src")) else args.toVector.map(Paths.get(_))
    val (found, checked) = offences(roots)
    Files.report(
      found,
      n => s"$checked page(s) checked, $n unresolved link(s)",
      "a page one directory down needs '../'; link /app and /api absolutely")

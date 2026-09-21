package it.grypho.scala.leonardo
package tools

import java.nio.file.{Path, Paths}

/** Fails if any workflow references a GitHub Action by tag or branch instead of a commit SHA.
 *
 *  Issue 2.10 pinned every `uses:` to a SHA, because a tag is mutable and can be repointed by
 *  whoever owns the action repository — and `ruby/setup-ruby@v1` was not even a tag but a
 *  *branch*, whose head moves on every push.  An unpinned action therefore lets its owner
 *  change what runs in `release.yml`, which carries the GPG signing key, and in
 *  `scala-steward.yml`, which carries a repository-writing PAT.
 *
 *  **That sweep was one-time and nothing kept it true.**  This guard does: a new workflow, or
 *  a hand-edited `uses:`, fails CI instead of silently reopening the hole.  Written when
 *  issue 2.14 added a fifth workflow with an action the repository had not used before — the
 *  first occasion since 2.10 where the invariant rested purely on remembering it.
 *
 *  Dependabot keeps the pins from going stale (`.github/dependabot.yml`), so pinning does not
 *  degrade into freezing; this only enforces that they are pins at all.
 */
object CheckActionPins:

  /** `uses: owner/repo@ref` or `uses: owner/repo/path@ref`, quoted or bare. */
  private val Uses = """^\s*-?\s*uses:\s*["']?([^"'\s#]+)["']?""".r.unanchored

  private val Sha = "^[0-9a-f]{40}$".r

  private val WorkflowExtensions = Set(".yml", ".yaml")

  private val DefaultRoot = Paths.get(".github", "workflows")

  /** Every `path:line: reference` in `roots` that names something other than a commit SHA.
   *
   *  **Local actions are exempt** (`./.github/…`): that is this repository's own code, and
   *  there is nothing third-party to pin.
   *
   *  @param roots the workflow directories to scan
   *  @return one finding per unpinned reference, and the number of workflows examined
   */
  def offences(roots: Seq[Path]): (Vector[String], Int) =
    val files = roots.toVector.flatMap(Files.walk(_, WorkflowExtensions))
    val found = files.flatMap { path =>
      Files.read(path).linesIterator.zipWithIndex.collect {
        case (Uses(ref), i) if !ref.startsWith("./") && !ref.startsWith(".\\") &&
                               !Sha.matches(ref.drop(ref.indexOf('@') + 1)) =>
          s"$path:${i + 1}: not SHA-pinned: $ref"
      }
    }
    (found, files.size)

  /** The check, as an exit code, over the given directories or `.github/workflows`.
   *
   *  @param args workflow directories; empty means the default
   *  @return 1 when anything is unpinned, 0 otherwise
   */
  def run(args: Seq[String]): Int =
    val roots = if args.isEmpty then Vector(DefaultRoot) else args.toVector.map(Paths.get(_))
    val (found, checked) = offences(roots)
    Files.report(
      found,
      n => s"\n$checked workflow(s) checked, $n unpinned reference(s)",
      "Pin to the commit SHA with the version as a trailing comment (issue 2.10);\n" +
      "`git ls-remote --tags https://github.com/OWNER/REPO` resolves it without an\n" +
      "API token or rate limit.")

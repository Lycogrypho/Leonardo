package it.grypho.scala.leonardo
package tools

/** The command-line entry points for the repository's guards (issue F_0027).
 *
 *  **A `@main` may not be named after the object it calls, and that is a filesystem fact
 *  rather than a Scala one.**  `@main def checkCharset` generates a class literally called
 *  `checkCharset`, while `object CheckCharset` generates `CheckCharset` — names that differ
 *  only in case, so on a case-insensitive filesystem (Windows, macOS by default) the second
 *  class file written **silently overwrites the first**.  The compile still reports success;
 *  what fails is the next separate compilation, with `value … is not a member of object …`
 *  against an object that plainly has it.  On Linux it would have worked, so CI would have
 *  been green and every Windows checkout broken.  Hence the plain nouns below.
 *
 *  **They also live in one file, away from the objects, for a second reason**: Scala 3 gathers
 *  a file's top-level members into a synthetic class named after the *file*, so a
 *  `Guard.scala` holding both `object Guard` and a top-level `@main` puts two things of that
 *  name in the package.  The same fact made splitting `Repl.scala` a binary change (see
 *  `CLAUDE.md`); here it would be a compile error, which is the luckier half.
 *
 *  Each is a thin shell over its object's `run`, so a test exercises the logic without the
 *  process exiting underneath the suite.  Run them through the `checks` alias — one sbt boot
 *  for all four — or individually:
 *  {{{
 *  sbt checks
 *  sbt "tools/runMain it.grypho.scala.leonardo.tools.charset core/src"
 *  }}}
 */

/** Reports, or repairs, UTF-8-read-as-cp1252 damage. See [[FixMojibake]]. */
@main def mojibake(args: String*): Unit = sys.exit(FixMojibake.run(args))

/** Flags characters from scripts this codebase cannot contain. See [[CheckCharset]]. */
@main def charset(args: String*): Unit = sys.exit(CheckCharset.run(args))

/** Flags a workflow `uses:` that is not pinned to a commit SHA. See [[CheckActionPins]]. */
@main def actionPins(args: String*): Unit = sys.exit(CheckActionPins.run(args))

/** Flags a hardcoded Scala version under `.github`. See [[CheckScalaVersion]]. */
@main def scalaVersion(args: String*): Unit = sys.exit(CheckScalaVersion.run(args))

package it.grypho.scala.leonardo
package tools

import java.nio.charset.StandardCharsets
import java.nio.file.{Files as JFiles, Path, Paths}
import org.scalatest.flatspec.AnyFlatSpec

/** The three pattern guards, each against a fixture that must fail and one that must pass
 *  (issue F_0027).
 *
 *  **A guard that never fires is indistinguishable from a guard that cannot fire**, which is
 *  the whole risk with a check nothing tests: it goes green either way.  Each case below
 *  plants the exact offence the guard exists for.
 */
class CheckersTest extends AnyFlatSpec:

  /** Writes `files` into a fresh temporary directory and answers it. */
  private def fixture(files: (String, String)*): Path =
    val dir = JFiles.createTempDirectory("leonardo-tools-")
    dir.toFile.deleteOnExit()
    files.foreach { (name, content) =>
      val path = dir.resolve(name)
      JFiles.createDirectories(path.getParent)
      JFiles.write(path, content.getBytes(StandardCharsets.UTF_8))
      path.toFile.deleteOnExit()
    }
    dir

  // --- action pins -------------------------------------------------------------------

  "the action-pin guard" should "flag a tag and a branch, which are both mutable" in
  {
    val dir = fixture("w.yml" ->
      """jobs:
         |  build:
         |    steps:
         |      - uses: actions/checkout@v4
         |      - uses: ruby/setup-ruby@v1
         |""".stripMargin)
    val (found, checked) = CheckActionPins.offences(List(dir))
    assert(checked == 1)
    assert(found.size == 2, s"expected both references flagged: $found")
  }

  it should "accept a 40-character SHA and exempt a local action" in
  {
    val dir = fixture("w.yml" ->
      """steps:
         |  - uses: actions/checkout@08c6903cd8c0fde910a37f88322edcfb5dd907a8 # v5.0.0
         |  - uses: ./.github/actions/local
         |""".stripMargin)
    assert(CheckActionPins.offences(List(dir))._1.isEmpty)
  }

  it should "agree with the workflows this repository actually ships" in
  {
    val (found, checked) = CheckActionPins.offences(List(Paths.get(".github", "workflows")))
    assert(checked >= 5, s"expected the five workflows, found $checked")
    assert(found.isEmpty, s"unpinned: $found")
  }

  // --- the Scala version -------------------------------------------------------------

  "the Scala-version guard" should "flag a build path that spells the version" in
  {
    val dir = fixture("w.yml" -> "    run: cat web/target/scala-3.3.6/leonardo-web-opt/main.js\n")
    assert(CheckScalaVersion.offences(List(dir)).size == 1)
  }

  it should "accept a glob, and prose that names the version generically" in
  {
    val dir = fixture("w.yml" ->
      """# No file here may spell scala-<version>; a bump would then edit this workflow.
         |    run: ls web/target/scala-*/leonardo-web-opt/main.js
         |""".stripMargin)
    assert(CheckScalaVersion.offences(List(dir)).isEmpty)
  }

  it should "find nothing under this repository's .github" in
  {
    assert(CheckScalaVersion.offences(List(Paths.get(".github"))).isEmpty)
  }

  // --- the charset blacklist ---------------------------------------------------------

  // The banned characters are built from their code points, never written: this suite is
  // itself scanned by the guard it tests, and a fixture written literally would make the
  // guards impossible to run over the whole tree.
  private def cp(code: Int): String = code.toChar.toString

  "the charset guard" should "flag a script this codebase cannot contain" in
  {
    // Hebrew ZAYIN is the specific character a bad mojibake repair produces from `×–`.
    val dir = fixture("a.scala" -> s"""val x = "100${cp(0x05D6)}4 700"\n""")
    assert(CheckCharset.offences(List(dir)).size == 1)
  }

  it should "flag the replacement character and a C1 control" in
  {
    val dir = fixture("a.md" -> s"half-decoded ${cp(0xFFFD)} and ${cp(0x0081)} invisible\n")
    assert(CheckCharset.offences(List(dir)).size == 1, "one finding per line, not per character")
  }

  it should "accept the mathematical notation this codebase is full of" in
  {
    // The blacklist exists because the whitelist produced 222 false positives on exactly this.
    val dir = fixture("a.scala" -> "// Aᵀ eˣ r̂ ⁻¹ ∫ λ → ⟹ ⱼ ───\n")
    assert(CheckCharset.offences(List(dir)).isEmpty,
           s"false positive on legitimate notation: ${CheckCharset.offences(List(dir))}")
  }

  it should "find nothing anywhere this repository guards" in
  {
    // Files.GuardedRoots, never a list spelled here: the coverage of a guard is one fact, and
    // a second copy of it is how a tree stops being scanned without anyone noticing (F_0028).
    assert(CheckCharset.offences(Files.GuardedRoots).isEmpty)
  }

  it should "default to the guarded roots when given no paths" in
  {
    // The default is what lets the `checks` alias, ci.yml and this suite name the same set
    // without any of them repeating it.
    assert(CheckCharset.run(Nil) == 0)
  }

  // --- what the guards cover ----------------------------------------------------------

  "the guarded roots" should "include every platform source tree, not only the shared one" in
  {
    // THE gap F_0028 found: the root list was inherited from the Python originals and named
    // `core/src repl/src` only, so the four CrossType.Pure platform trees -- real .scala,
    // holding Terminal.scala and DoubleRender.scala -- were never scanned at all. The 2.8
    // damage spread by copy-paste between files; an unscanned tree is where the next paste
    // lands.
    for tree <- List("core/jvm-src", "core/js-src", "repl/jvm-src", "repl/js-src") do
      val path = Paths.get(tree)
      assert(Files.GuardedRoots.contains(path), s"$tree is a source tree and must be guarded")
      assert(Files.walk(path, Files.TextExtensions).nonEmpty, s"$tree should hold sources")
  }

  it should "include the build definition and the published root documents" in
  {
    // `NOTICE` has no extension, and is reached because `walk` returns a named FILE as itself
    // rather than applying the extension filter -- which is what lets a root list mix trees
    // and single files with no special case.
    for name <- List("build.sbt", "project", "README.md", "CHANGELOG.md", "NOTICE") do
      val path = Paths.get(name)
      assert(Files.GuardedRoots.contains(path), s"$name must be guarded")
      assert(Files.walk(path, Files.TextExtensions).nonEmpty, s"$name should be readable")
  }

  it should "tolerate a root that this checkout does not have" in
  {
    // ToDo.md, Done.md and DesignNotes.md are untracked and absent from every CI checkout, so
    // the list is deliberately wider than any one working copy. `walk` answers an absent root
    // with nothing, which is what makes listing them safe rather than a per-environment case.
    assert(Files.walk(Paths.get("no-such-tree"), Files.TextExtensions).isEmpty)
    for local <- List("ToDo.md", "Done.md", "DesignNotes.md") do
      assert(Files.GuardedRoots.contains(Paths.get(local)), s"$local should be guarded when present")
  }

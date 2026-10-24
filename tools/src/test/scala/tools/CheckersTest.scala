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

  // --- documentation links -----------------------------------------------------------

  "the doc-link guard" should "flag a target that does not exist" in
  {
    val dir = fixture("page.md" -> "See [the other page](missing.md).\n")
    val (found, checked) = CheckDocLinks.offences(List(dir))
    assert(checked == 1)
    assert(found.size == 1, s"expected the broken link flagged: $found")
  }

  it should "resolve a link that climbs out of a subdirectory" in
  {
    // The lesson layout: docs/src/lessons/l1.md pointing back at a reference page.
    val dir = fixture("target.md" -> "# target\n", "sub/page.md" -> "[up](../target.md)\n")
    val (found, _) = CheckDocLinks.offences(List(dir))
    assert(found.isEmpty, s"expected '../' to resolve: $found")
  }

  it should "flag the same link when it forgets the '../'" in
  {
    val dir = fixture("target.md" -> "# target\n", "sub/page.md" -> "[up](target.md)\n")
    val (found, _) = CheckDocLinks.offences(List(dir))
    assert(found.size == 1, s"expected the missing '../' flagged: $found")
  }

  it should "check an html asset, which is how the shared logo breaks" in
  {
    val dir = fixture("sub/page.md" -> ("""<img src="logo_bw.svg" alt=""/>""" + "\n"))
    val (found, _) = CheckDocLinks.offences(List(dir))
    assert(found.size == 1, s"expected the image flagged: $found")
  }

  it should "ignore absolute urls, anchors and mdoc variables" in
  {
    // /app and /api MUST be absolute: both are produced after mdoc runs.
    val dir = fixture("page.md" ->
      """[app](https://lycogrypho.github.io/Leonardo/app/)
         |[mail](mailto:nobody@example.com)
         |[section](#heading)
         |`"it.grypho" %% "leonardo" % "@VERSION@"`
         |""".stripMargin)
    val (found, _) = CheckDocLinks.offences(List(dir))
    assert(found.isEmpty, s"expected all four ignored: $found")
  }

  it should "ignore a link inside a fenced code block" in
  {
    // A sample is not a link; flagging one would make the guard something to switch off.
    val dir = fixture("page.md" ->
      """```markdown
         |[an example](nowhere.md)
         |```
         |""".stripMargin)
    val (found, _) = CheckDocLinks.offences(List(dir))
    assert(found.isEmpty, s"expected the fenced sample ignored: $found")
  }

  it should "keep checking after a fence closes" in
  {
    val dir = fixture("page.md" ->
      """```
         |[ignored](nowhere.md)
         |```
         |[checked](alsonowhere.md)
         |""".stripMargin)
    val (found, _) = CheckDocLinks.offences(List(dir))
    assert(found.size == 1, s"expected only the link after the fence: $found")
  }

  it should "flag a wrongly cased directory, not only a wrongly cased file name (F_0215)" in
  {
    // `Lessons/` for `lessons/`: Windows and macOS resolve it, the Linux runner 404s.  On a
    // case-sensitive filesystem the link is broken outright, so it is flagged either way.
    val dir = fixture("lessons/l1.md" -> "# l1\n", "page.md" -> "[lesson](Lessons/l1.md)\n")
    val (found, _) = CheckDocLinks.offences(List(dir))
    assert(found.size == 1, s"expected the directory's case flagged: $found")
  }

  it should "strip a fragment before resolving" in
  {
    val dir = fixture("target.md" -> "# target\n", "page.md" -> "[deep](target.md#a-heading)\n")
    val (found, _) = CheckDocLinks.offences(List(dir))
    assert(found.isEmpty, s"expected the fragment stripped: $found")
  }

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

  it should "hold a Jekyll remote_theme to a SHA too (F_0217)" in
  {
    // The theme's scripts are published on the site's origin, beside /app: an unpinned theme
    // lets its owner change what runs there, the hole 2.10 closed for the workflows.
    val dir = fixture(
      "bare/_config.yml"   -> "remote_theme: just-the-docs/just-the-docs\n",
      "tagged/_config.yml" -> "remote_theme: \"just-the-docs/just-the-docs@v0.12.0\"\n",
      "pinned/_config.yml" -> "remote_theme: just-the-docs/just-the-docs@2ed4c0af735e95af5c0dcf7a605b07d903005b4e # v0.12.0\n")
    val (found, _) = CheckActionPins.offences(List(dir))
    assert(found.size == 2 && found.forall(_.contains("remote_theme")), found)
  }

  it should "agree with the site configuration this repository ships" in
  {
    val (found, _) = CheckActionPins.offences(CheckActionPins.DefaultRoots)
    assert(CheckActionPins.DefaultRoots.contains(Paths.get("docs", "_config.yml")))
    assert(found.isEmpty, s"unpinned: $found")
  }

  // --- the page's Content-Security-Policy (F_0112) -------------------------------------

  /** `leonardoStart();`'s SHA-256, computed outside the guard (and the hash Firefox accepted
   *  when the owner measured the page), so the guard is checked against an independent value. */
  private val StartHash = "'sha256-B5hDBCYSjSGBdp0jtHHlr4oyZJq4+lhIMhbJeyfn/qA='"

  /** A page with `policy` as its CSP and `body` after it. */
  private def page(policy: String, body: String): Path =
    fixture("index.html" ->
      s"""<html><head><meta http-equiv="Content-Security-Policy" content="$policy"></head>
         |<body>$body</body></html>
         |""".stripMargin).resolve("index.html")

  "the CSP guard" should "accept an inline script whose hash the policy lists" in
  {
    val p = page(s"default-src 'none'; script-src 'self' $StartHash",
                 "<script src=\"main.js\"></script><script>leonardoStart();</script>")
    assert(CheckCsp.offences(p).isEmpty, CheckCsp.offences(p))
  }

  it should "flag an inline script the policy does not list, naming the hash to add" in
  {
    val found = CheckCsp.offences(page("script-src 'self'", "<script>leonardoStart();</script>"))
    assert(found.size == 1 && found.head.contains(StartHash), found)
  }

  it should "flag a listed hash no inline script has" in
  {
    val found = CheckCsp.offences(page(s"script-src 'self' $StartHash", "<script src=\"main.js\"></script>"))
    assert(found.size == 1 && found.head.contains("stale"), found)
  }

  it should "flag an inline script that spans lines, whose bytes depend on the checkout" in
  {
    val found = CheckCsp.offences(page("script-src 'self'", "<script>\nleonardoStart();\n</script>"))
    assert(found.exists(_.contains("one line")), found)
  }

  it should "flag a page with no policy, and a script policy that allows inline or eval" in
  {
    assert(CheckCsp.offences(fixture("index.html" -> "<html><head></head></html>").resolve("index.html"))
             .exists(_.contains("no Content-Security-Policy")))
    assert(CheckCsp.offences(page(s"script-src 'self' 'unsafe-eval' $StartHash", "<script>leonardoStart();</script>"))
             .exists(_.contains("'unsafe-eval'")))
  }

  it should "flag an inline event handler, which the policy blocks without a word" in
  {
    val found = CheckCsp.offences(page("script-src 'self'", "<button onclick=\"go()\">Go</button>"))
    assert(found.exists(_.contains("onclick")), found)
  }

  it should "find nothing wrong with the page this repository ships" in
  {
    val found = CheckCsp.offences(Paths.get("web", "index.html"))
    assert(found.isEmpty, found)
  }

  // --- the Scala version -------------------------------------------------------------

  "the Scala-version guard" should "flag a build path that spells the version" in
  {
    val dir = fixture("w.yml" -> "    run: cat web/target/scala-3.3.8/leonardo-web-opt/main.js\n")
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
    // METHODOLOGY.md joined this list on 2026-09-30, having been overlooked when F_0028 widened
    // the roots, and left it on 2026-10-05 when the document moved outside the repository.
    for local <- List("ToDo.md", "Done.md", "DesignNotes.md") do
      assert(Files.GuardedRoots.contains(Paths.get(local)), s"$local should be guarded when present")
  }

  // --- reserved words (F_0148) ------------------------------------------------------------

  private def parserFile(reserved: String, grammar: String): Path =
    fixture("Parser.scala" ->
      s"object Parser:\n  val ReservedWords: Set[String] = Set(\n    $reserved\n  )\n$grammar\n").resolve("Parser.scala")

  "the reserved-word guard" should "flag a production whose word is not reserved" in
  {
    val found = CheckReservedWords.offences(parserFile("\"sin\"", "  val f = \"sin(\" | \"cos(\""))
    assert(found.size == 1 && found.head.contains("cos"), found)
  }

  it should "pass when every production's word is reserved, and ignore words quoted in comments" in
  {
    val grammar = "  val f = \"sin(\" // \"tan(\" is mentioned here\n  /* and \"atan(\" here */"
    assert(CheckReservedWords.offences(parserFile("\"sin\", \"cos\" // a comment, \"x\"", grammar)).isEmpty)
  }

  it should "read the words of a production generated from an enum (F_0215)" in
  {
    // The StructureKind shape: `Kind.values.toList.map { k => (k.word + "(") ~> … }`.
    val dir = fixture(
      "parser/Parser.scala" ->
        ("object Parser:\n  val ReservedWords: Set[String] = Set(\n    \"sin\", \"alpha\"\n  )\n" +
         "  val f = \"sin(\" | Kind.values.toList.map { k =>\n    (k.word + \"(\") ~> x\n  }\n"),
      "control/Kind.scala" ->
        ("enum Kind(val word: String):\n  case A extends Kind(\"alpha\")\n  case B extends Kind(\"beta\")\n"))
    val found = CheckReservedWords.offences(dir.resolve("parser").resolve("Parser.scala"))
    assert(found.size == 1 && found.head.contains("beta"), found)
  }

  it should "refuse a generated production whose words it cannot read" in
  {
    // Failing closed: a shape the guard does not know would otherwise be checked by nothing.
    val dir = fixture("parser/Parser.scala" ->
      "object Parser:\n  val ReservedWords: Set[String] = Set(\n    \"sin\"\n  )\n  val f = (name + \"(\") ~> x\n")
    val found = CheckReservedWords.offences(dir.resolve("parser").resolve("Parser.scala"))
    assert(found.size == 1 && found.head.contains("cannot read"), found)
  }

  it should "see the real grammar's productions, so its silence is not vacuous" in
  {
    val words = CheckReservedWords.productions(Paths.get("core", "src", "main", "scala", "parser", "Parser.scala"))
    assert(words.size > 150 && words.contains("fourierCausal") && words.contains("sin"), words.size)
    assert(words.contains("ctrb") && words.contains("observable"), "the StructureKind words are read")
    assert(CheckReservedWords.offences(Paths.get("core", "src", "main", "scala", "parser", "Parser.scala")).isEmpty)
  }

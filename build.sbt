ThisBuild / scalaVersion := "3.3.6"

// The release tags in this repository are bare (`3.6.3`), not `v`-prefixed. sbt-dynver
// defaults to matching only `v*`, so without this it silently ignored every bare tag and
// derived the version from the abandoned `v2.0.0` instead -- reporting `2.0.0+158-<sha>`
// on a commit whose own tag says `3.6.3`. Publishing that would have shipped a version
// number both wrong and *lower* than the project's visible history.
ThisBuild / dynverVTagPrefix := false

// The project licence, declared here as well as in LICENSE/NOTICE because this is the copy
// that reaches the published POM -- Maven Central rejects an artifact without it, and a
// consumer's licence-audit tooling reads the POM, never the repository. The SPDX identifier
// must match the LICENSE file exactly; changing one without the other is how a project ends
// up claiming two different licences in two places.
ThisBuild / licenses := Seq(
  "Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")
)

// Declares how to read this project's version numbers, and it reaches the published POM.
// Without it sbt warns on every publish, and downstream builds cannot tell a genuine binary
// incompatibility from a benign eviction -- they see two versions and no rule relating them.
// `early-semver` is the Scala-ecosystem reading of semver: within 1.x and above, the MINOR
// component promises binary compatibility, so 3.7.1 -> 3.7.2 is safe and 3.7 -> 3.8 is not.
ThisBuild / versionScheme := Some("early-semver")

// ── Maven Central coordinates and POM metadata (issue 5.2 phase 2) ─────────────
// Central REJECTS an artifact missing any of name, description, url, licences, scm or
// developers -- these are not decoration, they are validation gates. All are ThisBuild-scoped
// so they survive the module split of phase 1.1 unchanged; only `name`/`moduleName`/
// `description` are per-project and would move into each module's settings.
ThisBuild / organization     := "it.grypho"
ThisBuild / organizationName := "Grypho"
ThisBuild / homepage         := Some(url("https://lycogrypho.github.io/Leonardo/"))
ThisBuild / description :=
  "A Scala 3 symbolic mathematics library and Computer Algebra System: parsing, " +
  "simplification, calculus, linear algebra, equations, logic, probability and statistics."

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/Lycogrypho/Leonardo"),
    "scm:git:https://github.com/Lycogrypho/Leonardo.git",
    Some("scm:git:git@github.com:Lycogrypho/Leonardo.git")
  )
)

ThisBuild / developers := List(
  Developer(
    id    = "Lycogrypho",
    name  = "Cosimo Attanasi",
    email = "c.attanasi@grypho.it",
    url   = url("https://github.com/Lycogrypho")
  )
)

// The Central Portal, NOT the legacy OSSRH host. `it.grypho` is namespace-verified there, and
// the old oss.sonatype.org staging endpoints are retired -- pointing at them fails at upload
// rather than at validation, which is a confusing way to learn the host is wrong.
ThisBuild / sonatypeCredentialHost := xerial.sbt.Sonatype.sonatypeCentralHost

// NOTE on the published artifactId: it is `leonardo_3`, lowercase, and needs no setting here.
// sbt derives `moduleName` from `normalizedName`, which lowercases `name`, so the capitalised
// display name and the conventional lowercase coordinate coexist for free. An explicit
// `ThisBuild / moduleName` is worse than useless -- sbt defines that key at PROJECT scope, so
// a ThisBuild value is shadowed rather than inherited, and the build reports it as unused.
// The `_3` suffix is the Scala binary version, appended at publish time by `%%`.

// ThisBuild, not bare: a bare `scalacOptions ++=` in build.sbt applies to the ROOT project
// only, so after the module split the repl module would silently compile without -explain,
// -deprecation or the package.scala warning suppression.
ThisBuild / scalacOptions ++= Seq(
  "-explain", "-deprecation", "-feature",
  // package.scala files contain only a chained package clause + a /** */ doc comment;
  // scalac warns "No class/trait/object defined" because nothing is compiled per se.
  // The Scaladoc association is correct — only Zinc's dependency tracking is limited.
  "-Wconf:src=.*package\\.scala:silent"
)

// idePackagePrefix is consumed by the IntelliJ sbt import, not by any sbt task;
// exclude it from the lintUnused warning printed at every startup.
Global / excludeLintKeys += idePackagePrefix

// ── PlantUML ──────────────────────────────────────────────────────────────────
// Hidden Ivy config — PlantUML JAR is resolved by sbt but never placed on the
// compile or test classpath. `sbt puml` runs `java -jar plantuml.jar` via
// Process; Graphviz is NOT required because structure.puml uses `!pragma layout
// smetana` (PlantUML's built-in pure-Java layout engine).
// Check https://mvnrepository.com/artifact/net.sourceforge.plantuml/plantuml
// for the latest version and update the string below if needed.
lazy val PlantUML = config("plantuml").hide
lazy val puml     = taskKey[Unit]("Regenerate docs/src/structure.svg from docs/structure.puml")

// ── Scaladoc CSS injection ─────────────────────────────────────────────────────
// Scaladoc 3 copies _assets/ from the siteroot but does NOT inject <link> tags
// for custom CSS files.  This task runs after `sbt doc` and patches every HTML
// file in the api output: it copies docs/src/_assets/styles/custom.css into the
// output's styles/ directory and inserts a <link> element (with the correct
// relative path) just before </head>.  Re-running the task is idempotent because
// files already containing "custom.css" are skipped.
lazy val injectApiStyles = taskKey[Unit]("Inject custom CSS into generated scaladoc HTML pages")

// ── Module layout (issue 5.2 phase 1.1) ───────────────────────────────────────
// `core` is the library and publishes as `it.grypho:leonardo_3`; `replModule` is the
// interactive REPL and the only thing that needs JLine. Splitting them means a library
// consumer no longer drags in a terminal library they will never call, and it is what makes a
// Scala.js / Scala Native cross-build conceivable later, since JLine is the one thing that
// currently forbids it.
//
// The split is a BUILD change, not a code change: nothing outside `cli` ever imported it
// (verified -- the only occurrences elsewhere are in doc comments), which is what made it
// cheap. `Main.scala` moved across too: it is a demo entry point for humans, exactly like the
// REPL, and a published *library* artifact should carry no main class at all.
//
// `root` is a PURE AGGREGATE that publishes nothing -- it exists so `sbt test`, `sbt site`
// and `sbt ci-release` still work from one place. Making root itself the library instead
// looks tempting (it would have avoided moving 163 files into core/) but is a trap: root
// would have to aggregate replModule while replModule dependsOn root, and those two vals are
// then mutually recursive. Scala 3 first refuses to infer their types, and once annotated
// `: Project` the initialisation itself recurses -- `sbt projects` dies with a
// StackOverflowError. A stringly-typed `LocalProject("replModule")` breaks the cycle, but the
// conventional three-project layout has no cycle to break and stays correct when a third
// module appears.
lazy val root = (project in file("."))
  .enablePlugins(ScalaUnidocPlugin)
  .aggregate(core, replModule)
  .settings(
    name           := "Leonardo",
    // Nothing to publish from the aggregate: the artifacts are core's and replModule's. An
    // aggregate that publishes would ship an empty jar under a third coordinate.
    publish / skip := true,

    // ── PlantUML dependency (resolved, never on project classpath) ─────────────
    ivyConfigurations += PlantUML,
    libraryDependencies += "net.sourceforge.plantuml" % "plantuml" % "1.2026.0" % PlantUML,

    puml := {
      import scala.sys.process._
      val log = streams.value.log
      val jar = update.value
        .select(configurationFilter("plantuml"))
        .find(_.getName startsWith "plantuml")
        .getOrElse(sys.error("plantuml JAR not resolved — run `sbt update` first"))
      val src      = (baseDirectory.value / "docs" / "structure.puml").getAbsolutePath
      val out      = (baseDirectory.value / "docs" / "src").getAbsolutePath
      val dotArgs  = sys.env.get("GRAPHVIZ_DOT").toList.flatMap(d => List("-graphvizdot", d))
      // PlantUML reports a SYNTAX ERROR as a warning ("no image in ...") and still exits 0,
      // so the exit code alone is not evidence that anything was produced. Left unchecked
      // this task claims success while silently leaving the previous SVG in place -- which
      // is exactly what happened once, and a stale diagram was committed before anyone
      // noticed. Capture the output and treat that warning as the failure it is, and verify
      // the file was actually rewritten.
      val svg      = baseDirectory.value / "docs" / "src" / "structure.svg"
      val before   = if (svg.exists) svg.lastModified else 0L
      val output   = new StringBuilder
      val collect  = (s: String) => { output.append(s).append('\n'); () }
      val rc       = (Seq("java", "-jar", jar.getAbsolutePath) ++ dotArgs ++ Seq(src, "-tsvg", "-o", out))
                       .!(ProcessLogger(s => { collect(s); log.info(s) }, s => { collect(s); log.warn(s) }))
      if (rc != 0) sys.error(s"PlantUML exited with code $rc")
      if (output.toString.contains("no image in"))
        sys.error("PlantUML found no diagram in docs/structure.puml -- it is a syntax error, " +
                  "and the previous structure.svg is now stale. Fix the .puml and re-run.")
      if (!svg.exists || svg.lastModified == before)
        sys.error(s"PlantUML reported success but did not write ${svg.getName}")

      // PlantUML decorates the SVG with its own tracking metadata -- data-qualified-name,
      // data-source-line, data-entity-1/2, data-link-type, codeLine -- plus an HTML-form
      // `title` on every <a> and two xlink attributes that only restate their own defaults.
      // None of that is valid SVG 1.1, so an XML schema inspection rejects the committed
      // file: this is what refused a commit once the regenerated diagram entered a
      // changeset, even though the same attributes had been sitting in the committed file
      // all along (the IDE only inspects files being committed).
      //
      // It is metadata only. Every attribute the diagram actually needs is left alone --
      // `href`, `xlink:href`, `target` and `xlink:title` carry the links, and `xlink:show`
      // is not a default. Strip rather than pin the PlantUML version, so the cleanup keeps
      // working when the version is bumped.
      val strip = List(
        """\s+data-qualified-name="[^"]*"""",
        """\s+data-source-line="[^"]*"""",
        """\s+data-entity-1="[^"]*"""",
        """\s+data-entity-2="[^"]*"""",
        """\s+data-link-type="[^"]*"""",
        """\s+codeLine="[^"]*"""",
        """\s+title="[^"]*"""",           // the <a> duplicate; xlink:title survives
        """\s+xlink:actuate="onRequest"""",
        """\s+xlink:type="simple""""
      )
      val raw     = IO.read(svg)
      val cleaned = strip.foldLeft(raw)((s, re) => s.replaceAll(re, ""))
      if (cleaned != raw) {
        IO.write(svg, cleaned)
        log.info(s"Stripped ${raw.length - cleaned.length} bytes of non-standard PlantUML attributes")
      }
      log.success("Generated docs/src/structure.svg")
    },

    // ── Scaladoc 3 static site ────────────────────────────────────────────────
    // After `sbt docs/mdoc`, the verified markdown in target/mdoc/ is used as the site root:
    // Scaladoc 3 renders those pages alongside the API reference. Run `sbt site` to produce
    // the full site in target/scala-3.3.6/api/.
    //
    // These options hang off UNIDOC, not `Compile / doc`, and the distinction is deliberate:
    // `Compile / doc` still produces each module's own plain API, which is what `packageDoc`
    // publishes as the -javadoc.jar. Attaching the static site there would bundle the whole
    // prose site into the published artifact.
    ScalaUnidoc / unidoc / scalacOptions ++= Seq(
      "-siteroot",        (target.value / "mdoc").getAbsolutePath,
      "-project",         "Leonardo",
      "-project-version", version.value,
      // Banner.svg, matching the Jekyll sidebar (`logo:` in docs/_config.yml), so the two
      // published views of the project are branded identically. It also carries the word mark,
      // which is what a sidebar title should do, and has intrinsic dimensions (402x117) where
      // logo2.svg has only a viewBox. Absolute, because a relative path here resolves against
      // the JVM's working directory rather than the project base.
      "-project-logo",    (baseDirectory.value / "docs" / "src" / "Banner.svg").getAbsolutePath
    ),

    // unidoc defaults to target/scala-3.3.6/unidoc. Redirect it to the `api` directory the
    // Pages workflow copies and `injectApiStyles` patches, so the split changes no path
    // outside this file.
    ScalaUnidoc / unidoc / target := target.value / s"scala-${scalaVersion.value}" / "api",

    // ── Custom CSS injection ───────────────────────────────────────────────────
    injectApiStyles := {
      val log    = streams.value.log
      val apiDir = target.value / s"scala-${scalaVersion.value}" / "api"
      val source = baseDirectory.value / "docs" / "src" / "_assets" / "styles" / "custom.css"
      if (!apiDir.exists || !source.exists) {
        log.warn(s"injectApiStyles: skipping — apiDir or source missing")
      } else {
        IO.copyFile(source, apiDir / "styles" / "custom.css")
        val htmlFiles = (apiDir ** "*.html").get.filter(_.isFile)
        var count = 0
        htmlFiles.foreach { f =>
          val html = IO.read(f)
          if (!html.contains("custom.css")) {
            val rel   = IO.relativize(apiDir, f).getOrElse(f.getName)
            val depth = rel.replace('\\', '/').count(_ == '/')
            val link  = s"""<link rel="stylesheet" href="${"../" * depth}styles/custom.css">"""
            IO.write(f, html.replace("</head>", link + "</head>"))
            count += 1
          }
        }
        log.info(s"Custom CSS injected into $count scaladoc HTML pages")
      }
    }
  )

// ── The library: everything except the REPL ───────────────────────────────────
// Published as `it.grypho:leonardo_3` -- the plain coordinate, since this is what a consumer
// almost always wants. The directory is `core/` (the conventional name for the base module)
// while the artifact stays `leonardo`; `name` drives both, via sbt's `normalizedName`, which
// lowercases it.
lazy val core = (project in file("core"))
  .settings(
    name             := "Leonardo",
    idePackagePrefix := Some("it.grypho.scala.leonardo"),

    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",

    // Spire powers the exact-arithmetic tier's irrational engine (issue 4.N): `Real` computes
    // a transcendental to any requested precision, replacing the ~15-digit `Double` ceiling
    // that tier 1 had to live with. MIT licensed, so one-way compatible with Apache-2.0.
    //
    // PINNED DELIBERATELY. spire_3 has exactly one stable release, 0.18.0 of June 2022, and
    // the transitive `typelevel/algebra` is archived. The licence is what bounds that risk: if
    // spire is ever truly abandoned, the handful of `Real` sources can be vendored in under
    // Apache-2.0 with the MIT notice preserved and recorded in NOTICE. Do not plan on upgrades.
    libraryDependencies += "org.typelevel" %% "spire" % "0.18.0"
  )

// ── mdoc lives in its own project, and MUST NOT be enabled on root ─────────────
// MdocPlugin adds `org.scalameta:mdoc` to the enabled project's libraryDependencies, and
// from there it lands in the PUBLISHED POM at compile scope -- so every consumer of the
// library would transitively resolve a documentation tool and its whole dependency tree.
// mdoc is a build-time tool here, exactly like PlantUML, and belongs on no consumer's
// classpath.
//
// `mdocAutoDependency := false` looks like the one-line fix and is NOT: mdoc runs by
// invoking `mdoc.SbtMain` from the enabled project's own classpath, so removing the
// dependency makes `sbt mdoc` fail with ClassNotFoundException. Verified, not assumed.
// A separate project is the actual answer -- it is also what mdoc's own documentation
// recommends -- and `publish / skip` keeps it out of Central entirely.
//
// Base directory `docs/` only so the project has one; it compiles nothing (there is no
// docs/src/main/scala). `target` is redirected under the root's target/ so that no build
// output is written inside the directory Jekyll publishes from.
lazy val docs = (project in file("docs"))
  .enablePlugins(MdocPlugin)
  // Examples import `it.grypho.scala.leonardo.*`, so the library must be on the classpath
  // mdoc compiles against -- that dependency is the entire reason this project exists rather
  // than being a bare tool invocation.
  // BOTH modules: docs/src/getting-started.md has an mdoc block that imports `cli.Session`,
  // so depending on the library alone would break the site build rather than the compile.
  .dependsOn(core, replModule)
  .settings(
    name           := "leonardo-docs",
    publish / skip := true,

    target := (ThisBuild / baseDirectory).value / "target" / "docs-project",

    // Source markdown lives in docs/src/; mdoc compiles every scala mdoc block and writes
    // verified markdown to the ROOT's target/mdoc/. Keeping the output there is load-bearing:
    // `-siteroot` below and the Jekyll `--source ../target/mdoc` in pages.yml both read that
    // exact path, and a project-local output would silently orphan both.
    mdocIn  := (ThisBuild / baseDirectory).value / "docs" / "src",
    mdocOut := (ThisBuild / baseDirectory).value / "target" / "mdoc",
    mdocVariables := Map(
      "VERSION"       -> version.value,
      "SCALA_VERSION" -> scalaVersion.value
    )
  )

// ── The interactive REPL, and the ONLY module that needs JLine ─────────────────
// This is the whole point of the split: JLine powers the REPL's line editing and persistent
// history (see cli/Repl.scala), it is not on the library's public API, and the read loop
// degrades to a plain dumb terminal when no interactive console is attached (piped input,
// CI). A library consumer should never have resolved it, and now does not.
//
// The project *id* is `replModule`, not `repl`, deliberately: `repl` is a command alias below,
// and a project of the same name would make `sbt repl` ambiguous. The directory and the
// published artifact are both plainly `repl` / `leonardo-repl`.
lazy val replModule = (project in file("repl"))
  .dependsOn(core)
  .settings(
    name             := "leonardo-repl",
    idePackagePrefix := Some("it.grypho.scala.leonardo"),
    libraryDependencies += "org.jline" % "jline" % "3.30.15"
  )

// ThisBuild: both modules have test suites, and a bare `libraryDependencies +=` would give
// ScalaTest to root only -- leaving the four cli suites uncompilable in the repl module.
ThisBuild / libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"

ThisBuild / libraryDependencies += "org.scalatest" %% "scalatest-flatspec" % "3.2.19" % "test"

// Shortcut for the interactive REPL: `sbt repl` instead of the full runMain path.
// Project-qualified since the split -- `cli` lives in the repl module now.
addCommandAlias("repl", "replModule/runMain it.grypho.scala.leonardo.cli.repl")
// The parser demo, likewise moved: a published library artifact carries no main class.
addCommandAlias("demo", "replModule/runMain it.grypho.scala.leonardo.main")
// Full site: regenerate UML diagram → validate code examples → Scaladoc site + CSS.
// `docs/mdoc`, not `mdoc`: MdocPlugin is enabled on the docs project, not on root, so that
// the mdoc dependency stays out of the published POM. `unidoc`, not `doc`: since the module
// split, `doc` would render the library alone and drop `cli` from the API reference.
addCommandAlias("site", ";puml;docs/mdoc;unidoc;injectApiStyles")
// The gcd-policy sweep of issue 4.M. Test-scoped and on demand: it takes minutes, so it
// must not run under `sbt test` — only its cross-policy equality check does, in RationalTest.
addCommandAlias("bench", "core/Test/runMain it.grypho.scala.leonardo.core.RationalBenchmark")

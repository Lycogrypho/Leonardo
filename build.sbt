// MiMa's filter vocabulary (issue 2.9). `ProblemFilters` and the `Problem` hierarchy are not
// among the keys sbt auto-imports into a .sbt file, so without this a filter fails to compile
// with a bare "not found: value ProblemFilters". Kept while the filter list is empty: the next
// intentional break needs it, and that error message does not point at a missing import.
import com.typesafe.tools.mima.core.{MissingClassProblem, Problem, ProblemFilters}

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

// ── Binary compatibility (issue 2.9) ──────────────────────────────────────────
// The release each module is checked against by MiMa. `versionScheme := early-semver` above
// PROMISES that a patch release stays binary-compatible; MiMa is what makes that promise
// enforced rather than merely declared, and it only became checkable when 3.7.1 published the
// first baseline. Bump this on every release, and prefer bumping it in the release commit so
// the value and the tag cannot drift apart -- 3.7.2 shipped without the bump, which is how it
// stayed on 3.7.1 for a release.
lazy val mimaBaseline = "3.7.2"

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
  // The JS projects are aggregated so `sbt test` covers them; they are filtered out of unidoc
  // below, since they compile the same shared sources as their JVM twins.
  .aggregate(core.jvm, core.js, replModule.jvm, replModule.js)
  .settings(
    name           := "Leonardo",
    // Nothing to publish from the aggregate: the artifacts are core's and replModule's. An
    // aggregate that publishes would ship an empty jar under a third coordinate.
    publish / skip := true,

    // A project that publishes nothing has no previous artifact to compare against, and MiMa
    // fails by default when it finds none — a guard that would otherwise be tripped by the
    // aggregate rather than by a real incompatibility.
    mimaPreviousArtifacts   := Set.empty,
    mimaFailOnNoPrevious    := false,

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

    // core.js is excluded: it compiles the SAME shared sources as core.jvm, so including it
    // would feed unidoc two copies of every class and duplicate the whole API reference.
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(core.jvm, replModule.jvm),

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
// CROSS-BUILT for the JVM and Scala.js since F_0003 phase 1.  `core.jvm` is what publishes
// and is byte-for-byte the project this used to be; `core.js` exists to prove the library
// runs off the JVM and to keep that true -- `ci.yml` runs its suite.
//
// CrossType.Pure, NOT CrossType.Full, and the choice is worth stating: Full expects sources
// under `core/shared/src/main/scala` and would have MOVED 163 files for the sake of one
// platform-specific object.  Pure leaves every shared source exactly where it is and costs
// only the two `unmanagedSourceDirectories` lines below.  The `.jvm` / `.js` directories it
// creates under `core/` hold build output only and are already ignored by the unanchored
// `target/` rule.
lazy val core = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    name             := "Leonardo",
    idePackagePrefix := Some("it.grypho.scala.leonardo"),

    libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.4.0",
    libraryDependencies += "org.typelevel"          %%% "spire"                    % "0.18.0",
    libraryDependencies += "org.scalatest"          %%% "scalatest"                % "3.2.19" % Test
  )
  .jvmSettings(
    // Platform sources live beside the shared tree rather than under it, because CrossType.Pure
    // reserves `core/.jvm` and `core/.js` for build output.  `baseDirectory` for coreJVM IS
    // `core/.jvm`, hence the `getParentFile`.
    Compile / unmanagedSourceDirectories += baseDirectory.value.getParentFile / "jvm-src" / "main" / "scala",

    // Binary compatibility against the previous release (issue 2.9).
    mimaPreviousArtifacts := Set("it.grypho" %% "leonardo" % mimaBaseline),
    // Empty on purpose, and the emptiness is the point: this list is the record of every
    // deliberate incompatibility still in force, so a filter that no longer suppresses
    // anything is noise that makes the real ones harder to trust.
    //
    // It held one entry until the baseline moved to 3.7.2. The `expr` package -- a single
    // enum, `EvalResult`, that nothing ever referenced (issue 2.7) -- was removed after
    // 3.7.1 published it, so while the baseline WAS 3.7.1 its absence read as a binary
    // break and had to be declared. 3.7.2 shipped that removal, so against a 3.7.2 baseline
    // there is nothing to compare and nothing to excuse: the filter retired with the
    // baseline it existed for, which is the normal end of every entry here.
    mimaBinaryIssueFilters ++= Seq()
  )
  .jsSettings(
    Compile / unmanagedSourceDirectories += baseDirectory.value.getParentFile / "js-src" / "main" / "scala",

    // MiMa is disabled here because there is no baseline to check against: `leonardo_sjs1_3`
    // has never been published, so `mimaPreviousArtifacts` would ask Coursier for an artifact
    // that does not exist and fail the build rather than report an incompatibility.
    mimaPreviousArtifacts := Set.empty,

    // PHASE 1 DOES NOT PUBLISH A JS ARTIFACT.  Proving the port and committing to support it
    // are different decisions, and publishing is the irreversible one -- a coordinate on
    // Central can never be withdrawn.  Flip this when the browser front end (phase 3) gives
    // the artifact a consumer; until then the JS build exists to be tested, not resolved.
    publish / skip := true
  )

// Spire powers the exact-arithmetic tier's irrational engine (issue 4.N): `Real` computes a
// transcendental to any requested precision, replacing the ~15-digit `Double` ceiling that
// tier 1 had to live with. MIT licensed, so one-way compatible with Apache-2.0.
//
// PINNED DELIBERATELY. spire_3 has exactly one stable release, 0.18.0 of June 2022, and the
// transitive `typelevel/algebra` is archived. The licence is what bounds that risk: if spire
// is ever truly abandoned, the handful of `Real` sources can be vendored in under Apache-2.0
// with the MIT notice preserved and recorded in NOTICE. Do not plan on upgrades.
//
// Note the `%%%` above rather than `%%`: in a crossProject that resolves `spire_3` for the JVM
// and `spire_sjs1_3` for Scala.js. Both exist at 0.18.0 -- verified on Central before the
// cross-build was attempted, since a missing JS artifact would have sunk it.

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
  .dependsOn(core.jvm, replModule.jvm)
  .settings(
    name           := "leonardo-docs",
    publish / skip := true,

    // Publishes nothing, so there is no baseline to compare against (see root).
    mimaPreviousArtifacts := Set.empty,
    mimaFailOnNoPrevious  := false,

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
// CROSS-BUILT since F_0003 phase 2, on the same CrossType.Pure layout as core: shared sources
// stay in `repl/src`, platform code lives in `repl/jvm-src` and `repl/js-src`.
//
// `Session` -- the whole of the REPL's behaviour -- was ALREADY platform-free and needed no
// change: `execute`, `script` and `load` are pure string in / string out, and nothing above the
// read loop ever touched JLine. That is what made this phase cheap, and it was the finding
// phase 1 rested on. Only three things are JVM-only: the JLine read loop and its Greek chords
// (`jvm-src/cli/Terminal.scala`), the highlighter, and the `Main` demo.
lazy val replModule = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("repl"))
  .dependsOn(core)
  .settings(
    name             := "leonardo-repl",
    idePackagePrefix := Some("it.grypho.scala.leonardo"),

    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % Test
  )
  .jvmSettings(
    Compile / unmanagedSourceDirectories += baseDirectory.value.getParentFile / "jvm-src" / "main" / "scala",
    Test    / unmanagedSourceDirectories += baseDirectory.value.getParentFile / "jvm-src" / "test" / "scala",

    // JLine is confined to the JVM side, which is the point of the split: it is the one
    // dependency a browser cannot have, and the module boundary that made the 5.2 artifact
    // split worthwhile is the same one that makes this work.
    libraryDependencies += "org.jline" % "jline" % "3.30.15",

    // Binary compatibility against the previous release (issue 2.9).
    mimaPreviousArtifacts := Set("it.grypho" %% "leonardo-repl" % mimaBaseline),

    mimaBinaryIssueFilters ++= Seq(
      // INTENTIONAL, and an artefact of Scala 3's encoding rather than an API change.
      //
      // Scala 3 gathers a file's TOP-LEVEL defs and vals into a synthetic class named after
      // the FILE, so `Repl.scala` produced `cli.Repl$package`. Phase 2 moved the read loop and
      // its key bindings out of that file into `jvm-src/cli/Terminal.scala`, because they are
      // JLine-bound and cannot cross-compile -- so those members now live in
      // `cli.Terminal$package` and the old class is gone. Nothing was removed and no signature
      // changed: the definitions were renamed by being relocated.
      //
      // Safe because nothing outside the module referenced it. The launcher entry point is the
      // class `@main def repl` generates, named `cli.repl` independently of its file, so
      // `cs launch -M it.grypho.scala.leonardo.cli.repl` is unaffected; every other moved
      // member is `private[cli]`. `Session`, which IS this module's API, did not move.
      //
      // The rule worth carrying: in Scala 3, moving a top-level `def` or `val` to a
      // differently-named file is a BINARY change even though no source consumer can tell.
      // Splitting a file for a cross-build is exactly when that bites.
      ProblemFilters.exclude[MissingClassProblem]("it.grypho.scala.leonardo.cli.Repl$package"),
      ProblemFilters.exclude[MissingClassProblem]("it.grypho.scala.leonardo.cli.Repl$package$")
    )
  )
  .jsSettings(
    Compile / unmanagedSourceDirectories += baseDirectory.value.getParentFile / "js-src" / "main" / "scala",
    Test    / unmanagedSourceDirectories += baseDirectory.value.getParentFile / "js-src" / "test" / "scala",

    // As for coreJS: no published baseline to compare against, and phase 2 proves the port
    // rather than committing to support a published JS artifact.
    mimaPreviousArtifacts := Set.empty,
    publish / skip        := true
  )

// The browser front end (F_0003 phase 3).  JS ONLY and publishes NOTHING: it emits static
// files for the Pages site, so it has no coordinate and no consumer to stay compatible with.
//
// The size probe it started as was not a placeholder: `fullLinkJS` on a library with no entry
// point emits an EMPTY directory, because dead-code elimination removes everything
// unreachable.  That is why phase 1 could not measure the bundle and why this module is what
// finally could — a figure exists only once something calls the library.
//
// NOT aggregated by root, and that is a judgement rather than an oversight: linking a bundle
// on every `sbt test` would put front-end work in the path of every library change.  The cost
// is that `web/test` must be named explicitly, which ci.yml does.
lazy val web = (project in file("web"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(replModule.js)
  .settings(
    name           := "leonardo-web",
    publish / skip := true,

    // %%% and not %%: this project is Scala.js only, and the JVM `scalatest_3` artifact landing
    // on a Scala.js classpath beside `scalatest_sjs1_3` is a LINKER error rather than a
    // resolution one, so it fails late and confusingly (see the ThisBuild note below).
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % Test,

    // Publishes nothing, so there is no baseline to compare against (see root).
    mimaPreviousArtifacts := Set.empty,

    // NoModule: the output is a plain <script> the Pages site can include, with the exported
    // entry points reachable as globals.  ESModule would be the choice if a bundler were ever
    // introduced -- there is deliberately none, matching the vendored-not-bundled stance the
    // docs site already takes with svg-pan-zoom.
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.NoModule) }
  )

// ── The repository's own guards, in the repository's own language (issue F_0027) ────────
// The standing rule: **no program in another language lives in this repository unless it
// strictly must**.  Four Python checks did, and the alternative to porting them was a
// `scala-cli` toolchain CI does not have — a third-party setup action, or a coursier
// bootstrap.  The JDK and sbt are installed in every job already, so an ordinary project
// costs no new tooling and buys what a loose script cannot have: the checks are compiled,
// and they are TESTED.  `fix-mojibake` in particular had none, and it is the subtlest tool
// here.  `docs` is the precedent for a project that publishes nothing and exists to run one.
//
// Depends on nothing: these read files, not expressions.  NOT aggregated by root, following
// `web` rather than `docs` — its tests are repository hygiene, not library behaviour, and
// folding them into `sbt test` would quietly change what that number means.  ci.yml names
// `tools/test` explicitly, as it already does for `web/test`.
lazy val tools = (project in file("tools"))
  .settings(
    name           := "leonardo-tools",
    publish / skip := true,

    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test,

    // Publishes nothing, so there is no baseline to compare against (see root).
    mimaPreviousArtifacts := Set.empty,
    mimaFailOnNoPrevious  := false,

    // FORKED, and that is a correctness matter rather than a preference: a guard's answer IS
    // its exit code, and `sys.exit` in sbt's own JVM terminates the SESSION -- so the first
    // check of the `checks` alias would end the run and the other three would silently never
    // happen, reported as success. Forked, the exit code is a real one and sbt fails the task
    // on anything non-zero.
    Compile / run / fork := true,

    // A forked run starts in the PROJECT's directory, so `core/src` would resolve under
    // `tools/`. Every guard is given paths relative to the repository root, which is also
    // where ci.yml and a contributor stand when they run them.
    Compile / run / baseDirectory := (ThisBuild / baseDirectory).value
  )

// ScalaTest is declared PER PROJECT rather than on ThisBuild since the cross-build (F_0003).
// A `ThisBuild / libraryDependencies += ... %% "scalatest"` reaches core.js too, and the JVM
// artifact `scalatest_3` would land on the Scala.js classpath beside `scalatest_sjs1_3` --
// two jars carrying the same fully-qualified class names, which is a linker error rather than
// a resolution one and so fails late and confusingly.  core declares it with `%%%` (see its
// settings); replModule is JVM-only and declares it here.
ThisBuild / libraryDependencies ++= Nil

// Shortcut for the interactive REPL: `sbt repl` instead of the full runMain path.
// Project-qualified since the split -- `cli` lives in the repl module now.
addCommandAlias("repl", "replModuleJVM/runMain it.grypho.scala.leonardo.cli.repl")
// The parser demo, likewise moved: a published library artifact carries no main class.
addCommandAlias("demo", "replModuleJVM/runMain it.grypho.scala.leonardo.main")
// Full site: regenerate UML diagram → validate code examples → Scaladoc site + CSS.
// `docs/mdoc`, not `mdoc`: MdocPlugin is enabled on the docs project, not on root, so that
// the mdoc dependency stays out of the published POM. `unidoc`, not `doc`: since the module
// split, `doc` would render the library alone and drop `cli` from the API reference.
addCommandAlias("site", ";puml;docs/mdoc;unidoc;injectApiStyles")
// The gcd-policy sweep of issue 4.M. Test-scoped and on demand: it takes minutes, so it
// must not run under `sbt test` — only its cross-policy equality check does, in RationalTest.
addCommandAlias("bench", "core/Test/runMain it.grypho.scala.leonardo.core.RationalBenchmark")
// Every repository guard, in ONE sbt boot (issue F_0027): four separate `sbt` invocations
// would be four JVM starts, which is the only cost the Python originals did not have. A
// contributor runs exactly what ci.yml runs, which is the point of an alias over a list of
// commands copied into a workflow.
addCommandAlias("checks",
  ";tools/runMain it.grypho.scala.leonardo.tools.mojibake --check core/src repl/src web/src tools/src docs" +
  ";tools/runMain it.grypho.scala.leonardo.tools.charset core/src repl/src web/src tools/src docs" +
  ";tools/runMain it.grypho.scala.leonardo.tools.actionPins" +
  ";tools/runMain it.grypho.scala.leonardo.tools.scalaVersion")

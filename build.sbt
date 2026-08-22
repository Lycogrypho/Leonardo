ThisBuild / scalaVersion := "3.3.6"

scalacOptions ++= Seq(
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

lazy val root = (project in file("."))
  .enablePlugins(MdocPlugin)
  .settings(
    name             := "Leonardo",
    idePackagePrefix := Some("it.grypho.scala.leonardo"),

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
      log.success("Generated docs/src/structure.svg")
    },

    // ── mdoc ──────────────────────────────────────────────────────────────────
    // Source markdown lives in docs/src/; mdoc compiles every scala mdoc block
    // and writes verified markdown to target/mdoc/.
    mdocIn  := file("docs/src"),
    mdocOut := target.value / "mdoc",
    mdocVariables := Map(
      "VERSION"       -> version.value,
      "SCALA_VERSION" -> scalaVersion.value
    ),

    // ── Scaladoc 3 static site ────────────────────────────────────────────────
    // After `sbt mdoc`, the verified markdown in target/mdoc/ is used as the
    // site root: Scaladoc 3 renders those pages alongside the API reference.
    // Run `sbt site` to produce the full site in target/scala-3.3.6/api/.
    Compile / doc / scalacOptions ++= Seq(
      "-siteroot",        (target.value / "mdoc").getAbsolutePath,
      "-project",         "Leonardo",
      "-project-version", version.value,
      "-project-logo",    "docs/src/logo2.svg"
    ),

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

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"

// JLine powers the REPL's interactive line editing (arrow keys, in-line editing) and
// persistent command history (see cli/Repl.scala). Used only by the `repl` main — it
// is not on the library's public API, and the read loop degrades to a plain dumb
// terminal when no interactive console is attached (piped input, CI).
libraryDependencies += "org.jline" % "jline" % "3.30.15"

// Spire powers the exact-arithmetic tier's irrational engine (issue 4.N): `Real` computes a
// transcendental to any requested precision, replacing the ~15-digit `Double` ceiling that
// tier 1 had to live with. MIT licensed, so one-way compatible with Leonardo's GPL-3.
//
// PINNED DELIBERATELY. spire_3 has exactly one stable release, 0.18.0 of June 2022, and the
// transitive `typelevel/algebra` is archived. The licence is what bounds that risk: if spire
// is ever truly abandoned, the handful of `Real` sources can be vendored in under GPL-3 with
// the MIT notice preserved. Do not plan on upgrades.
libraryDependencies += "org.typelevel" %% "spire" % "0.18.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"

libraryDependencies += "org.scalatest" %% "scalatest-flatspec" % "3.2.19" % "test"

// Shortcut for the interactive REPL: `sbt repl` instead of the full runMain path.
addCommandAlias("repl", "runMain it.grypho.scala.leonardo.cli.repl")
// Full site: regenerate UML diagram → validate code examples → Scaladoc site + CSS.
addCommandAlias("site", ";puml;mdoc;doc;injectApiStyles")
// The gcd-policy sweep of issue 4.M. Test-scoped and on demand: it takes minutes, so it
// must not run under `sbt test` — only its cross-policy equality check does, in RationalTest.
addCommandAlias("bench", "Test/runMain it.grypho.scala.leonardo.core.RationalBenchmark")

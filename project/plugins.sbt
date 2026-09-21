addSbtPlugin("org.jetbrains.scala" % "sbt-ide-settings" % "1.1.4")
addSbtPlugin("com.github.sbt"     % "sbt-dynver"       % "5.1.1")
// Publishing to Maven Central (issue 5.2 phase 2). One plugin rather than four: it bundles
// sbt-dynver, sbt-pgp, sbt-sonatype and sbt-git, and reads its whole configuration from four
// environment variables -- so no credential is ever written into this repository.
//
// sbt-dynver is declared above as well, deliberately: this build depends on it directly (the
// `dynverVTagPrefix` setting in build.sbt), and an explicit dependency should not be implicit
// just because something else happens to pull it in. sbt evicts to the higher version.
//
// Check https://mvnrepository.com/artifact/com.github.sbt/sbt-ci-release for the latest.
addSbtPlugin("com.github.sbt"     % "sbt-ci-release"   % "1.9.3")
// One combined Scaladoc across both modules (issue 5.2 phase 1.1). Without it the module
// split would fragment the published /api into two disjoint trees, and `cli` would drop out
// of the API reference entirely. Build-only, like PlantUML: an sbt plugin never appears in a
// published POM.
addSbtPlugin("com.github.sbt"     % "sbt-unidoc"       % "0.6.1")
// Binary-compatibility checking against the previous release (issue 2.9). `versionScheme :=
// early-semver` in build.sbt is a *promise* that a patch release stays binary-compatible;
// until 3.7.1 published there was no baseline to check it against, so the promise was
// unverifiable. MiMa makes it enforced rather than declared.
// Check https://mvnrepository.com/artifact/com.typesafe/sbt-mima-plugin for the latest.
addSbtPlugin("com.typesafe"       % "sbt-mima-plugin"  % "1.2.1")
// Check https://mvnrepository.com/artifact/org.scalameta/sbt-mdoc for the latest version.
addSbtPlugin("org.scalameta"      % "sbt-mdoc"         % "2.6.1")

// Scala.js cross-build (issue F_0003 phase 1). Two plugins because they do different jobs:
// sbt-scalajs compiles Scala to JavaScript, sbt-scalajs-crossproject supplies the
// `crossProject(JVMPlatform, JSPlatform)` builder that lets ONE project definition produce
// both. Neither reaches a published POM -- they are build-only, like sbt-unidoc.
//
// The JVM build is unaffected: `coreJVM` compiles exactly the sources `core` did, and the
// only platform-specific file is the parallel-multiply shim (see build.sbt).
// Check https://mvnrepository.com/artifact/org.scala-js/sbt-scalajs for the latest.
addSbtPlugin("org.scala-js"       % "sbt-scalajs"                % "1.22.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"   % "1.4.0")

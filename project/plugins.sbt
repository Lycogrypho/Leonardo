addSbtPlugin("org.jetbrains.scala" % "sbt-ide-settings" % "1.1.4")
addSbtPlugin("com.github.sbt"     % "sbt-dynver"       % "5.1.1")
// Publishing to Maven Central (issue 5.2 phase 2). One plugin: it brings sbt-dynver and sbt-pgp,
// and reads its whole configuration from four environment variables -- so no credential is ever
// written into this repository.  Since 1.11 it uploads through sbt's OWN Central Portal support
// (sbt 1.11 or later, which project/build.properties satisfies) and no longer bundles the old
// xerial sbt-sonatype plugin, so no `sonatype*` key exists in this build any more (F_0001
// step 2a, 2026-10-28: Steward's PR #7 failed on exactly that, `xerial` not found).
//
// sbt-dynver is declared above as well, deliberately: this build depends on it directly (the
// `dynverVTagPrefix` setting in build.sbt), and an explicit dependency should not be implicit
// just because something else happens to pull it in. sbt evicts to the higher version.
//
// Check https://mvnrepository.com/artifact/com.github.sbt/sbt-ci-release for the latest.
addSbtPlugin("com.github.sbt"     % "sbt-ci-release"   % "1.12.1")
// F_0253: sbt-pgp 2.3.2 (from sbt-ci-release 1.12.1, both the latest) resolves BouncyCastle
// 1.78.1, which carries advisories (GHSA-574f-3g2m-x479, GHSA-9pwp-9qqc-pr26,
// GHSA-c3fc-8qff-9hwx, GHSA-qp49-qgx5-5m26 on bcprov; CVE-2026-3505 fixed in 1.84); 1.86 has none
// (deps.dev, 2026-10-30).  Build-time only -- it runs in release.yml's publish job and never
// reaches a published POM -- so the override costs nothing a consumer sees.  REMOVE IT once
// sbt-pgp itself depends on BC >= 1.84: an override left behind would pin an old version later.
dependencyOverrides ++= Seq(
  "org.bouncycastle" % "bcpg-jdk18on"   % "1.86",
  "org.bouncycastle" % "bcprov-jdk18on" % "1.86",
)
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
addSbtPlugin("org.scalameta"      % "sbt-mdoc"         % "2.9.2")

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

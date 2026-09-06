addSbtPlugin("org.jetbrains.scala" % "sbt-ide-settings" % "1.1.1")
addSbtPlugin("com.github.sbt"     % "sbt-dynver"       % "5.1.0")
// Publishing to Maven Central (issue 5.2 phase 2). One plugin rather than four: it bundles
// sbt-dynver, sbt-pgp, sbt-sonatype and sbt-git, and reads its whole configuration from four
// environment variables -- so no credential is ever written into this repository.
//
// sbt-dynver is declared above as well, deliberately: this build depends on it directly (the
// `dynverVTagPrefix` setting in build.sbt), and an explicit dependency should not be implicit
// just because something else happens to pull it in. sbt evicts to the higher version.
//
// Check https://mvnrepository.com/artifact/com.github.sbt/sbt-ci-release for the latest.
addSbtPlugin("com.github.sbt"     % "sbt-ci-release"   % "1.9.2")
// One combined Scaladoc across both modules (issue 5.2 phase 1.1). Without it the module
// split would fragment the published /api into two disjoint trees, and `cli` would drop out
// of the API reference entirely. Build-only, like PlantUML: an sbt plugin never appears in a
// published POM.
addSbtPlugin("com.github.sbt"     % "sbt-unidoc"       % "0.5.0")
// Check https://mvnrepository.com/artifact/org.scalameta/sbt-mdoc for the latest version.
addSbtPlugin("org.scalameta"      % "sbt-mdoc"         % "2.6.1")

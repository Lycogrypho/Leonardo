// The meta-build's classpath, for the `osvPlugins` alias in build.sbt (F_0001 step 2a, the owner's
// Decision 2, 2026-11-01).  A setting of the META-build -- this file sits beside plugins.sbt --
// because the tree it lists is sbt's own and its plugins': the one dependency tree the submitted
// dependency graph does not cover (D_0030), and the one holding the signing code.
//
// The CLASSPATH, not an update report: the meta-build's `update` lists the plugins' trees only
// (47 coordinates), while sbt itself and its dependencies (OkHttp, Guava, Log4j, JLine ...) come
// from the launcher's boot directory and appear in no report -- tried with `updateSbtClassifiers`
// and with resolving `sbtDependency` directly, both empty.  The classpath holds every jar that
// runs; `tools.OsvQuery` turns each path into a coordinate.
lazy val osvClasspath = taskKey[File]("Writes the meta-build's classpath, one jar per line, to target/osv/plugin-classpath.txt.")

osvClasspath := {
  val out  = baseDirectory.value.getParentFile / "target" / "osv" / "plugin-classpath.txt"
  val jars = (Compile / dependencyClasspath).value.files.filter(_.getName.endsWith(".jar")).map(_.getAbsolutePath).distinct
  IO.writeLines(out, jars)
  streams.value.log.info(s"${jars.size} jars written to $out")
  out
}

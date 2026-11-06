package it.grypho.scala.leonardo
package tools

import org.scalatest.flatspec.AnyFlatSpec

/** The parts of [[OsvQuery]] that do not touch the network: identifying jars, writing the batch
 *  request, and reading OSV's answer back in query order. */
class OsvQueryTest extends AnyFlatSpec:

  import OsvQuery.*

  private val Coursier = "C:\\Users\\u\\AppData\\Local\\Coursier\\cache\\v1\\https\\repo1.maven.org\\maven2\\"

  "coordinateOf" should "read a coordinate off a Maven or Ivy layout, either separator" in {
    assert(coordinateOf(Coursier + "org\\bouncycastle\\bcprov-jdk18on\\1.86\\bcprov-jdk18on-1.86.jar")
      .contains(Coordinate("org.bouncycastle", "bcprov-jdk18on", "1.86")))
    assert(coordinateOf("/home/u/.cache/coursier/v1/https/repo.scala-sbt.org/scalasbt/maven-releases/org/scala-sbt/io_2.12/1.10.5/io_2.12-1.10.5.jar")
      .contains(Coordinate("org.scala-sbt", "io_2.12", "1.10.5")))
    assert(coordinateOf("/home/u/.ivy2/cache/com.squareup.okhttp3/okhttp/jars/okhttp-3.14.0.jar")
      .contains(Coordinate("com.squareup.okhttp3", "okhttp", "3.14.0")))
    // Scala in the boot directory carries its version in the directory's name.
    assert(coordinateOf("C:\\Users\\u\\.sbt\\boot\\scala-2.12.21\\lib\\scala-library.jar")
      .contains(Coordinate("org.scala-lang", "scala-library", "2.12.21")))
    // sbt's own boot directory is flat: a file name and nothing to say whose it is.
    assert(coordinateOf("/home/u/.sbt/boot/scala-2.12.21/org.scala-sbt/sbt/1.13.0/actions_2.12-1.13.0.jar").isEmpty)
  }

  "coordinates" should "identify a flat jar by its name in a cache, and keep the jars nothing identifies" in {
    val boot  = "/home/u/.sbt/boot/scala-2.12.21/org.scala-sbt/sbt/1.13.1/"
    val cache = Map("actions_2.12-1.13.0.jar" ->
      "/home/u/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/actions_2.12/1.13.0/actions_2.12-1.13.0.jar")
    val (cs, unmatched) = coordinates(
      Seq(boot + "actions_2.12-1.13.0.jar", boot + "mystery.jar",
          Coursier + "com\\google\\guava\\guava\\33.4.8-jre\\guava-33.4.8-jre.jar"),
      cache.get)
    assert(cs == Vector(Coordinate("com.google.guava", "guava", "33.4.8-jre"),
                        Coordinate("org.scala-sbt", "actions_2.12", "1.13.1")))
    assert(unmatched == Vector(boot + "mystery.jar"))
  }

  "requestBody" should "write one Maven query per coordinate in OSV's schema" in {
    val body = requestBody(Seq(Coordinate("g.x", "a", "1.0"), Coordinate("h", "b\"q", "2")))
    assert(body ==
      """{"queries":[{"package":{"ecosystem":"Maven","name":"g.x:a"},"version":"1.0"},""" +
      """{"package":{"ecosystem":"Maven","name":"h:b\"q"},"version":"2"}]}""")
  }

  "advisories" should "read the ids of each result in query order, an empty result as none" in {
    val answer =
      """{"results":[{"vulns":[{"id":"GHSA-aaaa","modified":"2026-01-01T00:00:00Z"},
        |{"id":"CVE-2026-1","modified":"x"}]},{},{"vulns":[{"id":"GHSA-b}{c","summary":"a ] in \"text\""}]}]}""".stripMargin
    // Braces and brackets inside strings, escaped quotes included, do not end an object or the array.
    assert(advisories(answer, 3) == Right(Vector(Vector("GHSA-aaaa", "CVE-2026-1"), Vector(), Vector("GHSA-b}{c"))))
  }

  it should "refuse an answer that does not match the queries" in {
    assert(advisories("""{"results":[{}]}""", 2).isLeft)
    assert(advisories("""{"error":"bad"}""", 1).isLeft)
  }

  "run" should "say how to make the coordinates file when it is missing, without asking anyone" in {
    assert(OsvQuery.run(Seq("target/osv/no-such-file.txt")) == 1)
  }

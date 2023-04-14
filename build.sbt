ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"

lazy val root = (project in file("."))
  .settings(
    name := "Leonardo",
    idePackagePrefix := Some("it.grypho.scala.leonardo")
  )

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1"

libraryDependencies += "org.jline" % "jline" % "3.21.0"

libraryDependencies += "org.scalatest" % "scalatest_2.13" % "3.0.8" % "test"
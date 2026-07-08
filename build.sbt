ThisBuild / version := "1.2.3"

ThisBuild / scalaVersion := "3.3.4"

scalacOptions ++= Seq( "-explain", "-deprecation", "-feature" )

lazy val root = (project in file("."))
  .settings(
    name := "Leonardo",
    idePackagePrefix := Some("it.grypho.scala.leonardo")
  )

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"

libraryDependencies += "org.scalatest" %% "scalatest-flatspec" % "3.2.19" % "test"

// Shortcut for the interactive REPL: `sbt repl` instead of the full runMain path.
addCommandAlias("repl", "runMain it.grypho.scala.leonardo.cli.repl")

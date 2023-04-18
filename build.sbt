ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"

scalacOptions ++= Seq( "-rewrite", "-source:3.0-migration", "-explain", "-deprecation", "-feature" )

//scalacOptions ++= Seq( "-rewrite", "-source:3.0-migration", "-deprecation", "-feature" )

lazy val root = (project in file("."))
  .settings(
    name := "Leonardo",
    idePackagePrefix := Some("it.grypho.scala.leonardo")
  )

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1"

libraryDependencies += "org.jline" % "jline" % "3.23.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test"

libraryDependencies += "org.scalatest" %% "scalatest-flatspec" % "3.2.15" % "test"

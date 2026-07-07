ThisBuild / version := "1.1.8"

ThisBuild / scalaVersion := "3.3.4"

scalacOptions ++= Seq( "-explain", "-deprecation", "-feature" )

lazy val root = (project in file("."))
  .settings(
    name := "Leonardo",
    idePackagePrefix := Some("it.grypho.scala.leonardo")
  )

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"

libraryDependencies += "org.scalatest" %% "scalatest-flatspec" % "3.2.19" % "test"

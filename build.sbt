ThisBuild / version := "1.6.1"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "aoc-2023",
    idePackagePrefix := Some("joe.aoc")
  )

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.2.0"
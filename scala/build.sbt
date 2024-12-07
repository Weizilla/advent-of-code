ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.15"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code",
    idePackagePrefix := Some("com.weizilla.adventofcode")
  )

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.5.12"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"
libraryDependencies += "com.google.guava" % "guava" % "33.3.1-jre"

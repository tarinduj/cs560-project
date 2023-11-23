import Dependencies._

ThisBuild / scalaVersion     := "3.2.0"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "cs560-project",
    libraryDependencies += munit % Test
  )

Compile / unmanagedJars += {
  baseDirectory.value / "unmanaged" / s"scalaz3_3-4.8.14.jar"
}

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.

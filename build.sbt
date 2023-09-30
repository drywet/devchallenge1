ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "devchallenge1scala",
    libraryDependencies ++= Seq(
      "org.parboiled" %% "parboiled" % "2.5.0",
      "io.activej" % "activej-http" % "5.5",
      "org.scalatest" %% "scalatest" % "3.2.17" % "test"
    )
  )

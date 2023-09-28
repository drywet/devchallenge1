ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "devchallenge1scala",
    libraryDependencies ++= Seq(
      "org.parboiled" %% "parboiled" % "2.5.0",
      "org.scalatest" %% "scalatest" % "3.2.17" % "test"
    )
  )

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

val jsoniterVersion = "2.23.5"

lazy val root = (project in file("."))
  .settings(
    name := "devchallenge1scala",
    libraryDependencies ++= Seq(
      "com.github.tototoshi"                  %% "scala-csv"             % "1.3.10",
      "ch.qos.logback"                         % "logback-classic"       % "1.4.11",
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % jsoniterVersion % "provided",
      "org.scalatest"                         %% "scalatest"             % "3.2.17"        % "test"
    ),
    // Performance tests must not be run in parallel
    Test / parallelExecution := false
    // scalacOptions ++= Seq("-Ymacro-debug-lite", "-Xlog-implicits")
  )

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

val activejVersion  = "5.5"
val jsoniterVersion = "2.23.5"

lazy val root = (project in file("."))
  .settings(
    name := "devchallenge1scala",
    libraryDependencies ++= Seq(
      "org.parboiled"                         %% "parboiled"              % "2.5.0",
      "io.activej"                             % "activej-http"           % activejVersion,
      "io.activej"                             % "activej-launchers-http" % activejVersion,
      "io.activej"                             % "activej-inject"         % activejVersion,
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"    % jsoniterVersion,
      "ch.qos.logback"                         % "logback-classic"        % "1.4.11",
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros"  % jsoniterVersion % "provided",
      "org.scalatest"                         %% "scalatest"              % "3.2.17"        % "test"
    )
    // scalacOptions ++= Seq("-Ymacro-debug-lite", "-Xlog-implicits"),
  )

name := "core"
version := "0.1"
scalaVersion := "3.3.6"
organization := "cp"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.19" % Test,
)

lazy val core = (project in file("."))
  .settings(
    name := "core"
  )

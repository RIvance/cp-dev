name := "intermediate"
version := "0.1"
scalaVersion := "3.3.6"
organization := "cp"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.19" % Test,
)

lazy val core = ProjectRef(file("../core"), "core")

lazy val intermediate = (project in file("."))
  .dependsOn(core)
  .settings(
    name := "intermediate"
  )


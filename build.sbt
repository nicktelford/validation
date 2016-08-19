lazy val projectSettings = Seq(
  organization := "net.nicktelford",
  version := "1.0.0",
  scalaVersion := "2.11.7",
  crossScalaVersions := Seq("2.10.5", "2.11.7"),

  libraryDependencies += compilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3"),
  libraryDependencies += "org.typelevel" %% "cats" % "0.6.1",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0-M15" % Test,
  libraryDependencies += "org.typelevel" %% "discipline" % "0.5" % Test
)

lazy val root = (project in file("."))
  .aggregate(core)
  .settings(name := "validation")
  .settings(projectSettings: _*)

lazy val core = (project in file("core"))
  .settings(name := "core")
  .settings(moduleName := "validation-core")
  .settings(projectSettings: _*)

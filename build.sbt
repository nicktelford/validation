lazy val projectSettings = Seq(
  organization := "net.nicktelford",
  version := "1.0.0",
  scalaVersion := "2.12.1",
  crossScalaVersions := Seq("2.12.1", "2.11.7"),

  libraryDependencies += compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
  libraryDependencies += "org.typelevel" %% "cats" % "0.8.1",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % Test,
  libraryDependencies += "org.typelevel" %% "discipline" % "0.7.2" % Test
)

lazy val root = (project in file("."))
  .aggregate(core, macros, dsl)
  .settings(name := "validation")
  .settings(projectSettings: _*)

lazy val core = (project in file("core"))
  .settings(name := "core")
  .settings(moduleName := "validation-core")
  .settings(projectSettings: _*)

lazy val macros = (project in file("macros"))
  .settings(name := "macros")
  .settings(moduleName := "validation-macros")
  .settings(projectSettings: _*)
  .dependsOn(core)

lazy val dsl = (project in file("dsl"))
  .settings(name := "dsl")
  .settings(moduleName := "validation-dsl")
  .settings(projectSettings: _*)
  .dependsOn(core, macros)

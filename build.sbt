lazy val projectSettings = Seq(
  organization := "net.nicktelford",
  version := "1.0.0",
  scalaVersion := "2.11.7",
  crossScalaVersions := Seq("2.10.5", "2.11.7"),

  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0-M15" % Test,
  
  libraryDependencies += "org.typelevel" %% "cats" % "0.4.0"
)


lazy val root = (project in file("."))
  .aggregate(core, macros)
  .settings(name := "validation")
  .settings(projectSettings: _*)

lazy val core = (project in file("core"))
  .settings(name := "core")
  .settings(moduleName := "validation-core")
  .settings(projectSettings: _*)
  .dependsOn(macros)

lazy val macros = (project in file("macros"))
  .settings(name := "macros")
  .settings(moduleName := "validation-macros")
  .settings(projectSettings: _*)

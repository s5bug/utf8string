lazy val core = (crossProject(JSPlatform, JVMPlatform) in file("core")).settings(
  organization := "tf.bug",
  name := "utf8string",
  version := "0.1.0",
  scalaVersion := "2.13.8",
)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val fs2 = (crossProject(JSPlatform, JVMPlatform) in file("fs2")).settings(
  organization := "tf.bug",
  name := "utf8string-fs2",
  version := "0.1.0",
  scalaVersion := "2.13.8",
  libraryDependencies ++= Seq(
    "org.typelevel" %%% "cats-core" % "2.7.0",
    "org.typelevel" %%% "cats-effect" % "3.3.0",
    "co.fs2" %%% "fs2-core" % "3.2.4",
  ),
).dependsOn(core).jsSettings(scalaJSUseMainModuleInitializer := true)

lazy val fs2JVM = fs2.jvm
lazy val fs2JS = fs2.js

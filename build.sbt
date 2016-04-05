import sbtassembly.AssemblyPlugin.defaultShellScript

name := "miller-prime"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scala-lang.modules"   %% "scala-parser-combinators"   % "1.0.4",
  "org.scalacheck"           %% "scalacheck"                 % "1.12.2" % "test"
)


mainClass in (Test, test) := Some("Tests")

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Ypatmat-exhaust-depth", "80")

assemblyJarName in assembly := "miller.jar"

assemblyOption in assembly := (assemblyOption in assembly).value.copy(prependShellScript = Some(defaultShellScript))

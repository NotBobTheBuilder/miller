import sbtassembly.AssemblyPlugin.defaultShellScript

name := "miller-prime"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Ypatmat-exhaust-depth", "80")

assemblyJarName in assembly := "miller.jar"

assemblyOption in assembly := (assemblyOption in assembly).value.copy(prependShellScript = Some(defaultShellScript))

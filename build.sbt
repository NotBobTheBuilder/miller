name := "miller-prime"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Ypatmat-exhaust-depth", "80")

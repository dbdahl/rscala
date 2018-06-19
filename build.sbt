name := "rscala"

version := "3.0.0-SNAPSHOT"

scalaVersion := "2.12.6"

libraryDependencies ++= List(
  "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  "org.scala-lang" % "scalap" % scalaVersion.value
)

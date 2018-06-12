name := "rscala2"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.12.6"

libraryDependencies ++= List(
  "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  "org.scala-lang" % "scalap" % scalaVersion.value
)

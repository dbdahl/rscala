name := "rscala"

version := "3.0.2"
//version := "3.0.2-SNAPSHOT"

scalaVersion := "2.12.6"

crossScalaVersions := Seq("2.11.12", "2.12.6")

libraryDependencies ++= List(
  "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  "org.scala-lang" % "scalap" % scalaVersion.value
)

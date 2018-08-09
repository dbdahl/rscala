name := "rscala"

//version := "3.1.1"
version := "3.1.0-SNAPSHOT"

scalaVersion := "2.12.6"

crossScalaVersions := Seq("2.11.12", "2.12.6", "2.13.0-M4")

libraryDependencies ++= List(
  "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  "org.scala-lang" % "scalap" % scalaVersion.value
)

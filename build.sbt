name := "rscala"

version := "3.2.0"
//version := "3.2.0-SNAPSHOT"

scalaVersion := "2.12.6"

crossScalaVersions := Seq("2.11.12", "2.12.6", "2.13.0-M5")

libraryDependencies ++= List(
  "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  "org.scala-lang" % "scalap" % scalaVersion.value
)

sources in (Compile, doc) ~= (_ filter (_.getName endsWith ".scala"))

scalacOptions in (Compile,doc) ++= Seq("-no-link-warnings", "-skip-packages", "scala:org.ddahl.rscala.server")


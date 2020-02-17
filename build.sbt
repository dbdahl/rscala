name := "rscala"

organization := "org.ddahl"

version := "3.2.18"
//version := "3.2.17-SNAPSHOT"

scalaVersion := "2.13.1"

crossScalaVersions := Seq("2.11.12", "2.12.10", "2.13.1")

scalacOptions ++= List("-feature", "-deprecation", "-unchecked")

sources in (Compile, doc) ~= (_ filter (_.getName endsWith ".scala"))

scalacOptions in (Compile,doc) ++= Seq("-no-link-warnings", "-skip-packages", "scala:org.ddahl.rscala.server")

libraryDependencies ++= List(
  "org.scala-lang" % "scala-compiler" % scalaVersion.value
)

publishTo := sonatypePublishTo.value

licenses := List(("Apache-2.0",url("https://www.apache.org/licenses/LICENSE-2.0")))

publishMavenStyle := true

pomExtra := (
  <url>https://github.com/dbdahl/rscala/</url>
  <scm>
    <url>https://github.com/dbdahl/rscala/</url>
    <connection>scm:git:https://github.com/dbdahl/rscala.git</connection>
  </scm>
  <developers>
    <developer>
      <id>dbdahl</id>
      <name>David B. Dahl</name>
      <url>https://dahl.byu.edu</url>
    </developer>
  </developers>
)


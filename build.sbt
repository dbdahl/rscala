name := "rscala"

//version := "2.3.4"
version := "2.3.3-SNAPSHOT"

organization := "org.ddahl"

scalaVersion := "2.12.3"

crossScalaVersions := Seq("2.10.6", "2.11.11", "2.12.3")

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomExtra := (
  <url>https://dahl.byu.edu/software/rscala</url>
  <licenses>
    <license>
        <name>GNU General Public License, Version 2</name>
        <url>http://www.gnu.org/licenses/gpl-2.0.html</url>
        <distribution>repo</distribution>
    </license>
    <license>
        <name>GNU General Public License, Version 3</name>
        <url>http://www.gnu.org/licenses/gpl-3.0.html</url>
        <distribution>repo</distribution>
    </license>
    <license>
      <name>BSD 3-Clause License</name>
      <url>https://opensource.org/licenses/BSD-3-Clause</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>https://dahl-git.byu.edu/dahl/rscala/</url>
    <connection>scm:git:https://dahl-git.byu.edu/dahl/rscala.git</connection>
  </scm>
  <developers>
    <developer>
      <id>dbdahl</id>
      <name>David B. Dahl</name>
      <url>https://dahl.byu.edu</url>
    </developer>
  </developers>)

scalacOptions ++= List("-feature", "-deprecation", "-unchecked")

sources in (Compile, doc) ~= (_ filter (_.getName endsWith ".scala"))

scalacOptions in (Compile,doc) ++= Seq("-no-link-warnings", "-skip-packages", "scala:org.ddahl.rscala.server")

javacOptions ++= List("-deprecation")

compileOrder := CompileOrder.ScalaThenJava

libraryDependencies ++= List(
  "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  "org.scala-lang" % "scalap" % scalaVersion.value
)


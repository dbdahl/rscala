name := "rscala"

version := "1.0.10"

organization := "org.ddahl.rscala"

scalaVersion := "2.11.8"

crossScalaVersions := Seq("2.11.8", "2.10.6")

publishMavenStyle := true

scalacOptions ++= List("-feature", "-deprecation", "-unchecked")

scalacOptions in (Compile,doc) ++= Seq("-skip-packages", "scala:org.ddahl.rscala.java:org.ddahl.rscala.server")

javacOptions ++= List("-deprecation")

compileOrder := CompileOrder.ScalaThenJava

libraryDependencies ++= List(
  "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  "org.scala-lang" % "scalap" % scalaVersion.value
)

unmanagedSourceDirectories in Compile <+= (sourceDirectory in Compile, scalaBinaryVersion){
  (s, v) => s / ("scala_"+v)
}


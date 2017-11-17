\name{scalaRequire}
\alias{scalaRequire}
\docType{methods}
\title{
Add a JAR to the Classpath of an Existing Scala Instance
}
\description{
This function adds the supplied JAR file to the classpath of an existing Scala instance.
}
\usage{
scalaRequire(jar.file, interpreter = findScalaInstance())
}
\arguments{
    \item{jar.file}{A string giving the path to a JAR file.}
    \item{interpreter}{An instance of a Scala interpreter.}
}
\author{
David B. Dahl \email{dahl@stat.byu.edu}
}
\keyword{interface}


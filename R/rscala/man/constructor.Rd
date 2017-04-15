\name{constructor}
\alias{scala}
\alias{scalaInfo}
\alias{scalaInstall}
\alias{.rscalaJar}
\alias{.rscalaPackage}
\docType{methods}
\title{
Creates an instance of an embedded Scala compiler/interpreter.
Provides file path for Scala and this package's JAR file.
Provides functions to enable other packages to depend on this package.
}
\description{
The function \code{\link{scala}} creates an instance of an embedded Scala
compiler/interpreter and binds a Scala object named \code{R} to permit
callbacks to \R.  Options are available to customize where Scala is found and
how it is invoked (e.g., setting the classpath and maximum heap size). Multiple
interpreters can be created and each runs independently with its own memory.
Each interpreter can use multiple threads/cores, but the bridge between \R and
Scala is not thread-safe.  As such, multiple \R threads/cores should not
simultaneously access the same interpreter. 

The functions \code{\link{scalaInfo}} and \code{\link{.rscalaJar}} provide file
paths to JAR files, installation directories, the Scala executable, and this
package.  Note that if you only want to embed R in a Scala application, you do
not need to install the package. Simply add the following line to the your SBT
build.sbt file: \sQuote{libraryDependencies += "org.ddahl" % "rscala_2.11" %
"_VERSION_"}, where _VERSION_ is the rscala version number (i.e., 1.0.14).

\code{\link{scalaInstall}} downloads and installs Scala in \dQuote{~/.rscala}
in the user's home directory.  System administrators should install Scala
globally as described here:
\url{http://www.scala-lang.org/download/install.html}.  In short, simply
download the archive, unpack it, and add the \dQuote{scala} script to the path. 

The function \code{\link{.rscalaPackage}} should called by the \code{.onLoad}
function of a package that wishes to depend on this package.  The function
should not be called elsewhere.  This function sets the classpath to the JAR
files in the \sQuote{java} directory of the package and passes the \code{...}
arguments to the \code{\link{scala}} function.  This instance of Scala is
available as the object \code{s} in the namespace of the package (thereby
making it available to the package's function) but it is not exported from
the namespace.
}
\usage{
scala(classpath=character(0),
      serialize.output=FALSE, scala.home=NULL,
      heap.maximum=NULL, command.line.options=NULL, row.major=TRUE,
      timeout=60, debug=FALSE, stdout=TRUE, stderr=TRUE)
scalaInfo(scala.home=NULL, verbose=FALSE)
scalaInstall()
.rscalaJar(version="")
.rscalaPackage(pkgname, classpath.appendix=character(0), ...)
}
\arguments{
    \item{classpath}{A character vector whose elements are paths to JAR files or directories which specify the classpath for the Scala compiler/interpreter.}
    \item{serialize.output}{Should standard output (stdout) and standard error (stderr) be captured and serialized back to R?  \code{FALSE} (the default) is recommended except on Windows where \code{TRUE} may be needed to see output and error messages.}
    \item{scala.home}{A character vector of length one giving the path where Scala is installed.  When set to \code{NULL} (the default), the function sequentially tries to find the Scala home by:  i. using the environment variable \code{SCALA_HOME}, ii. querying the operating system search path, and iii. looking in \code{~/.rscala/scala-X} where \code{X} is the recommended Scala version number (e.g. 2.11.8) for the package.  If all these fail, the function offers to download and install Scala in the above mentioned path (if the current \R session is interactive) or it downloads and installs Scala in a temporary directory (if the current \R session is noninteractive).}
    \item{heap.maximum}{A character vector of length one used to specify the maximum heap size in the JVM.  If \code{NULL}, the global option \code{rscala.heap.maximum} is queried and, if that is also \code{NULL}, Scala's default value is used.  This option is ignored if \var{command.line.options} is not null.}
    \item{command.line.options}{A character vector whose elements are passed as command line arguments when invoking Scala.  If \code{NULL}, the global option \code{rscala.command.line.options} is queried and, if that is also \code{NULL}, the value is set to \code{NULL}.  A value of \code{NULL} means no extra arguments are provided.  If you simply want to add to the classpath and/or set the maximum heap size, use the \var{classpath} and \var{heap.maximum} arguments.}
    \item{row.major}{Should matrices in Scala be row major?}
    \item{timeout}{A numeric vector of length one giving the number of seconds to wait for Scala to start before aborting.  The default value is \code{60} seconds.}
    \item{debug}{An option meant only for developers of the package itself and not intended for users of the package.}
    \item{stdout, stderr}{Where \code{stdout} and \code{stderr} output that is not serialized is should be sent.  \code{TRUE} (the default) or \code{""} sends output to the R console (although that may not work on Windows).  \code{FALSE} or \code{NULL} discards the output.  Otherwise, this is the name of the file that receives the output.}
    \item{verbose}{A logical vector of length one indicating whether information regarding the search for the Scala installation should be displayed.}
    \item{version}{If \code{""}, the package JAR files for all Scala versions are returned. Or, if \code{"2.*.*"} where \code{*} is a placeholder, the JAR file for that specific major version of Scala is returned.}
    \item{pkgname}{A character string giving the name of the package (as provided the second argument of the \code{.onLoad} function) that wishes to depend on this package.}
    \item{classpath.appendix}{A character vector giving additional JAR files or directories for the Scala compiler/interpreter embedded within a package via the \code{\link{.rscalaPackage}} function.  The JAR files in the \sQuote{java} directory of the package are already included and do not need to be added here.}
    \item{...}{These arguments are passed by the \code{\link{.rscalaPackage}} function to the \code{\link{scala}} function.}
}
\value{
    \code{\link{scala}} returns an \R object representing an embedded Scala interpreter.

    \code{\link{scalaInfo}} returns a list detailing the Scala executable, version, jars, etc.
}
\author{
David B. Dahl \email{dahl@stat.byu.edu}
}
\seealso{
  \code{\link{scalaEval}},
  \code{\link{scalaSet}},
  \code{\link{scalaGet}},
  \code{\link{scalaDef}},
  \code{\link{scalaSettings}},
  \code{\link{strintrplt}}
}
\keyword{interface}
\examples{
.rscalaJar()
scalaInfo(verbose=TRUE)

\dontrun{
# Uncomment the next line to download and install Scala
# scalaInstall()

# Make an instance of the Scala interpreter and see how its output is captured.
s <- scala(serialize.output=TRUE)
capture.output(s \%~\% 'println("This is Scala "+scala.util.Properties.versionString)')

# Function defined in R
myMean <- function(x) {
  cat("Here I am in R.\n")
  s \%@\% 'println("Here I am in Scala.")'
  mean(x)
}

# Function defined in Scala that calls an R function
callRFunction <- s$def('functionName: String, x: Array[Double]','
  R.xx = x
  R.evalD0("y <- "+functionName+"(xx)")
')

# Have Scala call the R's myMean function with input 1:100.
callRFunction('myMean',1:100)
}
}


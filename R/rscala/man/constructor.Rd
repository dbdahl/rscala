\name{scala}
\alias{scala}
\alias{scala2}
\alias{scala3}
\alias{scalaInfo}
\alias{scalaInstall}
\alias{.rscalaJar}
\alias{.rscalaPackage}
\alias{.rscalaPackageUnload}
\docType{methods}
\title{
Create an Instance of an Embedded Scala Interpreter
}
\description{
The function \code{\link{scala}} creates an instance of an embedded Scala
interpreter/compiler and binds a Scala object named \code{R} to permit
callbacks to \R.  Options are available to customize where Scala is found and
how it is invoked (e.g., setting the classpath and maximum heap size). Multiple
interpreters can be created and each runs independently with its own memory.
Each interpreter can use multiple threads/cores, but the bridge between \R and
Scala is not thread-safe.  As such, multiple \R threads/cores should not
simultaneously access the same interpreter. 

The functions \code{\link{scala2}} and \code{\link{scala3}} is equivalent to
\code{\link{scala}} except they set \code{mode="lazy"} and
\code{mode="serial"}, respectively.

The functions \code{\link{scalaInfo}} and \code{\link{.rscalaJar}} provide file
paths to JAR files, installation directories, the Scala executable, and this
package.  Note that if you only want to embed R in a Scala application, you do
not need to install the package. Simply add the following line to the your SBT
build.sbt file: \sQuote{libraryDependencies += "org.ddahl" %% "rscala" %
"_VERSION_"}, where _VERSION_ is the rscala version number (i.e., 2.5.0).

\code{\link{scalaInstall}} downloads and installs Scala in \dQuote{~/.rscala}
in the user's home directory.  System administrators can install Scala
globally as described here:
\url{http://www.scala-lang.org/download/install.html}.  In short, simply
download the archive, unpack it, and add the \dQuote{scala} script to the path. 

The function \code{\link{.rscalaPackage}} should be called in the \code{.onLoad}
function of a package that wishes to depend on this package.  The function
should not be called elsewhere.  It sets the classpath to the JAR
files in the \sQuote{java} directory of the package and passes the \code{...}
arguments to the \code{\link{scala}} function.  This instance of Scala is
available as the object \code{s} in the namespace of the package (thereby
making it available to the package's function) but it is not exported from
the namespace.  The object \code{s} is only initialized on its first usage.
The function \code{\link{.rscalaPackageUnload}} is deprecated and is no longer
needed for packages that depend on rscala.
}
\usage{
scala(classpath=character(), classpath.packages=character(),
      serialize.output=.Platform$OS.type=="windows", scala.home=NULL,
      heap.maximum=NULL, command.line.options=NULL, row.major=TRUE,
      timeout=30, debug=FALSE, stdout=TRUE, stderr=TRUE, port=0,
      scala.info=NULL, major.release=c("2.10","2.11","2.12"),
      mode="parallel", assign.name="s", assign.env=parent.frame(),
      callback=function(s) {}, snippet=character())

scala2(...)

scala3(...)

scalaInfo(scala.home=NULL, major.release=c("2.10","2.11","2.12"),
          verbose=FALSE)

scalaInstall(major.release=c("2.10","2.11","2.12"), global=FALSE)

.rscalaJar(major.release=c("2.10","2.11","2.12"))

.rscalaPackage(pkgname, classpath.packages=character(), ...)

.rscalaPackageUnload()
}
\arguments{
    \item{classpath}{A character vector whose elements are paths to JAR files or directories which specify the classpath for the Scala compiler/interpreter.}
    \item{classpath.packages}{A character vector giving names of other installed packages whose JAR files should be appended to the classpath.}
    \item{serialize.output}{Should standard output (stdout) and standard error (stderr) be captured and serialized back to R?  The default is \code{TRUE} on Windows and \code{FALSE} on other operating systems.  \code{FALSE} requires less computing and is usually not necessary on Linux and Mac OS X.  Depending on the environment and operating system in which R is run, \code{TRUE} may be needed to see output and error messages.}
    \item{scala.home}{A character vector of length one giving the path where Scala is installed.  When set to \code{NULL} (the default), the function sequentially tries to find the Scala home by:  i. querying the global option \var{rscala.scala.home}, ii. using the environment variable \code{SCALA_HOME}, iii. querying the operating system search path, and iv. looking in subdirectories of \code{~/.rscala}.  If all these fail, the function displaces a message to help the user install Scala.  Alternatively, in the class of \code{\link{scalaInfo}}, \code{scala.home} may also be the result of the \code{\link{scala}} function.}
    \item{heap.maximum}{A character vector of length one used to specify the maximum heap size in the JVM.  If \code{NULL}, the global option \var{rscala.heap.maximum} is queried and, if that is also \code{NULL}, Scala's default value is used.  This option is ignored if \var{command.line.options} is not null.}
    \item{command.line.options}{A character vector whose elements are passed as command line arguments when invoking Scala.  If \code{NULL}, the global option \var{rscala.command.line.options} is queried and, if that is also \code{NULL}, the value is set to \code{NULL}.  A value of \code{NULL} means no extra arguments are provided.  If you simply want to add to the classpath and/or set the maximum heap size, use the \var{classpath} and \var{heap.maximum} arguments.}
    \item{row.major}{Should matrices in Scala be row major?}
    \item{timeout}{A numeric vector of length one giving the number of seconds to wait for Scala to start before aborting.}
    \item{debug}{An option meant only for developers of the package itself and not intended for users of the package.}
    \item{stdout, stderr}{Where standard output and standard error results that are not serialized should be sent.  \code{TRUE} (the default) or \code{""} sends output to the R console (although that may not work on Windows).  \code{FALSE} or \code{NULL} discards the output.  Otherwise, this is the name of the file that receives the output.}
    \item{port}{If \code{0}, two random ports are selected.  Otherwise, \code{port} and \code{port+1} are used to the TCP/IP connections.}
    \item{scala.info}{The result of a previous call to \code{\link{scalaInfo}}.}
    \item{verbose}{A logical vector of length one indicating whether information regarding the search for the Scala installation should be displayed.}
    \item{major.release}{The character vector giving acceptable major.release numbers (e.g., c("2.10","2.11","2.12")), or \code{NA} in which case the system picks the appropriate version.}
    \item{mode}{One of \code{"parallel"}, \code{"lazy"}, or \code{"serial"} to indicate how the R/Scala bridge is established.  \code{"serial"} starts Scala in the background, blocks until the bridge is established, and causes the function to return the interpreter.  \code{"parallel"} starts Scala in the background whereas \code{"lazy"} only starts Scala if and when the interpreter is first used.  Both \code{"parallel"} and \code{"lazy"} cause the function to return \code{NULL} and makes the interpreter (with the identifier given by \code{assign.name}) available as a promise.}
    \item{assign.name}{If \code{mode != "serial"}, the name of the (promise of the) interpreter.}
    \item{assign.env}{If \code{mode != "serial"}, the environment in which the (promise of the) interpreter is assigned.}
    \item{callback}{A function taking a Scala interpreter as its only argument.  This function is called immediately after the R/Scala bridge is established.}
    \item{snippet}{A character vector providing Scala code that will be evaluated when the interpreter in the package namespace is first used.}
    \item{global}{Should the Scala installation be global, specifically inside the package's directory?  This may require administrator privileges.}
    \item{pkgname}{A character string giving the name of the package (as provided the second argument of the \code{.onLoad} function) that wishes to depend on this package.}
    \item{...}{These arguments are passed by the \code{\link{.rscalaPackage}} function to the \code{\link{scala}} function.}
}
\value{
    \code{link{scala}} and \code{link{scala2}} return \code{NULL} but have the side effect of creating (a promise for) an \R object representing an embedded Scala interpreter.

    \code{\link{scala3}} returns an \R object representing an embedded Scala interpreter.

    \code{\link{scalaInfo}} returns a list detailing the Scala executable, version, jars, etc.
}
\author{
David B. Dahl \email{dahl@stat.byu.edu}
}
\seealso{
  \code{\link{scalaSettings}},
%  \code{\link{scalap}},
  \code{\link{strintrplt}},
  \code{\link{rscala-package}}
}
\keyword{interface}

\examples{
# Uncomment the next line to download and install Scala
# scalaInstall()

.rscalaJar()
scalaInfo(verbose=TRUE)

\donttest{
# Make an instance of the Scala interpreter 's' and see how its output is captured.
scala(serialize.output=TRUE)
capture.output(s \%~\% 'println("This is Scala "+scala.util.Properties.versionString)')
scalaSettings(s)

# Demonstrate convenient notation and string interpolation
stringFromScala <- s \%~\% '"Hello @{Sys.getenv("USER")} from @{R.Version()$nickname}" + "!"*10'
stringFromScala

# Set and get variables
s$rPi <- pi
s$rPi
s$val("rPi")
s$.val("rPi")

s$rPi <- I(pi)     # Now rPi is an array of length one.
s$rPi              # It doesn't matter to R...
s$.val("rPi")      # ... but it does to Scala.

# Convenient notation
a1 <- s \%~\%  "rPi(0)/2"   # As an R value
a2 <- s \%.~\% "rPi(0)/2"   # As a Scala reference

# References can be set
s$foo <- a2
s$foo

# Instantiate an object
seed <- 2349234L
rng <- s$.scala.util.Random$new(seed)  # Scala equivalent: new scala.util.Random(seed)

# Call method of a reference
system.time(rng$nextInt(100L))   # Scala equivalent: rng.nextInt(100)
system.time(rng$nextInt(100L))   # Notice it runs much faster the second time due to caching

rInt <- rng$nextInt(100L,.EVALUATE=FALSE)  # Define function to call quickly later without ...
rInt(100)                                     # ... needing to protect scalars and ensure type.

# Call method of companion object and call methods of a reference
# Scala equivalent: (scala.math.BigInt("777",8) - 500).intValue
s$.scala.math.BigInt$apply("777",8L)$'-'(500L)$intValue()

# Example showing callback functionality
f <- function(func=NULL, data=numeric(), quiet=TRUE) s \%!\% '
  if ( ! quiet ) println("Here I am in Scala.")
  R.invokeD1(func, data.map(2*_), "verbose" -> !quiet ).sum
'

cube <- function(x, ignored.argument, verbose=TRUE) {
  if ( verbose ) cat("Here I am in R.\n")
  x^3
}

identical( f(cube,1:4,FALSE), sum((2*(1:4))^3) )
identical( f(cube,1:4,TRUE),  sum((2*(1:4))^3) )

# Longer example showing more flexible than '\%~\%'
drawGaussian <- function(mean=0.0, sd=1.0, rng=scalaNull("scala.util.Random")) s \%!\% '
  mean+sd*rng.nextDouble
'
drawGaussian(3,0.1,rng)  # No scalar protection or casting is needed.
n.draws <- 100L
s$random <- rng
system.time({
  draws <- s \%~\% '
    val result = new Array[Double](@{n.draws})
    result(0) = random.nextGaussian
    for ( i <- 1 until @{n.draws} ) {
      result(i) = 0.5*result(i-1) + random.nextGaussian
    }
    result
  '
  acf(draws,plot=FALSE)
})
sampler <- function(nDraws, rho=0.0, rng=scalaNull("scala.util.Random")) s \%!\% '
  val result = new Array[Double](nDraws)
  result(0) = rng.nextGaussian
  for ( i <- 1 until nDraws ) {
    result(i) = rho*result(i-1) + rng.nextGaussian
  }
  result
'
system.time(acf(sampler(n.draws,0.5,rng),plot=FALSE))
system.time(acf(sampler(n.draws,0.5,rng),plot=FALSE))
close(s)
}
}


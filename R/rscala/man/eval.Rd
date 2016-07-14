\name{eval}
\alias{scalaEval}
\alias{scalaGet}
\alias{scalaSet}
\alias{scalaDef}
\alias{scalaWrap}
\alias{scalaUnwrap}
\alias{scalaGC}
\alias{scalaReset}
\alias{scalap}
\alias{\%~\%}
\alias{\%.~\%}
\alias{\%@\%}
\alias{$.ScalaInterpreter}
\alias{$<-.ScalaInterpreter}
\docType{methods}
\title{
Execute code, set values, and get values in an embedded interpreter.
}
\description{
These functions define the package's interface to an embedded interpreter.
Through these functions, code is executed in the embedded interpreter and data
is passed between \R and the embedded interpreter.  Some of these functions
return a function for invoking a function in the interpreter.  Others return a
reference to the result or attempt to return an \R object representing the
result.  Some functions also provide the ability to clear variables or reclaim
memory previously allocated to references.  There are convenient shorthand
equivalents for many of the functions.
}
\usage{
scalaEval(interpreter, snippet, interpolate="")
scalaGet(interpreter, identifier, as.reference=NA)
scalaSet(interpreter, identifier, value, length.one.as.vector="")
scalaDef(interpreter, args, body, interpolate = "", reference=NULL)
scalaWrap(interpreter, value)
scalaUnwrap(interpreter, value)
scalaGC(interpreter)
scalaReset(interpreter)
scalap(interpreter, item.name)

interpreter \%~\% snippet
interpreter \%.~\% snippet
interpreter \%@\% snippet

\method{$}{ScalaInterpreter}(interpreter, identifier)
\method{$}{ScalaInterpreter}(interpreter, identifier) <- value
}
\arguments{
    \item{interpreter}{An interpreter from an interpreter constructor (i.e. \code{\link{scala}}).}
    \item{snippet}{A character vector of arbitrary length to be evaluated by the interpreter.}
    \item{args}{A character vector of length one giving the argument list for a function to be defined in the interpreter.  Use \code{""} for a function which takes no arguments.}
    \item{body}{A character vector of any length giving the body for a function to be defined in the interpreter.  The function body can be a multiline character vector of length one.}
    \item{identifier}{A character vector of length one containing a valid variable name in the embedded language.}
    \item{value}{For \code{\link{scalaWrap}}, an arbitrary R object.  For \code{\link{scalaUnwrap}}, a result of a call to \code{\link{scalaWrap}}.  For \code{\link{$<-}} either: i. a vector or matrix of integers, doubles, logicals, or characters, or ii. a reference.}
    \item{as.reference}{A logical vector of length one indicating whether the result of \code{\link{scalaGet}} should return a reference to the function result or whether these functions should try to interpret the result as a vector or matrix of integer, doubles, logicals, or characters.  The default is \code{NA} which first tries \code{\var{as.reference}=FALSE} and, if no conversion is possible, \code{\var{as.reference}=TRUE} is done.}
    \item{interpolate}{When equal to \code{""} (the default), the default behavior from \code{\link{scalaSettings}} is used.  If equal to \code{TRUE}, the interpreter will call \code{\link{strintrplt}} on the supplied code before running it.  If equal to \code{FALSE}, the character vector is passed unaltered.}
    \item{length.one.as.vector}{When equal to \code{""} (the default), the default behavior from \code{\link{scalaSettings}} is used.  If equal to \code{TRUE}, setting a vector of length one in the interpreter will result in an array of length one.  If equal to \code{FALSE}, setting a vector of length one results in an atomic value.  For example, \code{pi} is a vector of length one in \R and, when set in an interpreter, would result in an array of doubles of length one if \var{length.one.as.vector} is \code{TRUE} or a double if \var{length.one.as.vector} is \code{FALSE}.}
    \item{reference}{This option is meant only for developers of the package itself and not intended for users of the package.}
    \item{item.name}{A character vector of length one containing the name of an object/class for which the \sQuote{scalap} command should be run.}
}
\details{
The convenient shorthand notation \code{interpreter \%@\% snippet} evaluates
the expression \var{snippet}, whereas \code{interpreter \%~\% snippet} both
evaluates the expression \var{snippet} and attempts to return a vector or
matrix of integers, doubles, logicals, or characters or --- if this is not
possible --- \code{NULL} is returned.  The convenient shorthand notation
\code{interpreter \%.~\% snippet} has the same behavior except it always
returns a reference.  References may be used later as the \var{value} in
\code{\link{scalaSet}} or as an argument to a function returned by
\code{\link{scalaDef}}.  Note, however, the memory associated with values
returned by \code{\%~\%} and \code{\%.~\%} is never garbage-collected.
Likewise, memory allocated in the global environment of the Scala interpreter
is never garbage-collected.  (See the Scala bug detailed here:
\url{https://issues.scala-lang.org/browse/SI-4331}.)

Memory associated with the return values by function defined by
\code{\link{scalaDef}}, however, is able to be garbage-collected.  Thus, heavy
usage of functions defined by \code{\link{scalaDef}} is encouraged for memory
intensive applications.  (Use \code{\link{scalaGC}} to invoke the interpreter's
garbage collector.) Further, functions defined by \code{\link{scalaDef}} have
less invocation latency than the equivalent code using \code{\link{\%~\%}} and
\code{\link{\%.~\%}}.  The speed difference in invocation is especially
noticeable for quick functions.

The convenient shorthand notations \code{interpreter$identifier} and
\code{interpreter$identifier <- value} make it easy to get and set variables in
the interpreter.  Note, however, that \code{def} is a reserved word in Scala
and, as such, is treated as a special case: \code{interpreter$def(args,body)}
is equivalent to \code{scalaDef(interpreter,args,body)}.  See the examples
below.
}
\value{
    \code{\link{scalaEval}}, \code{\link{scalaSet}}, \code{\link{scalaGC}}, and \code{\link{scalaReset}} silently return \code{NULL}.

    \code{\link{scalaGet}} returns a reference to the result if \var{as.reference} is \code{TRUE}.  If \var{as.reference} is \code{FALSE}, the function tries to convert the result to a vector or matrix of integers, doubles, logicals, or characters.  If a conversion is not possible, \code{NULL} is silently returned.

    \code{\link{scalaDef}} returns an \R function that calls the corresponding function defined by the \var{args} and \var{body}.  In addition to the arguments specified in \var{args}, the resulting function also has named arguments \var{as.reference} and \var{gc}.  The first two have the same behavior as described above.  The \var{gc} argument specifies whether the interpreter's garbage collector should be run to free references.  The default is \code{FALSE} for performance reasons.  Instead of setting \var{gc} to \code{TRUE}, it is recommended that the function \code{\link{scalaGC}} be periodically run if many references from a function returned by \code{\link{scalaDef}} are generated.
}
\author{
David B. Dahl \email{dahl@stat.byu.edu}
}
\seealso{
  \code{\link{scala}},
  \code{\link{scalaSettings}},
  \code{\link{strintrplt}}
}
\keyword{interface}
\examples{
\dontrun{
# Get an instance of a Scala interpreter and see the default settings
s <- scala()
scalaSettings(s)

# Demonstrate convenient notation and string interpolation
stringFromScala <- s \%~\% '"Hello @{Sys.getenv("USER")} from @{R.Version()$nickname}" + "!"*10'
stringFromScala

# Set and get variables
s$rPi <- pi
s$rPi

# Make vectors of length one be set as arrays
scalaSettings(s,length.one.as.vector=TRUE)

# Unlike above, now 'pi' is set as an array of length one
s$rPi <- pi
scalaGet(s,"rPi")
scalaGet(s,"rPi",as.reference=TRUE)              # Get the result as a reference
scalaSet(s,"rPi",pi,length.one.as.vector=FALSE)  # Override current global setting
scalaSettings(s,length.one.as.vector=FALSE)      # Put it back to the default

# Convenient notation
a1 <- s \%~\%  "rPi/2"   # As an R value
a2 <- s \%.~\% "rPi/2"   # As a reference

# References can be set
s$foo <- a2

# Get a reference to an R object
myList <- list(a=2, b=matrix(1:8,nrow=2))
wrappedList <- scalaWrap(s,myList)
identical(myList,scalaUnwrap(s,wrappedList))
s$.myList <- myList
identical(myList,s$myList)

# Instantiate an object
seed <- 2349234L
scalap(s,'scala.util.Random')
rng <- s$do('scala.util.Random')$new(seed)  # Scala equivalent: new scala.util.Random(seed)

# Call method of a reference
system.time(rng$nextInt(100L))   # Scala equivalent: rng.nextInt(100)
system.time(rng$nextInt(100L))   # Notice it runs much faster after the first time.

# Call method of companion object and call methods of a reference
# Scala equivalent: (scala.math.BigInt('777',8) - 500).intValue
s$do('scala.math.BigInt')$apply('777',8L)$'-'(500L)$intValue()

# Longer example showing that 'scalaDef' is more flexible and faster than '\%~\%'
scalaSet(s,"rng",rng)
drawGaussian <- scalaDef(s,'mean: Double, sd: Double','mean+sd*rng.nextDouble')
drawGaussian(3,0.1)
n.draws <- 100
system.time({
  draws <- s \%~\% '
    val result = new Array[Double](@{n.draws})
    result(0) = rng.nextGaussian
    for ( i <- 1 until @{n.draws} ) {
      result(i) = 0.5*result(i-1) + rng.nextGaussian
    }
    result
  '
  acf(draws,plot=FALSE)
})
sampler <- s$def('nDraws: Int, rho: Double','
  val result = new Array[Double](nDraws)
  result(0) = rng.nextGaussian
  for ( i <- 1 until nDraws ) {
    result(i) = rho*result(i-1) + rng.nextGaussian
  }
  result
')
system.time(acf(sampler(n.draws,0.5),plot=FALSE))
system.time(acf(sampler(n.draws,0.9),plot=FALSE))
}
}


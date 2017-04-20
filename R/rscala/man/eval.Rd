\name{eval}
\alias{scalap}
\alias{\%@\%}
\alias{\%~\%}
\alias{\%.~\%}
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
scalap(interpreter, class.name)

interpreter \%@\% snippet
interpreter \%~\% snippet
interpreter \%.~\% snippet

\method{$}{ScalaInterpreter}(interpreter, identifier)
\method{$}{ScalaInterpreter}(interpreter, identifier) <- value
}
\arguments{
    \item{interpreter}{An interpreter from the \code{\link{scala}} function.}
    \item{snippet}{A character vector of arbitrary length to be evaluated by the interpreter.  Multiline elements are accepted.}
    \item{identifier}{A character vector of length one containing a valid Scala identifier.}
    \item{class.name}{A character vector of length one containing a full-qualified class name.}
    \item{value}{Either: i. a vector or matrix of integers, doubles, logicals, or characters, or ii. a Scala reference.}
}
\details{

The convenient shorthand notation \code{interpreter \%@\% snippet} evaluates
the expression \var{snippet}, whereas \code{interpreter \%~\% snippet} both
evaluates the expression \var{snippet} and attempts to return a vector or
matrix of integers, doubles, logicals, or characters or --- if this is not
possible --- \code{NULL} is returned.  The convenient shorthand notation
\code{interpreter \%.~\% snippet} has the same behavior except it always
returns a reference.  References may be used later for assignment and Scala
function arguments.  Note that memory associated as a result of assignment or
executing \code{snippet} will never be garbage collected due to a limitation of
Scala's REPL.  (See the Scala bug detailed here:
\url{https://issues.scala-lang.org/browse/SI-4331}.)

Memory associated with the return values by function defined by
\code{s$def(...) \%~\% snippet} and \code{s$def(...) \%.~\% snippet}, however, is
garbage-collected.  See examples below.  Thus, heavy usage of \code{s$def}
functions is encouraged for memory intensive applications.  Further, these
functions have much less less invocation latency than the equivalent code using
\code{\link{\%~\%}} and \code{\link{\%.~\%}}.  The speed difference in
invocation is especially noticeable for quick functions.

The convenient shorthand notations \code{interpreter$identifier} and
\code{interpreter$identifier <- value} make it easy to get and set variables in
the interpreter.
}
\value{
    \code{s$x} and \code{s$val("x")} try to convert the Scala value \code{x} to a vector or matrix of integers, doubles, logicals, characters, or raw.
    If a conversion is not possible, a Scala reference is returned.  \code{s$.val("x")} always returns a Scala reference.

    \code{s$x <- value} always return \code{value} after having set the Scala variable \code{x} to it.

    \code{s$def(...) \%~\% snippet} returns a Scala function where \code{...} should be named arguments that can be used in the Scala code given in \code{snippet}.
    \code{s$def(...) \%.~\% snippet} is the same, except a Scala reference is always returned.

    \code{s$null(scalaType)} returns a null reference whose Scala type is \code{scalaType}.  This can be helpful in defining Scala functions through \code{s$def}.

    \code{s$.CLASSNAME$METHODNAME(...)} returns the result of calling \code{METHODNAME} in the class \code{CLASSNAME} using the unnamed, positional arguments in \code{...}.
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
s$val("rPi")
s$.val("rPi")

s$rPi <- I(pi)     # Now rPi is a scalar, not an array of length one.
s$rPi              # It doesn't matter to R...
s$.val("rPi")      # ... but it does to Scala.

# Convenient notation
a1 <- s \%~\%  "rPi/2"   # As an R value
a2 <- s \%.~\% "rPi/2"   # As a Scala reference

# References can be set
s$foo <- a2
s$foo

# Instantiate an object
seed <- 2349234L
scalap(s,'scala.util.Random')
s$.scala.util.Random             # More convenient that using scalap.
rng <- s$.scala.util.Random$new(I(seed))  # Scala equivalent: new scala.util.Random(seed)

# Call method of a reference
system.time(rng$nextInt(I(100L)))   # Scala equivalent: rng.nextInt(100)
system.time(rng$nextInt(I(100L)))   # Notice it runs much faster the second time due to caching

rInt <- rng$nextInt(I(100L),.EVALUATE=FALSE)  # Define function to call quickly later without ...
rInt(100)                                     # ... needing to protect scalars and ensure type.

# Call method of companion object and call methods of a reference
# Scala equivalent: (scala.math.BigInt("777",8) - 500).intValue
s$.scala.math.BigInt$apply(I("777"),I(8L))$'-'(I(500L))$intValue()

# Example showing callback functionality
f1 <- s$def(func=NULL,data=numeric()) \%~\% '
  R.observations = data.map(2*_)
  R.evalD1(func,"observations")
'
f2 <- function(x) x^3
f1(f2,1:4)

# Longer example showing that s$def(...) is more flexible and faster than '\%~\%'
s$rng <- rng
drawGaussian <- s$def(mean=I(0.0), sd=I(1.0)) \%~\% 'mean+sd*rng.nextDouble'
drawGaussian(3,0.1)  # No scalar protection or casting is needed.
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
sampler <- s$def(nDraws=I(1L), rho=I(0.0)) \%~\% '
  val result = new Array[Double](nDraws)
  result(0) = rng.nextGaussian
  for ( i <- 1 until nDraws ) {
    result(i) = rho*result(i-1) + rng.nextGaussian
  }
  result
'
system.time(acf(sampler(n.draws,0.5),plot=FALSE))
system.time(acf(sampler(n.draws,0.9),plot=FALSE))
}
}


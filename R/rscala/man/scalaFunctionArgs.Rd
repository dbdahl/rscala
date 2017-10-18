\name{scalaFunctionArgs}
\alias{scalaFunctionArgs}
\docType{methods}
\title{
Manipulate Arguments to a Scala Function
}
\description{
This function allows the user to control which variables are passed to a Scala
function, e.g., \code{function(x,y) s { \%!\% 'x+y'} }.  All arguments of the
enclosing function are passed to Scala, but this can be customized by calling
the \code{scalaFunctionArgs} function before the Scala snippet.  Including
additional items can be convenient and excluding items avoids potentially
costly serializing over the R/Scala bridge.
}
\usage{
scalaFunctionArgs(include=character(), exclude=character())
}
\arguments{
    \item{include}{A character vector indicating the names of additional variables that should be passed to Scala.}
    \item{exclude}{A character vector indicating the names of variables that should not be passed to Scala.}
}
\author{
David B. Dahl \email{dahl@stat.byu.edu}
}
\keyword{interface}

\examples{
\donttest{
# Make an instance of the Scala interpreter 's' and see how its output is captured.
scala(serialize.output=TRUE)
f <- function(x, y) {
  z <- c(quantile(x,0.05), quantile(y,0.05))
  scalaFunctionArgs(include="z", exclude=c("x","y"))
  s %!% 'z(0) / z(1)'
}
f(rnorm(10000), runif(10000))
}
}


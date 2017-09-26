\name{scalaSettings}
\alias{scalaSettings}
\docType{methods}
\title{
Settings of the Scala Interpreter
}
\description{
When only the interpreter is provided, this function retrieves information about the interpreter.
Otherwise, the code interpolation setting can be modified.
}
\usage{
scalaSettings(interpreter, interpolate=NULL, show.header=NULL, info=NULL)
}
\arguments{
    \item{interpreter}{A Scala interpreter from the constructor (i.e. \code{\link{scala}}).}
    \item{interpolate}{If \code{TRUE}, the interpreter will call \code{\link{strintrplt}} before running code.}
    \item{show.header}{If \code{TRUE}, the code for the conversion of Scala function arguments will be displayed (for debugging purposes).}
    \item{info}{An argument that cannot be set by the user.}
}
\author{
David B. Dahl \email{dahl@stat.byu.edu}
}
\seealso{
  \code{\link{scala}},
  \code{\link{strintrplt}}
}
\keyword{interface}


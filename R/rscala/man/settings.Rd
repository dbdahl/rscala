\name{scalaSettings}
\alias{scalaSettings}
\docType{methods}
\title{
Retrieves information about the Scala interpreter and sets the interpolation behavior.
}
\description{
When only the interpreter is provided, this function retrieves information about the interpreter.
Otherwise, the code interpolation setting can be modified.
}
\usage{
scalaSettings(interpreter, interpolate=NULL, info=NULL)
}
\arguments{
    \item{interpreter}{A Scala interpreter from the constructor (i.e. \code{\link{scala}}).}
    \item{interpolate}{If \code{TRUE}, the interpreter will call \code{\link{strintrplt}} on the supplied code before running it.}
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


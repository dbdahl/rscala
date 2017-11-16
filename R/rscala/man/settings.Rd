\name{scalaSettings}
\alias{scalaSettings}
\docType{methods}
\title{
Settings of a Scala Instance
}
\description{
This function retrieves information about the a Scala instance when using the default values of function arguments.
Otherwise, the code interpolation and show snippet settings can be modified.
}
\usage{
scalaSettings(interpolate=NULL, show.snippet=NULL, info=NULL,
              interpreter=findScalaInstance())
}
\arguments{
    \item{interpolate}{If \code{TRUE}, the interpreter will call \code{\link{strintrplt}} before running code.}
    \item{show.snippet}{If \code{TRUE}, the code for the conversion of Scala function arguments will be displayed (for debugging purposes).}
    \item{info}{An argument that cannot be set by the user.}
    \item{interpreter}{A Scala instance resulting from or side effect of \code{\link{scala}}, \code{\link{scala2}}, or \code{\link{scala3}}.}
}
\author{
David B. Dahl \email{dahl@stat.byu.edu}
}
\seealso{
  \code{\link{scala}},
  \code{\link{strintrplt}}
}
\keyword{interface}


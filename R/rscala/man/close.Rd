\name{close}
\alias{close.ScalaInterpreter}
\docType{methods}
\title{
Closes the resources associated with an interpreter.
}
\description{
This function closes (i.e., frees) the resources associated with an interpreter.  Subsequent uses of the interpreter object will fail.
}
\usage{
\method{close}{ScalaInterpreter}(con, ...)
}
\arguments{
    \item{con}{An interpreter from an interpreter constructor (i.e. \code{\link{scalaInterpreter}}).}
    \item{...}{Currently ignored.}
}
\author{
David B. Dahl \email{dahl@stat.byu.edu}
}
\keyword{interface}


\name{print}
\alias{print.ScalaInterpreter}
\alias{toString.ScalaInterpreter}
\alias{print.ScalaInterpreterItem}
\alias{toString.ScalaInterpreterItem}
\alias{print.ScalaInterpreterReference}
\alias{toString.ScalaInterpreterReference}
\alias{print.ScalaCachedReference}
\alias{toString.ScalaCachedReference}
\docType{methods}
\title{
String Representation of a Scala Interpreter, Reference, or Function
}
\description{
These functions print or return a string representation of a Scala interpreter, a Scala reference, or a Scala function.
}
\usage{
\method{print}{ScalaInterpreter}(x, ...)
\method{toString}{ScalaInterpreter}(x, ...)
\method{print}{ScalaInterpreterReference}(x, ...)
\method{toString}{ScalaInterpreterReference}(x, ...)
\method{print}{ScalaInterpreterItem}(x, ...)
\method{toString}{ScalaInterpreterItem}(x, ...)
}
\arguments{
    \item{x}{An interpreter from the \code{\link{scala}} function, a Scala reference, or a Scala function.}
    \item{...}{Currently ignored.}
}
\author{
David B. Dahl \email{dahl@stat.byu.edu}
}
\keyword{interface}


\name{.rscalaOptions}
\alias{.rscalaOptions}
\docType{methods}
\title{
Set global options
}
\description{
This function can be used to set \pkg{rscala}'s global options before any
packages that depend on \pkg{rscala} are loaded.  Using the function is
equivalent to using the \code{options} function with the arguments
\code{rscala.scala.home}, \code{rscala.heap.maximum}, and/or
\code{rscala.command.line.options}.
}
\usage{
.rscalaOptions(scala.home=NULL, heap.maximum=NULL, command.line.options=NULL)
}
\arguments{
    \item{scala.home}{See the \code{\link{scala}} function.}
    \item{heap.maximum}{See the \code{\link{scala}} function.}
    \item{command.line.options}{See the \code{\link{scala}} function.}
}
\author{
David B. Dahl \email{dahl@stat.byu.edu}
}
\seealso{
  \code{\link{scala}}
}
\keyword{interface}
\examples{
.rscalaOptions(heap.maximum="2G")
}


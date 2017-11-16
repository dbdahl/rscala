\name{scalaConvert}
\alias{scalaConvert}
\alias{scalaConvert.data.frame}
\docType{methods}
\title{
Convert an R object to a Scala reference. 
}
\description{
These functions convert R objects to Scala references and vice versa.  The function \code{scalaConvert} supports copyable types whereas
\code{scalaConvert.data.frame} supports data frames.  These functions provide prototypes for others to write their own conversions
for other types.  Factors in data frames are converted to strings when converting to Scala.
}
\usage{
scalaConvert(x, interpreter = findScalaInstance())

scalaConvert.data.frame(x, interpreter = findScalaInstance())
}
\arguments{
    \item{x}{An R object to convert to a Scala reference.}
    \item{interpreter}{An instance of a Scala interpreter.}
}
\author{
David B. Dahl \email{dahl@stat.byu.edu}
}
\keyword{interface}
\examples{
\donttest{
scala(serialize.output=TRUE)

identical(pi, scalaConvert(scalaConvert(pi)))

identical(mtcars, scalaConvert.data.frame(scalaConvert.data.frame(mtcars)))

dfInScala <- scalaConvert.data.frame(ToothGrowth)
dfInScala$keys()
dfInScala$apply("supp")$"_2"()
dfInScala$apply("supp")$"_1"()
}
}


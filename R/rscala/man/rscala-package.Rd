\name{rscala-package}
\alias{rscala-package}
\alias{rscala}
\docType{package}
\title{
Bi-directional interface between \R and Scala with callbacks.
}
\description{
This package embeds: i. the \R interpreter in Scala, and ii. the Scala compiler/interpreter in \R (with callbacks to \R).
}
\author{David B. Dahl \email{dahl@stat.byu.edu}}
%\references{}
\seealso{
  \code{\link{scala}},
  \code{\link{scalaEval}},
  \code{\link{scalaSet}},
  \code{\link{scalaGet}},
  \code{\link{scalaDef}},
  \code{\link{scalaSettings}},
  \code{\link{strintrplt}}
}
\keyword{package}
\examples{
\dontrun{
s <- scala()
s %~% 'sys.props("java.verson")'
s$a <- pi
cat("The value of pi is ",s$a,"\n",sep="")
}
}


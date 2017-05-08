\name{strintrplt}
\alias{strintrplt}
\docType{methods}
\title{
Performs string interpolation with arbitrary R code.
}
\description{
This function allows a character vector to have arbitrary \R code embedded within it.  The embedded \R code --- placed between \sQuote{@\{} and \sQuote{\}} --- is evaluated by default in the environment from which this function is called (not defined).  The result of the function is a character vector in which the embedded \R code is replaced by character representations of the results of evaluating the embedded \R code.
}
\usage{
strintrplt(snippet,envir=parent.frame())
}
\arguments{
    \item{snippet}{A character vector of length one which may contain multiple \sQuote{@\{RCODE\}} substrings which will be replaced by a character representation of the result of evaluating \sQuote{RCODE}.  This character vector may contain many \R statements and span multiple lines.}
    \item{envir}{The environment in which to evaluate the embedded \R code.}
}
\author{
David B. Dahl \email{dahl@stat.byu.edu}
}
\keyword{interface}
\examples{
strintrplt('val msg = "Current R version is @{paste0(R.Version()$major,".",R.Version()$minor)}"')
index <- 5
strintrplt('var i = @{index-1}')
}


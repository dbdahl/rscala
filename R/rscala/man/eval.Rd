\name{\%~\%}
\alias{scalap}
\alias{\%@\%}
\alias{\%~\%}
\alias{\%.~\%}
\alias{\%~~\%}
\alias{$.ScalaInterpreter}
\alias{$<-.ScalaInterpreter}
\docType{methods}
\title{
Execute code, set values, and get values in an embedded interpreter.
}
\description{
These functions define the package's interface to an embedded Scala
interpreter/compiler.  Code is executed in Scala and data is passed between \R
and Scala.  Callbacks to the original \R interpreter are supported.  \R functions
can be implemented in Scala.  The result of a Scala execution can be a Scala
reference or an \R object.
}
\usage{
scalap(interpreter, class.name)

interpreter \%@\% snippet
interpreter \%~\% snippet
interpreter \%.~\% snippet

\method{$}{ScalaInterpreter}(interpreter, identifier)
\method{$}{ScalaInterpreter}(interpreter, identifier) <- value
}
\arguments{
    \item{interpreter}{An interpreter from the \code{\link{scala}} function.}
    \item{snippet}{A character vector of arbitrary length to be evaluated by the interpreter.  Multiline elements are accepted.}
    \item{identifier}{A character vector of length one containing a valid Scala identifier.}
    \item{class.name}{A character vector of length one containing a full-qualified class name.}
    \item{value}{Either: i. a vector or matrix of integers, doubles, logicals, or characters, or ii. a Scala reference.}
}
\details{

The convenient shorthand notation \code{interpreter \%@\% snippet} evaluates
the expression \var{snippet}, whereas \code{interpreter \%~\% snippet} both
evaluates the expression \var{snippet} and attempts to return a vector or
matrix of integers, doubles, logicals, or characters or --- if this is not
possible --- \code{NULL} is returned.  The convenient shorthand notation
\code{interpreter \%.~\% snippet} has the same behavior except it always
returns a reference.  References may be used later for assignment and Scala
function arguments.  Note that memory associated as a result of assignment or
executing \code{snippet} will never be garbage collected due to a limitation of
Scala's REPL.  (See the Scala bug detailed here:
\url{https://issues.scala-lang.org/browse/SI-4331}.)

Memory associated with the return values by function defined by
\code{s$def(...) \%~\% snippet} and \code{s$def(...) \%.~\% snippet}, however, is
garbage-collected.  See examples below.  Thus, heavy usage of \code{s$def}
functions is encouraged for memory intensive applications.  Further, these
functions have much less less invocation latency than the equivalent code using
\code{\link{\%~\%}} and \code{\link{\%.~\%}}.  The speed difference in
invocation is especially noticeable for quick functions.

The convenient shorthand notations \code{interpreter$identifier} and
\code{interpreter$identifier <- value} make it easy to get and set variables in
the interpreter.
}
\value{
    \code{s$x} and \code{s$val("x")} try to convert the Scala value \code{x} to a vector or matrix of integers, doubles, logicals, characters, or raw.
    If a conversion is not possible, a Scala reference is returned.  \code{s$.val("x")} always returns a Scala reference.

    \code{s$x <- value} always return \code{value} after having set the Scala variable \code{x} to it.

    \code{s$def(...) \%~\% snippet} returns a Scala function where \code{...} should be named arguments that can be used in the Scala code given in \code{snippet}.
    \code{s$def(...) \%.~\% snippet} is the same, except a Scala reference is always returned.

    \code{s$null(scalaType)} returns a null reference whose Scala type is \code{scalaType}.  This can be helpful in defining Scala functions through \code{s$def}.

    \code{s$.CLASSNAME$METHODNAME(...)} returns the result of calling \code{METHODNAME} in the class \code{CLASSNAME} using the unnamed, positional arguments in \code{...}.
}
\author{
David B. Dahl \email{dahl@stat.byu.edu}
}
\seealso{
  \code{\link{scala}},
  \code{\link{scalaSettings}},
  \code{\link{strintrplt}}
}
\keyword{interface}


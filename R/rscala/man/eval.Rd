\name{\%~\%}
\alias{scalap}
\alias{\%@\%}
\alias{\%~\%}
\alias{\%.~\%}
\alias{\%!\%}
\alias{\%.!\%}
\alias{scalaOptimize}
\alias{scalaNull}
\alias{$.ScalaInterpreter}
\alias{$<-.ScalaInterpreter}
\docType{methods}
\title{
Execute Code, Set Values, and Get Values in a Scala Interpreter
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

interpreter \%!\% snippet
interpreter \%.!\% snippet

scalaOptimize(scalaFunction)

scalaNull(type)

\method{$}{ScalaInterpreter}(interpreter, identifier)
\method{$}{ScalaInterpreter}(interpreter, identifier) <- value
}
\arguments{
    \item{interpreter}{An interpreter from the \code{\link{scala}} function.  Or, when used in the \code{\link{scalap}} function, a Scala reference or a Scala interpreter.}
    \item{snippet}{A character vector of arbitrary length to be evaluated by the interpreter.  Multiline elements are accepted.}
    \item{scalaFunction}{A (unoptimized) Scala function created by calling \code{interpreter \%!\% snippet} and \code{interpreter \%.!\% snippet} within a function.}
    \item{type}{A string containing a valid Scala type, e.g., \code{"List[Double]"}.}
    \item{identifier}{A character vector of length one containing a valid Scala identifier.}
    \item{value}{Either: i. a vector or matrix of integers, doubles, logicals, characters, or raw, or ii. a Scala reference.}
    \item{class.name}{A character vector of length one containing a full-qualified class name.  This should not be provided if the first argument to the \code{\link{scalap}} function is a Scala reference.}
}
\details{

\code{interpreter \%@\% snippet} evaluates
the expression \var{snippet}, whereas \code{interpreter \%~\% snippet} both
evaluates the expression \var{snippet} and attempts to return a vector or
matrix of integers, doubles, logicals, characters, or raw or --- if this is not
possible --- \code{NULL} is returned.  
\code{interpreter \%.~\% snippet} has the same behavior except it always
returns a Scala reference.  References may be used later for assignment and Scala
function arguments.  Note that memory allocated as a result of assignment or
executing \code{snippet} will never be garbage collected due to a limitation of
Scala's REPL.  (See the Scala bug detailed here:
\url{https://issues.scala-lang.org/browse/SI-4331}.)

\code{interpreter \%!\% snippet} and \code{interpreter \%.!\% snippet}
are the analog to \code{interpreter \%~\% snippet} and \code{interpreter \%~\% snippet},
respectively, that should only be used within a function to define a Scala function.
Memory associated with the return values by function defined by
\code{interpreter \%!\% snippet} and \code{interpreter \%.!\% snippet} is
garbage collected.  Thus, heavy usage of \code{interpreter \%!\% snippet} and \code{interpreter \%.!\% snippet}
functions is encouraged for memory intensive applications.  Importantly, these
functions have much less invocation latency than the equivalent code using
\code{\link{\%~\%}} and \code{\link{\%.~\%}}.  The speed difference in
invocation is especially noticeable for quick functions.  These Scala functions
can be further optimized by using the \code{\link{scalaOptimize}} function.

\code{interpreter$identifier} and
\code{interpreter$identifier <- value} get and set variables in the interpreter.

}
\value{
    \code{s$x} try to convert the Scala object \code{x} to a vector or matrix of integers, doubles, logicals, characters, or raw.
    If a conversion is not possible, a Scala reference is returned.  \code{s$.x} always returns a Scala reference for the Scala object \code{x}.

    \code{s$x <- value} always return \code{value} after having set the Scala variable \code{x} to it.

    \code{function(...) s \%!\% snippet} returns a Scala function where \code{...} should be named arguments that can be used in the Scala code given in \code{snippet}.
    \code{function(...) s \%.!\% snippet} is the same, except a Scala reference is always returned.

    \code{scalaFunction(f)} returns an optimized Scala function having less invocation latency than the provided (unoptimized) Scala function \code{f}.

    \code{scalaNull(type)} returns a null reference whose Scala type is \code{type}.  This can be helpful in defining Scala functions.

    \code{s$.CLASSNAME$METHODNAME(...)} returns the result of calling \code{METHODNAME} in the class \code{CLASSNAME} using the unnamed, positional arguments in \code{...}.
    \code{ref$METHODNAME(...)} returns the result of calling \code{METHODNAME} on the Scala reference \code{ref} using the unnamed, positional arguments in \code{...}.
    Adding \code{.EVALUATE=FALSE} after \code{...} will yield an optimized Scala function instead of evaluating the function.
    Adding \code{.AS.REFERENCE=TRUE} after \code{...} will always yield a Scala reference.

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


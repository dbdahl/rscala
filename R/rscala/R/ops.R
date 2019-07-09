#' Evaluation Operator
#'
#' This operator compiles and executes a snippet of Scala code.  All definitions
#' are \emph{local} to the supplied Scala snippet.  Subsequent uses of the same
#' code snippet skips the time-consuming compilation step.  The return value is
#' a vector or matrix of \R's basic types (if possible) or an rscala reference
#' (otherwise).
#'
#' @param bridge A Scala bridge.
#' @param snippet String providing a Scala code snippet.
#'
#' @return Returns a vector or matrix of \R's basic types (if possible) or an
#'   rscala reference (otherwise).
#' @seealso \code{\link{^.rscalaBridge}}, \code{\link{+.rscalaBridge}},
#'   \code{\link{scala}}
#' @export
#' @examples \donttest{
#' s <- scala()
#' s * 'scala.util.Random.nextDouble() <= 0.75'
#' s(mean=10, sd=2.5) * 'mean + sd * scala.util.Random.nextGaussian()'
#' close(s)
#' }
#' 
'*.rscalaBridge' <- function(bridge, snippet) {
  details <- attr(bridge,"details")
  args <- if ( is.function(bridge) ) list() else bridge
  scalaInvoke(details, snippet, args, parent.frame(1), withNames=TRUE)
}

#' Evaluation Operator Returning a Reference and Transcompile Operator
#'
#' This operator is equivalent to \code{\link{*.rscalaBridge}}, except the
#' return value is always an rscala reference. This operator also allows (a
#' small subset of) R code to be transcompiled to Scala code and produces an
#' rscala reference to an anonymous Scala function.
#'
#' @inheritParams *.rscalaBridge
#'
#' @return Returns an rscala reference.
#' @seealso \code{\link{*.rscalaBridge}}, \code{\link{+.rscalaBridge}},
#'   \code{\link{scala}}
#' @export
#' @examples \donttest{
#' s <- scala()
#' x <- s ^ 'new scala.util.Random()'  # These two lines ...
#' x <- s $ .new_scala.util.Random()   # ... are equivalent
#' s(rng=x) * 'rng.nextDouble()'
#' f <- s ^ function(x=scalaType('Double')) { pi - x }
#' f$apply(3.14)
#' s(n=10L, mapper=s ^ function(x=scalaType("Int")) { 2 * x }) * "Array.tabulate(n)(mapper)"
#' logStdNormalDensity <- s ^ function(x=scalaType("Double"), mean=0.0, sd=1.0) {
#'   variance <- sd^2
#'   -0.5*log(2*pi*variance) - 0.5/variance * (x-mean)^2
#' }
#' identical(logStdNormalDensity$apply(1.0), dnorm(1.0, log=TRUE))
#' close(s)
#' }
#' 
'^.rscalaBridge' <- function(bridge, snippet) {
  details <- attr(bridge,"details")
  args <- if ( is.function(bridge) ) list() else bridge
  if ( ! is.function(snippet) ) scalaInvoke(details, paste0(".",snippet), args, parent.frame(1), withNames=TRUE)
  else {
    if ( any(sapply(args,function(x) inherits(x,"rscalaType"))) ) stop("'scalaType' arguments must be in the function itself, not the Scala bridge.")
    ast <- as.list(snippet)
    args2 <- lapply(ast[-length(ast)],eval,envir=environment(snippet))
    ast <- ast[[length(ast)]]
    details <- attr(bridge,"details")
    if ( details[["disconnected"]] ) scalaConnect(details)
    symbolEnv <- new.env(parent=emptyenv())
    transcompilation <- r2scala(ast,details[["debugTranscompilation"]],symbolEnv,details[["transcompileSubstitute"]])
    returnString <- if ( exists("_returnType",envir=symbolEnv) ) paste0(": ",get("_returnType",envir=symbolEnv)) else ""
    header <- details[["transcompileHeader"]]
    header <- if ( length(header) > 0 ) paste0(paste0(header,collapse="\n"),"\n") else NULL
    whichInternal <- if ( length(args2) == 0 ) logical(0) else sapply(args2,function(x) inherits(x,"rscalaType"))
    internalArgs <- args2[whichInternal]
    args <- c(args,args2[!whichInternal])
    internalArgsList <- if ( length(internalArgs) > 0 ) paste0(names(internalArgs),": ",internalArgs,collapse=", ") else NULL
    scalaInvoke(details, paste0(".",header,"def self(", internalArgsList, ")",returnString," = ", transcompilation, "\nself _"), args, parent.frame(1), withNames=TRUE,
                transcompileInfo=list(argTypes=internalArgs,original=snippet))
  }
}

#' Declaration Operator
#'
#' This operator compiles and executes a snippet of Scala code \emph{in Scala's
#' global environment}, where subsequent uses of the same code snippet do
#' \emph{not} skip the time-consuming compilation step and the return value is
#' \code{NULL}.  As such, this operator is used to define \emph{global} imports,
#' objects, classes, methods, etc.
#'
#' @inheritParams *.rscalaBridge
#'
#' @return Returns \code{NULL}, invisibly.
#' @seealso \code{\link{*.rscalaBridge}}, \code{\link{^.rscalaBridge}},
#'   \code{\link{scala}}
#' @export
#' @examples \donttest{
#' s <- scala()
#' s + '
#'   import scala.util.Random.nextInt
#'   import scala.math.{Pi, log, exp, sqrt}
#'   val const = -log(sqrt(2*Pi))
#'   def dnorm(x: Double, mean: Double, sd: Double, logScale: Boolean) = {
#'     val z = ( x - mean ) / sd
#'     val result = const - log(sd) - z * z / 2
#'     if ( logScale ) result else exp(result)
#'   }
#' '
#' s $ const()
#' s $ nextInt(100L)
#' s $ dnorm(8, 10, 2, FALSE)
#' close(s)
#' }
#' 
'+.rscalaBridge' <- function(bridge, snippet) {
  if ( ( ! is.function(bridge) ) && ( length(bridge) > 0 ) ) stop("When using the `+` operator, no arguments may be supplied to the bridge.")
  scalaInvoke(attr(bridge,"details"), snippet, NULL, parent.frame(1))
}


#' Evaluation Operator
#'
#' This operator compiles and executes a snippet of Scala code.  All definitions
#' are \emph{local} to Scala snippet itself.  Subsequent uses of the same code
#' snippet skips the time-consuming compilation step.  The return value is a
#' vector or matrix of \R's basic types (if possible) or an rscala reference
#' (otherwise).
#'
#' @param bridge An rscala bridge.
#' @param snippet String providing a Scala code snippet.
#'
#' @return Returns a vector or matrix of \R's basic types (if possible) or an
#'   rscala reference (otherwise).
#' @seealso \code{\link{^.rscalaBridge}}, \code{\link{+.rscalaBridge}},
#'   \code{\link{scala}}
#' @export
#' @examples \donttest{
#' scala(assign.name='e')      # Implicitly defines the bridge 'e'.
#' e * 'scala.util.Random.nextDouble() <= 0.75'
#' e(mean=10, sd=2.5) * 'mean + sd * scala.util.Random.nextGaussian()'
#' close(e)
#' }
#' 
'*.rscalaBridge' <- function(bridge, snippet) {
  details <- attr(bridge,"details")
  args <- if ( is.function(bridge) ) list() else bridge
  scalaInvoke(details, snippet, args, withNames=TRUE)
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
#' scala(assign.name='e')              # Implicitly defines the bridge 'e'.
#' x <- e ^ 'new scala.util.Random()'  # These two lines ...
#' x <- e $ .new_scala.util.Random()   # ... are equivalent
#' e(rng=x) * 'rng.nextDouble()'
#' f <- e ^ function(x=scalaType('Double')) { pi - x }
#' f$apply(3.14)
#' e(n=10L, mapper=e ^ function(x=scalaType("Int")) { 2 * x }) * "Array.tabulate(n)(mapper)"
#' logStdNormalDensity <- e ^ function(x=scalaType("Double"), mean=0.0, sd=1.0) {
#'   variance <- sd^2
#'   -0.5*log(2*pi*variance) - 0.5/variance * (x-mean)^2
#' }
#' identical(logStdNormalDensity$apply(1.0), dnorm(1.0, log=TRUE))
#' close(e)
#' }
#' 
'^.rscalaBridge' <- function(bridge, snippet) {
  details <- attr(bridge,"details")
  args <- if ( is.function(bridge) ) list() else bridge
  if ( ! is.function(snippet) ) scalaInvoke(details, paste0(".",snippet), args, withNames=TRUE)
  else {
    ast <- as.list(snippet)
    args <- c(args,lapply(ast[-length(ast)],eval))
    ast <- ast[[length(ast)]]
    details <- attr(bridge,"details")
    symbolEnv <- new.env(parent=emptyenv())
    transcompilation <- r2scala(ast,details[["debugTranscompilation"]],symbolEnv,details[["transcompileSubstitute"]])
    returnString <- if ( exists("_returnType",envir=symbolEnv) ) paste0(": ",get("_returnType",envir=symbolEnv)) else ""
    header <- details[["transcompileHeader"]]
    header <- if ( length(header) > 0 ) paste0(paste0(header,collapse="\n"),"\n") else NULL
    whichInternal <- if ( length(args) == 0 ) logical(0) else sapply(args,function(x) inherits(x,"rscalaType"))
    internalArgs <- args[whichInternal]
    args <- args[!whichInternal]
    internalArgsList <- if ( length(internalArgs) > 0 ) paste0(names(internalArgs),": ",internalArgs,collapse=", ") else NULL
    scalaInvoke(details, paste0(".",header,"def self(", internalArgsList, ")",returnString," = ", transcompilation, "\nself _"), args, withNames=TRUE,
                transcompileInfo=list(argTypes=internalArgs,original=snippet))
  }
}

#' Execution Operator
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
#' scala(assign.name='e')      # Implicitly defines the bridge 'e'.
#' e + '
#'   import scala.util.Random.nextInt
#'   import scala.math.{Pi, log, exp, sqrt}
#'   val const = -log(sqrt(2*Pi))
#'   def dnorm(x: Double, mean: Double, sd: Double, logScale: Boolean) = {
#'     val z = ( x - mean ) / sd
#'     val result = const - log(sd) - z * z / 2
#'     if ( logScale ) result else exp(result)
#'   }
#' '
#' e $ const()
#' e $ nextInt(100L)
#' e $ dnorm(8, 10, 2, FALSE)
#' close(e)
#' }
#' 
'+.rscalaBridge' <- function(bridge, snippet) {
  scalaInvoke(attr(bridge,"details"), snippet, NULL)
}

#' Operator to Make an R Object Reference
#'
#' This operator creates an rscala reference from an arbitrary R object.  This reference
#' can be passed as an argument to a method call of the embedded \code{RClient} or
#' may be reconstituted using the unary minus operator \code{\link{-.rscalaReference}}.
#'
#' @param bridge An rscala bridge
#' @param rObject An arbitrary R object
#' 
#' @seealso \code{\link{-.rscalaReference}}
#' @export
#' @examples \donttest{
#' scala(assign.name='e')      # Implicitly defines the bridge 'e'.
#' wrappedFunction <- e - rnorm
#' (-wrappedFunction)(10)
#' close(e)
#' }
#' 
'-.rscalaBridge' <- function(bridge,rObject) {
  if ( is.list(rObject) && ( ! inherits(rObject,"AsIs") ) ) {
    bridge(len=length(rObject)) ^ '
      List.tabulate(len) { i =>
        R.evalObject("rObject[[%-]]",i+1)
      }
    '
  } else {
    bridge$.R.evalObject('rObject')
  }
}

#' Operator to Reconstitute an R object
#' 
#' This operator reconstitutes an R object that has been created by the binary minus
#' operator \code{\link{-.rscalaReference}} on a scala bridge.
#'
#' @param rscalaReference An rscala reference of type \code{org.ddahl.rscala.RObject}.
#' @param e2 Ignored since only the unary minus operator is supported for rscala references.
#'
#' @aliases unaryMinus
#' @export
#' @seealso \code{\link{-.rscalaBridge}}
#' @examples \donttest{
#' scala(assign.name='e')      # Implicitly defines the bridge 'e'.
#' e(func=e-rnorm, arg=10) * 'R.evalD1("%-(%-)",func,arg)'
#' wrappedFunction <- e - dnorm
#' identical((-wrappedFunction)(1.0), dnorm(1.0))
#' close(e)
#' }
#'  
'-.rscalaReference' <- function(rscalaReference, e2) {
  env <- attr(rscalaReference,"rscalaReferenceEnvironment")
  type <- env[["type"]] 
  if ( type == "org.ddahl.rscala.RObject" ) {
    unserialize(rscalaReference$x())
  } else if ( type == "List[org.ddahl.rscala.RObject]" ) {
    args <- list(arr=rscalaReference)
    snippet <- '.(arr.flatMap(_.x).toArray, arr.scanLeft(1)((sum,y) => sum + y.x.length).toArray)'
    pair <- scalaInvoke(env[["details"]], snippet, args, withNames=TRUE)
    bytes <- pair$"_1"()
    sizes <- pair$"_2"()
    lapply(seq_along(sizes[-1]), function(i) {
      unserialize(bytes[sizes[i]:(sizes[i+1]-1)])
    }) 
  } else stop("Only references to RObject or List[RObject] are allowed.")
}

#' @export
#' 
`[[.rscalaReference` <- function(x, condition) {
  ## DBD 2018-07-12: This is a hack to allow current CRAN versions of sdols (1.6) and shallot (0.4.4) to pass CRAN checks without being updated.
  ##                 Working versions of sdols and shallot without this hack are already in git.  When these packages are updated, this hack
  ##                 can be dropped.
  if ( condition == "type" ) scalaType(x)
  else stop("Not supported.")
}

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

#' Evaluation Operator Returning a Reference
#'
#' This operator is equivalent to \code{\link{*.rscalaBridge}}, except the
#' return value is always an rscala reference.
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
#' close(e)
#' }
#' 
'^.rscalaBridge' <- function(bridge, snippet) {
  details <- attr(bridge,"details")
  args <- if ( is.function(bridge) ) list() else bridge
  scalaInvoke(details, paste0(".",snippet), args, withNames=TRUE)
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
  details <- attr(bridge,"details")
  scalaEvaluate(details, snippet)
}

#' Operator to Make an R Object Reference
#'
#' This operator creates an rscala reference to an arbitrary R object.  This reference
#' can be passed as an argument to a method call of the embedded \code{RClient} or
#' may be reconstituted using the unary minus operator \code{\link{-.rscalaReference}}.
#'
#' @param bridge 
#' @param rObject 
#' 
#' @seealso \code{\link{-.rscalaReference}}
#' @export
#' @examples
#' scala(assign.name='e')      # Implicitly defines the bridge 'e'.
#' wrappedFunction <- e - rnorm
#' (-wrappedFunction)(10)
#' close(e)
#' 
'-.rscalaBridge' <- function(bridge,rObject) {
  if ( is.list(rObject) && ( ! inherits(rObject,"AsIs") ) ) {
    bridge(len=length(rObject)) ^ '
      Array.tabulate(len) { i =>
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
#'
#' @aliases unaryMinus
#' @export
#' @seealso \code{\link{-.rscalaBridge}}
#' @examples
#' scala(assign.name='e')      # Implicitly defines the bridge 'e'.
#' wrappedFunction <- e - rnorm
#' (-wrappedFunction)(10)
#' close(e)
#'  
'-.rscalaReference' <- function(rscalaReference, e2) {
  type <- rscalaReference[["type"]] 
  if ( type == "org.ddahl.rscala.RObject" ) {
    unserialize(rscalaReference$x())
  } else if ( type == "Array[org.ddahl.rscala.RObject]" ) {
    args <- list(arr=rscalaReference)
    snippet <- '.(arr.flatMap(_.x), arr.scanLeft(1)((sum,y) => sum + y.x.length))'
    pair <- scalaInvoke(rscalaReference[["details"]], snippet, args, withNames=TRUE)
    bytes <- pair$"_1"()
    sizes <- pair$"_2"()
    lapply(seq_along(sizes[-1]), function(i) {
      unserialize(bytes[sizes[i]:(sizes[i+1]-1)])
    }) 
  } else stop("Only references to RObject or Array[RObject] are allowed.")
}

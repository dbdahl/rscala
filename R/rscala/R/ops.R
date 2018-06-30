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

#' @export
#' 
'-.rscalaBridge' <- function(bridge,rObject) bridge$.R.evalObject('rObject')

#' @export
#' 
'-.rscalaReference' <- function(rscalaReference, e2) unserialize(rscalaReference$x())

.rscalaOptions <- function(scala.home=NULL, heap.maximum=NULL, command.line.options=NULL) {
  if ( ! is.null(scala.home) ) options(rscala.scala.home=scala.home)
  if ( ! is.null(heap.maximum) ) options(rscala.heap.maximum=heap.maximum)
  if ( ! is.null(command.line.options) ) options(rscala.command.line.options=command.line.options)
}


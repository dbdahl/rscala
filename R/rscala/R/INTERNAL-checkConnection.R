checkConnection <- function(details) {
  if ( Sys.getpid() != details[["pid"]] ) {
    close.rscalaBridge(details)
    stop("Bridge is closed.")
  }
}

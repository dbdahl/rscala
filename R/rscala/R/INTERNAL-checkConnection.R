checkConnection <- function(details) {
  if ( Sys.getpid() != details[["pid"]] ) {
    close.rscalaBridge(details)
    stop("ERROR:  Each process should have its own rscala bridge.  Closing this bridge.")
  }
}

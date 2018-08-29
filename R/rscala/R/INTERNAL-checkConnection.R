checkConnection <- function(details) {
  if ( Sys.getpid() != details[["pid"]] ) {
    close.rscalaBridge(details)
    stop("Each process should have its own rscala bridge.  Closing this bridge.")
  }
  if ( details[['closed']] ) stop("Connection is already closed.")
}
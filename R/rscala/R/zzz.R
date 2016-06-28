typeMap <- list()
typeMap[[INTEGER]] <- "integer"
typeMap[[DOUBLE]] <- "double"
typeMap[[BOOLEAN]] <- "integer"
typeMap[[STRING]] <- "character"

.onAttach <- function(libname, pkgname) {
  output <- capture.output({
    info <- scalaInfo()
  })
  if ( is.null(info) ) packageStartupMessage(paste(output,collapse="\n"))
}

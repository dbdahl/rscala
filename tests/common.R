library(rscala)
. <- scalaScalar

s <- local({
  getenv <- function(key,orElse="") {
    value <- Sys.getenv(key)
    if ( value != "" ) value else orElse
  }
  serialize <- as.logical(getenv("RSCALA_SERIALIZE",FALSE))
  output <- as.logical(getenv("RSCALA_OUTPUT",TRUE))
  version <- Sys.getenv("RSCALA_SCALA_VERSION",NA)
  if ( ! exists("jars") ) jars <- character()
  if ( ! exists("heap.maximum") ) heap.maximum <- NULL
  if ( ! exists("debug") ) debug <- FALSE
  s <- scala(classpath=jars,serialize=serialize,stdout=output,stderr=output,heap.maximum=heap.maximum,debug=debug)
  actualVersion <- s %~% "scala.util.Properties.versionNumberString"
  if ( !is.na(version) && ( version != s %~% "scala.util.Properties.versionNumberString" ) ) {
    cat("Requested version: ",version,"\n")
    cat("Actual version:    ",actualVersion,"\n")
    stop("Version mismatch.")
  }
  cat(scalaSettings(s)$serialize,' ',output,'\n',sep='')
  s
})


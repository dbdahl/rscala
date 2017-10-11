library(rscala)

s <- local({
  getenv <- function(key,orElse="") {
    value <- Sys.getenv(key)
    if ( value != "" ) value else orElse
  }
  serialize <- as.logical(getenv("RSCALA_SERIALIZE",FALSE))
  output <- as.logical(getenv("RSCALA_OUTPUT",TRUE))
  version <- getenv("RSCALA_SCALA_VERSION",NA)
  if ( ! exists("jars") ) jars <- character()
  if ( ! exists("heap.maximum") ) heap.maximum <- NULL
  if ( ! exists("row.major") ) row.major <- TRUE
  if ( ! exists("port") ) port <- 0
  if ( ! exists("debug") ) debug <- FALSE
  s <- scala3(classpath=jars,serialize=serialize,row.major=row.major,stdout=output,stderr=output,heap.maximum=heap.maximum,debug=debug,port=port)
  actualVersion <- s %~% "scala.util.Properties.versionNumberString"
  if ( !is.na(version) && ( version != actualVersion ) ) {
    cat("Requested version: ",version,"\n")
    cat("Actual version:    ",actualVersion,"\n")
    stop("Version mismatch.")
  }
  cat('# ')
  cat(paste(R.Version()$version.string),Sys.info()[["nodename"]],scalaSettings(s)$serialize,output,sep=' # ')
  cat('\n')
  s
})


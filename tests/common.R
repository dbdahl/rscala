library(rscala)

s <- local({
  getenv <- function(key,orElse="") {
    value <- Sys.getenv("RSCALA_SERIALIZE")
    if ( value != "" ) value else orElse
  }
  serialize <- as.logical(getenv("RSCALA_SERIALIZE",FALSE))
  output <- as.logical(getenv("RSCALA_OUTPUT"),TRUE)
  version <- Sys.getenv("RSCALA_SCALA_VERSION",NA)
  s <- scala(serialize=serialize,stdout=output,stderr=output)
  actualVersion <- s %~% "scala.util.Properties.versionNumberString"
  if ( !is.na(version) && ( version != s %~% "scala.util.Properties.versionNumberString" ) ) {
    cat("Requested version: ",version,"\n")
    cat("Actual version:    ",actualVersion,"\n")
    stop("Version mismatch.")
  }
  s
})


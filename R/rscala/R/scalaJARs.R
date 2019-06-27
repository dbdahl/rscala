#' Add JAR Files to Classpath
#'
#' @param JARs Character vector whose elements are some combination of
#'   individual JAR files or package names which contain embedded JARs.  These
#'   JAR files are added to the runtime classpath.
#' @param bridge An rscala bridge from the \code{scala} function.
#'
#' If the \code{JARs} argument is missing, a character vector of loaded JARs
#' is returned.
#'
#' @return Returns \code{NULL}, invisibly.
#' 
#' @export
#' @seealso \code{\link{scalaFindBridge}}
#'
#' @examples \dontrun{
#' 
#' scalaJARs("PATH/TO/jarFileToLoad.jar", e)
#' }
scalaJARs <- function(JARs, bridge=scalaFindBridge()) {
  details <- if ( inherits(bridge,"rscalaBridge") ) attr(bridge,"details") else bridge
  if ( missing(JARs) ) {
    if ( details[["disconnected"]] ) scalaConnect(details)
    return(details$JARs)
  }
  if ( ! is.character(JARs) ) stop("'JARs' should be a character vector.")
  if ( details[["disconnected"]] ) {
    assign("pendingJARs",c(get("pendingJARs",envir=details),JARs),envir=details)
  } else scalaJARsEngine(JARs, details)
}

scalaJARsEngine <- function(JARs, details) {
  checkConnection(details)
  socketOut <- details[["socketOut"]]
  scalaLastEngine(details)
  if ( details[["interrupted"]] ) return(invisible())
  JARs <- unlist(lapply(JARs, function(x) {
    if ( ! identical(find.package(x,quiet=TRUE),character(0)) ) {
      newHeaders <- unlist(lapply(x,transcompileHeaderOfPackage))
      if ( length(newHeaders) > 0 ) {
        transcompileHeader <- c(get("transcompileHeader",envir=details), newHeaders)
        assign("transcompileHeader",transcompileHeader,envir=details)
      }
      newSubstitutes <- lapply(x,transcompileSubstituteOfPackage)
      if ( length(newSubstitutes) > 0 ) {
        transcompileSubstitute <- unlist(c(get("transcompileSubstitute",envir=details),newSubstitutes))
        assign("transcompileSubstitute",transcompileSubstitute,envir=details)
      }
      jarsOfPackage(x,details[["config"]]$scalaMajorVersion)
    } else x
  }))
  if ( is.null(JARs) ) JARs <- character(0)
  JARs <- path.expand(JARs)
  sapply(JARs, function(JAR) if ( ! file.exists(JAR) ) stop(paste0('File or package "',JAR,'" does not exist.')))
  for ( JAR in JARs ) {
    wb(socketOut,PCODE_ADD_TO_CLASSPATH)
    wc(socketOut,JAR)
    tryCatch(pop(details), error=function(e) stop(paste0("Failed to add ",JAR," to classpath.")))
  }
  assign("JARs",c(get("JARs",envir=details),JARs),envir=details)
  invisible() 
}

jarsOfPackage <- function(pkgname, major.release) {
  dir <- if ( file.exists(system.file("inst",package=pkgname)) ) file.path("inst/java") else "java"
  jarsMajor <- list.files(file.path(system.file(dir,package=pkgname),paste0("scala-",major.release)),pattern=".*\\.jar$",full.names=TRUE,recursive=FALSE)
  jarsAny <- list.files(system.file(dir,package=pkgname),pattern=".*\\.jar$",full.names=TRUE,recursive=FALSE)
  result <- normalizePath(c(jarsMajor,jarsAny))
  if ( length(result) == 0 ) {
    supported.versions <- list.files(system.file(dir,package=pkgname),pattern="scala-.*",full.names=FALSE,recursive=FALSE)
    recommended.version <- pickLatestStableScalaVersion(sub("^scala-","",supported.versions))
    stop(paste0("It appears that package '",pkgname,"' does not support Scala ",major.release,".  Hint, run:\n\n  Sys.setenv(RSCALA_SCALA_VERSION='",recommended.version,"'); rscala::scalaConfig(download='scala')\n\n  Then restart your R session and try again.\n"))
  }
  result
}

#' @importFrom utils getFromNamespace
#' 
transcompileHeaderOfPackage <- function(pkgname) {
  tryCatch( getFromNamespace("rscalaTranscompileHeader", pkgname), error=function(e) NULL )
}

#' @importFrom utils getFromNamespace
#' 
transcompileSubstituteOfPackage <- function(pkgname) {
  tryCatch( getFromNamespace("rscalaTranscompileSubstitute", pkgname), error=function(e) NULL )
}

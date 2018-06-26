#' Retrieve Information on Scala Installation
#'
#' This function retrieves information about the installation of Scala.
#'
#' @param verbose Should information on the search be provided?
#'
#' @return Returns a list containing the path to the Scala executable and version information.
#' @export
#'
#' @examples{
#' scalaInfo()
#' }
#' 
scalaInfo <- function(verbose=TRUE) {
  if ( verbose ) cat("\nSearch details:\n")
  execName <- if ( .Platform$OS.type == "windows" ) "scala.bat" else "scala"
  scalaHome <- Sys.getenv("SCALA_HOME")
  sInfo <- if ( scalaHome != "" ) {
    verifyCandidate(file.path(scalaHome,"bin",execName), "SCALA_HOME environment variable", verbose)
  } else {
    noCandidate("SCALA_HOME environment variable is not set.", verbose)
  }
  if ( is.null(sInfo) ) {
    scalaExec <- unname(Sys.which("scala"))
    sInfo <- if ( scalaExec != "" ) {
      verifyCandidate(scalaExec, "PATH environment variable", verbose)
    } else {
      noCandidate("Could not find 'scala' in shell's PATH.", verbose)
    }
  }
  if ( is.null(sInfo) ) {
    candidates <- sort(list.files("~/.rscala",paste0("^",execName,"$"),recursive=TRUE,full.names=TRUE),decreasing=TRUE)
    if ( length(candidates) == 0 ) noCandidate("Nothing found in ~/.rscala directory.", verbose)
    details <- file.info(candidates)
    details <- details[order(as.POSIXct(details$mtime),decreasing=TRUE), ]
    candidates <- rownames(details)
    for ( candidate in candidates ) {
      sInfo <- verifyCandidate(candidate, "~/.rscala directory", verbose)
      if ( ! is.null(sInfo) ) break
    }
    if ( is.null(sInfo) && interactive() ) {
      # Offer installation
      while ( TRUE ) {
        cat("\n")
        response <- readline(prompt="Scala is not found.  Would you like to install it now? [Y/n] ")
        response <- toupper(trimws(response))
        if ( response == "" ) response <- "Y"
        if ( response == "Y" ) {
          candidate <- scalaInstall()
          sInfo <- if ( ! is.null(candidate) ) verifyCandidate(candidate, "~/.rscala directory", verbose)
          else noCandidate("Installation failed.", verbose)
          break
        }
        if ( response == "N" ) break
      }
    }
  }
  if ( is.null(sInfo) ) stop("Cannot find a suitable Scala insallation.")
  if ( verbose ) cat("\n")
  sInfo
}

noCandidate <- function(message, verbose) {
  if ( verbose ) cat("    ",message,"\n",sep="")
  NULL
}

verifyCandidate <- function(candidate, message, verbose) {
  if ( verbose ) cat("    Looking for Scala using ",message,".\n")
  candidate <- normalizePath(candidate, mustWork=FALSE)
  if ( file.exists(candidate) ) {
    if ( verbose ) cat("    Found 'scala' at ",candidate,".\n",sep="")
    fullVersion <- tryCatch({
      jars <- list.files(file.path(dirname(dirname(candidate)),"lib"),".*.jar$",full.names=TRUE)
      libraryJar <- jars[grepl("^scala-library",basename(jars))]
      fn <- unz(libraryJar,"library.properties")
      lines <- readLines(fn)
      close(fn)
      sub("^version.number=","",lines[grepl("^version.number=",lines)])[1]
    }, warning=function(e) { NULL }, error=function(e) { NULL } )
    if ( is.null(fullVersion) ) {
      cat("    ... but the version number is unknown.")
      return(NULL)
    }
    majorVersion <- gsub("(^[23]\\.[0-9]+)\\..*","\\1",fullVersion)
    list(cmd=candidate, fullVersion=fullVersion, majorVersion=majorVersion)
  }
}

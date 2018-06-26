SCALA_212_VERSION <- "2.12.6"
SCALA_211_VERSION <- "2.11.12"

#' Install Scala
#'
#' This function installs Scala in the user's \code{~/.rscala} directory or (if
#' \code{internal=TRUE}) in the package's directory.
#'
#' @param majorVersion A valid major Scala release numbers (e.g., "2.12").
#' @param internal Should the Scala installation be inside the package's
#'   directory?  This may require administrator privileges.
#'
#' @return Returns the path to the newly installed executable.
#' @export
#' @seealso \code{\link{scalaInfo}}
#' @examples \dontrun{
#' scalaInstall()
#'
#' }
scalaInstall <- function(majorVersion=c("2.11","2.12"), global=FALSE) {
  if ( length(majorVersion) > 1 ) return(scalaInstall(latestVersion(majorVersion), global=global))
  if ( length(majorVersion) == 0 ) stop("At least one major release must be supplied.")
  javaVersion <- javaVersion(findJava())
  if ( ( javaVersion <= 7 ) && ( compareVersion("2.11",majorVersion) < 0 ) ) {
    cat("It appears you are using an Java version <= 7, so Scala 2.11 will be installed.\n")
    return(scalaInstall("2.11", global=global))
  } else if ( ( javaVersion >= 9 ) && ( compareVersion("2.12",majorVersion) > 0 ) ) {
    cat("It appears you are using an Java version >= 9, so Scala 2.12 will be installed.\n")
    return(scalaInstall("2.12", global=global))
  }
  if ( majorVersion == "2.12" ) version <- SCALA_212_VERSION
  else if ( majorVersion == "2.11" ) version <- SCALA_211_VERSION
  else stop("Unsupported major version.")
  installPath <- if ( global ) system.file(package="rscala") else file.path("~",".rscala")
  installPath <- normalizePath(installPath, mustWork=FALSE)
  url <- sprintf("https://downloads.lightbend.com/scala/%s/scala-%s.tgz",version,version)
  dir.create(installPath,showWarnings=FALSE,recursive=TRUE)
  destfile <- file.path(installPath,basename(url))
  result <- download.file(url,destfile)
  if ( result != 0 ) {
    cat("Failed to download installation.\n")
    return(invisible(NULL))
  }
  result <- untar(destfile,exdir=installPath,tar="internal")    # Use internal to avoid problems on a Mac.
  unlink(destfile)
  if ( result == 0 ) {
    cat("Successfully installed Scala in ",file.path(installPath,sprintf("scala-%s",version)),"\n\n",sep="")
    execName <- if ( .Platform$OS.type == "windows" ) "scala.bat" else "scala"
    file.path(installPath,sprintf("scala-%s",version),"bin",execName)
  } else {
    cat("Failed to extract installation.\n")
    invisible(NULL)
  }
}

findJava <- function() {  ## Mimic how the 'scala' shell script finds Java.
  javaName <- if ( .Platform$OS.type == "windows" ) "java.exe" else "java"
  fromPath <- function() {
    candidate <- Sys.which(javaName)[[javaName]]
    if ( ! file.exists(candidate) ) stop("Cannot find Java.  Is it installed?")
    candidate
  }
  candidate <- Sys.getenv("JAVACMD",NA)
  if ( ! is.na(candidate) && file.exists(candidate) ) normalizePath(candidate)
  else {
    javaHome <- Sys.getenv("JAVA_HOME",NA)
    if ( ! is.na(javaHome) ) {
      candidate <- file.path(javaHome,"bin",javaName)
      if ( file.exists(candidate) ) normalizePath(candidate)
      else fromPath()
    } else fromPath()
  }
}

latestVersion <- function(majorVersion) {
  max <- majorVersion[1]
  for ( i in majorVersion[-1] ) {
    if ( compareVersion(max,i) < 0 ) max <- i
  }
  max
}

javaVersion <- function(javaCmd) {
  response <- system2(javaCmd,"-version",stdout=TRUE,stderr=TRUE)
  regexp <- '(java|openjdk) version "([^"]*)".*'
  line <- response[grepl(regexp,response)]
  if ( length(line) != 1 ) stop(paste0("Cannot determine Java version.\n",paste(response,collapse="\n")))
  versionString <- gsub(regexp,"\\2",line)
  versionParts <- strsplit(versionString,"\\.")[[1]]
  versionNumber <- if ( versionParts[1] == '1' ) {
    as.numeric(versionParts[2])
  } else {
    as.numeric(versionParts[1])
  }
  if ( ! ( versionNumber %in% c(7,8,9,10,11) ) ) stop(paste0("Unsupported Java version.\n",paste(response,collapse="\n")))
  versionNumber
}


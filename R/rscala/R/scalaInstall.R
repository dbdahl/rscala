SCALA_213_VERSION <- "2.13.0-M4"
SCALA_212_VERSION <- "2.12.6"
SCALA_211_VERSION <- "2.11.12"

#' Install Scala
#'
#' This function installs Scala in the user's \code{~/.rscala} directory.
#'
#' @param majorVersion A valid major Scala release numbers (e.g., "2.12").
#'
#' @return Returns the path to the newly installed executable.
#' @export
#' @seealso \code{\link{scalaInfo}}
#' @examples \dontrun{
#' 
#' scalaInstall()
#' }
scalaInstall <- function(majorVersion="2.12") {
  if ( length(majorVersion) > 1 ) return(scalaInstall(latestVersion(majorVersion)))
  if ( length(majorVersion) == 0 ) stop("At least one major release must be supplied.")
  javaVersion <- javaVersion(findJava())[1]
  if ( ( javaVersion <= 7 ) && ( utils::compareVersion("2.11",majorVersion) < 0 ) ) {
    cat("\n\nIt appears you are using an Java version <= 7, so Scala 2.11 will be installed.\n\n\n")
    return(scalaInstall("2.11"))
  } else if ( ( javaVersion >= 9 ) && ( utils::compareVersion("2.12",majorVersion) > 0 ) ) {
    cat("\n\nIt appears you are using an Java version >= 9, so Scala 2.12 will be installed.\n\n\n")
    return(scalaInstall("2.12"))
  }
  if ( majorVersion == "2.13" ) version <- SCALA_213_VERSION
  else if ( majorVersion == "2.12" ) version <- SCALA_212_VERSION
  else if ( majorVersion == "2.11" ) version <- SCALA_211_VERSION
  else stop("Unsupported major version.")
  installPath <- file.path("~",".rscala")
  installPath <- normalizePath(installPath, mustWork=FALSE)
  url <- sprintf("https://downloads.lightbend.com/scala/%s/scala-%s.tgz",version,version)
  dir.create(installPath,showWarnings=FALSE,recursive=TRUE)
  destfile <- file.path(installPath,basename(url))
  result <- utils::download.file(url,destfile)
  if ( result != 0 ) {
    cat("Failed to download installation.\n")
    return(invisible(NULL))
  }
  result <- utils::untar(destfile,exdir=installPath,tar="internal")    # Use internal to avoid problems on a Mac.
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

isOS64bit <- function() {
  if ( identical(.Platform$OS.type,"windows") ) {
    out <- system2("wmic",c("/locale:ms_409","OS","get","osarchitecture","/VALUE"),stdout=TRUE)
    any("OSArchitecture=64-bit"==trimws(out))
  } else identical(system2("uname","-m",stdout=TRUE),"x86_64")
}

findJava <- function() {  ## Mimic how the 'scala' shell script finds Java.
  javaName <- if ( .Platform$OS.type == "windows" ) "java.exe" else "java"
  fromPath <- function() {
    candidate <- Sys.which(javaName)[[javaName]]
    if ( ! file.exists(candidate) ) stop(paste0("Cannot find Java.\nPlease download the ",(if ( isOS64bit() ) "64" else "32"),"-bit version of Java here: https://www.java.com.\nThen restart R and try again."))
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
    if ( utils::compareVersion(max,i) < 0 ) max <- i
  }
  max
}

javaVersion <- function(javaCmd) {
  response <- system2(javaCmd,"-version",stdout=TRUE,stderr=TRUE)
  # Get version information
  versionRegexp <- '(java|openjdk) version "([^"]*)".*'
  line <- response[grepl(versionRegexp,response)]
  if ( length(line) != 1 ) stop(paste0("Cannot determine Java version.\n",paste(response,collapse="\n")))
  versionString <- gsub(versionRegexp,"\\2",line)
  versionParts <- strsplit(versionString,"\\.")[[1]]
  versionNumber <- if ( versionParts[1] == '1' ) {
    as.numeric(versionParts[2])
  } else {
    as.numeric(versionParts[1])
  }
  if ( ! ( versionNumber %in% c(7,8,9,10,11) ) ) stop(paste0("Unsupported Java version.\n",paste(versionString,collapse="\n")))
  # Determine if 32 or 64 bit
  bit <- if ( any(grepl('^(Java HotSpot|OpenJDK).* 64-Bit (Server|Client) VM.*$',response)) ||
              any(grepl('^IBM .* amd64-64 .*$',response)) ) 64 else 32
  c(versionNumber,bit)
}


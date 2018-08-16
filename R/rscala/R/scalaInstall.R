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
#' scalaConfig()
#' }
scalaConfig <- function(verbose=TRUE, overwrite=FALSE, download=FALSE) {
  installPath <- normalizePath(file.path("~",".rscala"), mustWork=FALSE)
  configPath  <- file.path(installPath,"config.R")
  consent <- overwrite || download
  if ( !overwrite && file.exists(installPath) ) {
    if ( verbose ) cat(paste0("Read existing configuration file: ",configPath,"\n\n"))
    source(configPath,chdir=TRUE)
    config
  } else {
    unlink(installPath,recursive=TRUE)
    javaCmd <- if ( download ) installJava(installPath) else {
      cmd <- findExecutable("java",verbose)
      if ( ! is.null(cmd) ) cmd
      else {
        consent <- TRUE
        installJava(installPath)
      }
    }
    javaConf <- javaInfo(javaCmd)
    scalaCmd <- if ( download ) installScala(javaConf,installPath) else {
      cmd <- findExecutable("scala",verbose)
      if ( ! is.null(cmd) ) cmd
      else {
        consent <- TRUE
        installScala(javaConf,installPath)
      }
    }
    isOS64bit <- if ( identical(.Platform$OS.type,"windows") ) {
      out <- system2("wmic",c("/locale:ms_409","OS","get","osarchitecture","/VALUE"),stdout=TRUE)
      any("OSArchitecture=64-bit"==trimws(out))
    } else identical(system2("uname","-m",stdout=TRUE),"x86_64")
    config <- c(scalaInfo(scalaCmd),javaConf,osArchitecture=if ( isOS64bit ) 64 else 32)
    writeConfig <- consent || offerInstall(paste0("File '",configPath,"' is not found."))
    if ( writeConfig ) {
      dir.create(installPath,showWarnings=FALSE,recursive=TRUE)
      outFile <- file(configPath,open="w")
      dump("config",file=outFile)
      close(outFile)
      if ( verbose ) cat(paste0("Wrote configuration file: ",configPath,"\n\n"))
    } else if ( verbose ) cat("\n")
    config
  }
}

offerInstall <- function(msg) {
  if ( interactive() ) {
    while ( TRUE ) {
      cat("\n")
      response <- toupper(trimws(readline(prompt=paste0(msg,"  Would you like to install it now? [Y/n] "))))
      if ( response == "N" ) return(FALSE)
      if ( response %in% c("Y","") ) return(TRUE)
    }
  } else FALSE
}

findExecutable <- function(mode,verbose=TRUE) {  ## Mimic how the 'scala' script finds Java.
  titleCaps <- paste0(toupper(substring(mode,1,1)),substring(mode,2))
  allCaps <- toupper(mode)
  if ( verbose ) cat(paste0("\nSearching the system for ",titleCaps,".\n"))
  label <- paste0(allCaps,"CMD environment variable")
  candidate <- Sys.getenv(paste0(allCaps,"CMD"))
  if ( candidate != "" && file.exists(candidate) ) {
    if ( verbose ) cat(paste0("  Success with ",label,".\n"))
    return(normalizePath(candidate))
  } else {
    if ( verbose ) cat(paste0("  Failure with ",label,".\n"))
  }
  label <- paste0(allCaps,"_HOME environment variable")
  home <- Sys.getenv(paste0(allCaps,"_HOME"))
  if ( home != "" ) {
    candidate <- file.path(home,"bin",mode) 
    if ( file.exists(candidate) ) {
      if ( verbose ) cat(paste0("  Success with ",label,".\n"))
      return(normalizePath(candidate))
    } else {
      if ( verbose ) cat(paste0("  Failure with ",label,".\n"))
    }
  } else {
    if ( verbose ) cat(paste0("  Failure with ",label,".\n"))
  }
  label <- "PATH environment variable"
  candidate <- Sys.which(mode)[[mode]]
  if ( file.exists(candidate) ) {
    if ( verbose ) cat(paste0("  Success with ",label,".\n"))
    return(normalizePath(candidate))
  } else {
    if ( verbose ) cat(paste0("  Failure with ",label,".\n"))
  }
  NULL
}

installJava <- function(installPath, verbose=TRUE) {
  dir.create(installPath,showWarnings=FALSE,recursive=TRUE)
  sapply(list.files(installPath,"jdk-.*"), function(x) unlink(x,recursive=TRUE))  # Delete older versions
  if ( verbose ) cat("Downloading Java.\n")
  url <- sprintf("https://download.java.net/java/GA/jdk10/10.0.2/19aef61b38124481863b1413dce1855f/13/openjdk-10.0.2_%s-x64_bin.tar.gz",osType())
  destfile <- file.path(installPath,basename(url))
  result <- utils::download.file(url,destfile)
  if ( result != 0 ) {
    unlink(destfile)
    stop("Failed to download installation.")
  }
  result <- utils::untar(destfile,exdir=installPath,tar="internal")    # Use internal to avoid problems on a Mac.
  unlink(destfile)
  if ( result == 0 ) {
    if ( verbose ) cat("Successfully installed Java in ",file.path(installPath,sprintf("scala-%s",version)),"\n\n",sep="")
    javaHome <-  list.files(installPath,"jdk-.*")
    javaName <- if ( osType() == "windows" ) "java.exe" else "java"
    file.path(javaHome,"bin",javaName)
  } else {
    stop("Failed to extract installation.")
  }
}

installScala <- function(javaConfig, majorVersion="2.12") {
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
  installPath <- normalizePath(file.path("~",".rscala"), mustWork=FALSE)
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
    scalaName <- if ( .Platform$OS.type == "windows" ) "scala.bat" else "scala"
    file.path(installPath,sprintf("scala-%s",version),"bin",scalaName)
  } else {
    cat("Failed to extract installation.\n")
    invisible(NULL)
  }
}

javaInfo <- function(javaCmd) {
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
  list(javaCmd=javaCmd, javaMajorVersion=versionNumber, javaArchitecture=bit)
}

scalaInfo <- function(scalaCmd) {
  fullVersion <- system2(scalaCmd,c("-e",shQuote("print(util.Properties.versionNumberString)")),stdout=TRUE)
  majorVersion <- gsub("(^[23]\\.[0-9]+)\\..*","\\1",fullVersion)
  if ( ! ( majorVersion %in% c("2.11","2.12","2.13") ) ) {
    stop(paste0("Version number ",majorVersion," is not supported."))
  } else {
    list(scalaCmd=scalaCmd, scalaMajorVersion=majorVersion, scalaFullVersion=fullVersion)
  }
}

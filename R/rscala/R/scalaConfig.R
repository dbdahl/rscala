#' Configure Scala and Java
#'
#' This function installs Scala in the user's \code{~/.rscala} directory.
#'
#' @param verbose Should details of the search for Scala and Java be provided?
#' @param reconfig Should the '~/.rscala/config.R' script be rewritten based on a new search for Scala and Java?
#' @param downloal.java Should Java be downloaded and installed in '~/.rscala/java'?
#' @param downloal.scala Should Scala be downloaded and installed in '~/.rscala/scala'?
#'
#' @return Returns a list of details of the Scala and Java binaries.
#' @export
#' @examples \dontrun{
#' 
#' scalaConfig()
#' }
scalaConfig <- function(verbose=TRUE, reconfig=FALSE, download.java=FALSE, download.scala=FALSE) {
  installPath <- file.path("~",".rscala")
  configPath  <- file.path(installPath,"config.R")
  consent <- reconfig || download.java || download.scala
  if ( !reconfig && file.exists(configPath) && !download.java && !download.scala ) {
    if ( verbose ) cat(paste0("Read existing configuration file: ",configPath,"\n\n"))
    source(configPath,chdir=TRUE)
    config
  } else {
    if ( download.java ) installJava(installPath,verbose)
    javaConf <- findExecutable("java",installPath,javaSpecifics,verbose)
    if ( is.null(javaConf) ) {
      consent2 <- offerInstall(paste0("Java is not found.")) 
      if ( consent2 ) {
        installJava(installPath,verbose)
        javaConf <- findExecutable("java",installPath,javaSpecifics,verbose)
        if ( is.null(javaConf) ) stop("Java is not found and cannot be successfully installed.")
      } else stop("Java is not found.  Please run 'scalaConfig(reconfig=TRUE, download.java=TRUE)'.")
      consent <- consent || consent2
    }
    if ( download.scala ) installScala(installPath,javaConf,verbose)
    scalaSpecifics2 <- function(x) scalaSpecifics(x,javaConf)
    scalaConf <- findExecutable("scala",installPath,scalaSpecifics2,verbose)
    if ( is.null(scalaConf) ) {
      consent2 <- offerInstall(paste0("Scala is not found.")) 
      if ( consent2 ) {
        installScala(installPath,javaConf,verbose)
        scalaConf <- findExecutable("scala",installPath,scalaSpecifics2,verbose)
        if ( is.null(scalaConf) ) stop("Scala is not found and cannot be successfully installed.")
      } else stop("Scala is not found.  Please run 'scalaConfig(reconfig=TRUE, download.scala=TRUE)'.")
      consent <- consent || consent2
    }
    isOS64bit <- if ( identical(.Platform$OS.type,"windows") ) {
      out <- system2("wmic",c("/locale:ms_409","OS","get","osarchitecture","/VALUE"),stdout=TRUE)
      any("OSArchitecture=64-bit"==trimws(out))
    } else identical(system2("uname","-m",stdout=TRUE),"x86_64")
    osArchitecture <- if ( isOS64bit ) 64 else 32
    if ( ( javaConf$javaArchitecture == 32 ) && ( osArchitecture == 64 ) ) {
      warning("32-bit Java is paired with a 64-bit operating system.  Consider installing 64-bit Java to access more memory.")
    }
    config <- c(scalaConf,javaConf,osArchitecture=osArchitecture)
    writeConfig <- consent || offerInstall(paste0("File '",configPath,"' is not found."))
    if ( writeConfig ) {
      dir.create(installPath,showWarnings=FALSE,recursive=TRUE)
      outFile <- file(configPath,open="w")
      dump("config",file=outFile)
      close(outFile)
      if ( verbose ) cat(paste0("\nWrote configuration file: ",configPath,"\n\n"))
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

findExecutable <- function(mode,installPath,mapper,verbose=TRUE) {  ## Mimic how the 'scala' script finds Java.
  tryCandidate <- function(candidate) {
    if ( candidate != "" && file.exists(candidate) ) {
      if ( verbose ) cat(paste0("  Success with ",label,".\n"))
      result <- mapper(candidate)
      if ( is.character(result) ) {
        cat(paste0("  ... but ",result,"\n"))
        NULL
      } else result
    } else {
      if ( verbose ) cat(paste0("  Failure with ",label,".\n"))
      NULL
    }
  }
  titleCaps <- paste0(toupper(substring(mode,1,1)),substring(mode,2))
  allCaps <- toupper(mode)
  if ( verbose ) cat(paste0("\nSearching the system for ",titleCaps,".\n"))
  ###
  label <- "user directory"
  candidate <- if ( mode == "java" ) {
    file.path(installPath,"java","bin",paste0("java",if ( .Platform$OS.type == "windows" ) ".exe" else ""))
  } else if ( mode == "scala" ) {
    file.path("~",".rscala","scala","bin",paste0("scala",if ( .Platform$OS.type == "windows" ) ".bat" else ""))
  } else stop("Unsupported mode.")
  conf <- tryCandidate(candidate)
  if ( ! is.null(conf) ) return(conf)
  ###
  label <- paste0(allCaps,"CMD environment variable")
  conf <- tryCandidate(Sys.getenv(paste0(allCaps,"CMD")))
  if ( ! is.null(conf) ) return(conf)
  ###
  label <- paste0(allCaps,"_HOME environment variable")
  home <- Sys.getenv(paste0(allCaps,"_HOME"))
  conf <- tryCandidate(if ( home != "" ) file.path(home,"bin",mode) else "")
  if ( ! is.null(conf) ) return(conf)
  ###
  label <- "PATH environment variable"
  conf <- tryCandidate(Sys.which(mode)[[mode]])
  if ( ! is.null(conf) ) return(conf)
  ###
  NULL
}

installJava <- function(installPath, verbose) {
  if ( verbose ) cat("\nDownloading Java...\n")
  dir.create(installPath,showWarnings=FALSE,recursive=TRUE)
  unlink(file.path(installPath,"java"),recursive=TRUE)  # Delete older version
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
    destdir <- file.path(installPath,"java")
    javaHome <- list.files(installPath,"jdk-.*",full.names=TRUE)
    file.rename(javaHome,destdir)
    if ( verbose ) cat("Successfully installed Java at ",destdir,"\n",sep="")
  } else {
    stop("Failed to extract installation.")
  }
}

installScala <- function(installPath, javaConf, verbose) {
  if ( verbose ) cat("\nDownloading Scala...\n")
  SCALA_213_VERSION <- "2.13.0-M4"
  SCALA_212_VERSION <- "2.12.6"
  SCALA_211_VERSION <- "2.11.12"
  dir.create(installPath,showWarnings=FALSE,recursive=TRUE)
  unlink(file.path(installPath,"scala"),recursive=TRUE)  # Delete older version
  if ( javaConf$javaMajorVersion <= 7 ) majorVersion <- "2.11"
  majorVersion <- "2.12"
  if ( majorVersion == "2.13" ) version <- SCALA_213_VERSION
  else if ( majorVersion == "2.12" ) version <- SCALA_212_VERSION
  else if ( majorVersion == "2.11" ) version <- SCALA_211_VERSION
  else stop("Unsupported major version.")
  url <- sprintf("https://downloads.lightbend.com/scala/%s/scala-%s.tgz",version,version)
  dir.create(installPath,showWarnings=FALSE,recursive=TRUE)
  destfile <- file.path(installPath,basename(url))
  result <- utils::download.file(url,destfile)
  if ( result != 0 ) {
    unlink(destfile)
    stop("Failed to download installation.")   
  }
  result <- utils::untar(destfile,exdir=installPath,tar="internal")    # Use internal to avoid problems on a Mac.
  unlink(destfile)
  if ( result == 0 ) {
    destdir <- file.path(installPath,"scala")
    scalaHome <- file.path(installPath,sprintf("scala-%s",version))
    file.rename(scalaHome,destdir)
    if ( verbose ) cat("Successfully installed Scala at ",destdir,"\n",sep="")   
  } else {
    stop("Failed to extract installation.")
  }
}

javaSpecifics <- function(javaCmd) {
  response <- system2(normalizePath(javaCmd,mustWork=TRUE),"-version",stdout=TRUE,stderr=TRUE)
  # Get version information
  versionRegexp <- '(java|openjdk) version "([^"]*)".*'
  line <- response[grepl(versionRegexp,response)]
  if ( length(line) != 1 ) return(paste0("cannot determine Java version.  Output is:\n",paste(response,collapse="\n")))
  versionString <- gsub(versionRegexp,"\\2",line)
  versionParts <- strsplit(versionString,"\\.")[[1]]
  versionNumber <- if ( versionParts[1] == '1' ) {
    as.numeric(versionParts[2])
  } else {
    as.numeric(versionParts[1])
  }
  if ( ! ( versionNumber %in% c(7,8,9,10,11) ) ) return(paste0("unsupported Java version: ",versionString))
  # Determine if 32 or 64 bit
  bit <- if ( any(grepl('^(Java HotSpot|OpenJDK).* 64-Bit (Server|Client) VM.*$',response)) ||
              any(grepl('^IBM .* amd64-64 .*$',response)) ) 64 else 32
  list(javaCmd=javaCmd, javaMajorVersion=versionNumber, javaArchitecture=bit)
}

scalaSpecifics <- function(scalaCmd,javaConf) {
  fullVersion <- system2(normalizePath(scalaCmd,mustWork=TRUE),c("-e",shQuote("print(util.Properties.versionNumberString)")),env=paste0("JAVACMD=",javaConf$javaCmd),stdout=TRUE)
  majorVersion <- gsub("(^[23]\\.[0-9]+)\\..*","\\1",fullVersion)
  if ( ! ( majorVersion %in% c("2.11","2.12","2.13") ) ) {
    return(paste0("unsupport Scala version: ",majorVersion))
  } else {
    list(scalaCmd=scalaCmd, scalaMajorVersion=majorVersion, scalaFullVersion=fullVersion)
  }
}

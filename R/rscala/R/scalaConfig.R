#' Configure Scala and Java
#'
#' This function installs Scala and/or Java in the user's \code{~/.rscala}
#' directory.
#'
#' @param verbose Should details of the search for Scala and Java be provided?
#'   Or, if an rscala bridge is provided instead of a logical, the function
#'   returns a list of details associated with the supplied bridge.
#' @param reconfig If \code{TRUE}, the script \code{~/.rscala/config.R} is
#'   rewritten based on a new search for Scala and Java.  If \code{FALSE}, the
#'   previous configuration is sourced from the script
#'   \code{~/.rscala/config.R}.  If \code{"live"}, a new search for Scala and
#'   Java is performed, but the results do not overwrite the previous
#'   configuration script.
#' @param download.java Should Java be downloaded and installed in
#'   '~/.rscala/java'?
#' @param download.scala Should Scala be downloaded and installed in
#'   '~/.rscala/scala'?
#' @param download.sbt Should SBT be downloaded and installed in
#'   '~/.rscala/sbt'?
#' @param require.sbt Should SBT be required, downloading and installing it in
#'   '~/.rscala/sbt' if necessary?
#'
#' @return Returns a list of details of the Scala and Java binaries.
#' @export
#' @examples \dontrun{
#'
#' scalaConfig()
#' }
scalaConfig <- function(verbose=TRUE, reconfig=FALSE, download.java=FALSE, download.scala=FALSE, download.sbt=FALSE, require.sbt=FALSE) {
  if ( inherits(verbose,"rscalaBridge") ) return(attr(verbose,"details")$config)
  offerInstall <- function(msg) {
    if ( !identical(reconfig,"live") && interactive() ) {
      while ( TRUE ) {
        response <- toupper(trimws(readline(prompt=paste0(msg,"  Would you like to install it now? [Y/n] "))))
        if ( response == "N" ) return(FALSE)
        if ( response %in% c("Y","") ) return(TRUE)
      }
    } else FALSE
  }
  installPath <- file.path("~",".rscala")
  configPath  <- file.path(installPath,"config.R")
  consent <- identical(reconfig,TRUE) || download.java || download.scala || download.sbt
  if ( ! reconfig && file.exists(configPath) && !download.java && !download.scala && !download.sbt ) {
    if ( verbose ) cat(paste0("\nRead existing configuration file: ",configPath,"\n\n"))
    source(configPath,chdir=TRUE,local=TRUE)
    if ( is.null(config$format) || ( config$format < 1L ) || ( ! all(file.exists(c(config$javaCmd,config$scalaCmd))) ) || ( is.null(config$sbtCmd) && require.sbt ) || ( ! is.null(config$sbtCmd) && ! file.exists(config$sbtCmd) ) ) {
      if ( verbose ) cat("The 'config.R' is out-of-date.  Reconfiguring...\n")
      unlink(configPath)
      scalaConfig(verbose)
    } else config
  } else {
    if ( download.java ) installJava(installPath,verbose)
    javaConf <- findExecutable("java","Java",installPath,javaSpecifics,verbose)
    if ( is.null(javaConf) ) {
      if ( verbose ) cat("\n")
      consent2 <- offerInstall(paste0("Java is not found.")) 
      stopMsg <- "\n\n<<<<<<<<<<\n<<<<<<<<<<\n<<<<<<<<<<\n\nJava is not found!  Please run 'rscala::scalaConfig(download.java=TRUE)'\n\n>>>>>>>>>>\n>>>>>>>>>>\n>>>>>>>>>>\n"
      if ( consent2 ) {
        installJava(installPath,verbose)
        javaConf <- findExecutable("java","Java",installPath,javaSpecifics,verbose)
        if ( is.null(javaConf) ) stop(stopMsg)
      } else stop(stopMsg)
      consent <- consent || consent2
    }
    if ( download.scala ) installScala(installPath,javaConf,verbose)
    scalaSpecifics2 <- function(x,y) scalaSpecifics(x,javaConf,y)
    scalaConf <- findExecutable("scala","Scala",installPath,scalaSpecifics2,verbose)
    if ( is.null(scalaConf) ) {
      if ( verbose ) cat("\n")
      consent2 <- offerInstall(paste0("Scala is not found.")) 
      stopMsg <- "\n\n<<<<<<<<<<\n<<<<<<<<<<\n<<<<<<<<<<\n\nScala is not found!  Please run 'rscala::scalaConfig(download.scala=TRUE)'\n\n>>>>>>>>>>\n>>>>>>>>>>\n>>>>>>>>>>\n"
      if ( consent2 ) {
        installScala(installPath,javaConf,verbose)
        scalaConf <- findExecutable("scala","Scala",installPath,scalaSpecifics2,verbose)
        if ( is.null(scalaConf) ) stop(stopMsg)
      } else stop(stopMsg)
      consent <- consent || consent2
    }
    osArchitecture <- if ( javaConf$javaArchitecture == 32 ) {
      isOS64bit <- if ( identical(.Platform$OS.type,"windows") ) {
        out <- system2("wmic",c("/locale:ms_409","OS","get","osarchitecture","/VALUE"),stdout=TRUE)
        any("OSArchitecture=64-bit"==trimws(out))
      } else identical(system2("uname","-m",stdout=TRUE),"x86_64")
      osArchitecture <- if ( isOS64bit ) 64 else 32
      if ( osArchitecture == 64 ) {
        warning("32-bit Java is paired with a 64-bit operating system.  To access more memory, please run 'scalaConfig(download.java=TRUE)'.")
      }
      osArchitecture
    } else javaConf$javaArchitecture
    if ( download.sbt ) installSBT(installPath,javaConf,verbose)
    sbtSpecifics <- function(x,y) list(sbtCmd=x)
    sbtConf <- findExecutable("sbt","SBT",installPath,sbtSpecifics,verbose)
    if ( is.null(sbtConf) && require.sbt ) {
      installSBT(installPath,javaConf,verbose)
      sbtConf <- findExecutable("sbt","SBT",installPath,sbtSpecifics,verbose)
      if ( is.null(sbtConf) ) {
        stopMsg <- "\n\n<<<<<<<<<<\n<<<<<<<<<<\n<<<<<<<<<<\n\nSBT is not found!  Please run 'rscala::scalaConfig(require.sbt=TRUE)'\n\n>>>>>>>>>>\n>>>>>>>>>>\n>>>>>>>>>>\n"
        stop(stopMsg)
      }
    }
    config <- c(scalaConf,javaConf,sbtConf,osArchitecture=osArchitecture,format=1L)
    if ( !consent && verbose ) cat("\n")
    writeConfig <- consent || offerInstall(paste0("File '",configPath,"' is not found or is out-of-date."))
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

findExecutable <- function(mode,label,installPath,mapper,verbose=TRUE) {  ## Mimic how the 'scala' script finds Java.
  tryCandidate <- function(candidate) {
    if ( candidate != "" && file.exists(candidate) ) {
      if ( verbose ) cat(paste0("  Success with ",label,".\n"))
      result <- mapper(candidate,verbose)
      if ( is.character(result) ) {
        cat(paste0("  ... but ",result,"\n"))
        NULL
      } else result
    } else {
      if ( verbose ) cat(paste0("  Failure with ",label,".\n"))
      NULL
    }
  }
  allCaps <- toupper(mode)
  if ( verbose ) cat(paste0("\nSearching the system for ",label,".\n"))
  ###
  label <- "user directory"
  candidate <- if ( mode == "java" ) {
    file.path(installPath,"java","bin",paste0("java",if ( .Platform$OS.type == "windows" ) ".exe" else ""))
  } else if ( mode == "scala" ) {
    file.path(installPath,"scala","bin",paste0("scala",if ( .Platform$OS.type == "windows" ) ".bat" else ""))
  } else if ( mode == "sbt" ) {
    file.path(installPath,"sbt","bin",paste0("sbt",if ( .Platform$OS.type == "windows" ) ".bat" else ""))   
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

installJava <- function(installPath, verbose, urlCounter=1) {
  if ( verbose ) cat("\nDownloading Java.\n")
  dir.create(installPath,showWarnings=FALSE,recursive=TRUE)
  os <- osType()
  url <- if ( urlCounter == 1 ) sprintf("https://api.adoptopenjdk.net/v2/binary/releases/openjdk8?openjdk_impl=hotspot&os=%s&arch=x64&release=latest&type=jdk",os)
  else if ( urlCounter == 2 ) {
    if ( os == "linux" ) "https://byu.box.com/shared/static/0t29ifd8l023fd91g1vh8p70hx5v7lg0.gz"
    else if ( os == "mac" ) "https://byu.box.com/shared/static/s3l05yhi8ez7tuoe7732hsr53imxdc5u.gz"
    else if ( os == "windows" ) "https://byu.box.com/shared/static/wfhgu72szx3y2m05sdreje0dlfzhnag2.zip"
    else stop("Unsupported operating system.")
  }
  installPathTemp <- file.path(installPath,"tmp")
  unlink(installPathTemp,recursive=TRUE,force=TRUE)
  dir.create(installPathTemp,showWarnings=FALSE,recursive=TRUE)
  destfile <- tempfile("jdk",tmpdir=installPathTemp)
  result <- tryCatch(utils::download.file(url, destfile, mode="wb"), error=function(e) 1, warning=function(e) 1)
  if ( result != 0 ) {
    unlink(destfile,force=TRUE)
    msg <- "Failed to download installation."
    if ( urlCounter < 2 ) {
      if ( verbose ) cat(paste0(msg,"\n"))
      installJava(installPath,verbose,urlCounter+1)
      return(NULL)
    } else stop(msg)
  }
  if ( verbose ) cat("\nExtracting Java.\n")
  func <- if ( os == "windows" ) function(x) utils::unzip(x,exdir=installPathTemp,unzip="internal")
  else function(x) utils::untar(x,exdir=installPathTemp,tar="internal")
  func(destfile)
  unlink(destfile,force=TRUE)
  destdir <- file.path(installPath,"java")
  unlink(destdir,recursive=TRUE,force=TRUE)  # Delete older version
  javaHome <- list.files(installPathTemp,full.names=TRUE,recursive=FALSE)
  javaHome <- javaHome[dir.exists(javaHome)]
  if ( length(javaHome) != 1 ) stop(paste0("Problem extracting Java.  Clean delete the directory '",path.expand(installPath),"' and try again."))
  file.rename(javaHome,destdir)
  unlink(installPathTemp,recursive=TRUE,force=TRUE)
  if ( verbose ) cat("Successfully installed Java at ",destdir,"\n",sep="")
  NULL
}

installScala <- function(installPath, javaConf, verbose, useFallBack=FALSE) {
  if ( verbose ) cat("\nDownloading Scala.\n")
  SCALA_213_VERSION <- "2.13.0-M5"
  SCALA_212_VERSION <- "2.12.6"
  SCALA_211_VERSION <- "2.11.12"
  dir.create(installPath,showWarnings=FALSE,recursive=TRUE)
  unlink(file.path(installPath,"scala"),recursive=TRUE)  # Delete older version
  if ( javaConf$javaMajorVersion <= 7 ) majorVersion <- "2.11" else majorVersion <- "2.12"
  if ( majorVersion == "2.13" ) version <- SCALA_213_VERSION
  else if ( majorVersion == "2.12" ) version <- SCALA_212_VERSION
  else if ( majorVersion == "2.11" ) version <- SCALA_211_VERSION
  else stop("Unsupported major version.")
  url <- sprintf("https://downloads.lightbend.com/scala/%s/scala-%s.tgz",version,version)
  url2 <- if ( version == SCALA_213_VERSION ) "https://byu.box.com/shared/static/tctag0lirhhf9z3n0yq2gmdpo2poqlsk.tgz"
  else if ( version == SCALA_212_VERSION ) "https://byu.box.com/shared/static/ix0lkraln4sf17191r30jooo3qb3bz19.tgz"
  else if ( version == SCALA_211_VERSION ) "https://byu.box.com/shared/static/kh0pew26qbai9mznyp6jvq3ve8g8gwuh.tgz"
  else stop("Unsupported version.")
  dir.create(installPath,showWarnings=FALSE,recursive=TRUE)
  destfile <- file.path(installPath,basename(url))
  result <- tryCatch( utils::download.file(if ( !useFallBack ) url else url2, destfile), error=function(e) 1, warning=function(e) 1)
  if ( result != 0 ) {
    unlink(destfile)
    msg <- "Failed to download installation."
    if ( ! useFallBack ) {
      if ( verbose ) cat(paste0(msg,"\n"))
      installScala(installPath,javaConf,verbose,TRUE)
      return(NULL)
    } else stop(msg)
  }
  if ( verbose ) cat("\nExtracting Scala.\n")
  result <- utils::untar(destfile,exdir=installPath,tar="internal")    # Use internal to avoid problems on a Mac.
  unlink(destfile)
  if ( result == 0 ) {
    destdir <- file.path(installPath,"scala")
    scalaHome <- file.path(installPath,sprintf("scala-%s",version))
    scalaHome <- scalaHome[dir.exists(scalaHome)]
    if ( length(scalaHome) != 1 ) stop(paste0("Problem extracting Scala.  Clean delete the directory '",path.expand(installPath),"' and try again."))
    file.rename(scalaHome,destdir)
    if ( verbose ) cat("Successfully installed Scala at ",destdir,"\n",sep="")   
  } else {
    stop("Failed to extract installation.")
  }
  NULL
}

installSBT <- function(installPath, javaConf, verbose, useFallBack=FALSE) {
  if ( verbose ) cat("\nDownloading SBT.\n")
  version <- "1.2.3"
  dir.create(installPath,showWarnings=FALSE,recursive=TRUE)
  unlink(file.path(installPath,"sbt"),recursive=TRUE)  # Delete older version
  # if ( javaConf$javaMajorVersion != 8 ) stop("Java 8 is recommended for SBT.")
  url <- sprintf("https://piccolo.link/sbt-%s.tgz",version)
  url2 <- if ( version == "1.2.3" ) "https://byu.box.com/shared/static/gfafbp0rfhcf1i50ghrvfj9gf9l55kor.tgz"
  else stop("Unsupported version.")
  dir.create(installPath,showWarnings=FALSE,recursive=TRUE)
  destfile <- file.path(installPath,basename(url))
  result <- tryCatch( utils::download.file(if ( !useFallBack ) url else url2, destfile), error=function(e) 1, warning=function(e) 1)
  if ( result != 0 ) {
    unlink(destfile)
    msg <- "Failed to download installation."
    if ( ! useFallBack ) {
      if ( verbose ) cat(paste0(msg,"\n"))
      installSBT(installPath,javaConf,verbose,TRUE)
      return(NULL)
    } else stop(msg)
  }
  if ( verbose ) cat("\nExtracting SBT.\n")
  result <- utils::untar(destfile,exdir=installPath,tar="internal")    # Use internal to avoid problems on a Mac.
  unlink(destfile)
  if ( result == 0 ) {
    destdir <- file.path(installPath,"sbt")
    if ( file.exists(destdir) ) if ( verbose ) cat("Successfully installed SBT at ",destdir,"\n",sep="")   
    else stop("Failed to extract installation.")
  } else {
    stop("Failed to extract installation.")
  }
  NULL
}

javaSpecifics <- function(javaCmd,verbose) {
  if ( verbose ) cat("\nQuerying Java specifics.\n")
  response <- system2(path.expand(javaCmd),"-version",stdout=TRUE,stderr=TRUE)
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
  javaHome <- dirname(dirname(normalizePath(javaCmd)))
  list(javaCmd=javaCmd, javaHome=javaHome, javaMajorVersion=versionNumber, javaArchitecture=bit)
}

setJavaEnv <- function(javaConf) {
  oldJAVACMD <- Sys.getenv("JAVACMD")
  oldJAVAHOME <- Sys.getenv("JAVA_HOME")
  eJAVACMD <- if ( javaConf$javaCmd != "" ) normalizePath(javaConf$javaCmd) else ""
  eJAVAHOME <- if ( javaConf$javaHome != "" ) normalizePath(javaConf$javaHome) else ""
  Sys.setenv(JAVACMD=eJAVACMD)
  Sys.setenv(JAVA_HOME=eJAVAHOME)
  list(javaCmd=oldJAVACMD,javaHome=oldJAVAHOME)
}

scalaSpecifics <- function(scalaCmd,javaConf,verbose) {
  if ( verbose ) cat("\nQuerying Scala specifics.\n")
  oldJavaEnv <- setJavaEnv(javaConf)
  fullVersion <- system2(path.expand(scalaCmd),c("-nc","-e",shQuote("print(util.Properties.versionNumberString)")),stdout=TRUE)
  setJavaEnv(oldJavaEnv)
  majorVersion <- gsub("(^[23]\\.[0-9]+)\\..*","\\1",fullVersion)
  if ( ! ( majorVersion %in% c("2.11","2.12","2.13") ) ) {
    return(paste0("unsupport Scala version: ",majorVersion))
  } else {
    list(scalaCmd=scalaCmd, scalaMajorVersion=majorVersion, scalaFullVersion=fullVersion)
  }
}

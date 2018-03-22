SCALA_212_VERSION <- "2.12.5"
SCALA_211_VERSION <- "2.11.12"

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

latestVersion <- function(major.release) {
  max <- major.release[1]
  for ( i in major.release[-1] ) {
    if ( compareVersion(max,i) < 0 ) max <- i
  }
  max
}

javaVersion <- function(javaCmd) {
  response <- system2(javaCmd,"-version",stdout=TRUE,stderr=TRUE)
  regexp <- '(java|openjdk) version "(.*)"'
  line <- response[grepl(regexp,response)]
  if ( length(line) != 1 ) stop(paste0("Cannot determine Java version.\n",paste(response,collapse="\n")))
  versionString <- gsub(regexp,"\\2",line)
  versionParts <- strsplit(versionString,"\\.")[[1]]
  if ( versionParts[1] != '1' ) stop(paste0("Unexpected Java version.\n",paste(response,collapse="\n")))
  versionNumber <- as.numeric(versionParts[2])
  if ( ! ( versionNumber %in% c(6,7,8) ) ) stop(paste0("Unsupported Java version.\n",paste(response,collapse="\n")))
  versionNumber
}

scalaInstall <- function(major.release=c("2.11","2.12"), global=FALSE) {
  if ( length(major.release) > 1 ) return(scalaInstall(latestVersion(major.release), global=global))
  if ( length(major.release) == 0 ) stop("At least one major release must be supplied.")
  javaVersion <- javaVersion(findJava())
  if ( ( javaVersion <= 7 ) && ( compareVersion("2.11",major.release) < 0 ) ) {
    cat("It appears you are using an old version of Java, so Scala 2.11 will be installed.\n")
    return(scalaInstall("2.11", global=global))
  }
  if ( major.release == "2.12" ) version <- SCALA_212_VERSION
  else if ( major.release == "2.11" ) version <- SCALA_211_VERSION
  else stop("Unsupported major release.")
  installPath <- if ( global ) system.file(package="rscala") else file.path("~",".rscala")
  installPath <- normalizePath(installPath, mustWork=FALSE)
  url <- sprintf("https://downloads.lightbend.com/scala/%s/scala-%s.tgz",version,version)
  dir.create(installPath,showWarnings=FALSE,recursive=TRUE)
  destfile <- file.path(installPath,basename(url))
  result <- download.file(url,destfile)
  if ( result != 0 ) return(invisible(result))
  result <- untar(destfile,exdir=installPath,tar="internal")    # Use internal to avoid problems on a Mac.
  unlink(destfile)
  if ( result == 0 ) cat("Successfully installed Scala in ",file.path(installPath,sprintf("scala-%s",version)),"\n\n",sep="")
  invisible(result)
}


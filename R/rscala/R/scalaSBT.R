#' Run SBT and Deploy JAR Files
#'
#' This function runs SBT (Scala Build Tool) to package JAR files and then copy
#' them to the appropriate directories of the R package source.
#'
#' Starting from the current working directory and moving up the file system
#' hierarchy as needed, this function searches for the directory containing the
#' file \code{'build.sbt'}, the SBT build file. It temporarily changes the
#' working directory to this directory. Unless the \code{args} argument is
#' overwritten, it then runs \code{sbt +package packageSrc +publishLocal} to
#' package the cross-compiled the Scala code, package the source code, and
#' publish the JAR files locally. Finally, it optionally copies the JAR files to
#' the appropriate directories of the R package source. Specifically, source JAR
#' files go into \code{(PKGHOME)/java} and binary JAR files go into
#' \code{(PKGHOME)/inst/java/scala-(VERSION)}, where \code{(PKGHOME)} is the
#' package home and \code{(VERSION)} is the major Scala version (e.g., 2.12).
#' It is assumed that the package home is a subdirectory of the directory
#' containing the \code{'build.sbt'} file.
#'
#' Note that SBT may give weird errors about not being able to download needed
#' dependences.  The issue is that some OpenJDK builds less than version 10 do
#' not include root certificates.  The solution is to either: i. manually
#' install OpenJDK version 10 or greater, or ii. manually install Oracle's
#' version of Java. Both are capable with the rscala package.
#'
#' @param args A character vector giving the arguments to be passed to the SBT
#'   command.
#' @param copy.to.package Should the jars be to the appropriate directories of
#'   the R package source.
#'
#' @return \code{NULL}
#' @export
scalaSBT <- function(args=c("+package","packageSrc","+publishLocal"), copy.to.package=TRUE) {
  sConfig <- scalaConfig(FALSE,require.sbt=TRUE)
  oldWD <- normalizePath(getwd())
  on.exit(setwd(oldWD))
  while ( TRUE ) {
    if ( file.exists("build.sbt") ) break
    currentWD <- getwd()
    setwd("..")
    if ( currentWD == getwd() ) stop("Cannot find 'build.sbt' file.")
  }
  oldJavaEnv <- setJavaEnv(sConfig)
  status <- system2(path.expand(sConfig$sbtCmd),args)
  setJavaEnv(oldJavaEnv)
  if ( status != 0 ) stop("Non-zero exit status.") 
  if ( copy.to.package ) {
    lines <- readLines("build.sbt")
    nameLine <- lines[grepl("^\\s*name\\s*:=",lines)]
    if ( length(nameLine) != 1 ) stop("Could not find one and only one 'name' line in 'build.sbt'.")
    name <- sub('^[^"]*"(.*)"\\s*$','\\1',nameLine)
    versionLine <- lines[grepl("^\\s*version\\s*:=",lines)]
    if ( length(versionLine) != 1 ) stop("Could not find one and only one 'version' line in 'build.sbt'.")
    version <- sub('^[^"]*"(.*)"\\s*$','\\1',versionLine)
    scalaVersionLine <- lines[grepl("^\\s*scalaVersion\\s*:=",lines)]
    if ( length(scalaVersionLine) != 1 ) stop("Could not find one and only one 'scalaVersion' line in 'build.sbt'.")
    scalaVersion <- scalaMajorVersion(sub('^[^"]*"(.*)"\\s*$','\\1',scalaVersionLine))
    crossLine <-   lines[grepl("^\\s*crossScalaVersions\\s*:=",lines)]
    if ( length(crossLine) != 1 ) stop("Could not find one and only one 'crossScalaVersion' line in 'build.sbt'.")
    scalaVersions <- strsplit(gsub('["),]','',sub('[^"]*','',crossLine)),"\\s+")[[1]]
    scalaVersions <- sapply(scalaVersions, function(x) {
      if ( grepl("^2.1[12]\\.",x) ) substr(x,1,4) else x
    })
    oldJARs <- list.files("target",pattern=".*\\.jar",recursive=TRUE)
    unlink(oldJARs)
    if ( is.null(sConfig$sbtCmd) ) {
      stopMsg <- "\n\n<<<<<<<<<<\n<<<<<<<<<<\n<<<<<<<<<<\n\nSBT is not found!  Please run 'rscala::scalaConfig(require.sbt=TRUE)'\n\n>>>>>>>>>>\n>>>>>>>>>>\n>>>>>>>>>>\n"
      stop(stopMsg)
    }
    newJARs <- list.files("target",pattern=".*\\.jar",recursive=TRUE)
    srcJARs <- newJARs[grepl(".*-sources.jar$",newJARs)]
    binJARs <- setdiff(newJARs,srcJARs)
    pkgHome <- unique(dirname(list.files(".","DESCRIPTION",recursive=TRUE)))   # unique(...) because on Mac OS X, duplicates are possible.
    pkgHome <- pkgHome[!grepl(".*\\.Rcheck",pkgHome)]
    if ( length(pkgHome) > 1 ) {
      if ( sum(normalizePath(pkgHome)==oldWD) == 1 ) pkgHome <- pkgHome[normalizePath(pkgHome)==oldWD]
      else {
        if ( sum(basename(pkgHome)==name) == 1 ) pkgHome <- pkgHome[basename(pkgHome)==name]
        else stop(paste0("Cannot determine package home among: ",paste(pkgHome,collapse="  ")))
      }
    } else if ( length(pkgHome) == 0 ) stop(paste0("Cannot find any candidates for package home."))
    binDir <- file.path(pkgHome,"inst","java")
    oldDirs <- list.files(binDir,"^scala-*")
    unlink(oldDirs)
    for ( v in scalaVersions ) {
      currentJARs <- binJARs[grepl(sprintf("^scala-%s",v),binJARs)]
      currentJARs <- currentJARs[grepl(sprintf(".*_%s-%s.jar$",v,version),basename(currentJARs))]
      if ( length(currentJARs) > 0 ) {
        destDir <- file.path(binDir,sprintf("scala-%s",v))
        dir.create(destDir,FALSE,TRUE)
        file.copy(file.path("target",currentJARs),destDir,TRUE)
      }
    }
    srcDir <- file.path(pkgHome,"java")
    dir.create(srcDir,FALSE,TRUE)
    oldJARs <- list.files(srcDir,pattern=".*\\.jar",recursive=TRUE)
    unlink(file.path(srcDir,oldJARs))
    currentJARs <- srcJARs[grepl(sprintf("^scala-%s",scalaVersion),srcJARs)]
    currentJARs <- currentJARs[grepl(sprintf(".*_%s-%s-sources.jar$",scalaVersion,version),basename(currentJARs))]
    if ( length(currentJARs) > 0 ) file.copy(file.path("target",currentJARs),srcDir,TRUE)
  }
  invisible(NULL)
}

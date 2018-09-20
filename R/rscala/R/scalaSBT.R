#' Run SBT and Deploy JAR Files
#'
#' This function runs SBT (Scala Build Tool) to package JAR files and then copy
#' them to the appropriate directories of the R package source.
#'
#' Starting from the current working directory and moving up the file system
#' hierarchy as needed, this function searches for the directory containing the
#' file \code{'build.sbt'}, the SBT build file. It temporarily changes the
#' working directory to this directory. Unless the \code{args} argument is
#' overwritten, it then runs \code{sbt +package packageSrc} to package the
#' cross-compiled the Scala code and package the source code. Finally, it copies
#' these JAR files to the appropriate directories of the R package source.
#' Specifically, source JAR files go into \code{(PKGHOME)/java} and binary JAR
#' files go into \code{(PKGHOME)/inst/java/scala-(VERSION)}, where
#' \code{(PKGHOME)} is the package home and \code{(VERSION)} is the major Scala
#' version (e.g., 2.12).  It is assumed that the package home is a subdirectory
#' of the directory containing the \code{'build.sbt'} file.
#'
#' Note that SBT may give weird errors about not being able to download needed
#' dependences.  The issue is that some OpenJDK builds less than version 10 ---
#' like the one provided by \code{scalaConfig(download.java=TRUE)} --- do not
#' include a root certificates.  The solution is to either: i. manually install
#' OpenJDK version 10 or greater, or ii. manually install Oracle's version of
#' Java 8. Both are capable with the rscala package.
#'
#' @param args A character vector giving the arguments to be passed to the SBT
#'   command.
#'
#' @return \code{NULL}
#' @export
scalaSBT <- function(args=c("+package","packageSrc")) {
  sConfig <- scalaConfig(FALSE)
  oldWD <- getwd()
  on.exit(setwd(oldWD))
  while ( TRUE ) {
    if ( file.exists("build.sbt") ) break
    currentWD <- getwd()
    setwd("..")
    if ( currentWD == getwd() ) stop("Cannot find 'build.sbt' file.")
  }
  lines <- readLines("build.sbt")
  lines <- lines[grepl("^\\s*crossScalaVersions\\s*:=",lines)]
  if ( length(lines) != 1 ) stop("Could not find one and only one 'crossScalaVersion' line in 'build.sbt'.")
  scalaVersions <- strsplit(gsub('["),]','',sub('[^"]*','',lines)),"\\s+")[[1]]
  scalaVersions <- sapply(scalaVersions, function(x) {
    if ( grepl("^2.1[12]\\.",x) ) substr(x,1,4) else x
  })
  oldJARs <- list.files("target",pattern=".*\\.jar",recursive=TRUE)
  unlink(oldJARs)
  if ( is.null(sConfig$sbtCmd) ) {
    stopMsg <- "\n\n<<<<<<<<<<\n<<<<<<<<<<\n<<<<<<<<<<\n\nSBT is not found!  Please run 'rscala::scalaConfig(require.sbt=TRUE)'\n\n>>>>>>>>>>\n>>>>>>>>>>\n>>>>>>>>>>\n"
    stop(stopMsg)
  }
  oldJavaEnv <- setJavaEnv(sConfig)
  status <- system2(path.expand(sConfig$sbtCmd),args)
  setJavaEnv(oldJavaEnv)
  if ( status != 0 ) stop("Non-zero exit status.")
  newJARs <- list.files("target",pattern=".*\\.jar",recursive=TRUE)
  srcJARs <- newJARs[grepl(".*-sources.jar$",newJARs)]
  binJARs <- setdiff(newJARs,srcJARs)
  pkgHome <- dirname(list.files(".","DESCRIPTION",recursive=TRUE))
  pkgHome <- pkgHome[!grepl(".*\\.Rcheck",pkgHome)]
  if ( length(pkgHome) > 1 ) pkgHome <- pkgHome[grepl("^R",pkgHome)]
  if ( length(pkgHome) != 1 ) stop("Cannot find package home.")
  binDir <- file.path(pkgHome,"inst","java")
  oldJARs <- list.files(binDir,pattern=".*\\.jar",recursive=TRUE)
  unlink(file.path(binDir,oldJARs))
  for ( v in scalaVersions ) {
    currentJARs <- binJARs[grepl(sprintf("^scala-%s",v),binJARs)]
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
  for ( v in scalaVersions ) {
    currentJARs <- srcJARs[grepl(sprintf("^scala-%s",v),srcJARs)]
    if ( length(currentJARs) > 0 ) file.copy(file.path("target",currentJARs),srcDir,TRUE)
  }
  invisible(NULL)
}

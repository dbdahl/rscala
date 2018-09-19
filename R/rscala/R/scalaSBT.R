#' Run SBT
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
#' @return \code{NULL}
#' @export
scalaSBT <- function(args=c("+package","packageSrc")) {
  scalaVersions <- c("2.11","2.12")
  config <- scalaConfig(FALSE)
  oldWD <- getwd()
  on.exit(setwd(oldWD))
  while ( TRUE ) {
    if ( file.exists("build.sbt") ) break
    currentWD <- getwd()
    setwd("..")
    if ( currentWD == getwd() ) stop("Cannot find 'build.sbt' file.")
  }
  oldJARs <- list.files("target",pattern=".*\\.jar",recursive=TRUE)
  unlink(oldJARs)
  status <- system2(config$sbtCmd,args)
  if ( status != 0 ) stop("Non-zero exit status.")
  newJARs <- list.files("target",pattern=".*\\.jar",recursive=TRUE)
  srcJARs <- newJARs[grepl(".*-sources.jar$",newJARs)]
  binJARs <- setdiff(newJARs,srcJARs)
  pkgHome <- dirname(list.files(".","DESCRIPTION",recursive=TRUE))
  if ( length(pkgHome) != 1 ) stop("Cannot find package home.")
  binDir <- file.path(pkgHome,"inst","java")
  dir.create(binDir,FALSE,TRUE)
  oldJARs <- list.files(binDir,pattern=".*\\.jar",recursive=TRUE)
  unlink(oldJARs)
  for ( v in scalaVersions ) {
    currentJARs <- binJARs[grepl(sprintf("^scala-%s",v),binJARs)]
    if ( length(currentJARs) > 0 ) file.copy(file.path("target",currentJARs),binDir,TRUE)
  }
  srcDir <- file.path(pkgHome,"java")
  dir.create(srcDir,FALSE,TRUE)
  oldJARs <- list.files(srcDir,pattern=".*\\.jar",recursive=TRUE)
  unlink(oldJARs)
  for ( v in scalaVersions ) {
    currentJARs <- srcJARs[grepl(sprintf("^scala-%s",v),srcJARs)]
    if ( length(currentJARs) > 0 ) file.copy(file.path("target",currentJARs),srcDir,TRUE)
  }
  invisible(NULL)
}

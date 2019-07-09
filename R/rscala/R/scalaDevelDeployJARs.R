#' Deploy JAR Files into the Package File System
#'
#' This function copies the JAR files to the appropriate directories of the R
#' package source. Specifically, source JAR files go into \code{(PKGHOME)/java}
#' and binary JAR files go into \code{(PKGHOME)/inst/java/scala-(VERSION)},
#' where \code{(PKGHOME)} is the package home and \code{(VERSION)} is the major
#' Scala version (e.g., 2.13).
#'
#' @param name The package name (as a string).
#' @param root The file system path to package root directory (as a string).
#' @param srcJAR The file system path to source JAR file (as a string).
#' @param binJARs A named character vector of file system paths, where each name
#'   is a Scala major version (e.g., \code{"2.13"}.)
#'
#' @export
#' 
scalaDevelDeployJARs <- function(name, root, srcJAR, binJARs) {
  if ( missing(name) || ( ! is.vector(name) ) || ( ! is.character(name) ) || ( length(name) != 1 ) || ( name == "" ) ) stop("'name' is mispecified.")
  if ( missing(root) || ( ! is.vector(root) ) || ( ! is.character(root) ) || ( length(root) != 1 ) || ( ! dir.exists(root) ) ) stop("'root' directory does not exist.")
  if ( ! file.exists(file.path(root,"DESCRIPTION")) ) stop("'root' does not appear to be a package root directory.")
  if ( missing(srcJAR) || ( ! is.vector(srcJAR) ) || ( ! is.character(srcJAR) ) || ( length(srcJAR) != 1 ) ) stop("'srcJAR' must be a path to JAR file.")
  if ( ! file.exists(srcJAR) ) stop("'srcJAR' does not exists in the file system.")
  if ( missing(binJARs) || ( ! is.vector(binJARs) ) || ( ! is.character(binJARs) || ( length(binJARs) == 0 ) || is.null(names(binJARs))) ) stop("'binJARs' must be a named vector of paths to JAR files.")
  if ( ! all(file.exists(binJARs)) ) stop("'binJARs' has elements that do not exists in the file system.")
  binDir <- file.path(root,"inst","java")
  for ( index in seq_along(binJARs) ) {
    v <- scalaMajorVersion(names(binJARs)[index])
    destDir <- file.path(binDir,sprintf("scala-%s",v))
    dir.create(destDir,FALSE,TRUE)
    file.copy(binJARs[index],file.path(destDir,paste0(name,".jar")),TRUE)
  }
  srcDir <- file.path(root,"java")
  dir.create(srcDir,FALSE,TRUE)
  file.copy(srcJAR,file.path(srcDir,paste0(name,"-source.jar")),TRUE)
  invisible()
}


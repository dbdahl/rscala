#' Download and Deploy JAR Files into the Package File System
#'
#' This function only takes effect during package installation. It is meant to
#' be called from bare code of a package that depends on \pkg{rscala} in a
#' script such as \code{zzz.R}. When called during package installation, it
#' downloads JAR files to the appropriate directories.  This avoids the need to
#' distribute some JAR files in the source package.
#'
#' @param description A character vector describing the JAR files to download,
#'   e,g. \code{"org.apache.commons:commons-math3:3.6.1"}.
#' @param scalaMajorVersion A Scala major version, such as \code{"2.13"}, if the
#'   JAR should be placed in a directory specific to a Scala version. Otherwise,
#'   \code{""} is used to specify a general installation.
#' @param prefix A string giving the prefix of the download URL.
#'
#' @export
#' @examples \dontrun{
#' ## To be run in bare code of a package that depends on rscala and needs,
#' ## for example, the Apache Commons Math Library.
#' rscala::scalaDevelDownloadJARs("org.apache.commons:commons-math3:3.6.1")
#' }
#' 
#' @importFrom utils download.file
#' 
scalaDevelDownloadJARs <- function(description, scalaMajorVersion="", prefix="https://search.maven.org/remotecontent?filepath=") {
  if ( identical(Sys.getenv("R_INSTALL_PKG"),"") ) {
    if ( interactive() ) message("This function only takes effect during package installation.")
    return(invisible())
  }
  if ( ( ! is.vector(scalaMajorVersion) ) || ( ! is.character(scalaMajorVersion) ) || ( length(scalaMajorVersion) != 1L ) ) {
    stop("Unexpected value for 'scalaMajorVersion'.")
  }
  if ( ( nchar(scalaMajorVersion) > 0 ) && ! ( scalaMajorVersion %in% names(scalaVersionJARs()) ) ) {
    stop("Unsupported major Scala version in 'scalaMajorVersion'.")
  }
  destDir <- file.path(Sys.getenv("R_PACKAGE_DIR"), "java")
  if ( nchar(scalaMajorVersion) > 0 ) destDir <- file.path(destDir, paste0("scala-", scalaMajorVersion))
  dir.create(destDir, FALSE, TRUE)
  for ( w in description ) {
    cells    <- strsplit(w, ":", fixed=TRUE)[[1]]
    group    <- gsub("\\.", "/", cells[1])
    artifact <- cells[2]
    version  <- cells[3]
    jar <- sprintf("%s%s/%s/%s/%s.jar",prefix,group,artifact,version,paste(artifact,version,sep="-"))
    download.file(jar, file.path(destDir, basename(jar)), mode="wb")
  }
}


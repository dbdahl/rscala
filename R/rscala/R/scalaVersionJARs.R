#' JAR Files for Support Scala Versions
#'
#' This function returns a named list whose elements give the file system paths of the JAR files for the supported major versions of Scala.
#'
#' @return A list whose names correspond to Scala major versions and whose elements are file system paths.
#' @export
#'
#' @examples
#' scalaVersionJARs()
#' 
scalaVersionJARs <- function() {
  pkgHome <- if ( Sys.getenv("RSCALA_BUILDING") != "" ) file.path(getwd(),"inst") else find.package("rscala")
  majorVersions <- gsub("^scala-(.*)","\\1",list.dirs(file.path(pkgHome,"java"),full.names=FALSE,recursive=FALSE))
  if ( length(majorVersions) == 0 ) return(character(0))
  result <- sapply(majorVersions, function(v) list.files(file.path(pkgHome,"java",paste0("scala-",v)),full.names=TRUE))
  as.list(result[sapply(result, function(x) !is.null(x) && length(x) > 0)])
}

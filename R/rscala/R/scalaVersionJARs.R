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
  pkgHome <- find.package("rscala")
  majorVersions <- gsub("^scala-(.*)","\\1",list.dirs(system.file("java",package="rscala"),full.names=FALSE,recursive=FALSE))
  result <- sapply(majorVersions, function(v) list.files(file.path(pkgHome,"java",paste0("scala-",v)),full.names=TRUE))
  as.list(result[sapply(result, function(x) !is.null(x) && length(x) > 0)])
}
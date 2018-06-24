CANONICAL_SCALA_VERSION <- "2.12.6"
CANONICAL_SCALA_MAJOR_VERSION <- "2.12"

#' Install Scala
#'
#' @return Returns \code{NULL}, invisibly.
#' @export
#'
#' @examples \dontrun{
#' scalaInstall()
#' }
scalaInstall <- function() {
  installPath <- installPath()
  result <- unlink(installPath,recursive=TRUE,force=TRUE)
  if ( result != 0 ) stop(paste0("Please delete the following directory and retry the installation: ",installPath))
  url <- sprintf("https://downloads.lightbend.com/scala/%s/scala-%s.tgz",CANONICAL_SCALA_VERSION,CANONICAL_SCALA_VERSION)
  tgzFile <- tempfile("rscala-tgz-")
  result <- utils::download.file(url,tgzFile)
  if ( result != 0 ) stop("Unable to download Scala.")
  tmpDir <- paste0(installPath,"-tmp")
  result <- utils::untar(tgzFile,exdir=tmpDir,tar="internal")    # Use internal to avoid problems on a Mac.
  unlink(tgzFile)
  if ( result != 0 ) stop("Unsuccessful installation of Scala in ",installPath,"\n\n",sep="")
  file.rename(list.files(tmpDir,full.names=TRUE),installPath)
  unlink(tmpDir,recursive=TRUE,force=TRUE)
  invisible()
}

installPath <- function() normalizePath(file.path(path.expand("~"),".rscala"),mustWork=FALSE)

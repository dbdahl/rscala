#' Run SBT and Deploy JAR Files
#'
#' This function runs SBT (Scala Build Tool) to package JAR files and then copy
#' them to the appropriate directories of the R package source.
#'
#' Starting from the current working directory and moving up the file system
#' hierarchy as needed, this function searches for the directory containing the
#' file \code{'build.sbt'}, the SBT build file. It temporarily changes the
#' working directory to this directory. It then runs \code{sbt +package
#' packageSrc} to package the cross-compiled the Scala code and package the
#' source code. publish the JAR files locally. Finally, it copies the JAR files
#' to the appropriate directories of the R package source. Specifically, source
#' JAR files go into \code{(PKGHOME)/java} and binary JAR files go into
#' \code{(PKGHOME)/inst/java/scala-(VERSION)}, where \code{(PKGHOME)} is the
#' package home and \code{(VERSION)} is the major Scala version (e.g., 2.12). It
#' is assumed that the package home is a subdirectory of the directory
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
#' @param copy.to.package Should the JARs files be copied to the appropriate
#'   directories of the R package source?'
#' @param only.if.newer Should compilation be avoided if it appears Scala code
#'   has not changed?
#
#' 
#' @return \code{NULL}
#' @export
scalaSBT <- function(args=c("+package","packageSrc"), copy.to.package=TRUE, only.if.newer=TRUE) {
  if ( ( ! is.vector(args) ) || ( ! is.character(args) ) ) stop("'args' is mispecified.")
  sConfig <- scalaConfig(FALSE,require.sbt=TRUE)
  info <- scalaDevelInfo()
  latest <- function(path,pattern=NULL) {
    files <- list.files(path, pattern=pattern, recursive=TRUE, full.names=TRUE)
    xx <- sapply(files, function(f) { file.info(f)$mtime })
    if ( length(xx) == 0 ) -Inf else max(xx)
  }
  srcHome <- file.path(info$projectRoot,"src")
  if ( only.if.newer && file.exists(srcHome) && file.exists(info$packageRoot) && ( latest(srcHome) < latest(info$packageRoot,'.*\\.jar') ) ) {
    cat("[info] Latest Scala source is older that JARs.  There is no need to re-compile.\n")
    return(invisible())
  }
  oldWD <- getwd()
  on.exit(setwd(normalizePath(oldWD,mustWork=FALSE))) 
  setwd(info$projectRoot)
  oldJavaEnv <- setJavaEnv(sConfig)
  oldPath <- Sys.getenv("PATH")
  Sys.setenv(PATH=paste0(file.path(sConfig$javaHome,"bin"),.Platform$path.sep,oldPath))
  runUsingProcessX <- if ( requireNamespace("rstudioapi", quietly = TRUE) ) {
    if ( rstudioapi::isAvailable() ) {
      if ( requireNamespace("processx", quietly = TRUE) ) TRUE
      else {
        stop("This function requires the 'processx' package when run under RStudio.  Please run \"install.packages('processx')\" and try again.")
      }
    } else FALSE
  }
  status <- if ( runUsingProcessX ) {
    processx::run(path.expand(sConfig$sbtCmd),args,echo=TRUE)$status
  } else {
    # Since RStudio 1.2, this does not seem to work, hence the alternative using the processx package.
    system2(path.expand(sConfig$sbtCmd),args)
  }
  Sys.setenv(PATH=oldPath)
  setJavaEnv(oldJavaEnv)
  if ( status != 0 ) stop("Non-zero exit status.")
  if ( copy.to.package ) {
    srcJAR <- scalaFindLatestJARsSrcSBT(info$projectRoot)
    binJARs <- scalaFindLatestJARsBinSBT(info$projectRoot)
    scalaDevelDeployJARs(info$name, info$packageRoot, srcJAR, binJARs)
  }
  invisible(NULL)
}

#' Deploy JAR Files into the Package File System
#'
#' This function copies the JAR files to the appropriate directories of the R
#' package source. Specifically, source JAR files go into \code{(PKGHOME)/java}
#' and binary JAR files go into \code{(PKGHOME)/inst/java/scala-(VERSION)},
#' where \code{(PKGHOME)} is the package home and \code{(VERSION)} is the major
#' Scala version (e.g., 2.12).
#'
#' @param name The package name (as a string).
#' @param root The file system path to package root directory (as a string).
#' @param srcJAR The file system path to source JAR file (as a string).
#' @param binJARs A named character vector of file system paths, where each name is a Scala major version (e.g., \code{"2.12"}.)
#'
#' @export
scalaDevelDeployJARs <- function(name, root, srcJAR, binJARs) {
  if ( missing(name) || ( ! is.vector(name) ) || ( ! is.character(name) ) || ( length(name) != 1 ) || ( name == "" ) ) stop("'name' is mispecified.")
  if ( missing(root) || ( ! is.vector(root) ) || ( ! is.character(root) ) || ( length(root) != 1 ) || ( ! dir.exists(root) ) ) stop("'root' directory does not exist.")
  if ( ! file.exists(file.path(root,"DESCRIPTION")) ) stop("'root' does not appear to be a package root directory.")
  if ( missing(srcJAR) || ( ! is.vector(srcJAR) ) || ( ! is.character(srcJAR) ) || ( length(srcJAR) != 1 ) ) stop("'srcJAR' must be a path to JAR file.")
  if ( ! file.exists(srcJAR) ) stop("'srcJAR' does not exists in the file system.")
  if ( missing(binJARs) || ( ! is.vector(binJARs) ) || ( ! is.character(binJARs) || ( length(binJARs) == 0 ) || is.null(names(binJARs))) ) stop("'binJARs' must be a named vector of paths to JAR files.")
  if ( ! all(file.exists(binJARs)) ) stop("'binJARs' has elements that do not exists in the file system.")
  binDir <- file.path(root,"inst","java")
  oldDirs <- list.files(binDir,"^scala-.*")
  unlink(file.path(binDir,oldDirs),recursive=TRUE)
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

#' @importFrom utils compareVersion
pickLatestStableScalaVersion <- function(candidates, latestSoFar=NULL) {
  if ( length(candidates) == 0 ) {
    if ( is.null(latestSoFar) ) stop("Cannot determine the stable Scala version.")
    else return(latestSoFar)
  }
  if ( is.null(latestSoFar) ) {
    latestSoFar <- candidates[1]
    result <- tryCatch(compareVersion(latestSoFar, latestSoFar), warning=function(e) NULL, error=function(e) NULL)
    if ( is.null(result) ) return(pickLatestStableScalaVersion(candidates[-1], NULL))
    else return(pickLatestStableScalaVersion(candidates[-1],latestSoFar))
  }
  result <- tryCatch(compareVersion(candidates[1], latestSoFar), warning=function(e) NULL, error=function(e) NULL)
  if ( is.null(result) ) pickLatestStableScalaVersion(candidates[-1], latestSoFar)
  else if ( result > 0 ) pickLatestStableScalaVersion(candidates[-1], candidates[1])
  else pickLatestStableScalaVersion(candidates[-1], latestSoFar)
}

scalaDevelInfo <- function() {
  oldWD <- getwd()
  on.exit(setwd(normalizePath(oldWD,mustWork=FALSE)))
  packageRoot <- NULL
  buildSystem <- NULL
  while ( TRUE ) {
    if ( file.exists("DESCRIPTION") ) packageRoot <- getwd()
    if ( file.exists("build.sc") ) {
      buildSystem <- "mill"
      break
    }
    if ( file.exists("build.sbt") ) {
      buildSystem <- "sbt"
      break
    }
    currentWD <- getwd()
    setwd("..")
    if ( currentWD == getwd() ) stop("Cannot find project root directory.")
  }
  packageRoot <- if ( ! is.null(packageRoot) ) packageRoot
  else {
    search <- function(depth) {
      candidates <- Sys.glob(paste0(c(rep("*",depth),"DESCRIPTION"),collapse=.Platform$file.sep))
      descriptionFiles <- sapply(candidates, function(a) as.vector(read.dcf(a,"Package")))
      names(descriptionFiles[!is.na(descriptionFiles)])
    }
    i <- 1
    while ( ( i < 3 ) && ( length(search(i)) != 1 ) ) i <- i + 1
    if ( i < 3 ) normalizePath(dirname(search(i)))
    else stop("Cannot find package root directory.")
  }
  name <- as.vector(read.dcf(file.path(packageRoot,"DESCRIPTION"),"Package"))
  list(name=name, projectRoot=getwd(), packageRoot=packageRoot, buildSystem=buildSystem)
}

# # Probably broken
# mill <- function(args,stderr=FALSE) {
#   outString <- system2("mill",args,stdout=TRUE,stderr=stderr)
#   gsub('^\\s*"(.*)"\\s*$',"\\1",outString) 
# }
# 
# # Probably broken
# scalaDevelBuildJARs <- function(info=scalaDevelInfo()) {
#   oldWD <- getwd()
#   on.exit(setwd(normalizePath(oldWD,mustWork=FALSE)))
#   if ( is.null(info$projectRoot) ) stop("Cannot find project root.")
#   setwd(info$projectRoot)
#   if ( is.null(info$buildSystem) ) stop("No build system detected.")
#   result <- if ( info$buildSystem == "mill" ) {
#     scalaVersions <- mill(c("show","scala[_].scalaVersion"))
#     srcScalaVersion <- pickLatestStableScalaVersion(scalaVersions)
#     binJARs <- gsub('^ref:[^:]*:(.*)$',"\\1",mill(c("show","scala[_].jar")))
#     srcJAR <- gsub('^ref:[^:]*:(.*)$',"\\1",mill(c("show",paste0("scala[",srcScalaVersion,"].sourceJar"))))
#     names(binJARs) <- scalaVersions
#     list(binJARs=binJARs, srcJAR=srcJAR)
#   } else if ( info$buildSystem == "sbt" ) {
#     stop("Not yet implemented.")
#   } else stop(paste0("Unrecognized build system: ",info$buildSystem))
#   result
# }

scalaFindLatestJARs <- function(dir, version2Path, jarFilter) {
  oldWD <- getwd()
  on.exit(setwd(normalizePath(oldWD,mustWork=FALSE)))
  setwd(dir)
  majorVersions <- names(scalaVersionJARs())
  jars <- sapply(majorVersions, function(mv) {
    candidates <- jarFilter(list.files(version2Path(mv),".*\\.jar",full.names=TRUE))
    latest <- which.max(file.info(candidates)$mtime)
    normalizePath(candidates[latest],mustWork=FALSE)
  })
  names(jars) <- majorVersions
  unlist(jars[sapply(jars, function(x) length(x)==1)])
}

scalaFindLatestJARsBinSBT <- function(dir) {
  scalaFindLatestJARs(dir,
          function(majorVersion) file.path("target",sprintf("scala-%s",majorVersion)),
          function(candidates) candidates[!(grepl(".*-sources\\.jar",candidates) | grepl(".*-scaladoc\\.jar",candidates))])
}

scalaFindLatestJARsSrcSBT <- function(dir) {
  candidates <- scalaFindLatestJARs(dir,
          function(majorVersion) file.path("target",sprintf("scala-%s",majorVersion)),
          function(candidates) candidates[grepl(".*-sources\\.jar",candidates)])
  names <- names(candidates)
  latest <- pickLatestStableScalaVersion(names)
  candidates[[latest]]
}



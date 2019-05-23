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
#' @param copy.to.package Should the JARs files be copied to the appropriate
#'   directories of the R package source?
#' @param use.cache Should compilation be avoided if it appears Scala code
#'   has not changed?
#'
#' @return \code{NULL}
scalaSBT_OLD <- function(args=c("+package","packageSrc"), copy.to.package=TRUE, use.cache=TRUE) {
  sConfig <- scalaConfig(FALSE,require.sbt=TRUE)
  oldWD <- normalizePath(getwd(),mustWork=FALSE)
  on.exit(setwd(oldWD))
  packageHome <- NULL
  while ( TRUE ) {
    if ( file.exists("DESCRIPTION") ) packageHome <- getwd()
    if ( file.exists("build.sbt") ) break
    currentWD <- getwd()
    setwd("..")
    if ( currentWD == getwd() ) stop("Cannot find 'build.sbt' file.")
  }
  if ( all(grepl("^(\\+?assembly|\\+?package|\\+?packageSrc)$",args)) ) {
    packageHome <- if ( ! is.null(packageHome) ) packageHome
    else {
      descriptionFile <- Sys.glob("*/DESCRIPTION")
      if ( length(descriptionFile) == 1 ) normalizePath(dirname(descriptionFile))
      else if ( length(descriptionFile) == 0 ) {
        descriptionFile <- Sys.glob("*/*/DESCRIPTION")
        if ( length(descriptionFile) == 1 ) normalizePath(dirname(descriptionFile))
        else NULL
      } else NULL
    }
    latest <- function(path,pattern=NULL) {
      files <- list.files(path, pattern=pattern, recursive=TRUE, full.names=TRUE)
      xx <- sapply(files, function(f) { file.info(f)$mtime })
      if ( length(xx) == 0 ) -Inf else max(xx)
    }
    srcHome <- "src"
    if ( use.cache && file.exists(srcHome) && !is.null(packageHome) && file.exists(packageHome) && ( latest(srcHome) < latest(packageHome,'.*\\.jar') ) ) {
      cat("[info] Latest Scala source is older that JARs.  There is no need to re-compile.\n")
      return(invisible())
    }
  }
  oldJavaEnv <- setJavaEnv(sConfig)
  oldPath <- Sys.getenv("PATH")
  Sys.setenv(PATH=paste0(file.path(sConfig$javaHome,"bin"),.Platform$path.sep,oldPath))
  status <- system2(path.expand(sConfig$sbtCmd),args)
  Sys.setenv(PATH=oldPath)
  setJavaEnv(oldJavaEnv)
  if ( status != 0 ) stop("Non-zero exit status.") 
  if ( copy.to.package ) {
    lines <- readLines("build.sbt")
    nameLine <- lines[grepl("^\\s*name\\s*:=",lines)]
    if ( length(nameLine) != 1 ) stop("Could not find one and only one 'name' line in 'build.sbt'.")
    name <- sub('^[^"]*"(.*)"[,\\s]*$','\\1',nameLine)
    versionLine <- lines[grepl("^\\s*version\\s*:=",lines)]
    if ( length(versionLine) != 1 ) stop("Could not find one and only one 'version' line in 'build.sbt'.")
    version <- sub('^[^"]*"(.*)"[,\\s]*$','\\1',versionLine)
    scalaVersionLine <- lines[grepl("^\\s*scalaVersion\\s*:=",lines)]
    if ( length(scalaVersionLine) != 1 ) stop("Could not find one and only one 'scalaVersion' line in 'build.sbt'.")
    scalaVersion <- scalaMajorVersion(sub('^[^"]*"(.*)"[,\\s]*$','\\1',scalaVersionLine))
    crossLine <-   lines[grepl("^\\s*crossScalaVersions\\s*:=",lines)]
    if ( length(crossLine) != 1 ) stop("Could not find one and only one 'crossScalaVersion' line in 'build.sbt'.")
    scalaVersions <- strsplit(gsub('["),]','',sub('[^"]*','',crossLine)),"\\s+")[[1]]
    scalaVersions <- sapply(scalaVersions, function(x) {
      if ( grepl("^2.1[12]\\.",x) ) substr(x,1,4) else x
    })
    if ( is.null(sConfig$sbtCmd) ) {
      stopMsg <- "\n\n<<<<<<<<<<\n<<<<<<<<<<\n<<<<<<<<<<\n\nSBT is not found!  Please run 'rscala::scalaConfig(require.sbt=TRUE)'\n\n>>>>>>>>>>\n>>>>>>>>>>\n>>>>>>>>>>\n"
      stop(stopMsg)
    }
    srcJARs <- paste0("scala-",scalaVersions,"/",tolower(name),"_",scalaVersions,"-",version,"-sources.jar")
    if ( any(grepl("^\\+?assembly$",args)) ) {
      binJARs <- paste0("scala-",scalaVersions,"/",name,"-assembly-",version,".jar")
    } else {
      binJARs <- paste0("scala-",scalaVersions,"/",tolower(name),"_",scalaVersions,"-",version,".jar")
    }
    pkgHome <- unique(dirname(list.files(".","DESCRIPTION",recursive=TRUE)))   # unique(...) because on Mac OS X, duplicates are possible.
    pkgHome <- pkgHome[!grepl(".*\\.Rcheck",pkgHome)]
    if ( length(pkgHome) > 1 ) {
      if ( sum(normalizePath(pkgHome,mustWork=FALSE)==oldWD) == 1 ) pkgHome <- pkgHome[normalizePath(pkgHome,mustWork=FALSE)==oldWD]
      else {
        if ( sum(basename(pkgHome)==name) == 1 ) pkgHome <- pkgHome[basename(pkgHome)==name]
        else stop(paste0("Cannot determine package home among: ",paste(pkgHome,collapse="  ")))
      }
    } else if ( length(pkgHome) == 0 ) stop(paste0("Cannot find any candidates for package home."))
    binDir <- file.path(pkgHome,"inst","java")
    oldDirs <- list.files(binDir,"^scala-.*")
    unlink(file.path(binDir,oldDirs),recursive=TRUE)
    for ( v in scalaVersions ) {
      currentJARs <- binJARs[grepl(sprintf("^scala-%s",v),binJARs)]
      currentJARs <- currentJARs[grepl(sprintf(".*-%s.jar$",version),basename(currentJARs))]
      if ( length(currentJARs) > 0 ) {
        destDir <- file.path(binDir,sprintf("scala-%s",v))
        dir.create(destDir,FALSE,TRUE)
        file.copy(file.path("target",currentJARs),destDir,TRUE)
      }
    }
    srcDir <- file.path(pkgHome,"java")
    unlink(list.files(srcDir,pattern=".*\\.jar",recursive=TRUE,full.names=TRUE))
    dir.create(srcDir,FALSE,TRUE)
    currentJARs <- srcJARs[grepl(sprintf("^scala-%s",scalaVersion),srcJARs)]
    currentJARs <- currentJARs[grepl(sprintf(".*_%s-%s-sources.jar$",scalaVersion,version),basename(currentJARs))]
    if ( length(currentJARs) > 0 ) file.copy(file.path("target",currentJARs),srcDir,TRUE)
  }
  invisible(NULL)
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
    descriptionFile <- Sys.glob("*/DESCRIPTION")
    if ( length(descriptionFile) == 1 ) normalizePath(dirname(descriptionFile))
    else if ( length(descriptionFile) == 0 ) {
      descriptionFile <- Sys.glob("*/*/DESCRIPTION")
      if ( length(descriptionFile) == 1 ) normalizePath(dirname(descriptionFile))
      else stop("Cannot find package root directory.")
    } else stop("Cannot find package root directory.")
  }
  name <- as.vector(read.dcf(file.path(packageRoot,"DESCRIPTION"),"Package"))
  list(name=name, projectRoot=getwd(), packageRoot=packageRoot, buildSystem=buildSystem)
}

# Probably broken
mill <- function(args,stderr=FALSE) {
  outString <- system2("mill",args,stdout=TRUE,stderr=stderr)
  gsub('^\\s*"(.*)"\\s*$',"\\1",outString) 
}

# Probably broken
scalaDevelBuildJARs <- function(info=scalaDevelInfo()) {
  oldWD <- getwd()
  on.exit(setwd(normalizePath(oldWD,mustWork=FALSE)))
  if ( is.null(info$projectRoot) ) stop("Cannot find project root.")
  setwd(info$projectRoot)
  if ( is.null(info$buildSystem) ) stop("No build system detected.")
  result <- if ( info$buildSystem == "mill" ) {
    scalaVersions <- mill(c("show","scala[_].scalaVersion"))
    srcScalaVersion <- pickLatestStableScalaVersion(scalaVersions)
    binJARs <- gsub('^ref:[^:]*:(.*)$',"\\1",mill(c("show","scala[_].jar")))
    srcJAR <- gsub('^ref:[^:]*:(.*)$',"\\1",mill(c("show",paste0("scala[",srcScalaVersion,"].sourceJar"))))
    names(binJARs) <- scalaVersions
    list(binJARs=binJARs, srcJAR=srcJAR)
  } else if ( info$buildSystem == "sbt" ) {
    stop("Not yet implemented.")
  } else stop(paste0("Unrecognized build system: ",info$buildSystem))
  result
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
#' 
#' @return \code{NULL}
#' @export
scalaSBT <- function(args=c("+package","packageSrc"), copy.to.package=TRUE) {
  if ( ( ! is.vector(args) ) || ( ! is.character(args) ) ) stop("'args' is mispecified.")
  sConfig <- scalaConfig(FALSE,require.sbt=TRUE)
  info <- scalaDevelInfo()
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

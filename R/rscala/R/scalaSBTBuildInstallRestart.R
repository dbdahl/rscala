#' Run scalaSBT(), Build, Install, & Restart
#' 
#' This function runs \code{scalaSBT()}, builds the source package, installs the package, and restarts R.
#'
#' @export
#'
scalaSBTBuildInstallRestart <- function() {
  cat("\n==> Compiling Scala code","\n",sep="")
  scalaSBT()
  if ( requireNamespace("rstudioapi", quietly = TRUE) && requireNamespace("devtools", quietly = TRUE) ) {
    if ( ! rstudioapi::isAvailable() ) stop("This function requires that RStudio is running.")
    CWD <- getwd()
    ROOT <- rstudioapi::getActiveProject()
    if ( ! is.null(ROOT) ) {
      projectFile <- rstudioapi::initializeProject(ROOT)
      lines <- readLines(projectFile)
      i <- grep("^PackageRoxygenize: ",lines)
      if ( length(i) > 0 ) {
        i <- i[1]
        args <- strsplit(sub("^PackageRoxygenize: (.*)","\\1",lines[i]),',')[[1]]
        cat("\n==> Writing documentation","\n",sep="")
        devtools::document(roclets=args)
      }
      cat("\n==> Building source package","\n",sep="")
      i <- grep("^PackageBuildArgs: ",lines)
      if ( length(i) > 0 ) {
        i <- i[1]
        args <- strsplit(sub("^PackageBuildArgs: (.*)","\\1",lines[i]),' ')[[1]]
        devtools::build(args=args)
      } else {
        devtools::build()
      }
      cat("\n==> Installing package","\n",sep="")
      i <- grep("^PackageInstallArgs: ",lines)
      args <- if ( length(i) > 0 ) {
        i <- i[1]
        strsplit(sub("^PackageInstallArgs: (.*)","\\1",lines[i]),' ')[[1]]
      } else character(0)
      setwd(dirname(ROOT))
      pkgName <- basename(ROOT)
      system2(file.path(R.home(),"bin","R"),c("CMD","INSTALL",args,pkgName))
      setwd(CWD)
      rstudioapi::restartSession(sprintf('library("%s")',pkgName))
    }
  } else {
    stop('Please run: install.packages("devtools")')
  }
  invisible(NULL)
}

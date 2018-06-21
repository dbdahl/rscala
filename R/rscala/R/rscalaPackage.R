#' @export
#'
.rscalaPackage <- function(packages=character(),
                           assign.callback=function(s) {},
                           assign.name="s",
                           mode="",
                           ...) {
  pkgEnv <- parent.env(parent.frame())    # Environment of depending package (assuming this function is only called in .onLoad function).
  if ( is.null(assign.name) || ( assign.name == "" ) ) stop("'assign.name' must be supplied.")
  if ( mode == "" ) {
    assign("rscalaBridgeOwner",TRUE,envir=pkgEnv)
    assign("rscalaBridgeName",assign.name,envir=pkgEnv)
    scala(packages,assign.callback,assign.name,pkgEnv,...)
  }
  else {
    assign("rscalaBridgeOwner",FALSE,envir=pkgEnv)
    delayedAssign(assign.name,{
      s <- eval(parse(text=mode))
      assign.callback(s)
      s
    },assign.env=pkgEnv)
  }
}

#' @export
#'
.rscalaPackageUnload <- function() {
  pkgEnv <- parent.env(parent.frame())    # Environment of depending package (assuming this function is only called in .onUnload function).
  if ( pkgEnv[["rscalaBridgeOwner"]] ) close(pkgEnv[[pkgEnv[["rscalaBridgeName"]]]])
}

#' @export
#'
.rscalaPackage <- function(packages=character(),
                           assign.callback=function(s) {},
                           assign.name="s",
                           mode="",
                           ...) {
  pkgEnv <- parent.env(parent.frame())    # Environment of depending package (assuming this function is only called in .onLoad function).
  if ( is.null(assign.name) || ( assign.name == "" ) ) stop("'assign.name' must be supplied")
  if ( mode == "" ) scala(packages,assign.callback,assign.name,pkgEnv,...)
  else {
    delayedAssign(assign.name,{
      s <- eval(parse(text=mode))
      assign.callback(s)
      s
    },assign.env=pkgEnv)
  }
}

.rscalaPackageOld <- function(pkgname, ..., classpath.packages=character(), mode="parallel", assign.name="s") {
  env <- parent.env(parent.frame())    # Environment of depending package (assuming this function is only called in .onLoad function).
  rscalaPackageEnv <- new.env(parent=emptyenv())
  assign(".rscalaPackageEnv",rscalaPackageEnv, envir=env)
  assign("isConnected",FALSE,envir=rscalaPackageEnv)
  assign("assign.name",assign.name,envir=rscalaPackageEnv)
  callback <- if ( ! ( mode %in% c("parallel","lazy","serial") ) ) function(ss) {}
  else function(ss) { assign("isConnected",TRUE,envir=rscalaPackageEnv) }
  scala(classpath.packages=c(pkgname,classpath.packages),assign.env=env,callback=callback,mode=mode,assign.name=assign.name,...)
  invisible()
}

.rscalaPackageUnload <- function() {
  env <- parent.env(parent.frame())
  if ( get("isConnected",envir=get(".rscalaPackageEnv",envir=env)) ) {
    assign.name <- get("assign.name",envir=get(".rscalaPackageEnv",envir=env))
    close(get(assign.name,envir=env))
  }
}

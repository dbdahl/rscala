# DBD 2018-09-12: These are hacks to allow current CRAN versions of bamboo,
# sdols, and shallot to pass CRAN checks without being updated.  Working
# versions of sdols and shallot without this hack are already in git.  When
# these packages are updated, this hack can be dropped.

#' @export
#' 
`[[.rscalaReference` <- function(x, condition) {
  if ( condition == "type" ) scalaType(x)
  else stop("Not supported.")
}

#' @export
#'
scalaPackage <- function(packages, assign.callback, mode="") {
  if ( mode == "" ) {
    s <- scala()
  } else {
    s <- eval(parse(text=mode))
  }
  scalaJARs(packages)
  scalaLazy(assign.callback)
  assign("s",s,envir=parent.env(parent.frame()))
}

#' @export
#'
scalaPackageUnload <- function() {
  s <- get("s",envir=parent.env(parent.frame()))
  close(s)
}


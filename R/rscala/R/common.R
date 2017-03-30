'%~%'   <- function(interpreter,snippet) UseMethod("%~%")
'%.~%'  <- function(interpreter,snippet) UseMethod("%.~%")
'%@%'   <- function(interpreter,snippet) UseMethod("%@%")
'%new%' <- function(interpreter,snippet) UseMethod("%new%")

strintrplt <- function(snippet,envir=parent.frame()) {
  if ( ! is.character(snippet) ) stop("Character vector expected.")
  if ( length(snippet) != 1 ) stop("Length of vector must be exactly one.")
  m <- regexpr("@\\{([^\\}]+)\\}",snippet)
  if ( m != -1 ) {
    s1 <- substr(snippet,1,m-1)
    s2 <- substr(snippet,m+2,m+attr(m,"match.length")-2)
    s3 <- substr(snippet,m+attr(m,"match.length"),nchar(snippet))
    strintrplt(paste(s1,paste(toString(eval(parse(text=s2),envir=envir)),collapse=" ",sep=""),s3,sep=""),envir)
  } else snippet
}

scalaSettings <- function(interpreter,interpolate=NULL,length.one.as.vector=NULL) {
  if ( is.null(interpolate) && is.null(length.one.as.vector) ) {
    list(debug=get("debug",envir=interpreter[['env']]),
         serialize=get("serialize",envir=interpreter[['env']]),
         interpolate=get("interpolate",envir=interpreter[['env']]),
         length.one.as.vector=get("length.one.as.vector",envir=interpreter[['env']]))
  } else {
    if ( !is.null(interpolate) ) assign("interpolate",as.logical(interpolate)[1],envir=interpreter[['env']])
    if ( !is.null(length.one.as.vector) ) assign("length.one.as.vector",as.logical(length.one.as.vector)[1],envir=interpreter[['env']])
  }
}

msg <- function(...,withTime=FALSE) {
  msg <- paste0(...,collapse="\n")
  cat(paste0("DEBUG (  R  ) ",ifelse(withTime,format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"),""),": ",msg,"\n"))
}


scalaInvoke <- function(details, snippet, args, withNames=FALSE) {
  socketOut <- details[["socketOut"]]
  cache <- details[["cache"]]
  args <- rev(args)
  tipes <- sapply(args, push, socketOut=socketOut)
  body <- paste0(
    "() => {\n"
    ,paste0("val ",names(args)," = ES.pop[",tipes,"]()",collapse="\n")
    ,"\n"
    ,snippet
    ,"\n}"
  )
  if ( ! exists(body, envir=cache) ) {
    functionID <- 341234L
    assign(body, functionID, envir=cache)
  }
  functionID <- get(body, envir=cache)
  wb(socketOut, PCODE_INVOKE)
  wb(socketOut, functionID)   # Function reference
  flush(socketOut)
  pop(details[["socketIn"]])
}

#' @export
#' 
scalaEcho <- function(bridge) {
  details <- attr(bridge,"details")
  socketOut <- details[["socketOut"]]
  wb(socketOut,PCODE_ECHO)
  wb(socketOut,as.integer(runif(1,0,10)))
  flush(socketOut)
  pop(details[["socketIn"]])
}

scalaInvoke <- function(details, snippet, args, withNames=FALSE) {
  socketOut <- details[["socketOut"]]
  cache <- details[["cache"]]
  buffer <- details[["buffer"]]
  seek(buffer,where=0)
  truncate(buffer)
  args <- rev(args)
  tipes <- sapply(args, push, socketOut=buffer)
#  body <- paste0(
#    "() => {\n"
#    ,paste0("val ",names(args)," = ES.pop[",tipes,"]()",collapse="\n")
#    ,"\n"
#    ,snippet
#    ,"\n}"
#  )
  body <- "adf"
  if ( ! exists(body, envir=cache) ) {
    functionID <- 341234L
    assign(body, functionID, envir=cache)
  }
  functionID <- get(body, envir=cache)
  wb(buffer, PCODE_INVOKE)
  wb(buffer, functionID)   # Function reference
  wb(socketOut,rawConnectionValue(buffer))
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

scalaInvoke <- function(details, snippet, args, withNames=FALSE) {
  useBuffer <- details[["useBuffer"]] 
  buffer <- if ( useBuffer ) {
    buffer <- details[["buffer"]]
    seek(buffer,where=0)
    truncate(buffer)
    buffer
  } else details[['socketOut']]
  args <- rev(args)
  sapply(args, push, socketOut=buffer)
  if ( withNames ) {
    wb(buffer, PCODE_INVOKE_WITH_NAMES)
    sapply(names(args), function(name) wc(buffer,name))
  } else {
    wb(buffer, PCODE_INVOKE_WITHOUT_NAMES)
  }
  wc(buffer,snippet)
  if ( useBuffer ) {
    wb(details[["socketOut"]],rawConnectionValue(buffer))
  }
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

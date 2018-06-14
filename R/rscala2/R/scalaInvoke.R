scalaInvoke <- function(details, snippet, args, withNames=FALSE, withReference=FALSE) {
  socketOut <- details[["buffer"]]
  seek(socketOut,where=0)
  truncate(socketOut)
  garbageLength <- length(details[["garbage"]]) 
  if ( garbageLength > 0 ) {
    wb(socketOut, PCODE_GARBAGE_COLLECT)
    wb(socketOut, c(garbageLength,details[["garbage"]]))
    details[["garbage"]] <- integer()
  }
  args <- rev(args)
  sapply(args, push, socketOut=socketOut)
  if ( withNames ) {
    wb(socketOut, PCODE_INVOKE_WITH_NAMES)
    sapply(names(args), function(name) wc(socketOut,name))
  } else {
    if ( withReference ) {
      wb(socketOut, PCODE_INVOKE_WITH_REFERENCE)
    } else {
      wb(socketOut, PCODE_INVOKE_WITHOUT_NAMES)
    }
  }
  wc(socketOut,snippet)
  wb(details[["socketOut"]],rawConnectionValue(socketOut))
  pop(details)
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

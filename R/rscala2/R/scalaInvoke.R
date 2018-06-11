scalaInvoke <- function(details, snippet, args, withNames=FALSE) {
  socketOut <- details[["socketOut"]]
  args <- rev(args)
  sapply(args, push, details=details)
  wb(socketOut, if ( withNames ) PCODE_INVOKE_WITH_NAMES else PCODE_INVOKE_WITHOUT_NAMES)
  wb(socketOut,length(args))
  if ( withNames ) sapply(names(args), function(name) wc(socketOut,name))
  wc(socketOut,snippet)
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

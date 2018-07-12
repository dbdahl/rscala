scalaInvoke <- function(details, snippet, args, withNames=FALSE, withReference=FALSE, functionArgTypes=NULL) {
  scalaLastEngine(details)
  if ( details[["interrupted"]] ) return(invisible())
  socketOut <- details[["socketOut"]]
  garbageLength <- length(details[["garbage"]]) 
  if ( garbageLength > 0 ) {
    wb(socketOut,PCODE_GARBAGE_COLLECT)
    wb(socketOut,c(garbageLength,details[["garbage"]]))
    details[["garbage"]] <- integer()
  }
  args <- rev(args)
  names <- names(args)
  for ( i in seq_along(args) ) {
    result <- push(args[[i]], names[i], socketOut) 
    if ( ! identical(result,TRUE) ) {
      if ( i > 1 ) {
        wb(socketOut,PCODE_CLEAR)
        wb(socketOut,as.integer(i-1L))
      }
      stop(attr(result,"msg"))
    }
  }
  if ( withReference ) {
    wb(socketOut,PCODE_INVOKE_WITH_REFERENCE)
  } else {
    if ( withNames ) wb(socketOut,PCODE_INVOKE_FREEFORM)
    else wb(socketOut,PCODE_INVOKE)
  }
  wb(socketOut,length(args))
  wc(socketOut,snippet)
  pop(details, functionArgTypes)
}

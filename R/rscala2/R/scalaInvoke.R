scalaInvoke <- function(details, snippet, args, withNames=FALSE) {
  socketOut <- details[["socketOut"]]
  sapply(args, push, details=details)
  code <- if ( withNames ) PCODE_INVOKE_WITH_NAMES else PCODE_INVOKE_WITHOUT_NAMES
  writeBin(c(code,length(args)),socketOut,endian="big")
  if ( withNames ) sapply(names(args), function(name) writeString(socketOut, name))
  writeString(socketOut, snippet)
  pop(details[["socketIn"]])
}

writeString <- function(socketOut, x) {
  bytes <- charToRaw(iconv(x,to="UTF-8"))
  writeBin(length(bytes),socketOut,endian="big")
  writeBin(bytes,socketOut,endian="big",useBytes=TRUE)  
}

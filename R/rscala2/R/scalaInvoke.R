scalaInvokeWithNames <- function(details, snippet, args) {
  socketOut <- details[["socketOut"]]
  socketIn <- details[["socketIn"]]
  sapply(args, push, details=details)
  writeBin(c(PCODE_INVOKE_WITH_NAMES,length(args)),socketOut,endian="big")
  sapply(names(args), function(name) writeString(socketOut, name))
  writeString(socketOut, snippet)
  readBin(socketIn,what=RTYPE_INT,endian="big")
}

scalaInvokeWithoutNames <- function(details, snippet, args) {
  socketOut <- details[["socketOut"]]
  socketIn <- details[["socketIn"]]
  sapply(args, push, details=details)
  writeBin(c(PCODE_INVOKE_WITHOUT_NAMES,length(args)),socketOut,endian="big")
  writeString(socketOut, snippet)
  readBin(socketIn,what=RTYPE_INT,endian="big")
}

writeString <- function(socketOut, x) {
  bytes <- charToRaw(iconv(x,to="UTF-8"))
  writeBin(length(bytes),socketOut,endian="big")
  writeBin(bytes,socketOut,endian="big",useBytes=TRUE)  
}

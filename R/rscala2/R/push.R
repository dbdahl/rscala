push <- function(what, details) {
  socketOut <- details[["socketOut"]]
  if ( is.integer(what) ) {
    if ( length(what) == 1L ) {
      wb(socketOut,c(PCODE_PUSH,TCODE_INT_0))
      wb(socketOut,what)
    } else {
      wb(socketOut,c(PCODE_PUSH,TCODE_INT_1))
      wb(socketOut,c(length(what),what))
    }
  } else if ( is.double(what) ) {
    if ( length(what) == 1L ) {
      wb(socketOut,c(PCODE_PUSH,TCODE_DOUBLE_0))
      wb(socketOut,what)
    } else {
      wb(socketOut,c(PCODE_PUSH,TCODE_DOUBLE_1))
      wb(socketOut,length(what))
      wb(socketOut,what)
    }
  } else if ( is.character(what) ) {
    if ( length(what) == 1L ) {
      wb(socketOut,c(PCODE_PUSH,TCODE_CHARACTER_0))
      wc(socketOut,what)
    } else {
      stop("Not supported.")
      bytesVector <- charToRaw(iconv(what,to="UTF-8"))
      lengths <- sapply(bytesVector, function(bytes) length(bytes))
      writeBin(c(TCODE_CHARACTER_1,length(what),lengths),socketOut,endian="big")
      lapply(bytesVector, function(bytes) {
        writeBin(bytes,socketOut,endian="big",useBytes=TRUE)
      })
    }
  } else stop(paste0("Unsupported type: ",class(what)))
  flush(socketOut)
}

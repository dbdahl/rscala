push <- function(what, details) {
  socketOut <- details[["socketOut"]]
  if ( is.integer(what) ) {
    if ( length(what) == 1L ) {
      writeBin(c(PCODE_PUSH,TCODE_INT_0,what),socketOut,endian="big")
    } else {
      writeBin(c(PCODE_PUSH,TCODE_INT_1,length(what),what),socketOut,endian="big")
    }
  } else if ( is.double(what) ) {
    if ( length(what) == 1L ) {
      writeBin(c(PCODE_PUSH,TCODE_DOUBLE_0),socketOut,endian="big")
      writeBin(what,socketOut,endian="big")
    } else {
      writeBin(c(PCODE_PUSH,TCODE_DOUBLE_1,length(what)),socketOut,endian="big")
      writeBin(what,socketOut,endian="big")
    }
  } else if ( is.character(what) ) {
    if ( length(what) == 1L ) {
      bytes <- charToRaw(iconv(what,to="UTF-8"))
      writeBin(c(PCODE_PUSH,TCODE_CHARACTER_0,length(bytes)),socketOut,endian="big")
      writeBin(bytes,socketOut,endian="big",useBytes=TRUE)
    } else {
      bytesVector <- charToRaw(iconv(what,to="UTF-8"))
      lengths <- sapply(bytesVector, function(bytes) length(bytes))
      writeBin(c(PCODE_PUSH,TCODE_CHARACTER_1,length(what),lengths),socketOut,endian="big")
      lapply(bytesVector, function(bytes) {
        writeBin(bytes,socketOut,endian="big",useBytes=TRUE)
      })
    }
  } else stop(paste0("Unsupported type: ",class(what)))
}

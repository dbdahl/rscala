#' @export
#' 
push <- function(what, bridge, getType=FALSE) {
  if ( is.integer(what) ) {
    if ( length(what) == 1L ) {
      if ( getType ) TCODE_INT_0
      else writeBin(c(PCODE_PUSH,TCODE_INT_0,what),bridge[["socketOut"]],endian="big")
    } else {
      if ( getType ) TCODE_INT_1
      else writeBin(c(PCODE_PUSH,TCODE_INT_1,length(what),what),bridge[["socketOut"]],endian="big")
    }
  } else if ( is.double(what) ) {
    if ( length(what) == 1L ) {
      if ( getType ) TCODE_DOUBLE_0
      else {
        writeBin(c(PCODE_PUSH,TCODE_DOUBLE_0),bridge[["socketOut"]],endian="big")
        writeBin(what,bridge[["socketOut"]],endian="big")
      }
    } else {
      if ( getType ) TCODE_DOUBLE_1
      else {
        writeBin(c(PCODE_PUSH,TCODE_DOUBLE_1,length(what)),bridge[["socketOut"]],endian="big")
        writeBin(what,bridge[["socketOut"]],endian="big")
      }
    }
  } else if ( is.character(what) ) {
    if ( length(what) == 1L ) {
      if ( getType ) TCODE_STRING_0
      else {
        bytes <- charToRaw(iconv(what,to="UTF-8"))
        writeBin(c(PCODE_PUSH,TCODE_STRING_0,length(bytes)),bridge[["socketOut"]],endian="big")
        writeBin(bytes,bridge[["socketOut"]],endian="big",useBytes=TRUE)
      }
    } else {
      if ( getType ) TCODE_STRING_1
      else {
        bytesVector <- charToRaw(iconv(what,to="UTF-8"))
        lengths <- sapply(bytesVector, function(bytes) length(bytes))
        writeBin(c(PCODE_PUSH,TCODE_STRING_1,length(what),lengths),bridge[["socketOut"]],endian="big")
        lapply(bytesVector, function(bytes) {
          writeBin(bytes,bridge[["socketOut"]],endian="big",useBytes=TRUE)
        })
      }
    }
  } else stop(paste0("Unsupported type: ",class(what)))
}

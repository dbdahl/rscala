#' @export
#' 
pop <- function(details) {
  socketOut <- details[["socketOut"]]
  socketIn <- details[["socketIn"]]
  writeBin(PCODE_POP,socketOut,endian="big")
  tipe <- readBin(socketIn,what=RTYPE_INT,endian="big")
  if ( tipe == TCODE_INT_0 ) {
    readBin(socketIn,what=RTYPE_INT,endian="big")
  } else if ( tipe == TCODE_INT_1 ) {
    len <- readBin(socketIn,what=RTYPE_INT,endian="big")
    readBin(socketIn,n=len,what=RTYPE_INT,endian="big")
  } else if ( tipe == TCODE_DOUBLE_0 ) {
    readBin(socketIn,what=RTYPE_DOUBLE,endian="big")
  } else if ( tipe == TCODE_DOUBLE_1 ) {
    len <- readBin(socketIn,what=RTYPE_INT,endian="big")
    readBin(socketIn,n=len,what=RTYPE_DOUBLE,endian="big")
  } else if ( tipe == TCODE_CHARACTER_0 ) {
    len <- readBin(socketIn,what=RTYPE_INT,endian="big")
    r <- readBin(socketIn,n=len,what=RTYPE_RAW,endian="big")
    iconv(rawToChar(r),from="UTF-8")
  } else if ( tipe == TCODE_CHARACTER_1 ) {

  } else stop(paste0("Unsupported type: ",tipe))
}

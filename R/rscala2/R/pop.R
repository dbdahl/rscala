#' @export
#' 
pop <- function(bridge) {
  writeBin(PCODE_POP,bridge[["socketOut"]],endian="big")
  tipe <- readBin(bridge[["socketIn"]],what=RTYPE_INT,endian="big")
  if ( tipe == TCODE_INT_0 ) {
    readBin(bridge[["socketIn"]],what=RTYPE_INT,endian="big")
  } else if ( tipe == TCODE_INT_1 ) {
    len <- readBin(bridge[["socketIn"]],what=RTYPE_INT,endian="big")
    readBin(bridge[["socketIn"]],n=len,what=RTYPE_INT,endian="big")
  } else if ( tipe == TCODE_DOUBLE_0 ) {
    readBin(bridge[["socketIn"]],what=RTYPE_DOUBLE,endian="big")
  } else if ( tipe == TCODE_DOUBLE_1 ) {
    len <- readBin(bridge[["socketIn"]],what=RTYPE_INT,endian="big")
    readBin(bridge[["socketIn"]],n=len,what=RTYPE_DOUBLE,endian="big")
  } else if ( tipe == TCODE_STRING_0 ) {
    len <- readBin(bridge[["socketIn"]],what=RTYPE_INT,endian="big")
    r <- readBin(bridge[["socketIn"]],n=len,what=RTYPE_RAW,endian="big")
    iconv(rawToChar(r),from="UTF-8")
  } else if ( tipe == TCODE_STRING_1 ) {

  } else stop(paste0("Unsupported type: ",tipe))
}

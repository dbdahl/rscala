#' @export
#' 
pop <- function(socketIn) {
  tipe <- rbyte(socketIn)
  if ( tipe == TCODE_INT_0 ) {
    rb(socketIn,RTYPE_INT)
  } else if ( tipe == TCODE_INT_1 ) {
    len <- rb(socketIn,RTYPE_INT)
    rb(socketIn,RTYPE_INT,len)
  } else if ( tipe == TCODE_DOUBLE_0 ) {
    rb(socketIn,RTYPE_DOUBLE)
  } else if ( tipe == TCODE_DOUBLE_1 ) {
    len <- rb(socketIn,RTYPE_INT)
    rb(socketIn,RTYPE_DOUBLE,len)
  } else if ( tipe == TCODE_CHARACTER_0 ) {
    len <- rb(socketIn,RTYPE_INT)
    r <- rb(socketIn,RTYPE_RAW,len)
    iconv(rawToChar(r),from="UTF-8")
  } else if ( tipe == TCODE_CHARACTER_1 ) {

  } else stop(paste0("Unsupported type: ",tipe))
}

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

rbyte <- function(c) {
  while ( TRUE ) {
    x <- readBin(c,RTYPE_RAW,endian="big")
    if ( length(x) > 0 ) return(x)
  }
}

rb <- function(c,v,n=1L) {
  tryCatch({
    r <- readBin(c,v,n,endian="big")
    if ( length(r) == n ) r
    else {
      counter <- 0L
      while ( length(r) != n ) {
        if ( counter >= 100 ) stop("Connection isn't providing data.")
        counter <- counter + 1L
        r <- c(r,readBin(c,v,n-length(r),endian="big"))
      }
      r
    }
  },error=function(e) {
    stop("The bridge is invalid.")
  })
}


wb <- function(con,x) {
  writeBin(x,con,endian="big")
}

wc <- function(con,x) {
  bytes <- charToRaw(iconv(x,to="UTF-8"))
  writeBin(length(bytes),con,endian="big")
  writeBin(bytes,con,endian="big")
}

rbyte <- function(con) {
  while ( TRUE ) {
    x <- readBin(con,RTYPE_RAW,endian="big")
    if ( length(x) > 0 ) return(x)
    browser()
  }
}

rb <- function(con,v,n=1L) {
  tryCatch({
    r <- readBin(con,v,n,endian="big")
    if ( length(r) == n ) r
    else {
      browser()
      counter <- 0L
      while ( length(r) != n ) {
        if ( counter >= 100 ) stop("Connection isn't providing data.")
        counter <- counter + 1L
        r <- c(r,readBin(con,v,n-length(r),endian="big"))
      }
      r
    }
  },error=function(e) {
    stop("The bridge is invalid.")
  })
}
  
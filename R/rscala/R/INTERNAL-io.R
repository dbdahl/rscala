wb <- function(con,x) {
  writeBin(x,con,endian="big")
}

wc <- function(con,x) {
  bytes <- charToRaw(iconv(x,to="UTF-8"))
  writeBin(length(bytes),con,endian="big")
  writeBin(bytes,con,endian="big")
}

rbyte <- function(con) {
  counter <- 0L
  while ( TRUE ) {
    x <- tryCatch({
      readBin(con,RTYPE_RAW,endian="big")
    },interrupt=function(e) {
      TCODE_INTERRUPTED
    })
    if ( length(x) > 0 ) return(x)
    counter <- counter + 1L
    if ( ( counter %% 100L == 0L ) && ( scalaIsDead(con,counter) ) ) stop("Scala seems to have died.")
  }
}

rc <- function(con) {
  len <- rb(con,RTYPE_INT)
  r <- rb(con,RTYPE_RAW,len)
  iconv(rawToChar(r),from="UTF-8") 
}

rb <- function(con,v,n=1L) {
  tryCatch({
    r <- readBin(con,v,n,endian="big")
    if ( length(r) == n ) r
    else {
      counter <- 0L
      while ( length(r) != n ) {
        counter <- counter + 1L
        if ( ( counter %% 100L == 0L ) && ( scalaIsDead(con,counter) ) ) stop("Scala seems to have died.")
        r <- c(r,readBin(con,v,n-length(r),endian="big"))
      }
      r
    }
  },error=function(e) {
    stop("The bridge is invalid.")
  })
}

scalaIsDead <- function(con,counter) {
  pidOfScala <- attr(con,"pidOfScala")
  if ( .Platform$OS.type == "unix" ) {
    tryCatch(length(system2("ps",c("-o","pid=","-p",pidOfScala),stdout=TRUE)) == 0, warning=function(e) TRUE)
  } else if ( .Platform$OS.type == "windows" ) {
    counter >= 1000L
  } else stop("Unsupported OS.")
}


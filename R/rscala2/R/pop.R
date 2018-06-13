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
    rc(socketInt)
  } else if ( tipe == TCODE_CHARACTER_1 ) {
  } else if ( tipe == TCODE_UNIT ) {
    invisible()
  } else if ( tipe == TCODE_REFERENCE ) {
    referenceType <- rc(socketIn)
    structure(list2env(list(type=referenceType),parent=emptyenv()),class="rscalaReference")
  } else if ( tipe == TCODE_ERROR_DEF ) {
    code <- rc(socketIn)
    stop(paste0("Compilation error. Code is:\n",code))
  } else if ( tipe == TCODE_ERROR_INVOKE ) {
    stop("Invocation error.")  
  } else stop(paste0("Unsupported type: ",tipe))
}

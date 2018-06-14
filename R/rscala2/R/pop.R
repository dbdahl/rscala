pop <- function(details) {
  socketIn <- details[["socketIn"]]
  tipe <- rbyte(socketIn)
  if ( tipe == TCODE_INT_0 ) {
    rb(socketIn,RTYPE_INT)
  } else if ( tipe == TCODE_INT_1 ) {
    len <- rb(socketIn,RTYPE_INT)
    rb(socketIn,RTYPE_INT,len)
  } else if ( tipe == TCODE_INT_2 ) {
    dim <- rb(socketIn,RTYPE_INT,2L)
    matrix(rb(socketIn,RTYPE_INT,prod(dim)),nrow=dim[1],byrow=TRUE)
  } else if ( tipe == TCODE_DOUBLE_0 ) {
    rb(socketIn,RTYPE_DOUBLE)
  } else if ( tipe == TCODE_DOUBLE_1 ) {
    len <- rb(socketIn,RTYPE_INT)
    rb(socketIn,RTYPE_DOUBLE,len)
  } else if ( tipe == TCODE_CHARACTER_0 ) {
    rc(socketIn)
  } else if ( tipe == TCODE_CHARACTER_1 ) {
    len <- rb(socketIn,RTYPE_INT)
    sapply(seq_len(len), function(i) rc(socketIn))
  } else if  ( tipe == TCODE_LOGICAL_0 ) {
    as.logical(rb(socketIn,RTYPE_RAW))
  } else if  ( tipe == TCODE_LOGICAL_1 ) {
    len <- rb(socketIn,RTYPE_INT)
    as.logical(rb(socketIn,RTYPE_RAW,len))
  } else if ( tipe == TCODE_UNIT ) {
    invisible()
  } else if ( tipe == TCODE_REFERENCE ) {
    referenceID <- rb(socketIn,RTYPE_INT)
    referenceType <- rc(socketIn)
    env <- list2env(list(id=referenceID,type=referenceType,details=details),parent=emptyenv())
    reg.finalizer(env, details[["gcFunction"]])
    structure(env,class="rscalaReference")
  } else if ( tipe == TCODE_ERROR_DEF ) {
    code <- rc(socketIn)
    stop(paste0("Compilation error. Code is:\n",code))
  } else if ( tipe == TCODE_ERROR_INVOKE ) {
    stop("Invocation error.")  
  } else stop(paste0("Unsupported type: ",tipe))
}

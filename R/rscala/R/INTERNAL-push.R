push <- function(what, name, socketOut) {
  pcode <- if ( is.null(name) ) PCODE_PUSH_WITHOUT_NAME
  else PCODE_PUSH_WITH_NAME
  forceVector <- if ( inherits(what,"AsIs") ) {
    what <- unclass(what)
    TRUE
  } else FALSE
  if ( inherits(what, "rscalaReference") ) {
    wb(socketOut,c(pcode,TCODE_REFERENCE))
    wb(socketOut,what[["id"]])
  } else if ( is.integer(what) ) {
    if ( is.matrix(what) ) {
      dim <- dim(what)
      if ( dim[1] == 0L ) return(structure(FALSE,msg="Number of rows must be at least 1."))
      wb(socketOut,c(pcode,TCODE_INT_2))
      wb(socketOut,dim)
      what <- t(what)
      attr(what,"dim") <- NULL
      wb(socketOut,what)
    } else if ( ( ! forceVector ) && ( length(what) == 1L ) ) {
      wb(socketOut,c(pcode,TCODE_INT_0))
      wb(socketOut,what)
    } else {
      wb(socketOut,c(pcode,TCODE_INT_1))
      wb(socketOut,c(length(what),what))
    }
  } else if ( is.double(what) ) {
    if ( is.matrix(what) ) {
      dim <- dim(what)
      if ( dim[1] == 0L ) return(structure(FALSE,msg="Number of rows must be at least 1."))
      wb(socketOut,c(pcode,TCODE_DOUBLE_2))
      wb(socketOut,dim)
      what <- t(what)
      attr(what,"dim") <- NULL
      wb(socketOut,what)
    } else if ( ( ! forceVector ) && ( length(what) == 1L ) ) {
      wb(socketOut,c(pcode,TCODE_DOUBLE_0))
      wb(socketOut,what)
    } else {
      wb(socketOut,c(pcode,TCODE_DOUBLE_1))
      wb(socketOut,length(what))
      wb(socketOut,what)
    }
  } else if ( is.logical(what) ) {
    if ( is.matrix(what) ) {
      dim <- dim(what)
      if ( dim[1] == 0L ) return(structure(FALSE,msg="Number of rows must be at least 1."))
      wb(socketOut,c(pcode,TCODE_LOGICAL_2))
      wb(socketOut,dim)
      what <- t(what)
      attr(what,"dim") <- NULL
      wb(socketOut,as.raw(what))
    } else if ( ( ! forceVector ) && ( length(what) == 1L ) ) {
      wb(socketOut,c(pcode,TCODE_LOGICAL_0))
      wb(socketOut,as.raw(what))
    } else {
      wb(socketOut,c(pcode,TCODE_LOGICAL_1))
      wb(socketOut,length(what))
      wb(socketOut,as.raw(what))
    }
  } else if ( is.raw(what) ) {
    if ( is.matrix(what) ) {
      dim <- dim(what)
      if ( dim[1] == 0L ) return(structure(FALSE,msg="Number of rows must be at least 1."))
      wb(socketOut,c(pcode,TCODE_RAW_2))
      wb(socketOut,dim)
      what <- t(what)
      attr(what,"dim") <- NULL
      wb(socketOut,what)
    } else if ( ( ! forceVector ) && ( length(what) == 1L ) ) {
      wb(socketOut,c(pcode,TCODE_RAW_0))
      wb(socketOut,what)
    } else {
      wb(socketOut,c(pcode,TCODE_RAW_1))
      wb(socketOut,length(what))
      wb(socketOut,what)
    }   
  } else if ( is.character(what) ) {
    if ( is.matrix(what) ) {
      dim <- dim(what)
      if ( dim[1] == 0L ) return(structure(FALSE,msg="Number of rows must be at least 1."))
      wb(socketOut,c(pcode,TCODE_CHARACTER_2))
      wb(socketOut,dim)
      what <- t(what)
      attr(what,"dim") <- NULL
      sapply(what, function(x) wc(socketOut,x))
    } else if ( ( ! forceVector ) && ( length(what) == 1L ) ) {
      wb(socketOut,c(pcode,TCODE_CHARACTER_0))
      wc(socketOut,what)
    } else {
      wb(socketOut,c(pcode,TCODE_CHARACTER_1))
      wb(socketOut,length(what))
      sapply(what, function(x) wc(socketOut,x))
    }
  } else if ( is.null(what) ) {
    wb(socketOut,c(pcode,TCODE_UNIT))
  } else return(structure(FALSE,msg=paste0("Unsupported type: ",class(what))))
  if ( ! is.null(name) ) wc(socketOut,name)
  TRUE
}

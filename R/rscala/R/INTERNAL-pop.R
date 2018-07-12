pop <- function(details, functionArgTypes=NULL) {
  socketIn <- details[["socketIn"]]
  serializeOutput <- details[["serializeOutput"]]
  goAgain <- TRUE
  while ( goAgain ) {
    goAgain <- FALSE
    if ( serializeOutput ) cat(rc(socketIn))
    tipe <- rbyte(socketIn)
    result <- if ( tipe == TCODE_INT_0 ) {
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
    } else if ( tipe == TCODE_DOUBLE_2 ) {
      dim <- rb(socketIn,RTYPE_INT,2L)
      matrix(rb(socketIn,RTYPE_DOUBLE,prod(dim)),nrow=dim[1],byrow=TRUE)
    } else if  ( tipe == TCODE_LOGICAL_0 ) {
      as.logical(rb(socketIn,RTYPE_RAW))
    } else if  ( tipe == TCODE_LOGICAL_1 ) {
      len <- rb(socketIn,RTYPE_INT)
      as.logical(rb(socketIn,RTYPE_RAW,len))
    } else if ( tipe == TCODE_LOGICAL_2 ) {
      dim <- rb(socketIn,RTYPE_INT,2L)
      matrix(as.logical(rb(socketIn,RTYPE_RAW,prod(dim))),nrow=dim[1],byrow=TRUE)
    } else if  ( tipe == TCODE_RAW_0 ) {
      rb(socketIn,RTYPE_RAW)
    } else if  ( tipe == TCODE_RAW_1 ) {
      len <- rb(socketIn,RTYPE_INT)
      rb(socketIn,RTYPE_RAW,len)
    } else if ( tipe == TCODE_ROBJECT ) {
      len <- rb(socketIn,RTYPE_INT)
      list(value=rb(socketIn,RTYPE_RAW,len))
    } else if ( tipe == TCODE_RAW_2 ) {
      dim <- rb(socketIn,RTYPE_INT,2L)
      matrix(rb(socketIn,RTYPE_RAW,prod(dim)),nrow=dim[1],byrow=TRUE)
    } else if ( tipe == TCODE_CHARACTER_0 ) {
      rc(socketIn)
    } else if ( tipe == TCODE_CHARACTER_1 ) {
      len <- rb(socketIn,RTYPE_INT)
      result <- character(len)
      for ( i in seq_len(len) ) result[i] <- rc(socketIn)
      result
    } else if ( tipe == TCODE_CHARACTER_2 ) {
      dim <- rb(socketIn,RTYPE_INT,2L)
      result <- matrix(character(prod(dim)),nrow=dim[1])
      for ( i in seq_len(dim[1]) )  for ( j in seq_len(dim[2]) )  result[i,j] <- rc(socketIn)
      result
    } else if ( tipe == TCODE_UNIT ) {
      invisible()
    } else if ( tipe == TCODE_REFERENCE ) {
      referenceID <- rb(socketIn,RTYPE_INT)
      referenceType <- rc(socketIn)
      env <- structure(list2env(list(id=referenceID,type=referenceType,details=details),parent=emptyenv()), class="rscalaReferenceEnvironment")
      reg.finalizer(env, details[["gcFunction"]])
      func <- if ( is.null(functionArgTypes) ) {
        function(...) {
          scalaInvoke(details, "apply", list(..., env), withReference=TRUE)
        }
      } else {
        function(...) {
          args <- list(...)
          for ( i in seq_along(args) ) {
            args[[i]] <- if ( functionArgTypes[i] == "Double" ) as.double(args[[i]][1])
            else if ( functionArgTypes[i] == "Int" ) as.integer(args[[i]][1])
            else if ( functionArgTypes[i] == "Boolean" ) as.logical(args[[i]][1])
            else if ( functionArgTypes[i] == "String" ) as.character(args[[i]][1])
            else if ( functionArgTypes[i] == "Array[Double]" ) I(as.double(args[[i]]))
            else if ( functionArgTypes[i] == "Array[Int]" ) I(as.integer(args[[i]]))
            else if ( functionArgTypes[i] == "Array[Boolean]" ) I(as.logical(args[[i]]))
            else if ( functionArgTypes[i] == "Array[String]" ) I(as.character(args[[i]]))
            else args[[i]]
          }
          scalaInvoke(details, "apply", c(args, env), withReference=TRUE)
        }        
      }
      class(func) <- "rscalaReference"
      attr(func,"rscalaReferenceEnvironment")  <- env
      func
    } else if ( tipe == TCODE_ERROR_DEF ) {
      code <- rc(socketIn)
      stop(paste0("Compilation error. Code is:\n",code))
    } else if ( tipe == TCODE_ERROR_INVOKE ) {
      stop("Invocation error.")  
    } else if ( tipe == TCODE_INTERRUPTED ) {
      cat("<< computation interrupted >>\n")
      assign("interrupted",TRUE,envir=details)
      return(invisible())
    } else if ( tipe == TCODE_CALLBACK ) {
      callback(details)
      goAgain <- TRUE
    } else stop(paste0("Unsupported type: ",tipe))
  }
  assign("last",result,envir=details)
  if ( is.null(result) ) invisible() else result
}

pop <- function(details) {
  socketIn <- details[["socketIn"]]
  if ( details[["serializeOutput"]] ) cat(rc(socketIn))
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
  } else if ( tipe == TCODE_RAW_2 ) {
    dim <- rb(socketIn,RTYPE_INT,2L)
    matrix(rb(socketIn,RTYPE_RAW,prod(dim)),nrow=dim[1],byrow=TRUE)
  } else if ( tipe == TCODE_CHARACTER_0 ) {
    rc(socketIn)
  } else if ( tipe == TCODE_CHARACTER_1 ) {
    len <- rb(socketIn,RTYPE_INT)
    sapply(seq_len(len), function(i) rc(socketIn))
  } else if ( tipe == TCODE_CHARACTER_2 ) {
    dim <- rb(socketIn,RTYPE_INT,2L)
    t(sapply(seq_len(dim[1]), function(i) sapply(seq_len(dim[2]), function(j) rc(socketIn))))
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
  } else if ( tipe == TCODE_INTERRUPTED ) {
    cat("<< computation interrupted >>\n")
    assign("interrupted",TRUE,envir=details)
    return(invisible())
  } else if ( tipe == TCODE_CALLBACK ) {
    callback(details)
  } else stop(paste0("Unsupported type: ",tipe))
  assign("last",result,envir=details)
  if ( is.null(result) ) invisible() else result
}

callback <- function(details) {
  socketIn <- details[["socketIn"]]
  snippet <- rc(socketIn)
  nArgs <- rb(socketIn,RTYPE_INT)
  env <- details[["callbackEnv"]]
  args <- vector(mode="list", length=nArgs)
  while ( TRUE ) {
    argsListName <- paste0(".rs",sample.int(.Machine$integer.max,1L))
    if ( ! exists(argsListName,envir=env) ) break
  }
  for ( i in seq_len(nArgs) ) {
    snippet <- sub("%-",paste0(argsListName,"[[",i,"]]"),snippet)
    args[[i]] <- pop(details)
  }
  assign(argsListName,args,envir=env)  
  result <- tryCatch(eval(parse(text=snippet),envir=env), error=function(e) {
    cat(toString(e))
    NULL
  })
  socketOut <- details[["socketOut"]]
  rm(list=argsListName,envir=env)
  pushOkay <- push(result, NULL, socketOut)
  if ( ! identical(pushOkay,TRUE) ) {
    cat(attr(pushOkay,"msg"),"\n",sep="")
    push(NULL, NULL, socketOut)
  }
  pop(details)
}

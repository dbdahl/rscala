rServe <- function(sockets,with.callbacks,workspace=.GlobalEnv) {
  debug <- get("debug",envir=sockets[['env']])
  if ( debug ) msg(paste0("R server using environment: ",capture.output(print(workspace))))
  while ( TRUE ) {
    if ( debug ) msg("R server at top of the loop waiting for a command.")
    cmd <- rb(sockets,"integer")
    if ( ( cmd == EVAL ) || ( cmd == EVALNAKED ) ) {
      if ( debug ) msg("Got EVAL/EVALNAKED: ",cmd)
      snippet <- rc(sockets)
      if ( cmd == EVAL ) {
        output <- NULL
        file <- textConnection("output","w",local=TRUE)
        sink(file)
        sink(file,type="message")
        result <- try(eval(parse(text=snippet),envir=workspace))
        sink(type="message")
        sink()
        close(file)
      } else {
        result <- try(eval(parse(text=snippet),envir=workspace))
        output <- character()
      }
      if ( with.callbacks ) wb(sockets,EXIT)
      if ( inherits(result,"try-error") ) {
        wb(sockets,ERROR)
        msg <- paste(c(output,attr(result,"condition")$message),collapse="\n")
        wc(sockets,msg)
      } else {
        wb(sockets,OK)
        output <- paste(output,collapse="\n")
        wc(sockets,output)
      }
      assign(".rsX",result,envir=workspace)
    } else if ( cmd %in% c(SET,SET_SINGLE,SET_DOUBLE) ) {
      if ( debug ) msg("Got SET")
      if ( cmd != SET ) index <- rc(sockets)
      identifier <- rc(sockets)
      dataStructure <- rb(sockets,"integer")
      if ( dataStructure == NULLTYPE ) {
        if ( cmd == SET ) assign(identifier,NULL,envir=workspace)
        else subassign(sockets,identifier,index,NULL,cmd==SET_SINGLE,workspace)
      } else if ( dataStructure == SCALAR ) {
        dataType <- rb(sockets,"integer")
        if ( dataType == INTEGER ) value <- rb(sockets,"integer")
        else if ( dataType == DOUBLE ) value <- rb(sockets,"double")
        else if ( dataType == BOOLEAN ) value <- rb(sockets,"integer") != 0
        else if ( dataType == STRING ) value <- rc(sockets)
        else if ( dataType == BYTE ) value <- rb(sockets,"raw")
        else stop(paste("Unknown data type:",dataType))
        if ( cmd == SET ) assign(identifier,value,envir=workspace)
        else subassign(sockets,identifier,index,value,cmd==SET_SINGLE,workspace)
      } else if ( dataStructure == VECTOR ) {
        dataLength <- rb(sockets,"integer")
        dataType <- rb(sockets,"integer")
        if ( dataType == INTEGER ) value <- rb(sockets,"integer",n=dataLength)
        else if ( dataType == DOUBLE ) value <- rb(sockets,"double",n=dataLength)
        else if ( dataType == BOOLEAN ) value <- rb(sockets,"integer",n=dataLength) != 0
        else if ( dataType == STRING ) value <- sapply(seq_len(dataLength),function(i) rc(sockets))
        else if ( dataType == BYTE ) value <- rb(sockets,"raw",n=dataLength)
        else stop(paste("Unknown data type:",dataType))
        if ( cmd == SET ) assign(identifier,value,envir=workspace)
        else subassign(sockets,identifier,index,value,cmd==SET_SINGLE,workspace)
      } else if ( dataStructure == MATRIX ) {
        dataNRow <- rb(sockets,"integer")
        dataNCol <- rb(sockets,"integer")
        dataLength <- dataNRow * dataNCol
        dataType <- rb(sockets,"integer")
        if ( dataType == INTEGER ) value <- matrix(rb(sockets,"integer",n=dataLength),nrow=dataNRow,byrow=FALSE)
        else if ( dataType == DOUBLE ) value <- matrix(rb(sockets,"double",n=dataLength),nrow=dataNRow,byrow=FALSE)
        else if ( dataType == BOOLEAN ) value <- matrix(rb(sockets,"integer",n=dataLength),nrow=dataNRow,byrow=FALSE) != 0
        else if ( dataType == STRING ) value <- matrix(sapply(seq_len(dataLength),function(i) rc(sockets)),nrow=dataNRow,byrow=FALSE)
        else if ( dataType == BYTE ) value <- matrix(rb(sockets,"raw",n=dataLength),nrow=dataNRow,byrow=FALSE)
        else stop(paste("Unknown data type:",dataType))
        if ( cmd == SET ) assign(identifier,value,envir=workspace)
        else subassign(sockets,identifier,index,value,cmd==SET_SINGLE,workspace)
      } else if ( dataStructure == UNSUPPORTED_STRUCTURE ) {
        # Don't do anything more.
      } else stop("Protocol error.")
    } else if ( cmd == GET ) {
      if ( debug ) msg("Got GET")
      identifier <- rc(sockets)
      value <- tryCatch(get(identifier,envir=workspace),error=function(e) e)
      if ( is.null(value) ) {
        wb(sockets,NULLTYPE)
      } else if ( inherits(value,"error") ) {
        wb(sockets,UNDEFINED_IDENTIFIER)
      } else if ( ! is.atomic(value) ) {
        wb(sockets,UNSUPPORTED_STRUCTURE)
      } else {
        asScalar <- if ( inherits(value,"AsIs") ) {
          value <- as.vector(value)
          FALSE
        } else length(value) == 1
        if ( is.vector(value) ) {
          type <- checkType(value)
          if ( asScalar ) {
            wb(sockets,SCALAR)
          } else {
            wb(sockets,VECTOR)
            wb(sockets,length(value))
          }
          wb(sockets,type)
          if ( type == STRING ) {
            if ( length(value) > 0 ) for ( i in seq_len(length(value)) ) wc(sockets,value[i])
          } else {
            if ( type == BOOLEAN ) wb(sockets,as.integer(value))
            else wb(sockets,value)
          }
        } else if ( is.matrix(value) ) {
          type <- checkType(value)
          wb(sockets,MATRIX)
          wb(sockets,dim(value))
          wb(sockets,type)
          dim(value) <- NULL
          if ( type == STRING ) {
            if ( length(value) > 0 ) for ( i in seq_len(length(value)) ) wc(sockets,value[i])
          } else if ( type == BOOLEAN ) wb(sockets,as.integer(value))
          else wb(sockets,value)
        } else {
          wb(sockets,UNSUPPORTED_STRUCTURE)
        }
      }
    } else if ( cmd == GET_REFERENCE ) {
      if ( debug ) msg("Got GET_REFERENCE")
      identifier <- rc(sockets)
      value <- tryCatch(get(identifier,envir=workspace),error=function(e) e)
      if ( inherits(value,"error") ) {
        wb(sockets,UNDEFINED_IDENTIFIER)
      } else {
        wb(sockets,REFERENCE)
        if ( inherits(value,"ScalaAsIs") ) value <- value$original
        wc(sockets,uniqueName(value,sockets[['r']],""))
      }
    } else if ( cmd == FREE ) {
      if ( debug ) msg("Got FREE")
      dataLength <- rb(sockets,"integer")
      what <- sapply(seq_len(dataLength),function(i) rc(sockets))
      rm(list=what,envir=sockets[['r']])
    } else if ( cmd == SHUTDOWN ) {
      if ( debug ) msg("Got SHUTDOWN")
      return()
    } else if ( cmd == EXIT ) {
      if ( debug ) msg("Got EXIT")
      return()
    } else if ( cmd == PING ) {
      if ( debug ) msg("Got PING")
      wb(sockets,OK)
    } else stop(paste("Unknown command:",cmd))
    flush(sockets[['socketIn']])
  }
}

subassign <- function(sockets,x,i,value,single,workspace) {
  assign(".rscala.set.value",value,envir=workspace)
  brackets <- if ( single ) c("[","]") else c("[[","]]")
  output <- NULL
  file <- textConnection("output","w",local=TRUE)
  sink(file)
  sink(file,type="message")
  result <- try(eval(parse(text=paste0(x,brackets[1],i,brackets[2]," <- .rscala.set.value")),envir=workspace))
  sink(type="message")
  sink()
  close(file)
  if ( inherits(result,"try-error") ) {
    wb(sockets,ERROR)
    output <- paste(paste(output,collapse="\n"),paste(attr(result,"condition")$message,collapse="\n"),sep="\n")
    wc(sockets,output)
  } else {
    wb(sockets,OK)
  }
  rm(".rscala.set.value",envir=workspace)
  invisible(value)
}


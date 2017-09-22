## Scala scripting over TCP/IP

scala <- function(classpath=character(),classpath.packages=character(),serialize.output=.Platform$OS.type=="windows",scala.home=NULL,heap.maximum=NULL,command.line.options=NULL,row.major=TRUE,timeout=60,debug=FALSE,stdout=TRUE,stderr=TRUE,port=0,scalaInfo=NULL,major.release=c("2.10","2.11","2.12")) {
  if ( identical(stdout,TRUE) ) stdout <- ""
  if ( identical(stderr,TRUE) ) stderr <- ""
  debug <- identical(debug,TRUE)
  serialize.output <- identical(serialize.output,TRUE)
  row.major <- identical(row.major, TRUE)
  port <- as.integer(port[1])
  if ( debug && serialize.output ) stop("When debug is TRUE, serialize.output must be FALSE.")
  if ( debug && ( identical(stdout,FALSE) || identical(stdout,NULL) || identical(stderr,FALSE) || identical(stderr,NULL) ) ) stop("When debug is TRUE, stdout and stderr must not be discarded.")
  sInfo <- if ( is.null(scalaInfo) ) scalaInfo(scala.home, major.release) else scalaInfo
  if ( is.null(sInfo) ) stop('Please run "rscala::scalaInstall()" or install Scala manually.')
  pkgJars <- unlist(lapply(classpath.packages, function(p) jarsOfPackage(p, sInfo$major.release)))
  userJars <- c(unlist(strsplit(classpath,.Platform$path.sep)),pkgJars)
  if ( is.null(command.line.options) ) {
    command.line.options <- getOption("rscala.command.line.options",default=NULL)
    if ( is.null(command.line.options) ) {
      if ( is.null(heap.maximum) ) {
        heap.maximum <- getOption("rscala.heap.maximum",default=NULL)
      }
      if ( !is.null(heap.maximum) ) {
        command.line.options <- paste("-J",c(paste("-Xmx",heap.maximum,sep=""),"-Xms32M"),sep="")
      }
    }
  }
  rsJar <- .rscalaJar(sInfo$version)
  rsClasspath <- shQuote(paste(c(rsJar,userJars),collapse=.Platform$path.sep))
  command.line.options <- shQuote(command.line.options)
  portsFilename <- tempfile("rscala-")
  args <- c(command.line.options,paste0("-Drscala.classpath=",rsClasspath),"-classpath",rsClasspath,"org.ddahl.rscala.server.Main",portsFilename,debug,serialize.output,row.major,port)
  if ( debug ) msg("\n",sInfo$cmd)
  if ( debug ) msg("\n",paste0("<",args,">",collapse="\n"))
  system2(sInfo$cmd,args,wait=FALSE,stdout=stdout,stderr=stderr)
  sockets <- newSockets(portsFilename,debug,serialize.output,row.major,timeout)
  sInfo$classpath <- rsClasspath
  sInfo$command.line.options <- command.line.options
  scalaSettings(sockets,interpolate=TRUE,info=sInfo)
  sockets
}

newSockets <- function(portsFilename,debug,serialize.output,row.major,timeout) {
  env <- new.env(parent=emptyenv())
  assign("open",TRUE,envir=env)
  assign("debug",debug,envir=env)
  assign("rowMajor",row.major,envir=env)
  assign("serializeOutput",serialize.output,envir=env)
  functionCache <- new.env(parent=emptyenv())
  references <- new.env(parent=emptyenv())
  garbage <- new.env(parent=emptyenv())
  ports <- local({
    delay <- 0.1
    start <- proc.time()[3]
    while ( TRUE ) {
      if ( ( proc.time()[3] - start ) > timeout ) stop("Timed out waiting for Scala to start.")
      Sys.sleep(delay)
      delay <- 1.0*delay
      if ( file.exists(portsFilename) ) {
        line <- scan(portsFilename,n=2,what=character(),quiet=TRUE)
        if ( length(line) > 0 ) return(as.numeric(line))
      }
    }
  })
  file.remove(portsFilename)
  if ( debug ) msg("Trying to connect to port ",paste(ports,collapse=" "))
  socketConnectionIn  <- socketConnection(port=ports[1],blocking=TRUE,open="ab",timeout=2678400)
  socketConnectionOut <- socketConnection(port=ports[2],blocking=TRUE,open="rb",timeout=2678400)
  if ( debug ) msg("Connected")
  result <- list(socketIn=socketConnectionIn,socketOut=socketConnectionOut,env=env,
                 functionCache=functionCache,r=references,garbage=garbage,
                 garbageFunction=function(e) uniqueName(e[['identifier']],garbage,""))
  class(result) <- "ScalaInterpreter"
  status <- rb(result,"integer")
  if ( ( length(status) == 0 ) || ( status != OK ) ) stop("Error instantiating interpreter.")
  result
}

scalaEval <- function(interpreter,snippet,workspace) {
  debug <- get("debug",envir=interpreter[['env']])
  if ( debug ) msg(paste0('Sending EVAL request using environment:',capture.output(print(workspace))))
  snippet <- paste(snippet,collapse="\n")
  tryCatch({
    wb(interpreter,EVAL)
    wc(interpreter,snippet)
    flush(interpreter[['socketIn']])
    rServe(interpreter,TRUE,workspace)
    status <- rb(interpreter,"integer")
    if ( get("serializeOutput",envir=interpreter[['env']]) ) echoResponseScala(interpreter)
  }, interrupt = function(x) {
    assign("open",FALSE,envir=interpreter[['env']])
    stop("## Interpreter closed by interrupt. ##")
  })
  if ( ( length(status) == 0 ) || ( status != OK ) ) stop("Error in evaluation.")
  else invisible(NULL)
}

strintrpltIf <- function(snippet,env,interpreter) {
  if ( get("interpolate",envir=interpreter[['env']]) ) strintrplt(snippet,env) else snippet
}

'%~%.ScalaInterpreter'  <- function(interpreter,snippet) scalaEvalGet(interpreter,snippet,NA)
'%.~%.ScalaInterpreter' <- function(interpreter,snippet) scalaEvalGet(interpreter,snippet,TRUE)

scalaEvalGet <- function(interpreter,snippet,as.reference) {
  cc(interpreter)
  snippet <- strintrpltIf(paste(snippet,collapse="\n"),parent.frame(2),interpreter)
  result <- evalAndGet(interpreter,snippet,as.reference,parent.frame(2))
  if ( is.null(result) ) invisible(result)
  else result
}

'%@%.ScalaInterpreter' <- function(interpreter,snippet) {
  cc(interpreter)
  snippet <- strintrpltIf(paste(snippet,collapse="\n"),parent.frame(),interpreter)
  scalaEval(interpreter,snippet,parent.frame())
}

print.ScalaInterpreter <- function(x,...) {
  cat("ScalaInterpreter\n")
  invisible(x)
}

toString.ScalaInterpreter <- function(x,...) {
  "ScalaInterpreter"
}

print.ScalaInterpreterReference <- function(x,...) {
  type <- x[['type']]
  # scalap(x[['interpreter']],type)
  cat("ScalaInterpreterReference... ")
  cat(x[['identifier']],": ",type,"\n",sep="")
  invisible(x)
}

toString.ScalaInterpreterReference <- function(x,...) {
  x[['identifier']]
}

print.ScalaCachedReference <- function(x,...) {
  type <- x[['type']]
  # scalap(x[['interpreter']],type)
  cat("ScalaCachedReference... ")
  cat("*: ",type,"\n",sep="")
  invisible(x)
}

toString.ScalaCachedReference <- function(x,...) {
  x[['identifier']]
}

print.ScalaInterpreterItem <- function(x,...) {
  # scalap(x[['interpreter']],x[['snippet']])
  cat(toString(x),"\n",sep="")
  invisible(x)
}

toString.ScalaInterpreterItem <- function(x,...) {
  paste0("ScalaInterpreterItem for ",x[['snippet']])
}

scalaGet <- function(interpreter,identifier,as.reference) {
  tryCatch({
    if ( ( ! is.na(as.reference) ) && ( as.reference ) ) {
      if ( inherits(identifier,"ScalaInterpreterReference") || inherits(identifier,"ScalaCachedReference") ) return(identifier)
      if ( get("debug",envir=interpreter[['env']]) ) msg('Sending GET_REFERENCE request.')
      wb(interpreter,GET_REFERENCE)
      wc(interpreter,as.character(identifier[1]))
      flush(interpreter[['socketIn']])
      response <- rb(interpreter,"integer")
      if ( response == OK ) {
        id <- rc(interpreter)
        type <- rc(interpreter)
        if ( get("serializeOutput",envir=interpreter[['env']]) ) echoResponseScala(interpreter)
        if ( substr(id,1,1) == "." ) {
          result <- list2env(list(interpreter=interpreter,identifier=id,type=type),parent=globalenv())
          class(result) <- "ScalaCachedReference"
          reg.finalizer(result,interpreter[['garbageFunction']])
        } else {
          result <- list(interpreter=interpreter,identifier=id,type=type)
          class(result) <- "ScalaInterpreterReference"
        }
        return(result)
      } else if ( response == NULLTYPE ) {
        return(NULL)
      } else if ( response == ERROR ) {
        if ( get("serializeOutput",envir=interpreter[['env']]) ) echoResponseScala(interpreter)
        stop(paste("Exception thrown.",sep=""))
      } else if ( response == UNDEFINED_IDENTIFIER ) {
        if ( get("serializeOutput",envir=interpreter[['env']]) ) echoResponseScala(interpreter)
        stop(paste("Undefined identifier: ",identifier[1],sep=""))
      } else stop(paste0("Protocol error.  Got: ",response))
    }
    i <- if ( inherits(identifier,"ScalaInterpreterReference") || inherits(identifier,"ScalaCachedReference") ) {
      identifier[['identifier']]
    } else {
      as.character(identifier[1])
    }
    if ( get("debug",envir=interpreter[['env']]) ) msg('Sending GET request.')
    wb(interpreter,GET)
    wc(interpreter,i)
    flush(interpreter[['socketIn']])
    dataStructure <- rb(interpreter,"integer")
    value <- if ( dataStructure == NULLTYPE ) {
      NULL
    } else if ( dataStructure == SCALAR ) {
      dataType <- rb(interpreter,"integer")
      if ( dataType == STRING ) {
        rc(interpreter)
      } else {
        v <- rb(interpreter,typeMap[[dataType]])
        if ( dataType == BOOLEAN ) as.logical(v)
        else v
      }
    } else if ( dataStructure == VECTOR ) {
      length <- rb(interpreter,"integer")
      dataType <- rb(interpreter,"integer")
      if ( dataType == STRING ) {
        if ( length > 0 ) sapply(seq_len(length),function(x) rc(interpreter))
        else character(0)
      } else {
        v <- rb(interpreter,typeMap[[dataType]],length)
        if ( dataType == BOOLEAN ) as.logical(v)
        else v
      }
    } else if ( dataStructure == MATRIX ) {
      dim <- rb(interpreter,"integer",2L)
      dataType <- rb(interpreter,"integer")
      if ( dataType == STRING ) {
        v <- matrix("",nrow=dim[1],ncol=dim[2])
        if ( dim[2] > 0 ) for ( j in seq_len(dim[2]) ) {
          if ( dim[1] > 0 ) for ( i in seq_len(dim[1]) ) {
            v[i,j] <- rc(interpreter)
          }
        }
        v
      } else {
        v <- matrix(rb(interpreter,typeMap[[dataType]],dim[1]*dim[2]),nrow=dim[1],byrow=FALSE)
        if ( dataType == BOOLEAN ) mode(v) <- "logical"
        v
      }
    } else if ( dataStructure == ERROR ) {
      if ( get("serializeOutput",envir=interpreter[['env']]) ) echoResponseScala(interpreter)
      stop(paste("Exception thrown.",sep=""))
    } else if ( dataStructure == UNDEFINED_IDENTIFIER ) {
      if ( get("serializeOutput",envir=interpreter[['env']]) ) echoResponseScala(interpreter)
      stop(paste("Undefined identifier: ",i,sep=""))
    } else if ( dataStructure == UNSUPPORTED_STRUCTURE ) {
      if ( is.na(as.reference) ) {
        if ( get("serializeOutput",envir=interpreter[['env']]) ) echoResponseScala(interpreter)
        return(scalaGet(interpreter,identifier,TRUE))
      } else {
        if ( get("serializeOutput",envir=interpreter[['env']]) ) echoResponseScala(interpreter)
        stop("Unsupported data structure.")
      }
    } else stop(paste0("Protocol error.  Got: ",dataStructure))
    if ( get("serializeOutput",envir=interpreter[['env']]) ) echoResponseScala(interpreter)
    value
  }, interrupt = function(x) {
    assign("open",FALSE,envir=interpreter[['env']])
    stop("## Interpreter closed by interrupt. ##")
  })
}

scalaNull <- function(type) {
  structure(list(identifier='null',type=type), class="ScalaNullReference")
}

'$.ScalaInterpreter' <- function(interpreter,identifier) {
  cc(interpreter)
  if ( identifier == "def" ) function(...) {
    warning(paste0("Syntax \"s$def(...) %~% '// Scala code'\" is deprecated and will be removed.  Use \"function(...) s %!% '// Scala code'\" instead."))
    scalaFunctionArgs(.INTERPRETER=interpreter,...)
  } else if ( identifier == "null" ) function(type) {
    warning(paste0("This syntax is deprecated and will be removed.  Use the 'scalaNull' function instead."))
    result <- list(interpreter=interpreter,identifier='null',type=type)
    class(result) <- "ScalaInterpreterReference"
    result
  } else if ( identifier == "do" ) function(snippet) {
    # warning(paste0("Syntax \"s$do('",snippet,"')\" is deprecated and will be removed.  Use \"s$.",snippet,"\" instead."))
    result <- list(interpreter=interpreter,snippet=snippet)
    class(result) <- "ScalaInterpreterItem"
    result
  } else if ( identifier == "var" ) function(x) {
    if ( inherits(x,"ScalaCachedReference") && ( x[['type']] == "org.ddahl.rscala.PersistentReference" ) ) {
      get(x$name(),envir=x[['interpreter']][['r']])
    } else if ( is.character(x) && ( length(x) == 1 ) ) {
      get(x,envir=interpreter[['r']])
    } else stop("Argument must be a persistent R reference or a string ID of the persistent R reference.")
  } else if ( identifier == "val" ) function(x) {
    # warning(paste0("Syntax \"s$val()\" is deprecated and will be removed."))
    scalaGet(interpreter,x,NA)
  } else if ( identifier == ".val" ) function(x) {
    # warning(paste0("Syntax \"s$.val()\" is deprecated and will be removed."))
    scalaGet(interpreter,x,TRUE)
  } else if ( substr(identifier,1,1) == "." ) {
    structure(list(interpreter=interpreter,snippet=substring(identifier,2)),class="ScalaInterpreterItem")
  } else {
    scalaGet(interpreter,identifier,NA)
  }
}

scalaSet <- function(interpreter,identifier,value) {
  tryCatch({
    if ( inherits(value,"ScalaInterpreterReference") || inherits(value,"ScalaCachedReference") ) {
      if ( get("debug",envir=interpreter[['env']]) ) msg("Sending SET request.")
      wb(interpreter,SET)
      wc(interpreter,identifier)
      wb(interpreter,REFERENCE)
      wc(interpreter,value[['identifier']])
      flush(interpreter[['socketIn']])
      status <- rb(interpreter,"integer")
      if ( get("serializeOutput",envir=interpreter[['env']]) ) echoResponseScala(interpreter)
      if ( status == ERROR ) {
        stop("Setting error.")
      }
    } else {
      if ( ! is.atomic(value) ) stop("Data structure is not supported.")
      asScalar <- if ( inherits(value,"AsIs") ) {
        value <- as.vector(value)
        FALSE
      } else length(value) == 1
      if ( is.vector(value) ) {
        type <- checkType(value)
        if ( get("debug",envir=interpreter[['env']]) ) msg("Sending SET request.")
        wb(interpreter,SET)
        wc(interpreter,identifier)
        if ( asScalar ) {
          wb(interpreter,SCALAR)
        } else {
          wb(interpreter,VECTOR)
          wb(interpreter,length(value))
        }
        wb(interpreter,type)
        if ( type == STRING ) {
          if ( length(value) > 0 ) for ( i in seq_len(length(value)) ) wc(interpreter,value[i])
        } else {
          if ( type == BOOLEAN ) wb(interpreter,as.integer(value))
          else wb(interpreter,value)
        }
      } else if ( is.matrix(value) ) {
        type <- checkType(value)
        if ( get("debug",envir=interpreter[['env']]) ) msg("Sending SET request.")
        wb(interpreter,SET)
        wc(interpreter,identifier)
        wb(interpreter,MATRIX)
        wb(interpreter,dim(value))
        wb(interpreter,type)
        dim(value) <- NULL
        if ( type == STRING ) {
          if ( length(value) > 0 ) for ( i in seq_len(length(value)) ) wc(interpreter,value[i])
        } else if ( type == BOOLEAN ) wb(interpreter,as.integer(value))
        else wb(interpreter,value)
      } else if ( is.null(value) ) {
        if ( get("debug",envir=interpreter[['env']]) ) msg("Sending SET request.")
        wb(interpreter,SET)
        wc(interpreter,identifier)
        wb(interpreter,NULLTYPE)
      } else stop("Data structure is not supported.")
      flush(interpreter[['socketIn']])
      if ( get("serializeOutput",envir=interpreter[['env']]) ) echoResponseScala(interpreter)
    }
  }, interrupt = function(x) {
    assign("open",FALSE,envir=interpreter[['env']])
    stop("## Interpreter closed by interrupt. ##")
  })
  invisible()
}

'%!%.ScalaInterpreter'  <- function(interpreter,snippet) scalaDef(interpreter,snippet,NA)
'%.!%.ScalaInterpreter' <- function(interpreter,snippet) scalaDef(interpreter,snippet,TRUE)

scalaDef <- function(interpreter,snippet,as.reference) {
  pf <- parent.frame(2)
  snippet <- strintrpltIf(snippet,pf,interpreter)
  argsFormals <- as.list(formals(sys.function(-3)))
  argsFormals <- argsFormals[intersect(names(argsFormals),ls(envir=pf))]
  argsValues <- if ( length(argsFormals) > 0 ) {
    mget(names(argsFormals),envir=pf)
  } else {
    list()
  }
  if ( length(argsFormals) != length(argsValues) ) stop('A Scala function cannot have R code that defines new variables.')
  evaluate <- ! exists(".SCALA.OPTIMIZE", envir = parent.frame(3))
  func1 <- do.call(scalaFunctionArgs,c(list(.INTERPRETER=interpreter),argsFormals))
  func2 <- scalaMkFunction(func1,snippet,as.reference=as.reference,pf)
  if ( evaluate ) do.call(func2,argsValues)
  else func2
}

scalaOptimize <- function(scalaFunction) {
  .SCALA.OPTIMIZE <- TRUE
  scalaFunction()
}

'$<-.ScalaInterpreter' <- function(interpreter,identifier,value) {
  cc(interpreter)
  scalaSet(interpreter,identifier,value)
  interpreter
}

scalaFunctionArgs <- function(.INTERPRETER,...) {
  cc(.INTERPRETER)
  argsValues <- list(...)
  argsIdentifiers <- names(argsValues)
  if ( is.null(argsIdentifiers) ) {
    argsIdentifiers <- paste0(rep('x',length(argsValues)),seq_along(argsValues))
  } else if ( any(argsIdentifiers=='' ) ) {
    w <- argsIdentifiers==''
    argsIdentifiers[w] <- paste0(rep('x',sum(w)),seq_along(argsValues[argsIdentifiers=='']))
  }
  if ( length(unique(argsIdentifiers)) != length(argsIdentifiers) ) stop('Argument names must be unique.')
  header <- character(length(argsValues))
  for ( i in seq_along(argsValues) ) {
    value <- argsValues[[i]]
    name <- argsIdentifiers[[i]]
    if ( is.null(value) ) {
      header[i] <- paste0('val ',name,' = EphemeralReference("',name,'")')
    } else if ( inherits(value,"ScalaInterpreterReference") || inherits(value,"ScalaCachedReference") || inherits(value,"ScalaNullReference")) {
      header[i] <- paste0('val ',name,' = R.cached(R.evalS0("toString(',name,')")).asInstanceOf[',value[['type']],']')
    } else {
      if ( ( ! is.atomic(value) ) || is.null(value) ) stop(paste0('Type of "',name,'" is not supported.'))
      type <- checkType3(value)
      len <- if ( inherits(value,"AsIs") ) 1
      else if ( is.vector(value) ) {
        if ( length(value) == 1 ) 0 else 1
      } else if ( is.matrix(value) ) 2
      else stop(paste0('Type of "',name,'" is not supported.'))
      header[i] <- paste0('val ',name,' = R.get',type,len,'("',name,'")')
    }
  }
  result <- list(interpreter=.INTERPRETER,identifiers=argsIdentifiers,header=header)
  class(result) <- 'ScalaFunctionArgs'
  result
}

'%~%.ScalaFunctionArgs' <- function(interpreter,snippet) {
  # Deprecated
  snippet <- strintrpltIf(snippet,parent.frame(),interpreter$interpreter)
  scalaMkFunction(interpreter,snippet,as.reference=NA,parent.frame())
}

'%.~%.ScalaFunctionArgs' <- function(interpreter,snippet) {
  # Deprecated
  snippet <- strintrpltIf(snippet,parent.frame(),interpreter$interpreter)
  scalaMkFunction(interpreter,snippet,as.reference=TRUE,parent.frame())
}

scalaMkFunction <- function(func,body,as.reference,workspace) {
  interpreter <- func$interpreter
  body <- paste(body,collapse="\n")
  fullBody <- paste0(c(func$header,body),collapse='\n')
  if ( ! exists(fullBody,envir=interpreter[['functionCache']]) ) {
    snippet <- paste0('() => {\n',fullBody,'}')
    function0 <- evalAndGet(interpreter,snippet,TRUE,workspace)
    functionIdentifier <- function0[["identifier"]] 
    functionReturnType <- substring(function0[['type']],7)  # Drop off the leading '() => '.
    wb(interpreter,DEF)
    wc(interpreter,functionIdentifier)
    wc(interpreter,functionReturnType)
    flush(interpreter[['socketIn']])
    status <- rb(interpreter,"integer")
    if ( get("serializeOutput",envir=interpreter[['env']]) ) echoResponseScala(interpreter)
    if ( status != OK ) {
      stop("Problem caching function.")
    }
    assign(fullBody,list(functionIdentifier=functionIdentifier,functionReturnType=functionReturnType),envir=func$interpreter[['functionCache']])
  }
  funcList <- get(fullBody,envir=func$interpreter[['functionCache']])
  interpreterName <- uniqueName(interpreter, workspace, ".rsW")
  functionSnippet <- strintrplt('
    function(@{paste(func$identifiers,collapse=", ")}) {
      .rsI <- @{interpreterName}
      tryCatch({
        .rsWorkspace <- environment()
        @{ifelse(get("debug",envir=interpreter[["env"]]),\'rscala:::msg(paste("Evaluating Scala function from environment",capture.output(print(.rsWorkspace))))\',"")}
        rscala:::wb(.rsI,rscala:::INVOKE)
        rscala:::wc(.rsI,"@{funcList$functionIdentifier}")
        flush(.rsI[["socketIn"]])
        rscala:::rServe(.rsI,TRUE,.rsWorkspace)
        .rsStatus <- rscala:::rb(.rsI,"integer")
        @{ifelse(get("serializeOutput",envir=interpreter[["env"]]),"rscala:::echoResponseScala(.rsI)","")}
        if ( .rsStatus == rscala:::ERROR ) {
          stop("Invocation error.")
        } else {
          .rsResult <- rscala:::scalaGet(.rsI,"?",@{as.reference})
          if ( is.null(.rsResult) ) invisible(.rsResult)
          else .rsResult
        }
      }, interrupt = function(x) {
        assign("open",FALSE,envir=.rsI[["env"]])
        stop("## Interpreter closed by interrupt. ##")
      })
    }
  ')
  functionDefinition <- eval(parse(text=functionSnippet),envir=workspace)
  attr(functionDefinition,"identifiers") <- func$identifiers
  attr(functionDefinition,"scalaHeader") <- func$header
  attr(functionDefinition,"scalaBody") <- body
  attr(functionDefinition,"returnType") <- funcList$functionReturnType
  attr(functionDefinition,"asReference") <- as.reference
  class(functionDefinition) <- "ScalaFunction"
  functionDefinition
}

print.ScalaFunction <- function(x,...) {
  y <- formals(x)
  signature <- paste0('function(',paste0(names(y),ifelse(paste(y)=='','',paste0(' = ',y)),collapse=', '),'): ',
                      attr(x,'returnType'),' = { // Scala implementation; .AS.REFERENCE = ',attr(x,'asReference'))
  p <- pretty(attr(x,'scalaHeader'),attr(x,'scalaBody'))
  cat(signature,'\n',p,'}\n',sep='')
}

scalaAutoMkFunction <- function(reference,method) {
  if ( method == "type" ) {
    if ( inherits(reference,"ScalaInterpreterItem") ) return(reference[['snippet']])
    else return(reference[['type']])
  }
  interpreter <- reference[['interpreter']]
  function(..., .AS.REFERENCE = NA, .EVALUATE = TRUE) {
    args <- list(...)
    names <- names(args)
    if ( ! is.null(names) ) stop("Arguments should not have names.")
    names <- paste0(rep('x',length(args)),seq_along(args))
    argsString <- paste0(names,collapse=',')
    if ( nchar(argsString) > 0 ) argsString <- paste0("(",argsString,")")
    snippet <- if ( inherits(reference,"ScalaInterpreterReference") ) {
      '@{reference}.@{method}@{argsString}'
    } else if ( inherits(reference,"ScalaCachedReference") ) {
      'R.cached("@{toString(reference)}").asInstanceOf[@{reference[[\'type\']]}].@{method}@{argsString}'
    } else if ( inherits(reference,"ScalaInterpreterItem") ) {
      if ( method == 'new' ) {
        "new @{reference[['snippet']]}@{argsString}"
      } else {
        "@{reference[['snippet']]}.@{method}@{argsString}"
      }
    } else stop('Unrecognized reference type.')
    snippet <- strintrplt(snippet)
    f <- scalaMkFunction(scalaFunctionArgs(.INTERPRETER=interpreter,...),snippet,as.reference=.AS.REFERENCE,parent.frame())
    if ( .EVALUATE ) f(...)
    else f
  }
}

scalaUnboxReference <- function(x) {
  if ( ( ! inherits(x,"ScalaCachedReference") ) || ( x[['type']] != "org.ddahl.rscala.PersistentReference" ) ) {
    stop("The argument must be a Scala reference to an PersistentReference.")
  }
  get(x$name(),envir=x[['interpreter']][['r']])
}

scalaAutoMkFunction2 <- function(reference,method) {
  if ( method == "type" ) {
    if ( inherits(reference,"ScalaInterpreterItem") ) return(reference[['snippet']])
    else return(reference[['type']])
  }
  interpreter <- reference[['interpreter']]
  function(..., .AS.REFERENCE = NA) {
    args <- list(...)
    if ( ! is.null(names(args)) ) stop("Arguments should not have names.")
    workspace <- new.env(parent=parent.frame())
    headers <- character(length(args))
    for ( i in seq_len(length(args))) {
      value <- args[[i]]
      assign(paste0('$',i),value,envir=workspace)
      if ( inherits(value,"ScalaInterpreterReference") || inherits(value,"ScalaCachedReference") || inherits(value,"ScalaNullReference")) {
        headers[i] <- paste0('R.cached(R.evalS0("toString(get(\'$',i,'\'))")).asInstanceOf[',args[[i]][['type']],']')
      } else {
        if ( ( ! is.atomic(value) ) || is.null(value) ) stop(paste0('Type of argument ',i,' is not supported.'))
        type <- if ( is.integer(value) ) "I"
        else if ( is.double(value) ) "D"
        else if ( is.logical(value) ) "L"
        else if ( is.character(value) ) "S"
        else if ( is.raw(value) ) "R"
        else stop(paste0('Type of argument ',i,' is not supported.'))
        len <- if ( inherits(value,"AsIs") ) 1
        else if ( is.vector(value) ) {
          if ( length(value) == 1 ) 0 else 1
        } else if ( is.matrix(value) ) 2
        else stop(paste0('Type of argument ',i,' is not supported.'))
        headers[i] <- paste0('R.get',type,len,'("$',i,'")')
      }
    }
    wb(interpreter,INVOKE2)
    if ( inherits(reference,"ScalaInterpreterReference") )
      wc(interpreter,reference[['identifier']])
    else if ( inherits(reference,"ScalaCachedReference") )
      wc(interpreter,paste0('R.cached("',reference[['identifier']],'").asInstanceOf[',reference[['type']],']'))
    else if ( inherits(reference,"ScalaInterpreterItem") )
      wc(interpreter,reference[['snippet']])
    else stop('Unrecognized reference type.')
    wc(interpreter,method)
    wb(interpreter,length(args))
    sapply(headers, function(x) wc(interpreter,x))
    flush(interpreter[['socketIn']])
    rServe(interpreter,TRUE,workspace)
    status <- rb(interpreter,"integer")
    if ( get("serializeOutput",envir=interpreter[['env']]) ) echoResponseScala(interpreter)
    if ( status != OK ) stop("Problem invoking function.")
    result <- scalaGet(interpreter,"?",.AS.REFERENCE)
    if ( is.null(result) ) invisible(result)
    else result
  }
}

'$.ScalaCachedReference' <- scalaAutoMkFunction2
'$.ScalaInterpreterReference' <- scalaAutoMkFunction2
'$.ScalaInterpreterItem' <- scalaAutoMkFunction2

scalap <- function(interpreter,class.name) {
  if ( inherits(interpreter,"ScalaInterpreterReference") || inherits(interpreter,"ScalaCachedReference") ) {
    if ( ! missing(class.name) ) stop("'class.name' should not be provided if the first argument is a Scala reference.")
    return(scalap(interpreter[['interpreter']],interpreter[['type']]))
  }
  if ( ! inherits(interpreter,"ScalaInterpreter") ) stop("The first argument must be an interpreter.")
  if ( ! identical(class(class.name),"character") || length(class.name) != 1 ) stop("The second argument must be a string.")
  cc(interpreter)
  tryCatch({
    wb(interpreter,SCALAP)
    wc(interpreter,class.name)
    flush(interpreter[['socketIn']])
    status <- rb(interpreter,"integer")
    if ( get("serializeOutput",envir=interpreter[['env']]) ) echoResponseScala(interpreter)
  }, interrupt = function(x) {
    assign("open",FALSE,envir=interpreter[['env']])
    stop("## Interpreter closed by interrupt. ##")
  })
}

close.ScalaInterpreter <- function(con,...) {
  cc(con)
  assign("open",FALSE,envir=con[['env']])
  if ( get("debug",envir=con[['env']]) ) msg("Sending SHUTDOWN request.")
  wb(con,SHUTDOWN)
  flush(con[['socketIn']])
  close(con[['socketOut']])
  close(con[['socketIn']])
}

jarsOfPackage <- function(pkgname, major.release) {
  jarsMajor <- list.files(file.path(system.file("java",package=pkgname),paste0("scala-",major.release)),pattern=".*\\.jar$",full.names=TRUE,recursive=FALSE)
  jarsAny <- list.files(system.file("java",package=pkgname),pattern=".*\\.jar$",full.names=TRUE,recursive=FALSE)
  result <- c(jarsMajor,jarsAny)
  if ( length(result) == 0 ) stop(paste0("JAR files of package '",pkgname,"' were requested, but no JARs were found."))
  result
}

.rscalaPackage <- function(pkgname, snippet=character(), classpath.packages=character(), classpath.prepend=character(), classpath.append=character(), major.release=c("2.10","2.11","2.12"), ...) {
  env <- parent.env(parent.frame())    # Environment of depending package (assuming this function is only called in .onLoad function).
  assign(".rscalaPackageEnv",new.env(parent=emptyenv()), envir=env)
  assign("sIsForced",FALSE,envir=get(".rscalaPackageEnv",envir=env))
  # Lazy initialization of 's' in environment of depending package
  delayedAssign("s", {
    sInfo <- scalaInfo(major.release=major.release)
    if ( is.null(sInfo) ) stop('Please run "rscala::scalaInstall()" or install Scala manually.')
    pkgJars <- unlist(lapply(c(pkgname,classpath.packages), function(p) jarsOfPackage(p, sInfo$major.release)))
    classpath.prepend <- unlist(strsplit(classpath.prepend,.Platform$path.sep))
    classpath.append <- unlist(strsplit(classpath.append,.Platform$path.sep))
    classpath <- c(classpath.prepend,pkgJars,classpath.append)
    s <- scala(classpath=classpath,classpath.packages=NULL,scalaInfo=sInfo,...)
    assign("sIsForced",TRUE,envir=get(".rscalaPackageEnv",envir=env))
    if ( length(snippet) > 0 ) s %@% snippet
    s
  }, assign.env=env)
  # Deprecated and will be removed.
  if ( exists(".rscalaDelayed",envir=env) ) {
    delayedEnv <- get(".rscalaDelayed",envir=env)
    lapply(
      ls(envir=delayedEnv), function(x) {
        expression <- get(x,envir=delayedEnv)
        eval(expression,envir=env)
      }
    )
    rm(".rscalaDelayed",envir=env)
  }
  invisible()
}

.rscalaPackageUnload <- function() {
  env <- parent.env(parent.frame())
  sIsForced <- get("sIsForced",envir=get(".rscalaPackageEnv",envir=env))
  if ( sIsForced ) {
    close(get("s",envir=env))
  }
}

.rscalaDelay <- function(expression) {
  warning("The .rscalaDelay function is deprecated and will be removed.")
  env <- parent.frame()
  if ( ! exists(".rscalaDelayed",envir=env) ) {
    assign(".rscalaDelayed",new.env(parent=emptyenv()),envir=env)
  }
  env2 <- get(".rscalaDelayed",envir=env)
  assign(as.character(length(env2)),substitute(expression),envir=env2,inherits=FALSE)
  invisible()
}

.rscalaJar <- function(major.release=c("2.10","2.11","2.12")) {
  if ( length(major.release) > 1 ) {
    return(sapply(major.release,.rscalaJar,USE.NAMES=FALSE))
  }
  if ( length(major.release) == 0 ) stop("At least one major release must be supplied.")
  if ( is.na(major.release) ) {
    javaVersion <- javaVersion(findJava())
    if ( javaVersion <= 7 ) major.release <- "2.11"
    else major.release <- "2.12"
  }
  major.release <- substr(major.release,1,4)
  if ( ! ( major.release %in% c("2.10","2.11","2.12") ) ) stop(paste("Unsupported major release:",major.release))
  result <- jarsOfPackage("rscala",major.release)
  names(result) <- major.release
  result
}

latest <- function() {
  install.packages('https://dahl.byu.edu/public/rscala_LATEST.tar.gz',repos=NULL,type='source')
}

scalaInfoEngine <- function(scala.command,major.release,verbose) {
  scala.command <- normalizePath(scala.command,mustWork=FALSE)
  if ( ! file.exists(scala.command) ) return(NULL)
  if ( verbose ) {
    cat(sprintf("  * ATTEMPT: Found a candidate (%s)\n",scala.command))
    tab <- "    - "
  }
  scala.home <- dirname(dirname(scala.command))
  jars <- list.files(file.path(scala.home,"lib"),".*.jar$",full.names=TRUE)
  libraryJar <- jars[grepl("^scala-library",basename(jars))]
  if ( length(libraryJar) == 0 ) {
    if ( verbose ) cat(tab,sprintf("scala-library.jar is not in 'lib' directory of assumed Scala home (%s)\n",scala.home),sep="")
    scala.home <- NULL
    if ( .Platform$OS.type != "windows" ) {
      if ( verbose ) cat(tab,"Looking for 'scala.home' property in 'scala' script\n",sep="")
      showArguments <- system.file(file.path("bin","showArguments"),package="rscala")
      scala.home <- suppressWarnings(
        tryCatch({
          output <- system2(scala.command,c("-e",shQuote("")),stdout=TRUE,stderr=FALSE,env=paste0("JAVACMD=",shQuote(showArguments)))
          if ( ( ! is.null(attr(output,"status")) ) &&  ( attr(output,"status") != 0 ) ) NULL
          else sub("^-Dscala.home=","",output[grepl("^-Dscala.home=",output)])[1]
        },error=function(e) { NULL } )
      )
    }
    if ( is.null(scala.home) ) {
      if ( verbose ) cat(tab,"Executing snippet to find 'scala.home'\n",sep="")
      scala.home <- suppressWarnings(
        tryCatch(
          system2(scala.command,c("-e",shQuote("println(sys.props(\"scala.home\"))")),stdout=TRUE,stderr=FALSE)
        ,error=function(e) { NULL } )
      )
    }
    if ( ( is.null(scala.home) ) || ( length(scala.home) != 1 ) ) {
      if ( verbose ) cat(tab,"Cannot get 'scala.home' property from 'scala' script\n",sep="")
      return(NULL)
    }
    jars <- list.files(file.path(scala.home,"lib"),".*.jar$",full.names=TRUE)
    libraryJar <- jars[grepl("^scala-library",basename(jars))]
    if ( length(libraryJar) == 0 ) {
      if ( verbose ) cat(tab,sprintf("scala-library.jar is not in 'lib' directory of purported Scala home (%s)\n",scala.home),sep="")
      return(NULL)
    }
  }
  libraryJar <- libraryJar[1]
  actual.major.release <- tryCatch({
    fn <- unz(libraryJar,"library.properties")
    lines <- readLines(fn)
    close(fn)
    version <- sub("^version.number=","",lines[grepl("^version.number=",lines)])[1]
    if ( substr(version,4,4) == "." ) substr(version,1,3)
    else substr(version,1,4)
  },error=function(e) { NULL } )
  if ( is.null(actual.major.release) ) {
    if ( verbose ) cat(sprintf("Cannot get Scala version from library jar (%s)\n",libraryJar))
    return(NULL)
  }
  if ( ! ( actual.major.release %in% major.release ) ) {
    if ( verbose ) cat(sprintf("      Major release %s is not what was requested: %s\n",actual.major.release,paste(major.release,collapse=", ")))
    return(NULL)
  }
  list(cmd=scala.command,home=scala.home,version=version,major.release=actual.major.release)
}

scalaInfo <- function(scala.home=NULL,major.release=c("2.10","2.11","2.12"),verbose=FALSE) {
  if ( inherits(scala.home,"ScalaInterpreter") ) return(get("info",scala.home[['env']]))
  if ( verbose ) cat("\nSearching for a suitable Scala installation.\n")
  tab <- "  * "
  if ( verbose ) {
    successMsg <- "SUCCESS: "
    failureMsg <- "FAILURE: "
  }
  # Attempt 1
  if ( verbose ) techniqueMsg <- "'scala.home' argument"
  if ( is.null(scala.home) ) {
    if ( verbose ) cat(tab,failureMsg,techniqueMsg," is NULL","\n",sep="")
  } else {
    info <- scalaInfoEngine(file.path(scala.home,"bin","scala"),major.release,verbose)
    if ( verbose ) techniqueMsg <- sprintf("'scala.home' (%s) argument",scala.home)
    if ( ! is.null(info) ) { if ( verbose ) cat(tab,successMsg,techniqueMsg,"\n\n",sep=""); return(info) }
    else if ( verbose ) cat(tab,failureMsg,techniqueMsg,"\n",sep="")
    stop("Cannot find Scala using 'scala.home' argument.  Expand search by *not* specifying 'scala.home' argument.")
  }
  # Attempt 2
  scala.home.tmp <- getOption("rscala.scala.home",default="<UNSET>")
  info <- if ( scala.home.tmp != "<UNSET>" ) {
    scalaInfoEngine(file.path(scala.home.tmp,"bin","scala"),major.release,verbose)
  } else NULL
  if ( verbose ) techniqueMsg <- sprintf("'rscala.scala.home' (%s) global option",scala.home.tmp)
  if ( ! is.null(info) ) { if ( verbose ) cat(tab,successMsg,techniqueMsg,"\n\n",sep=""); return(info) }
  else if ( verbose ) cat(tab,failureMsg,techniqueMsg,"\n",sep="")
  # Attempt 3
  scala.home.tmp <- Sys.getenv("SCALA_HOME")
  if ( verbose ) techniqueMsg <- sprintf("SCALA_HOME (%s) environment variable",scala.home.tmp)
  info <- if ( scala.home.tmp != "" ) {
    scalaInfoEngine(file.path(scala.home.tmp,"bin","scala"),major.release,verbose)
  } else NULL
  if ( ! is.null(info) ) { if ( verbose ) cat(tab,successMsg,techniqueMsg,"\n\n",sep=""); return(info) }
  else if ( verbose ) cat(tab,failureMsg,techniqueMsg,"\n",sep="")
  # Attempt 4
  info <- scalaInfoEngine(Sys.which("scala"),major.release,verbose)
  if ( verbose ) techniqueMsg <- "'scala' in the shell's search path"
  if ( ! is.null(info) ) { if ( verbose ) cat(tab,successMsg,techniqueMsg,"\n\n",sep=""); return(info) }
  else if ( verbose ) cat(tab,failureMsg,techniqueMsg,"\n",sep="")
  # Attempt 5
  candidates <- normalizePath(c(
      list.files(system.file(package="rscala"),pattern="^scala-.*",full.names=TRUE),
      list.dirs(file.path("~",".rscala"),full.names=TRUE,recursive=FALSE)
    ),mustWork=FALSE)
  details <- file.info(candidates)
  details <- details[order(as.POSIXct(details$mtime),decreasing=TRUE), ]
  candidates <- rownames(details)
  for ( installDir in candidates ) {
    info <- scalaInfoEngine(file.path(installDir,"bin","scala"),major.release,verbose)
    if ( verbose ) techniqueMsg <- sprintf("special installation directory (%s)",installDir)
    if ( ! is.null(info) ) { if ( verbose ) cat(tab,successMsg,techniqueMsg,"\n\n",sep=""); return(info) }
    else if ( verbose ) cat(tab,failureMsg,techniqueMsg,"\n",sep="")
  }
  # Attempt 6
  if ( ! verbose ) scalaInfo(scala.home=scala.home,major.release=major.release,verbose=TRUE)
  else {
    cat("\nCannot find a suitable Scala installation.\n\n")
    readmePath <- system.file("README",package="rscala")
    if ( readmePath != "" ) {
      cat(readLines(readmePath),sep="\n")
      cat("\n")
    }
    if ( interactive() ){
      actual.major.release <- latestVersion(major.release)
      if ( askToInstall(actual.major.release) ) {
        global <- askWhereToInstall()
        scalaInstall(actual.major.release, global=global)
        scalaInfo(scala.home=scala.home,major.release=actual.major.release)
      } else invisible()
    } else invisible()
  }
}

askToInstall <- function(major.release) {
  while ( TRUE ) {
    response <- readline(prompt=paste("Would you like to install Scala",major.release,"now? [Y/n] "))
    response <- toupper(trimws(response))
    if ( response == "" ) response <- "Y"
    if ( response == "Y" ) return(TRUE)
    if ( response == "N" ) return(FALSE)
  }
}

askWhereToInstall <- function() {
#  cat("The installation can be in the package's directory or your home directory.\n")
#  cat("System administrators should install in the package's directory.\n")
  while ( TRUE ) {
    response <- readline(prompt=paste("Install in the package's directory? [y/N] "))
    response <- toupper(trimws(response))
    if ( response == "" ) response <- "N"
    if ( response == "Y" ) return(TRUE)
    if ( response == "N" ) return(FALSE)
  }
}

evalAndGet <- function(interpreter,snippet,as.reference,workspace) {
  scalaEval(interpreter,snippet,workspace)
  scalaGet(interpreter,".",as.reference)
}

checkType <- function(x) {
  if ( is.integer(x) ) INTEGER
  else if ( is.double(x) ) DOUBLE
  else if ( is.logical(x) ) BOOLEAN
  else if ( is.character(x) ) STRING
  else if ( is.raw(x) ) BYTE
  else stop("Unsupported data type.")
}

checkType3 <- function(x) {
  if ( is.integer(x) ) "I"
  else if ( is.double(x) ) "D"
  else if ( is.logical(x) ) "L"
  else if ( is.character(x) ) "S"
  else if ( is.raw(x) ) "R"
  else stop("Unsupported data type.")
}

echoResponseScala <- function(interpreter) {
  if ( get("debug",envir=interpreter[['env']]) ) msg('Reading serialized response.')
  response <- rc(interpreter)
  if ( response == "" ) return()
  if ( get("debug",envir=interpreter[['env']]) ) msg('Serialized output: <')
  cat(response)
  if ( get("debug",envir=interpreter[['env']]) ) msg('>')
}

cc <- function(c) {
  if ( ! get("open",envir=c[['env']]) ) stop("The connection has already been closed.")
  if ( length(c[['garbage']]) > 0 ) {
    env <- c[['garbage']]
    garbage <- ls(envir=env)
    if ( get("debug",envir=c[['env']]) ) msg(paste0('Sending FREE request for ',length(garbage),' items.'))
    wb(c,FREE)
    wb(c,length(garbage))
    for ( g in garbage ) wc(c,get(g,envir=env))
    rm(list=garbage,envir=env)
    if ( get("serializeOutput",envir=c[['env']]) ) echoResponseScala(c)
  }
}

wb <- function(c,v) {
  writeBin(v, c[['socketIn']], endian="big")
}

wc <- function(c,v) {
  bytes <- charToRaw(iconv(v, to="UTF-8"))
  wb(c,length(bytes))
  writeBin(bytes, c[['socketIn']], endian="big", useBytes=TRUE)
}

# Sockets should be blocking, but that contract is not fulfilled when other code uses functions from the parallel library.  Program around their problem.
rb <- function(c,v,n=1L) {
  r <- readBin(c[['socketOut']], v, n, endian="big")
  if ( length(r) == n ) r
  else c(r,rb(c,v,n-length(r)))
}

# Sockets should be blocking, but that contract is not fulfilled when other code uses functions from the parallel library.  Program around their problem.
rc <- function(c) {
  length <- rb(c,"integer")
  r <- raw(0)
  while ( length(r) != length ) r <- c(r,readBin(c[['socketOut']], "raw", length-length(r), endian="big"))
  iconv(rawToChar(r),from="UTF-8")
}

pretty <- function(header,body) {
  if ( body == '' ) return('')
  splitBody <- strsplit(body,'\n')[[1]]
  if ( grepl('^[[:space:]]*$',splitBody[1]) ) splitBody <- splitBody[-1]
  if ( grepl('^[[:space:]]*$',splitBody[length(splitBody)]) ) splitBody <- splitBody[-length(splitBody)]
  bodyWithoutBlanks <- strsplit(splitBody[!grepl("^[[:space:]]*$",splitBody)],'\n')[[1]]
  originalPadding <- min(attr(regexpr("^[[:space:]]*",bodyWithoutBlanks),"match.length"))
  headerWithPadding <- if ( length(header) > 0 ) paste0('  ',header) else NULL
  bodyWithPadding <- paste0('  ',substring(splitBody,originalPadding+1))
  paste0(paste(c(headerWithPadding,bodyWithPadding),collapse='\n'),'\n')
}


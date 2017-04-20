## Scala scripting over TCP/IP

scala <- function(classpath=character(0),serialize.output=FALSE,scala.home=NULL,heap.maximum=NULL,command.line.options=NULL,row.major=TRUE,timeout=60,debug=FALSE,stdout=TRUE,stderr=TRUE,port=0) {
  if ( identical(stdout,TRUE) ) stdout <- ""
  if ( identical(stderr,TRUE) ) stderr <- ""
  debug <- identical(debug,TRUE)
  serialize.output <- identical(serialize.output,TRUE)
  row.major <- identical(row.major, TRUE)
  port <- as.integer(port[1])
  if ( debug && serialize.output ) stop("When debug is TRUE, serialize.output must be FALSE.")
  if ( debug && ( identical(stdout,FALSE) || identical(stdout,NULL) || identical(stderr,FALSE) || identical(stderr,NULL) ) ) stop("When debug is TRUE, stdout and stderr must not be discarded.")
  userJars <- unlist(strsplit(classpath,.Platform$path.sep))
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
  sInfo <- scalaInfo(scala.home)
  if ( is.null(sInfo) ) stop("Cannot find a suitable Scala installation.  Please manually install Scala or run 'scalaInstall()'.")
  rsJar <- .rscalaJar(sInfo$version)
  rsClasspath <- shQuote(paste(c(rsJar,userJars),collapse=.Platform$path.sep))
  portsFilename <- tempfile("rscala-")
  args <- c(command.line.options,paste0("-Drscala.classpath=",rsClasspath),"-classpath",rsClasspath,"org.ddahl.rscala.Main",portsFilename,debug,serialize.output,row.major,port)
  if ( debug ) msg("\n",sInfo$cmd)
  if ( debug ) msg("\n",paste0("<",args,">",collapse="\n"))
  system2(sInfo$cmd,args,wait=FALSE,stdout=stdout,stderr=stderr)
  sockets <- newSockets(portsFilename,debug,serialize.output,row.major,timeout)
  scalaSettings(sockets,interpolate=TRUE)
  sockets
}

newSockets <- function(portsFilename,debug,serialize.output,row.major,timeout) {
  functionCache <- new.env()
  env <- new.env()
  assign("open",TRUE,envir=env)
  assign("debug",debug,envir=env)
  assign("rowMajor",row.major,envir=env)
  assign("serializeOutput",serialize.output,envir=env)
  assign("garbage",character(0),envir=env)
  ports <- local({
    delay <- 0.1
    start <- proc.time()[3]
    while ( TRUE ) {
      if ( ( proc.time()[3] - start ) > timeout ) stop("Timed out waiting for Scala to start.")
      Sys.sleep(delay)
      delay <- 1.0*delay
      if ( file.exists(portsFilename) ) {
        line <- scan(portsFilename,n=2,what=character(0),quiet=TRUE)
        if ( length(line) > 0 ) return(as.numeric(line))
      }
    }
  })
  file.remove(portsFilename)
  if ( debug ) msg("Trying to connect to port ",paste(ports,collapse=" "))
  socketConnectionIn  <- socketConnection(port=ports[1],blocking=TRUE,open="ab",timeout=2678400)
  socketConnectionOut <- socketConnection(port=ports[2],blocking=TRUE,open="rb",timeout=2678400)
  if ( debug ) msg("Connected")
  result <- list(socketIn=socketConnectionIn,socketOut=socketConnectionOut,env=env,functionCache=functionCache,
                 garbageFunction = function(e) {
                   garbage <- get("garbage",envir=env)
                   garbage[length(garbage)+1] <- e[['identifier']]
                   assign("garbage",garbage,envir=env)
                 })
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
    close(interpreter[['socketOut']])
    close(interpreter[['socketIn']])
    message("Interpreter closed by interrupt.")
  })
  if ( ( length(status) == 0 ) || ( status != OK ) ) stop("Error in evaluation.")
  else invisible(NULL)
}

'%~%.ScalaInterpreter' <- function(interpreter,snippet) {
  cc(interpreter)
  snippet <- paste(snippet,collapse="\n")
  if ( get("interpolate",envir=interpreter[['env']]) ) {
    snippet <- strintrplt(snippet,parent.frame())
  }
  result <- evalAndGet(interpreter,snippet,NA,parent.frame())
  if ( is.null(result) ) invisible(result)
  else result
}

'%.~%.ScalaInterpreter' <- function(interpreter,snippet) {
  cc(interpreter)
  snippet <- paste(snippet,collapse="\n")
  if ( get("interpolate",envir=interpreter[['env']]) ) {
    snippet <- strintrplt(snippet,parent.frame())
  }
  result <- evalAndGet(interpreter,snippet,TRUE,parent.frame())
  if ( is.null(result) ) invisible(result)
  else result
}

'%@%.ScalaInterpreter' <- function(interpreter,snippet) {
  cc(interpreter)
  snippet <- paste(snippet,collapse="\n")
  if ( get("interpolate",envir=interpreter[['env']]) ) {
    snippet <- strintrplt(snippet,parent.frame())
  }
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
  scalap(x[['interpreter']],type)
  cat("ScalaInterpreterReference... ")
  cat(x[['identifier']],": ",type,"\n",sep="")
  invisible(x)
}

toString.ScalaInterpreterReference <- function(x,...) {
  x[['identifier']]
}

print.ScalaCachedReference <- function(x,...) {
  type <- x[['type']]
  scalap(x[['interpreter']],type)
  cat("ScalaCachedReference... ")
  cat("*: ",type,"\n",sep="")
  invisible(x)
}

toString.ScalaCachedReference <- function(x,...) {
  x[['identifier']]
}

print.ScalaInterpreterItem <- function(x,...) {
  scalap(x[['interpreter']],x[['snippet']])
  cat("ScalaInterpreterItem\n")
  invisible(x)
}

toString.ScalaInterpreterItem <- function(x,...) {
  "ScalaInterpreterItem"
}

scalaGet <- function(interpreter,identifier,as.reference,workspace) {
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
        if ( length > 0 ) sapply(1:length,function(x) rc(interpreter))
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
        if ( dim[1] > 0 ) for ( i in 1:dim[1] ) {
          if ( dim[2] > 0 ) for ( j in 1:dim[2] ) {
            v[i,j] <- rc(interpreter)
          }
        }
        v
      } else {
        v <- matrix(rb(interpreter,typeMap[[dataType]],dim[1]*dim[2]),nrow=dim[1],byrow=TRUE)
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
        return(scalaGet(interpreter,identifier,as.reference=TRUE))
      } else {
        if ( get("serializeOutput",envir=interpreter[['env']]) ) echoResponseScala(interpreter)
        stop("Unsupported data structure.")
      }
    } else stop(paste0("Protocol error.  Got: ",dataStructure))
    if ( get("serializeOutput",envir=interpreter[['env']]) ) echoResponseScala(interpreter)
    value
  }, interrupt = function(x) {
    assign("open",FALSE,envir=interpreter[['env']])
    close(interpreter[['socketOut']])
    close(interpreter[['socketIn']])
    message("Interpreter closed by interrupt.")
  })
}

'$.ScalaInterpreter' <- function(interpreter,identifier) {
  cc(interpreter)
  if ( identifier == "def" ) function(...) {
    scalaDef(.INTERPRETER=interpreter,...)
  } else if ( identifier == "null" ) function(type) {
    result <- list(interpreter=interpreter,identifier='null',type=type)
    class(result) <- "ScalaInterpreterReference"
    result
  } else if ( identifier == "do" ) function(snippet) {
    warning(paste0("Syntax \"s$do('",snippet,"')\" is deprecated.  Use \"s$.",snippet,"\" instead."))
    result <- list(interpreter=interpreter,snippet=snippet)
    class(result) <- "ScalaInterpreterItem"
    result
  } else if ( identifier == "val" ) function(x) {
    scalaGet(interpreter,x,NA,parent.frame())
  } else if ( identifier == ".val" ) function(x) {
    scalaGet(interpreter,x,TRUE,parent.frame())
  } else if ( substr(identifier,1,1) == "." ) {
    identifier <- substring(identifier,2)
    result <- list(interpreter=interpreter,snippet=identifier)
    class(result) <- "ScalaInterpreterItem"
    result
  } else if ( identifier %in% names(interpreter) ) {
    stop("This item is not user accessible.")
  } else {
    scalaGet(interpreter,identifier,NA,parent.frame())
  }
}

scalaSet <- function(interpreter,identifier,value,workspace) {
  debug <- get("debug",envir=interpreter[['env']])
  if ( debug ) msg(paste0("Starting scalaSet with environment:",capture.output(print(workspace))))
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
        if ( length(value) != 1 ) stop("Values wrapped in I() must be of length one.")
        value <- as.vector(value)
        TRUE
      } else {
        FALSE
      }
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
          if ( length(value) > 0 ) for ( i in 1:length(value) ) wc(interpreter,value[i])
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
        if ( nrow(value) > 0 ) for ( i in 1:nrow(value) ) {
          if ( type == STRING ) {
            if ( ncol(value) > 0 ) for ( j in 1:ncol(value) ) wc(interpreter,value[i,j])
          }
          else if ( type == BOOLEAN ) wb(interpreter,as.integer(value[i,]))
          else wb(interpreter,value[i,])
        }
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
    close(interpreter[['socketOut']])
    close(interpreter[['socketIn']])
    message("Interpreter closed by interrupt.")
  })
  invisible()
}

'$<-.ScalaInterpreter' <- function(interpreter,identifier,value) {
  cc(interpreter)
  scalaSet(interpreter,identifier,value,parent.frame())
  interpreter
}

scalaDef <- function(.INTERPRETER,...) {
  argValues <- list(...)
  argIdentifiers <- names(argValues)
  if ( is.null(argIdentifiers) ) {
    argIdentifiers <- paste0(rep('x',length(argValues)),seq_along(argValues))
  } else if ( any(argIdentifiers=='' ) ) {
    w <- argIdentifiers==''
    argIdentifiers[w] <- paste0(rep('x',sum(w)),seq_along(argValues[argIdentifiers=='']))
  }
  if ( length(unique(argIdentifiers)) != length(argIdentifiers) ) stop('Argument names must be unique.')
  header <- character(length(argValues))
  for ( i in seq_along(argValues) ) {
    value <- argValues[[i]]
    name <- argIdentifiers[[i]]
    if ( is.null(value) ) {
      header[i] <- paste0('val ',name,' = "',name,'"')
    } else if ( inherits(value,"ScalaInterpreterReference") || inherits(value,"ScalaCachedReference") ) {
      header[i] <- paste0('val ',name,' = R.cached(R.evalS0("toString(',name,')")).asInstanceOf[',value[['type']],']')
    } else {
      if ( ( ! is.atomic(value) ) || is.null(value) ) stop(paste0('Type of "',name,'" is not supported.'))
      type <- checkType3(value)
      len <- if ( inherits(value,"AsIs") ) 0
      else if ( is.vector(value) ) 1
      else if ( is.matrix(value) ) 2
      else stop(paste0('Type of "',name,'" is not supported.'))
      header[i] <- paste0('val ',name,' = R.get',type,len,'("',name,'")')
    }
  }
  result <- list(interpreter=.INTERPRETER,identifiers=argIdentifiers,header=header)
  class(result) <- 'ScalaFunctionArgs'
  result
}

'%~%.ScalaFunctionArgs' <- function(func,body) {
  if ( get("interpolate",envir=func$interpreter[['env']]) ) {
    body <- strintrplt(body,parent.frame())
  }
  scalaFunctionArgs(func,body,as.reference=NA,parent.frame())
}

'%.~%.ScalaFunctionArgs' <- function(func,body) {
  if ( get("interpolate",envir=func$interpreter[['env']]) ) {
    body <- strintrplt(body,parent.frame())
  }
  scalaFunctionArgs(func,body,as.reference=TRUE,parent.frame())
}

scalaFunctionArgs <- function(func,body,as.reference,workspace) {
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
  interpreterName <- tempvar(interpreter, workspace)
  functionSnippet <- strintrplt('
    function(@{paste(func$identifiers,collapse=", ")}) {
      .rsInterpreter <- @{interpreterName}
      .rsWorkspace <- environment()
      @{ifelse(get("debug",envir=interpreter[["env"]]),\'rscala:::msg(paste("Evaluating Scala function from environment",capture.output(print(.rsWorkspace))))\',"")}
      rscala:::wb(.rsInterpreter,rscala:::INVOKE)
      rscala:::wc(.rsInterpreter,"@{funcList$functionIdentifier}")
      flush(.rsInterpreter[["socketIn"]])
      rscala:::rServe(.rsInterpreter,TRUE,.rsWorkspace)
      .rsStatus <- rscala:::rb(.rsInterpreter,"integer")
      @{ifelse(get("serializeOutput",envir=interpreter[["env"]]),"rscala:::echoResponseScala(.rsInterpreter)","")}
      if ( .rsStatus == rscala:::ERROR ) {
        stop("Invocation error.")
      } else {
        .rsResult <- rscala:::scalaGet(.rsInterpreter,"?",@{as.reference},.rsWorkspace)
        if ( is.null(.rsResult) ) invisible(.rsResult)
        else .rsResult
      }
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

scalaAutoDef <- function(reference,method) {
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
    f <- scalaFunctionArgs(scalaDef(.INTERPRETER=interpreter,...),snippet,as.reference=.AS.REFERENCE,parent.frame())
    if ( .EVALUATE ) f(...)
    else f
  }
}

'$.ScalaCachedReference' <- scalaAutoDef
'$.ScalaInterpreterReference' <- scalaAutoDef
'$.ScalaInterpreterItem' <- scalaAutoDef

scalap <- function(interpreter,item.name) {
  if ( ! inherits(interpreter,"ScalaInterpreter") ) stop("The first argument must be an interpreter.")
  cc(interpreter)
  tryCatch({
    wb(interpreter,SCALAP)
    wc(interpreter,item.name)
    flush(interpreter[['socketIn']])
    status <- rb(interpreter,"integer")
    if ( get("serializeOutput",envir=interpreter[['env']]) ) echoResponseScala(interpreter)
  }, interrupt = function(x) {
    assign("open",FALSE,envir=interpreter[['env']])
    close(interpreter[['socketOut']])
    close(interpreter[['socketIn']])
    message("Interpreter closed by interrupt.")
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

.rscalaPackage <- function(pkgname, classpath.appendix=character(0), ...) {
  classpath <- c(list.files(system.file("java",package=pkgname),pattern=".*.jar$",full.names=TRUE,recursive=TRUE),classpath.appendix)
  s <- scala(classpath=classpath,...)
  assign("s",s,envir=parent.env(parent.frame()))    # Assign to environment of depending package
  invisible()
}

.rscalaJar <- function(version="") {
  if ( version == "" ) major.version <- ".*"
  else major.version <- substr(version,1,4)
  list.files(system.file("java",package="rscala"),pattern=paste("rscala_",major.version,'-.*\\.jar$',sep=""),full.names=TRUE)
}

scalaInfoEngine <- function(scala.command,verbose) {
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
    if ( .Platform$OS != "windows" ) {
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
  major.version <- tryCatch({
    fn <- unz(libraryJar,"library.properties")
    lines <- readLines(fn)
    close(fn)
    version <- sub("^version.number=","",lines[grepl("^version.number=",lines)])[1]
    if ( substr(version,4,4) == "." ) substr(version,1,3)
    else substr(version,1,4)
  },error=function(e) { NULL } )
  if ( is.null(major.version) ) {
    if ( verbose ) cat(sprintf("Cannot get Scala version from library jar (%s)\n",libraryJar))
    return(NULL)
  }
  if ( ! ( major.version %in% c("2.10","2.11","2.12") ) ) {
    if ( verbose ) cat(sprintf("Unsupported major version (%s) from Scala executable (%s)\n",major.version,scala.command))
    return(NULL)
  }
  list(cmd=scala.command,home=scala.home,version=version,major.version=major.version)
}

scalaInfo <- function(scala.home=NULL,verbose=FALSE) {
  if ( verbose ) cat("\nSearching for a suitable Scala installation.\n")
  tab <- "  * "
  if ( verbose ) {
    successMsg <- "SUCCESS: "
    failureMsg <- "FAILURE: "
  }
  if ( verbose ) techniqueMsg <- "'scala.home' argument"
  if ( is.null(scala.home) ) {
    if ( verbose ) cat(tab,failureMsg,techniqueMsg," is NULL","\n",sep="")
  } else {
    # Attempt 1
    info <- scalaInfoEngine(file.path(scala.home,"bin","scala"),verbose)
    if ( verbose ) techniqueMsg <- sprintf("'scala.home' (%s) argument",scala.home)
    if ( ! is.null(info) ) { if ( verbose ) cat(tab,successMsg,techniqueMsg,"\n\n",sep=""); return(info) }
    else if ( verbose ) cat(tab,failureMsg,techniqueMsg,"\n",sep="")
    stop("Cannot find Scala using 'scala.home' argument.  Expand search by *not* specifying 'scala.home' argument.")
  }
  # Attempt 2
  scala.home.tmp <- Sys.getenv("SCALA_HOME")
  if ( verbose ) techniqueMsg <- sprintf("SCALA_HOME (%s) environment variable",scala.home.tmp)
  info <- if ( scala.home.tmp != "" ) {
    scalaInfoEngine(file.path(scala.home.tmp,"bin","scala"),verbose)
  } else NULL
  if ( ! is.null(info) ) { if ( verbose ) cat(tab,successMsg,techniqueMsg,"\n\n",sep=""); return(info) }
  else if ( verbose ) cat(tab,failureMsg,techniqueMsg,"\n",sep="")
  # Attempt 3
  info <- scalaInfoEngine(Sys.which("scala"),verbose)
  if ( verbose ) techniqueMsg <- "'scala' in the shell's search path"
  if ( ! is.null(info) ) { if ( verbose ) cat(tab,successMsg,techniqueMsg,"\n\n",sep=""); return(info) }
  else if ( verbose ) cat(tab,failureMsg,techniqueMsg,"\n",sep="")
  # Attempt 4
  installDir <- normalizePath(file.path("~",".rscala",sprintf("scala-%s",CURRENT_SUPPORTED_SCALA_VERSION)),mustWork=FALSE)
  info <- scalaInfoEngine(file.path(installDir,"bin","scala"),verbose)
  if ( verbose ) techniqueMsg <- sprintf("special installation directory (%s)",installDir)
  if ( ! is.null(info) ) { if ( verbose ) cat(tab,successMsg,techniqueMsg,"\n\n",sep=""); return(info) }
  else if ( verbose ) cat(tab,failureMsg,techniqueMsg,"\n",sep="")
  # Attempt 5
  if ( ! verbose ) scalaInfo(scala.home=scala.home,verbose=TRUE)
  else {
    cat("\nCannot find a suitable Scala installation.\n\n")
    f <- file(system.file("README",package="rscala"),open="r")
    readme <- readLines(f)
    close(f)
    cat(readme,sep="\n")
    cat("\n")
    invisible(NULL)
  }
}

# Private

evalAndGet <- function(interpreter,snippet,as.reference,workspace) {
  scalaEval(interpreter,snippet,workspace)
  scalaGet(interpreter,".",as.reference,workspace)
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
  else if ( is.logical(x) ) "B"
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
  if ( length(get("garbage",envir=c[['env']])) > 0 ) {
    env <- c[['env']]
    garbage <- get("garbage",envir=env)
    if ( get("debug",envir=env) ) msg(paste0('Sending FREE request for ',length(garbage),' items.'))
    wb(c,FREE)
    wb(c,length(garbage))
    for ( g in garbage ) wc(c,g)
    assign("garbage",character(),envir=env)
    if ( get("serializeOutput",envir=env) ) echoResponseScala(c)
  }
}

wb <- function(c,v) {
  writeBin(v, c[['socketIn']], endian="big")
}

wc <- function(c,v) {
  bytes <- charToRaw(v)
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
  rawToChar(r)
}

tempvar <- function(value, workspace) {
  while (TRUE) {
    name <- sprintf(".rs%d",sample.int(.Machine$integer.max,1L))
    if ( ! exists(name,envir=workspace,inherits=TRUE) ) {
      assign(name,value,envir=workspace)
      return(name)
    }
  }
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

scalaInstall <- function() {
  installPath <- normalizePath(file.path("~",".rscala"),mustWork=FALSE)
  url <- sprintf("http://downloads.lightbend.com/scala/%s/scala-%s.tgz",CURRENT_SUPPORTED_SCALA_VERSION,CURRENT_SUPPORTED_SCALA_VERSION)
  dir.create(installPath,showWarnings=FALSE,recursive=TRUE)
  destfile <- file.path(installPath,basename(url))
  result <- download.file(url,destfile)
  if ( result != 0 ) return(invisible(result))
  result <- untar(destfile,exdir=installPath,tar="internal")    # Use internal to avoid problems on a Mac.
  unlink(destfile)
  if ( result == 0 ) cat("Successfully installed Scala in ",file.path(installPath,sprintf("scala-%s",CURRENT_SUPPORTED_SCALA_VERSION)),"\n",sep="")
  invisible(result)
}


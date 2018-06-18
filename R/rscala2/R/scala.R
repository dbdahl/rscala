#' Title
#'
#' @return
#' 
#' @export
#'
#' @examples
#' 
#' @export
#'
scala <- function(classpath=character(),
                  classpath.packages=character(),
                  serialize.output=.Platform$OS.type=="windows",
                  heap.maximum=NULL,
                  command.line.options=NULL,
                  debug=FALSE,
                  stdout=TRUE,
                  stderr=TRUE,
                  port=0L,
                  assign.name="s",
                  assign.env=.GlobalEnv,
                  assign.callback=function(s) {},
                  snippet=character()) {
  if ( identical(stdout,TRUE) ) stdout <- ""
  if ( identical(stderr,TRUE) ) stderr <- ""
  debug <- identical(debug,TRUE)
  serialize.output <- identical(serialize.output,TRUE)
  port <- as.integer(port[1])
  if ( debug && serialize.output ) stop("When debug is TRUE, serialize.output must be FALSE.")
  if ( debug && ( identical(stdout,FALSE) || identical(stdout,NULL) || identical(stderr,FALSE) || identical(stderr,NULL) ) ) stop("When debug is TRUE, stdout and stderr must not be discarded.")
  if ( ! requireNamespace("rscalaCore", quietly=TRUE) ) {
    stop("Please install the 'rscalaCore' package.  Here's how...")
  }
  if ( packageVersion("rscalaCore") < "2.12.6" ) {
    stop("Please update the 'rscalaCore' package.  Here's how...")
  }
  scalaMajor <- rscalaCore::scalaVersion(TRUE)
  pkgJARs <- unlist(lapply(classpath.packages, function(p) jarsOfPackage(p, scalaMajor)))
  rscalaJAR <- list.files(system.file(file.path("java",paste0("scala-",scalaMajor)),package="rscala2",mustWork=TRUE),full.names=TRUE)
  rscalaClasspath <- shQuote(paste0(c(rscalaJAR,classpath,pkgJARs),collapse=.Platform$path.sep))
  command.line.options <- shQuote(mkCommandLineOptions(command.line.options,heap.maximum))
  snippetFilename <- tempfile("rscala-snippet-")
  writeLines(snippet,snippetFilename)
  portsFilename <- tempfile("rscala-ports-")
  args <- c(command.line.options,"-classpath",rscalaJAR,"org.ddahl.rscala2.server.Server",rscalaClasspath,port,portsFilename,snippetFilename,debug,serialize.output,FALSE)
  system2(rscalaCore::scalaExec(),args,wait=FALSE,stdout=stdout,stderr=stderr)
  details <- new.env(parent=emptyenv())
  assign("snippetFilename",snippetFilename,envir=details)
  assign("closed",FALSE,envir=details)
  assign("interrupted",FALSE,envir=details)
  assign("last",NULL,envir=details)
  assign("garbage",integer(),envir=details)
  assign("buffer",rawConnection(raw(),open="wb"),envir=details)
  gcFunction <- function(e) {
    garbage <- details[["garbage"]]
    garbage[length(garbage)+1] <- e[["id"]]
    assign("garbage",garbage,envir=details)
  }
  assign("gcFunction",gcFunction,envir=details)
  bridge <- function(...) {
    bridge2 <- list(...)
    argnames <- names(bridge2)
    if ( ( length(bridge2) > 0 )  && ( is.null(argnames) || ! all(grepl("^\\w+$",argnames,perl=TRUE)) ) ) {
      stop("argument names must be given and consist only alphanumeric & underscore characters.")
    }
    attr(bridge2,"details") <- details
    class(bridge2) <- "rscalaBridge"
    bridge2
  }
  attr(bridge,"details") <- details
  class(bridge) <- "rscalaBridge"    
  reg.finalizer(details,stopProcess,onexit=TRUE)
  if ( ! is.null(assign.name) && ( assign.name != "" ) ) {
    if ( interactive() ) {
      delayedAssign(assign.name,{
        newSockets(portsFilename, details)
        assign.callback(bridge)
        bridge
      },assign.env=assign.env)
    } else {
      assign(assign.name,{
        newSockets(portsFilename, details)
        assign.callback(bridge)
        bridge
      },envir=assign.env)      
    }
  } else {
    newSockets(portsFilename, details)
    assign.callback(bridge)
    bridge
  }
}

newSockets <- function(portsFilename, details) {
  ports <- local({
    delay <- 0.01
    while ( TRUE ) {
      if ( file.exists(portsFilename) ) {
        line <- scan(portsFilename,n=2,what=character(),quiet=TRUE)
        if ( length(line) > 0 ) return(as.numeric(line))
      }
      Sys.sleep(delay)
    }
  })
  unlink(portsFilename)
  socketIn  <- socketConnection(host="localhost", port=ports[1], server=FALSE, blocking=TRUE, open="rb", timeout=2678400L)
  socketOut <- socketConnection(host="localhost", port=ports[2], server=FALSE, blocking=TRUE, open="ab", timeout=2678400L)
  assign("socketIn",socketIn,envir=details)
  assign("socketOut",socketOut,envir=details) 
}

jarsOfPackage <- function(pkgname, major.release) {
  jarsMajor <- list.files(file.path(system.file("java",package=pkgname),paste0("scala-",major.release)),pattern=".*\\.jar$",full.names=TRUE,recursive=FALSE)
  jarsAny <- list.files(system.file("java",package=pkgname),pattern=".*\\.jar$",full.names=TRUE,recursive=FALSE)
  result <- c(jarsMajor,jarsAny)
  if ( length(result) == 0 ) stop(paste0("JAR files of package '",pkgname,"' for Scala ",major.release," were requested, but no JARs were found."))
  result
}

mkCommandLineOptions <- function(command.line.options, heap.maximum) {
  if ( is.null(command.line.options) ) {
    command.line.options <- getOption("rscala.command.line.options",default=NULL)
    if ( is.null(command.line.options) ) {
      if ( is.null(heap.maximum) ) {
        heap.maximum <- getOption("rscala.heap.maximum",default=NULL)
        if ( is.null(heap.maximum) ) {
          memoryPercentage <- 0.85
          bytes <- if ( file.exists("/proc/meminfo") ) {  # Linux
            outTemp <- readLines("/proc/meminfo")
            outTemp <- outTemp[grepl("^MemTotal:\\s*",outTemp)]
            outTemp <- gsub("^MemTotal:\\s*","",outTemp)
            outTemp <- gsub("\\s*kB$","",outTemp)
            as.numeric(outTemp) * 1024
          } else if ( .Platform$OS.type=="windows" ) {    # Windows
            outTemp <- system2("wmic",c("computersystem","get","TotalPhysicalMemory","/VALUE"),stdout=TRUE)
            outTemp <- outTemp[outTemp != "\r"]
            outTemp <- gsub("^TotalPhysicalMemory=","",outTemp)
            outTemp <- gsub("\r","",outTemp)
            as.numeric(outTemp)
          } else if ( grepl("^darwin", R.version$os) ) {  # Mac OS X
            outTemp <- system2("sysctl","hw.memsize",stdout=TRUE)
            outTemp <- gsub("^hw.memsize:\\s*","",outTemp)
            as.numeric(outTemp)
          } else NA                                       # Unknown, so do not do anything.
          heap.maximum <- if ( ! is.na(bytes) ) {
            paste0(as.integer(memoryPercentage * (bytes / 1024^2)),"m")
          } else NULL
        }
      }
      if ( !is.null(heap.maximum) ) {
        command.line.options <- paste("-J",c(paste("-Xmx",heap.maximum,sep=""),"-Xms32M"),sep="")
      }
    }
  } else command.line.options
}

stopProcess <- function(env) {
  # The 'close' function should be used to shutdown the interpreter. But this is
  # a backup method for unusual circumstances. Scala itself will recognize that
  # it needs to quit when the snippet file is deleted. Most platforms are okay
  # will Scala sticking around for a few seconds after R exits. But, on Windows,
  # package checks seem to require that the Scala process be finished before R
  # exits.
  snippetFilename <- env[['snippetFilename']]
  if ( file.exists(snippetFilename) ) {
    unlink(snippetFilename)
    pause <- 6
    diff <- 0
  } else {
    pause <- 3
    diff <- proc.time()['elapsed'] - get("killStamp",envir=env)
  }
  if ( identical(.Platform$OS.type,"windows") && ( ! interactive() ) && ( diff < pause ) ) {
    Sys.sleep(pause-diff)
  }
}

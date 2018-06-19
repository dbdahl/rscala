#' Instantiate a Scala Bridge
#'
#' The \code{\link{scala}} function creates an instance of a Scala bridge.
#' Multiple interpreters can be created and each runs independently with its own
#' memory space. Each interpreter can use multiple threads/cores, but the bridge
#' between \R and Scala is itself not thread-safe, so multiple \R threads/cores
#' should not simultaneously access the same bridge.
#'
#' @param packages Character vector of package names whose embedded JAR files
#'   are to be added to the runtime classpath.
#' @param assign.callback A function taking a Scala bridge as its only argument.
#'   This function is called immediately after the bridge is established.
#' @param assign.name The name of the (promise of the) bridge to be assigned in
#'   the environment given by the \code{assign.env} argument.
#' @param assign.env The environment in which the (promise of the) bridge is
#'   assigned.
#' @param JARs Character vector whose elements are individual JAR files to
#'   be added to the runtime classpath.
#' @param serialize.output Logical indicating whether Scala output should be
#'   serialized back to R.  This is slower and probably only needed on Windows.
#' @param stdout Whether "standard output" results that are not serialized
#'   should be sent.  \code{TRUE} or \code{""} sends output to the R console
#'   (although that may not work on Windows).  \code{FALSE} or \code{NULL}
#'   discards the output.  Otherwise, this is the name of the file that receives
#'   the output.
#' @param stderr Same as \code{stdout}, except influences the "standard error".
#' @param port If \code{0}, two random ports are selected.  Otherwise,
#'   \code{port} and \code{port+1} are used to the TCP/IP connections.
#' @param heap.maximum String indicating the JVM heap maximum, e.g., "8G".
#'   Without this being set, the heap maximum will be 85\% of the physical RAM.
#' @param debug (Developer use only.)  Logical indicating whether debugging
#'   should be enabled.
#' @param command.line.options (Developer use only.)  A character vector
#'   influencing the command line options when launching Scala.
#'
#' @return A Scala bridge
#'
#' @export
#'
#' @examples \dontrun{
#' scala()
#' rng <- s$.new_scala.util.Random()
#' rng$alphanumeric()$take(15L)$mkString(",")
#' s %~% "2+3"
#' h <- s(x=2, y=3) %.~% "x+y"
#' h$toString()
#' s(mean=h, sd=2, r=rng) %~% "mean + sd * r.nextGaussian()"
#' 
#' }
#' 
scala <- function(packages=character(),
                  assign.callback=function(s) {},
                  assign.name="s",
                  assign.env=.GlobalEnv,
                  JARs=character(),
                  serialize.output=.Platform$OS.type=="windows",
                  stdout=TRUE,
                  stderr=TRUE,
                  port=0L,
                  heap.maximum=NULL,
                  command.line.options=NULL,
                  debug=FALSE) {
  if ( identical(stdout,TRUE) ) stdout <- ""
  if ( identical(stderr,TRUE) ) stderr <- ""
  debug <- identical(debug,TRUE)
  serialize.output <- identical(serialize.output,TRUE)
  port <- as.integer(port[1])
  if ( debug && serialize.output ) stop("When debug is TRUE, serialize.output must be FALSE.")
  if ( debug && ( identical(stdout,FALSE) || identical(stdout,NULL) || identical(stderr,FALSE) || identical(stderr,NULL) ) ) stop("When debug is TRUE, stdout and stderr must not be discarded.")
  scalaMajor <- CANONICAL_SCALA_MAJOR_VERSION
  pkgJARs <- unlist(lapply(packages, function(p) jarsOfPackage(p, scalaMajor)))
  rscalaJAR <- list.files(system.file(file.path("java",paste0("scala-",scalaMajor)),package="rscala",mustWork=TRUE),full.names=TRUE)
  rscalaClasspath <- shQuote(paste0(c(rscalaJAR,JARs,pkgJARs),collapse=.Platform$path.sep))
  command.line.options <- shQuote(mkCommandLineOptions(command.line.options,heap.maximum))
  sessionFilename <- tempfile("rscala-session-")
  writeLines(character(),sessionFilename)
  portsFilename <- tempfile("rscala-ports-")
  args <- c(command.line.options,"-classpath",rscalaJAR,"org.ddahl.rscala.Server",rscalaClasspath,port,portsFilename,sessionFilename,debug,serialize.output,FALSE)
  system2(scalaExec(FALSE),args,wait=FALSE,stdout=stdout,stderr=stderr)
  details <- new.env(parent=emptyenv())
  assign("sessionFilename",sessionFilename,envir=details)
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
  # it needs to quit when the session file is deleted. Most platforms are okay
  # will Scala sticking around for a few seconds after R exits. But, on Windows,
  # package checks seem to require that the Scala process be finished before R
  # exits.
  sessionFilename <- env[['sessionFilename']]
  if ( file.exists(sessionFilename) ) {
    unlink(sessionFilename)
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

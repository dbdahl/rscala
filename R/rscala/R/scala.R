#' Instantiate a Scala Bridge
#'
#' This function creates an instance of an rscala bridge.
#'
#' Multiple interpreters can be created and each runs independently with its own
#' memory space. Each interpreter can use multiple threads/cores, but the bridge
#' between \R and Scala is itself not thread-safe, so multiple \R threads/cores
#' should not simultaneously access the same bridge.
#'
#' Terminate the bridge using \code{\link{close.rscalaBridge}}.
#'
#' Rather than calling this function explicitly, packages importing or depending
#' on rscala should instead use the \code{\link{scalaPackage}} and
#' \code{\link{scalaPackageUnload}} functions.
#'
#' @param packages Character vector of package names whose embedded JAR files
#'   are to be added to the classpath.
#' @param assign.callback A function taking a Scala bridge as its only argument.
#'   This function is called immediately after the bridge is connected, which
#'   does not happen until the bridge is actually used and may be long after
#'   this function finishes. This is where setup code goes, like \emph{global}
#'   imports, objects, classes, methods, etc.  For example, it might equal
#'   \code{function(s) { s + 'import scala.util.Random' }}.  \strong{Note} the
#'   use of the execution operator \code{+} instead of the evaluation operator
#'   \code{*}.
#' @param assign.name The name of the (promise of the) bridge to be assigned in
#'   the environment given by the \code{assign.env} argument.
#' @param JARs Character vector whose elements are individual JAR files to be
#'   added to the runtime classpath.
#' @param serialize.output Logical indicating whether Scala output should be
#'   serialized back to R.  This is slower and probably only needed on Windows.
#' @param stdout Whether "standard output" results that are not serialized
#'   should be sent.  \code{TRUE} or \code{""} sends output to the \R console
#'   (although that may not work on Windows).  \code{FALSE} or \code{NULL}
#'   discards the output.  Otherwise, this is the name of the file that receives
#'   the output.
#' @param stderr Same as \code{stdout}, except influences the "standard error".
#' @param port If \code{0}, two random ports are selected.  Otherwise,
#'   \code{port} and \code{port+1} are used to the TCP/IP connections.
#' @param heap.maximum String indicating the JVM heap maximum, e.g., "8G".
#'   Without this being set in either \code{\link{scala}} or
#'   \code{\link{scalaHeapMaximum}}, the heap maximum will be 85\% of the
#'   physical RAM.  The value from \code{\link{scalaHeapMaximum}} supersedes
#'   that from \code{\link{scala}}.
#' @param assign.env (Developer use only.) The environment in which the (promise
#'   of the) bridge is assigned.
#' @param debug (Developer use only.)  Logical indicating whether debugging
#'   should be enabled.
#'
#' @return Returns an rscala bridge.
#' @seealso \code{\link{close.rscalaBridge}}, \code{\link{scalaPackage}},
#'   \code{\link{scalaPackageUnload}}, \code{\link{scalaHeapMaximum}}
#' @export
#'
#' @examples \donttest{
#' scala(assign.name='e')      # Implicitly defines the bridge 'e'.
#' rng <- e $ .new_scala.util.Random()
#' rng $ alphanumeric() $ take(15L) $ mkString(',')
#' e * '2+3'
#' h <- e(x=2, y=3) ^ 'x+y'
#' h $ toString()
#' e(mean=h, sd=2, r=rng) * 'mean + sd * r.nextGaussian()'
#' close(e)
#' }
#' 
scala <- function(packages=character(),
                  assign.callback=function(s) {},
                  assign.name="s",
                  JARs=character(),
                  serialize.output=.Platform$OS.type=="windows",
                  stdout=TRUE,
                  stderr=TRUE,
                  port=0L,
                  heap.maximum=NULL,
                  assign.env=parent.frame(),
                  debug=FALSE) {
  if ( identical(stdout,TRUE) ) stdout <- ""
  if ( identical(stderr,TRUE) ) stderr <- ""
  debug <- identical(debug,TRUE)
  serialize.output <- identical(serialize.output,TRUE)
  port <- as.integer(port[1])
  if ( debug && serialize.output ) stop("When debug is TRUE, serialize.output must be FALSE.")
  if ( debug && ( identical(stdout,FALSE) || identical(stdout,NULL) || identical(stderr,FALSE) || identical(stderr,NULL) ) ) stop("When debug is TRUE, stdout and stderr must not be discarded.")
  sInfo <- scalaInfo(FALSE)
  scalaMajor <- sInfo$majorVersion
  JARs <- c(JARs,unlist(lapply(packages, function(p) jarsOfPackage(p, scalaMajor))))
  rscalaJAR <- shQuote(list.files(system.file(file.path("java",paste0("scala-",scalaMajor)),package="rscala",mustWork=TRUE),full.names=TRUE))
  heap.maximum <- getHeapMaximum(heap.maximum)
  command.line.options <- if ( is.null(heap.maximum) ) NULL
  else shQuote(paste0("-J",c(paste("-Xmx",heap.maximum,sep=""),"-Xms32M")))
  sessionFilename <- tempfile("rscala-session-")
  writeLines(character(),sessionFilename)
  portsFilename <- tempfile("rscala-ports-")
  args <- c(command.line.options,"-classpath",rscalaJAR,"org.ddahl.rscala.Main",rscalaJAR,port,portsFilename,sessionFilename,debug,serialize.output,FALSE)
  system2(sInfo$cmd,args,wait=FALSE,stdout=stdout,stderr=stderr)
  details <- new.env(parent=emptyenv())
  assign("sessionFilename",sessionFilename,envir=details)
  assign("closed",FALSE,envir=details)
  assign("connected",FALSE,envir=details) 
  assign("interrupted",FALSE,envir=details)
  assign("transcompilationHeader",c("import math._","import math.{Pi => pi}"),envir=details)
  assign("debugTranscompilation",FALSE,envir=details)
  assign("debug",debug,envir=details)
  assign("serializeOutput",serialize.output,envir=details)
  assign("last",NULL,envir=details)
  assign("garbage",integer(),envir=details)
  assign("scalaInfo",sInfo,envir=details)
  assign("heapMaximum",heap.maximum,envir=details)
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
      stop("Names must be given in parameter list and they consist only of alphanumeric & underscore characters.")
    }
    attr(bridge2,"details") <- details
    class(bridge2) <- "rscalaBridge"
    bridge2
  }
  attr(bridge,"details") <- details
  class(bridge) <- "rscalaBridge"    
  reg.finalizer(details,close.rscalaBridge,onexit=TRUE)
  if ( ! is.null(assign.name) && ( assign.name != "" ) ) {
    if ( interactive() ) {
      delayedAssign(assign.name,{
        newSockets(portsFilename, details, JARs)
        assign.callback(bridge)
        bridge
      },assign.env=assign.env)
    } else {
      assign(assign.name,{
        newSockets(portsFilename, details, JARs)
        assign.callback(bridge)
        bridge
      },envir=assign.env)
    }
  } else {
    newSockets(portsFilename, details, JARs)
    assign.callback(bridge)
    bridge
  }
}

newSockets <- function(portsFilename, details, JARs) {
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
  assign("connected",TRUE,envir=details)
  if ( length(JARs) > 0 ) scalaAddJARs(details, JARs)
}

jarsOfPackage <- function(pkgname, major.release) {
  jarsMajor <- list.files(file.path(system.file("java",package=pkgname),paste0("scala-",major.release)),pattern=".*\\.jar$",full.names=TRUE,recursive=FALSE)
  jarsAny <- list.files(system.file("java",package=pkgname),pattern=".*\\.jar$",full.names=TRUE,recursive=FALSE)
  result <- c(jarsMajor,jarsAny)
  if ( length(result) == 0 ) stop(paste0("JAR files of package '",pkgname,"' for Scala ",major.release," were requested, but no JARs were found."))
  result
}

getHeapMaximum <- function(heap.maximum) {
  heap.maximum <- getOption("rscala.heap.maximum", default=heap.maximum)
  if ( ! is.null(heap.maximum) ) heap.maximum
  else {
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
    if ( ! is.na(bytes) ) {
      if ( .Machine$sizeof.pointer < 8L ) bytes <- min(c(1.35*1024^3,bytes))   # 32 binaries have limited memory.
      paste0(as.integer(memoryPercentage * (bytes / 1024^2)),"M")
    } else NULL
  }
}

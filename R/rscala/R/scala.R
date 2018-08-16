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
#'   use of the declaration operator \code{+} instead of the operators \code{*}
#'   or \code{^}.  This function might also include calls to
#'   \code{\link{scalaSerializeRegister}} and
#'   \code{\link{scalaUnserializeRegister}}.
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
#' @param heap.maximum String giving Scala's heap maximum, e.g., "8G" or
#'   "512M".  The value here supersedes that from \code{\link{scalaMemory}}.
#'   Without this being set in either \code{\link{scala}} or
#'   \code{\link{scalaMemory}}, the heap maximum will be 90\% of the available
#'   RAM.
#' @param assign.env (Developer use only.) The environment in which the (promise
#'   of the) bridge is assigned.
#' @param debug (Developer use only.)  Logical indicating whether debugging
#'   should be enabled.
#'
#' @return Returns an rscala bridge.
#' @seealso \code{\link{close.rscalaBridge}}, \code{\link{scalaPackage}},
#'   \code{\link{scalaPackageUnload}}, \code{\link{scalaMemory}}
#'   \code{\link{scalaSerializeRegister}},
#'   \code{\link{scalaUnserializeRegister}}
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
  sConfig <- scalaConfig(FALSE)
  scalaMajor <- sConfig$scalaMajorVersion
  JARs <- c(JARs,unlist(lapply(packages, function(p) jarsOfPackage(p, scalaMajor))))
  rscalaJAR <- shQuote(list.files(system.file(file.path("java",paste0("scala-",scalaMajor)),package="rscala",mustWork=TRUE),full.names=TRUE))
  heap.maximum <- getHeapMaximum(heap.maximum,sConfig$javaArchitecture == 32)
  command.line.options <- if ( is.null(heap.maximum) ) NULL
  else shQuote(paste0("-J-Xmx",heap.maximum))
  sessionFilename <- tempfile("rscala-session-")
  writeLines(character(),sessionFilename)
  portsFilename <- tempfile("rscala-ports-")
  args <- c(command.line.options,"-classpath",rscalaJAR,"org.ddahl.rscala.Main",rscalaJAR,port,portsFilename,sessionFilename,debug,serialize.output,FALSE)
  system2(normalizePath(sConfig$scalaCmd,mustWork=TRUE),args,wait=FALSE,env=paste0("JAVACMD=",sConfig$javaCmd),stdout=stdout,stderr=stderr)
  details <- new.env(parent=emptyenv())
  assign("sessionFilename",sessionFilename,envir=details)
  assign("closed",FALSE,envir=details)
  assign("connected",FALSE,envir=details) 
  assign("pid",Sys.getpid(),envir=details)
  assign("interrupted",FALSE,envir=details)
  transcompileHeader <- c("import org.ddahl.rscala.Transcompile._","import scala.util.control.Breaks", unlist(lapply(packages,transcompileHeaderOfPackage)))
  assign("transcompileHeader",transcompileHeader,envir=details)
  assign("transcompileSubstitute",unlist(lapply(packages,transcompileSubstituteOfPackage)),envir=details)
  assign("debugTranscompilation",FALSE,envir=details)
  assign("serializers",list(scalaSerialize.list,scalaSerialize.generic),envir=details)
  assign("unserializers",list(scalaUnserialize.list,scalaUnserialize.generic),envir=details)
  assign("debug",debug,envir=details)
  assign("serializeOutput",serialize.output,envir=details)
  assign("last",NULL,envir=details)
  assign("garbage",integer(),envir=details)
  assign("config",sConfig,envir=details)
  assign("heapMaximum",heap.maximum,envir=details)
  assign("JARs",character(0),envir=details)
  gcFunction <- function(e) {
    garbage <- details[["garbage"]]
    garbage[length(garbage)+1] <- e[["id"]]
    assign("garbage",garbage,envir=details)
  }
  assign("gcFunction",gcFunction,envir=details)
  reg.finalizer(details,close.rscalaBridge,onexit=TRUE)
  bridge <- mkBridge(details)
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

mkBridge <- function(details) {
  bridge <- function(...) {
    bridge2 <- list(...)
    argnames <- names(bridge2)
    if ( is.null(argnames) ) {
      argnames <- sapply(substitute(list(...))[-1], deparse)
      names(bridge2) <- argnames
    } else {
      w <- argnames == ""
      if ( any(w) ) {
        argnames[w] <- sapply(substitute(list(...))[-1], deparse)[w]
        names(bridge2) <- argnames
      }
    }
    if( ( length(bridge2) > 0 )  && ( is.null(argnames) || ! all(grepl("^[a-zA-Z]\\w*$",argnames,perl=TRUE)) ) ) {
      stop("Every argument must be a named (e.g, x=3) or a symbol (e.g., x) and not a literal (e.g., 3).")
    }
    attr(bridge2,"details") <- details
    class(bridge2) <- "rscalaBridge"
    bridge2
  }
  attr(bridge,"details") <- details
  class(bridge) <- "rscalaBridge"    
  bridge
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
  if ( length(JARs) > 0 ) scalaAddJARs(JARs, details)
}

jarsOfPackage <- function(pkgname, major.release) {
  dir <- if ( file.exists(system.file("inst",package=pkgname)) ) file.path("inst/java") else "java"
  if ( major.release == "2.13" ) major.release <- paste0(major.release,".0-M4")
  jarsMajor <- list.files(file.path(system.file(dir,package=pkgname),paste0("scala-",major.release)),pattern=".*\\.jar$",full.names=TRUE,recursive=FALSE)
  jarsAny <- list.files(system.file(dir,package=pkgname),pattern=".*\\.jar$",full.names=TRUE,recursive=FALSE)
  result <- c(jarsMajor,jarsAny)
  if ( length(result) == 0 ) stop(paste0("JAR files of package '",pkgname,"' for Scala ",major.release," were requested, but no JARs were found."))
  result
}

#' @importFrom utils getFromNamespace
#' 
transcompileHeaderOfPackage <- function(pkgname) {
  tryCatch( getFromNamespace("rscalaTranscompileHeader", pkgname), error=function(e) NULL )
}

#' @importFrom utils getFromNamespace
#' 
transcompileSubstituteOfPackage <- function(pkgname) {
  tryCatch( getFromNamespace("rscalaTranscompileSubstitute", pkgname), error=function(e) NULL )
}

osType <- function() {
  if ( .Platform$OS.type == "windows" ) "windows"
  else if ( Sys.info()["sysname"] == "Darwin" ) "osx"
  else "linux"
}

getHeapMaximum <- function(heap.maximum,is32bit) {
  if ( ! is.null(heap.maximum) ) return(heap.maximum)
  heap.maximum <- getOption("rscala.heap.maximum")
  if ( ! is.null(heap.maximum) ) return(heap.maximum)
  memoryPercentage <- 0.90
  os <- osType()
  bytes <- if ( os == "linux" ) {
    outTemp <- readLines("/proc/meminfo")
    outTemp <- outTemp[grepl("^MemAvailable:\\s*",outTemp)]
    outTemp <- gsub("^MemAvailable:\\s*","",outTemp)
    outTemp <- gsub("\\s*kB$","",outTemp)
    as.numeric(outTemp) * 1024
  } else if ( os == "windows" ) {
    outTemp <- system2("wmic",c("/locale:ms_409","OS","get","FreePhysicalMemory","/VALUE"),stdout=TRUE)
    outTemp <- outTemp[outTemp != "\r"]
    outTemp <- gsub("^FreePhysicalMemory=","",outTemp)
    outTemp <- gsub("\r","",outTemp)
    as.numeric(outTemp) * 1024
  } else if ( os == "osx" ) {
    outTemp <- system2("vm_stat",stdout=TRUE)
    outTemp <- outTemp[grepl("(Pages free|Pages inactive|Pages speculative):.*",outTemp)]
    sum(sapply(strsplit(outTemp,":"),function(x) as.numeric(x[2]))) * 4096
  } else NA                                       # Unknown, so do not do anything.
  if ( ! is.na(bytes) ) {
    if ( is32bit ) bytes <- min(c(1.35*1024^3,bytes))   # 32 binaries have limited memory.
    paste0(max(32,as.integer(memoryPercentage * (bytes / 1024^2))),"M")  # At least 32M
  } else NULL
}

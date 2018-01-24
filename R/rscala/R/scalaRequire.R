scalaRequire <- function(jar.file, interpreter=findScalaInstance()) {
  if ( ! file.exists(jar.file) ) stop("JAR file does not exist.")
  cc(interpreter)
  wb(interpreter,ADD_CLASS_PATH)
  wc(interpreter,jar.file)
  flush(interpreter[['socketIn']])
  status <- rb(interpreter,"integer")
  if ( get("serializeOutput",envir=interpreter[['env']]) ) echoResponseScala(interpreter)
  if ( status != OK ) {
    stop("Problem defining function.")
  }
  invisible()
}

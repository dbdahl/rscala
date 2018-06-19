scalaEvaluate <- function(details, snippet) {
  scalaLastEngine(details)
  if ( details[["interrupted"]] ) return(invisible())
  socketOut <- details[["socketOut"]]
  wb(socketOut,PCODE_EVALUATE)
  wc(socketOut,snippet)
  invisible(pop(details))
}

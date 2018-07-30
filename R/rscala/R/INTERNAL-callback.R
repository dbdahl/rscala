callback <- function(details, envir) {
  socketIn <- details[["socketIn"]]
  snippet <- rc(socketIn)
  nArgs <- rb(socketIn,RTYPE_INT)
  # env <- parent.frame(4)   ### This is very fragile!
  env <- envir
  args <- vector(mode="list", length=nArgs)
  while ( TRUE ) {
    argsListName <- paste0(".rs",sample.int(.Machine$integer.max,1L))
    if ( ! exists(argsListName,envir=env) ) break
  }
  for ( i in seq_len(nArgs) ) {
    arg <- pop(details)
    rhs <- paste0(argsListName,"[[",i,"]]")
    if ( is.list(arg) ) {
      arg <- arg[['value']]
      rhs <- paste0("unserialize(",rhs,")")
    }
    snippet <- sub("%-",rhs,snippet)
    args[[i]] <- arg
  }
  assign(argsListName,args,envir=env)  
  result <- tryCatch(eval(parse(text=snippet),envir=env), error=function(e) {
    cat(toString(e))
    NULL
  })
  rm(list=argsListName,envir=env)
  socketOut <- details[["socketOut"]]
  wb(socketOut, PCODE_REXIT)
  pushOkay <- push(result, NULL, socketOut)
  if ( ! identical(pushOkay,TRUE) ) {
    cat(attr(pushOkay,"msg"),"\n",sep="")
    push(NULL, NULL, socketOut)
  }
}

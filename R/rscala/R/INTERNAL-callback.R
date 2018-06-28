callback <- function(details, callingEnv) {
  socketIn <- details[["socketIn"]]
  snippet <- rc(socketIn)
  nArgs <- rb(socketIn,RTYPE_INT)
  args <- vector(mode="list", length=nArgs)
  while ( TRUE ) {
    argsListName <- paste0(".rs",sample.int(.Machine$integer.max,1L))
    if ( ! exists(argsListName,envir=callingEnv) ) break
  }
  for ( i in seq_len(nArgs) ) {
    snippet <- sub("%-",paste0(argsListName,"[[",i,"]]"),snippet)
    args[[i]] <- pop(details, NULL)
  }
  assign(argsListName,args,envir=callingEnv)  
  result <- tryCatch(eval(parse(text=snippet),envir=callingEnv), error=function(e) {
    cat(toString(e))
    NULL
  })
  rm(list=argsListName,envir=callingEnv)
  socketOut <- details[["socketOut"]]
  wb(socketOut, PCODE_REXIT)
  pushOkay <- push(result, NULL, socketOut)
  if ( ! identical(pushOkay,TRUE) ) {
    cat(attr(pushOkay,"msg"),"\n",sep="")
    push(NULL, NULL, socketOut)
  }
}

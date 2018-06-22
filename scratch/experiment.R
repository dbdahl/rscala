env <- new.env(parent=.GlobalEnv)

snippet <- "rnorm(%-, mean=%-, sd=%-)"
args <- list(10L, 3.0, 1.0)

tempvar <- function(env) {
  while ( TRUE ) {
    candidate <- paste0(".rs",sample.int(.Machine$integer.max,1L))
    if ( ! exists(candidate,envir=env) ) return(candidate)
  }
}

argsListName <- tempvar(env)
assign(argsListName,args,envir=env)
for ( i in seq_along(args) ) {
  snippet <- sub("%-",paste0(argsListName,"[[",i,"]]"),snippet)
}
result <- eval(parse(text=snippet),envir=env)
rm(list=argsListName,envir=env)



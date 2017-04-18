port <- 42325
source("common.R",print.eval=TRUE)

port <- 42325
tryCatch(source("common.R",print.eval=TRUE), error=function(e) e)


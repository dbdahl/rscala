source("common.R",print.eval=TRUE)


set.seed(3432585)

s$asdf <- 3
s$asdf
s %~% "3+4"

tryCatch(s$adf,error = function(e) e)

tryCatch(s$.val("def"),error = function(e) e)
tryCatch(s$def <- 4,error = function(e) e)
s %~% "3+4"

f <- s$ def(x=I(numeric()),y=I(numeric())) %~% '
  x+y
'
f(3,4)

x <- as.character(0:8)
set.seed(234235)
s$x <- rnorm(100)
tryCatch(s$val(x),error = function(e) e)
identical(s$x,s$.val("x"))
identical(s$x,s$val("x"))

s %~% "val $bob = 4"
s$'$bob'
s$val('$bob')


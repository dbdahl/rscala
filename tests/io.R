source("common.R",print.eval=TRUE)

tryCatch(s %~% "new java.util.Random(23455).nextDoubllllllllle",error=function(e) e)
capture.output(s %~% "new java.util.Random(234523).nextDouble")
capture.output(s %~% "3+2")
capture.output(s %~% "println(3+2)")
capture.output(s %~% 'R.eval("""cat(R.version.string)""")')


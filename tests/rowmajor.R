a <- matrix(c(1,2,3,4,5,6),nrow=2,byrow=TRUE)
a

row.major <- TRUE
source("common.R",print.eval=TRUE)
s$a <- a
s %@% 'a.foreach(x => println(x.mkString(" ")))'
s$a
if ( ! identical(s %~% 'a.length', 2L) ) stop("Error")
s %@% 'R.set("b",a)'
if ( ! identical(s %~% 'R.getD2("b")', a) ) stop("Error")



a <- matrix(c(1,2,3,4,5,6),nrow=2,byrow=FALSE)
a

row.major <- FALSE
source("common.R",print.eval=TRUE)
s$a <- a
s %@% 'a.foreach(x => println(x.mkString(" ")))'
s$a
if ( ! identical(s %~% 'a.length', 3L) ) stop("Error")
s %@% 'R.set("b",a)'
if ( ! identical(s %~% 'R.getD2("b")', a) ) stop("Error")



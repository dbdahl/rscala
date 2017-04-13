source("common.R",print.eval=TRUE)


f <- s$def(x=s$null("(Int,Int)")) %~% 'x._1 + x._2'
g <- s %~% "(300,400)"
f(g)
f(s %~% "(30,40)")

f2 <- s$def() %~% 'println("Yes")'
f2()
capture.output(f2())

a <- s %.~% "(300,234)"
f1 <- s$def(x=s$null("(Int,Int)"),y=numeric()) %~% 'x._1 + x._2 + y.sum'
f1(a,c(2,3,4,6))

f1 <- s$def(x=s$null("(Int,Int)"),y=s$null("Array[Double]")) %~% 'x._1 + x._2 + y.sum'
b <- s %.~% "Array[Double](2,3,4,5)"
f1(a,b)



(s$def() %~% 'println("Yes")')()
(s$def() %~% '0')()
(s$def() %~% 'null')()




tryCatch(s$def('','a+b')(),error = function(e) e)
tryCatch(s$def('','a+')(),error = function(e) e)
tryCatch(s$def('','import org.asdfad')(),error = function(e) {e})
tryCatch(s$def('','throw new RuntimeException()')(),error = function(e) {e})
s %~% "5+6"   # Everything's still okay!

tryCatch(s$def('x,y','x+y')(),error = function(e) e)
s$def('x :    Int,y:Double','x+y')(2,3)
s$def('x:Int, y:Double','x+y')(4,2)

s$def('x:Boolean, y:Double','if ( x ) y else 2*y')(TRUE,2)
s$def('x:Boolean, y:Double','if ( x ) y else 2*y')(FALSE,2)

s$def('x:String, y:Int','if ( x=="bob" ) y else 2*y')('bob',2)
s$def('x:String, y:Int','if ( x=="bob" ) y else 2*y')('bobby',2)

s$def('x:Array[String], y:Array[Int]','if ( x(0)=="bob" ) y(0) else 2*y(0)')('bob',2)
s$def('x:Array[String], y:Array[Int]','if ( x(0)=="bob" ) y(0) else 2*y(0)')('bobby',2)
s$def('x:Array[Boolean], y:Array[Double]','if ( x(0) ) y(0) else 2*y(0)')(TRUE,2)
s$def('x:Array[Boolean], y:Array[Double]','if ( x(0) ) y(0) else 2*y(0)')(TRUE,2)

s$def('x:Array[String], y:Array[Int]','if ( x(0)=="bob" ) y(0) else 2*y(0)')(c('bob','bill'),c(2,10))
s$def('x:Array[String], y:Array[Int]','if ( x(0)=="bob" ) y(0) else 2*y(0)')(c('bobby','bill'),c(2,10))
s$def('x:Array[Boolean], y:Array[Double]','if ( x(0) ) y(0) else 2*y(0)')(c(TRUE,FALSE),c(2,10))
s$def('x:Array[Boolean], y:Array[Double]','if ( x(0) ) y(0) else 2*y(0)')(c(FALSE,TRUE),c(2,10))

s$def('x:Array[Array[String]], y:Array[Array[Int]]','if ( x(0)(0)=="bob" ) y(0)(0) else 2*y(0)(0)')(matrix(c('bob','bill'),ncol=1),matrix(c(2,10),ncol=1))
s$def('x:Array[Array[String]], y:Array[Array[Int]]','if ( x(0)(0)=="bob" ) y(0)(0) else 2*y(0)(0)')(matrix(c('bobby','bill'),ncol=1),matrix(c(2,10),ncol=1))
s$def('x:Array[Array[Boolean]], y:Array[Array[Double]]','if ( x(0)(0) ) y(0)(0) else 2*y(0)(0)')(matrix(c(TRUE,FALSE),ncol=1),matrix(c(2,10),ncol=1))
s$def('x:Array[Array[Boolean]], y:Array[Array[Double]]','if ( x(0)(0) ) y(0)(0) else 2*y(0)(0)')(matrix(c(FALSE,TRUE),ncol=1),matrix(c(2,10),ncol=1))

f <- s$def("x: Double, w: Int, r: Double","
  math.sqrt(x)+w-r
")
f
f(3,4,54)

g <- s$def('x:Array[String], y:Array[Int]','if ( x(0)=="bob" ) y(0) else 2*y(0)')
g("David",3)

f1 <- s$def("x: Double, w: Int, r: Double","
  math.sqrt(x)+w-r
")
f1(3,4,54)

f2 <- s$def("x: Double","2*x")
f2(f1(3,4,54))

system.time(sapply(1:100,function(i) f1(i,4,54)))


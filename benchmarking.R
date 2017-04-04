library(rscala)
library(microbenchmark)

s <- scala()
e <- s %~% 'new scala.util.Random()'
s$w <- 4

microbenchmark(
  s$w,
  s %~% 'w',
  s$a <- 4,
  s$b <- rnorm(100),
  e$nextDouble(),
  e$toString(),
  times=50
)



s <- scala(debug=TRUE)

(s %new% 'scala.util.Random')(5L)


(s %new% 'Array[Int]')(6L)








bob <- function() {
  s$a <- 1L
  s$b <- 8L
  s %~% 'Array(a,b)'
}

bill <- function(a,b) {
  s %~% 'Array(@{a},@{b})'
}

sam <- function(a,b) {
  s %~% 'Array(R.getI0("a"),R.getI0("b"))'
}

ultimate <- function(a,b) {
  method <- s %~% '() => { Array(R.getI0("a"),R.getI0("b")) }'
  s$invoke(method)
}

microbenchmark(
  (s %~% 'Array')$apply(1L,8L),
  bob(),
  bill(1L,8L),
  sam(1L,8L),
  times=50
)



is.integer((s %~% 'Array')$apply(1L,8L))
is.integer((s %~% 'Array')$'apply[Double]'(1L,8L))





library(rscala)

s <- scala(debug=FALSE)

see <- "David"
bob <- function() {
  see <- "B."
  x <- "Milly"
  print(environment())
  r <- s$def2(x=scalaPrimitive("Mack"),y=scalaPrimitive("Bob")) %~% '
    x+" "+y+" "+R.getS0("see")
  '
  r
}
y1 <- bob()
y1("Lisa","Dahl")


bob2 <- function() {
  see <- "MMMM."
  x <- "Milly"
  r <- s$def2(x=scalaPrimitive("Mack"),y=scalaPrimitive("Bob")) %.~% '
      x+" "+y+" "+R.getS0("see")
  '
  r
}
y2 <- bob2()
y2("Lisa","Dahl")
s %@% '2'
gc()



bill <- function() {
  see <- "Knudsen"
  y1("Lisa","Dahl")
}
bill()








y("Lisa","Dahl")

see <- "Mack"
y("Lisa","Dahl")





system.time(f <- s$def2(x=scalaPrimitive(0L),name=scalaPrimitive("dog")) %~% '
  name + " " + x
')

t <- f(340,34)
t

t <- f(x=101L,name="bob")
t

# system.time(e <- s$def2(x=scalaPrimitive(0),name=scalaPrimitive("dog"),rng=scalaType("java.util.Random")) %~% '
#   name + " " + rng.nextInteger(x)
# ')

system.time(e <- s$def2(x=scalaPrimitive(0L),name=scalaPrimitive("dog"),rng=s %.~% 'new scala.util.Random()') %~% '
  name + " " + rng.nextInt(x)
')

f <- s %.~% 'new scala.util.Random()'
g <- (s$def2() %.~% 'new scala.util.Random()')()

e(x=100L,name="bill",rng=f)
e(10,"bill",f)
e(10,"bill",g)



e(x=scalaPrimitive(0L),name=scalaPrimitive("dog"),rng=s %.~% 'new scala.util.Random()')

system.time(f <- s$def2(x=scalaPrimitive(0L),name=scalaPrimitive("dog"),rng=s %.~% 'new scala.util.Random()') %~% '
  name + " " + rng.nextInt(x)
')

identical(e,f)

















# Initial run to get hot-spot compiler going
system.time(e <- s$def2(x=scalaPrimitive(0),yy=scalaPrimitive(4L),name=scalaPrimitive("dog")) %~% '
  name + " " + ( x + yy )
')
system.time(e <- s$def2(x=scalaPrimitive(0),yyy=scalaPrimitive(4L),name=scalaPrimitive("dog")) %~% '
  name + " " + ( x + yyy )
')
system.time(e <- s$def2(x=scalaPrimitive(0),yyyy=scalaPrimitive(4L),name=scalaPrimitive("dog")) %~% '
  name + " " + ( x + yyyy )
')
system.time(e <- s$def2(x=scalaPrimitive(0),yyyyy=scalaPrimitive(4L),name=scalaPrimitive("dog")) %~% '
  name + " " + ( x + yyyyy )
')
system.time(e <- s$def2(x=scalaPrimitive(0),yyyyyy=scalaPrimitive(4L),name=scalaPrimitive("dog")) %~% '
  name + " " + ( x + yyyyyy )
')
system.time(e <- s$def2(x=scalaPrimitive(0),yyyyyyy=scalaPrimitive(4L),name=scalaPrimitive("dog")) %~% '
  name + " " + ( x + yyyyyyy )
')






# Realistic timing
system.time(e <- s$def2(x=scalaPrimitive(0),y=scalaPrimitive(4L),name=scalaPrimitive("dog")) %~% '
  name + " " + ( x + y )
')

# Taking advantage of caching
system.time(f <- s$def2(x=scalaPrimitive(0),y=scalaPrimitive(4L),name=scalaPrimitive("dog")) %~% '
  name + " " + ( x + y )
')

identical(e,f)




system.time(f <- s$def2(x=scalaPrimitive(0),y=scalaPrimitive(4L),name=scalaPrimitive("dog")) %~% '
  2+3
')

yy <- function(x,y) {
  paste0(x," ",y)
}

microbenchmark(
  y("Lisa","Dahl"),
  yy("Lisa","Dahl"),
  times=50
)


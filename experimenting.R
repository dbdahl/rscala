library(rscala)

s <- scala()

b <- 3

func <- function(xx,a) {
  a <- a * 3
  bob <- function(b) {
    print(s %~% '(0 until 100).foreach(i => println(R.evalD0("b")))')
  }
  bob(55)
  s %~% 'R.evalD0("a")'
}

func(55,4)


b <- 3
b
d <- s$def('a: Int','
  R.eval(s"b <- ${a}")
')
d(33)
b



sas <- function() {
  cat('Hello44aa.\n')
}

bobby <- s$def('','
  R.eval("sas()")
')

func <- function(xx,a) {
  a <- a * 3
  bob <- function(b) {
    print(s %~% '(0 until 100).foreach(i => println(R.evalD0("b")))')
  }
  bob(55)
  s %@% 'R.eval("xx()")'
  s %~% 'R.evalD0("a")'
}

func(bobby,4)


rng <- s %~% 'new scala.util.Random()'









x <- function() { 3 }
s %@% '
  object Bob {
    def doit() = {
      R.evalD0("x()")
    }
  }
'
d <- s$do("Bob")
d$doit()


billy <- function() {
  x <- function() { 7 }
  s %@% '
    object Bob {
      def doit() = {
        R.evalD0("x()")
      }
    }
  '
  s$do("Bob")
}

d <- billy()
d$doit()

bob <- function(e) {
  x <- function() { 8 }
  e$doit()
}

bob(d)


s$bob <- 4





a <- 5
y <- function() {
  a <- 10
  b <- s %~% 'R.getD0("a")'
  s %~% 'R.evalD0("a+b")'
}

y()


e <- 40
y <- function() {
  e <- 1
  s$def('a: Double','
    a + R.getD0("e")
  ')
}

yy <- function() {
  e <- 5
  y()(7)
}

yy()









scalaInvoke <- function(reference,workspace,method.name,...) {
  interpreter <- reference[['interpreter']]
  args <- list(...)
  argsStringVector <- sapply(seq_along(args), function(i) {
    x <- args[[i]]
    if ( inherits(x,"ScalaInterpreterReference") ) x[['identifier']]
    else {
      identifier <- paste0('_x',i)
      scalaSet(interpreter, identifier, x, FALSE, workspace)
      identifier
    }
  })
  argsString <- if ( length(argsStringVector) == 0 ) ""
  else paste0("(",argsStringVector,")",collapse=",")
  if ( is.na(method.name) ) method.name <- "apply"
  snippet <- paste0('@{reference}.',method.name,argsString)
  interpreter %~% snippet
}

'$.ScalaInterpreterReference' <- function(reference,functionName) {
  workspace <- parent.frame()
  function(...) { scalaInvoke(reference,workspace,functionName,...) }
}

e <- s %~% 'new scala.util.Random()'

b <- 56
f <- s %~% '(x: Int) => x + @{e}.nextDouble + R.evalD0("b")'
s %~% '@{f}(500)'






f$apply(500L)
e$nextDouble()
e$alphanumeric()$take(5L)$take(3L)$mkString()

f$apply(500L)
e$nextDouble()

r <- s %.~% 'scala.sys.env.get("HOME").get'
r$toString()

r <- s %.~% 'scala.sys.env'
r$get("HOME")$get()


t <- s %~% '(3,4)'
t$"_1"()


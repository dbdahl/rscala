library(rscala)
scala()

"/.rscalaBridge" <- function(bridge,snippet) {
  details <- attr(bridge,"details")
  transcompilation <- r2scala(substitute(snippet),details[["debugTranscompilation"]])
  header <- details[["transcompilationHeader"]]
  header <- if ( length(header) > 0 ) paste0(paste0(header,collapse="\n"),"\n") else NULL
  if ( is.function(bridge) ) bridge ^ paste0("() => {\n", header, transcompilation,"\n}")
  else {
    whichInternal <- sapply(bridge,function(x) inherits(x,"rscalaType"))
    internalArgs <- bridge[whichInternal]
    newBridge <- bridge[!whichInternal]
    namesNewBridge <- names(newBridge)
    mostattributes(newBridge) <- attributes(bridge)
    names(newBridge) <- namesNewBridge
    internalArgsList <- paste0(names(internalArgs),": ",internalArgs,collapse=", ")
    newBridge ^ paste0("(", internalArgsList, ") => {\n", header, transcompilation, "\n}")
  }
}

r2scala <- function(x, showCode=FALSE) {
  if ( length(x) == 0 ) return("")
  typeof  <- sapply(x,function(y) typeof(y))
  classes <- sapply(x,function(y) class(y))
  strings <- sapply(x,function(y) toString(y))
  if ( showCode ) {
    cat("<<\n")
    print(typeof)
    print(classes)
    print(strings)
    cat(">>\n")
  }
  if ( ( typeof[1] == "symbol" ) && ( classes[1] == "name" ) ) {
    if ( strings[1] == "{" ) paste0("{\n",paste0(sapply(x[-1],r2scala,showCode=showCode),collapse="\n"),"\n}")
    else if ( strings[1] == "<-" ) paste0("val ",r2scala(x[[2]],showCode=showCode)," = ",r2scala(x[[3]],showCode=showCode))
    else if ( strings[1] == ":" ) paste0("Array.range(",r2scala(x[[2]],showCode=showCode),".toInt,",r2scala(x[[3]],showCode=showCode),".toInt+1)")
    else if ( strings[1] == "(" ) paste0("(",r2scala(x[[2]],showCode=showCode),")")
    else if ( strings[1] == "+" ) if ( length(x) == 3 ) paste0(r2scala(x[[2]],showCode=showCode)," + ",r2scala(x[[3]],showCode=showCode)) else paste0("+",r2scala(x[[2]],showCode=showCode))
    else if ( strings[1] == "-" ) if ( length(x) == 3 ) paste0(r2scala(x[[2]],showCode=showCode)," - ",r2scala(x[[3]],showCode=showCode)) else paste0("-",r2scala(x[[2]],showCode=showCode))
    else if ( strings[1] == "*" ) paste0(r2scala(x[[2]],showCode=showCode)," * ",r2scala(x[[3]],showCode=showCode))
    else if ( strings[1] == "/" ) paste0(r2scala(x[[2]],showCode=showCode),".toDouble / ",r2scala(x[[3]],showCode=showCode))
    else if ( strings[1] == "%%" ) paste0(r2scala(x[[2]],showCode=showCode)," % ",r2scala(x[[3]],showCode=showCode))
    else if ( strings[1] == "^" ) paste0("pow(",r2scala(x[[2]],showCode=showCode),",",r2scala(x[[3]],showCode=showCode),")")
    else if ( ! is.list(x) && is.symbol(x) ) paste0(strings[1])
    else paste0(strings[1],"(",paste0(sapply(x[-1],r2scala,showCode=showCode),collapse=","),")")
  }
  else if ( typeof[1] == "integer" ) paste0("{",strings[1],":Int}")
  else if ( typeof[1] == "double" ) paste0("{",strings[1],":Double}")
  else if ( typeof[1] == "character" ) paste0('{"',strings[1],'":String}')
  else stop("33")
}

scalaType <- function(type) {
  class(type) <- "rscalaType"
  type
}

r <- s / {
  2+3 
}

r <- s(x=scalaType("Double"),mean=0,sigma=3) / {
  -0.5*log(2*pi*sigma^2) - 0.5/sigma^2 * (x-mean)^2
}

s$debugTranscompilation <- FALSE
s$showCode <- FALSE







s * r2scala(quote("DavidDahl"))

s * r2scala(quote(1 - 3L))

r2scala(quote(davidDahl <- 1 + 3L))
r2scala(quote(davidDahl <- 1L / 3L))
d <- s * paste0("(x: Double) => {\n",r2scala(quote(x + 3 / ( 2 * 4 * 3 ))),"\n}")
r2scala(quote(rbob(1,3L)))

r2scala(quote(rbob))
r2scala(quote(rbob()))
r2scala(quote(rbob(2,mean(1,2,3))))
r2scala(quote(rbob(2,3,4)))

cat(r2scala(quote({
  rbob(2,3,4)
  3+4
})))

cat(r2scala(quote({
  x <- rnorm(10)
  mean(x)
})))

cat(r2scala(quote({
  -0.5*log(2*pi*sigma^2) - 0.5/sigma^2 * (x-mean)^2
})))

a <- quote(rbob)
b <- quote(rbob())




asses
r2scala <- function(x) {
  if ( e == "{" ) {
    "locally {" + r2scala(e[-1]) + "}"
  } else for ( e in x ) {
    if ( e == "<-" ) 
  }
}



length(a)
a[[1]]
b2 <- a[[2]]
b3 <- a[[3]]

b <- quote(a)

a[[1]]
class(a[[2]][[2]])
as.list(a)


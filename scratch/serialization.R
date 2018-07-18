library(rscala)
scala()
s

verbose <- FALSE
a <- scalaSerialize(c(1,2,3),verbose=verbose)
b1 <- scalaSerialize(list(a=1,b=2,c=3),verbose=verbose)
b2 <- scalaSerialize.generic(list(a=1,b=2,c=3),verbose=verbose)

c <- scalaSerialize(mtcars,verbose=verbose)
d <- scalaSerialize(iris,verbose=TRUE)



e1 <- scalaSerialize("David",verbose=TRUE)

scalaSerialize.character <- function(x, bridge=scalaFindBridge(), verbose=FALSE) {
  if ( verbose ) cat("scalaSerializer.character: Trying...\n")
  if ( is.character(x) ) {
    cat("scalaSerializer.character: Success.\n")
    bridge(x=x) ^ 'x'
  }
}

scalaRegisterSerializer(scalaSerialize.character)

e2 <- scalaSerialize("David",verbose=TRUE)
e3 <- scalaSerialize.generic("David",verbose=TRUE)

library(rscala)
scala()
s

verbose <- TRUE
x <- c(1,2,3)
a <- scalaSerialize(x,verbose=verbose)
x2 <- scalaUnserialize(a,verbose=verbose)
identical(x,x2)


b1 <- scalaSerialize(list(a=1,b=2,c=3),verbose=verbose)

x <- list(1,2,3)
b2 <- scalaSerialize.generic(x,verbose=verbose)
x2 <- scalaUnserialize.generic(b2)
identical(x,x2)

c <- scalaSerialize(mtcars,verbose=verbose)
d <- scalaSerialize(iris,verbose=TRUE)



e1 <- scalaSerialize("David",verbose=TRUE)

scalaSerialize.character <- function(x, bridge=scalaFindBridge(), verbose=FALSE) {
  if ( verbose ) cat("scalaSerializer.character: Trying...\n")
  if ( is.character(x) ) {
    cat("scalaSerializer.character: Success.\n")
    bridge(x=x) ^ 'x'
  } else NULL
}

scalaRegisterSerializer(scalaSerialize.character)

e2 <- scalaSerialize("David",verbose=TRUE)
e3 <- scalaSerialize.generic("David",verbose=TRUE)



scalaUnserialize(e2)

scalaUnserialize.character <- function(reference, type=scalaType(reference), bridge=scalaFindBridge(reference), verbose=FALSE) {
  if ( verbose ) cat("scalaUnserializer.character: Trying...\n")
  if ( type == "String" ) {
    cat("scalaUnserializer.character: Success.\n")
    reference$toString()
  } else NULL
}
scalaRegisterUnserializer(scalaUnserialize.character)

scalaUnserialize(e2)



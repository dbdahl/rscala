library(rscala)
scala()

myList <- list(a=1, b=c(TRUE,FALSE), c=I(3.0))
a <- scalaSerialize(myList, s, verbose=TRUE)
scalaUnserialize(a)


identical(scalaUnserialize(scalaSerialize(myList, s)),myList)

identical(scalaUnserialize(scalaSerialize(mtcars, s)),mtcars)

iris$Species <- as.character(iris$Species)
identical(scalaUnserialize(scalaSerialize(iris, s)),iris)

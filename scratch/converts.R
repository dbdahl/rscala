library(rscala)
scala()

myList <- list(a=1, b=c(TRUE,FALSE), c=I(3.0))
identical(scalaUnserialize(scalaSerialize(myList, s)),myList)
identical(scalaUnserialize(scalaSerialize(myList, s), use.original=FALSE),myList)

identical(scalaUnserialize(scalaSerialize(mtcars, s)),mtcars)
identical(scalaUnserialize(scalaSerialize(mtcars, s), use.original=FALSE),mtcars)

iris$Species <- as.character(iris$Species)
identical(scalaUnserialize(scalaSerialize(iris, s)),iris)
identical(scalaUnserialize(scalaSerialize(iris, s), use.original=FALSE),iris)

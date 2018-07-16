library(rscala)
scala()

myList <- list(a=1, b=c(TRUE,FALSE), c=I(3.0))
identical(scalaUnserializeList(scalaSerializeList(s, myList)),myList)

iris$Species <- as.character(iris$Species)
identical(scalaUnserializeList(scalaSerializeList(s, iris[,-5], verbose=TRUE)),iris[,-5])


a <- scalaSerializeList(s, mtcars)
identical(scalaUnserializeList(a),mtcars)


big <- rbind(iris)
big <- rbind(big,big,big,big)
big <- rbind(big,big,big,big)
big <- rbind(big,big,big,big)
big <- rbind(big,big,big,big)
big <- rbind(big,big,big,big)
a <- scalaSerializeList(s, big, "Iris")
identical(scalaUnserializeList(a),big)




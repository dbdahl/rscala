library(rscala)
scala()

myList <- list(a=1, b=c(TRUE,FALSE), c=I(3.0))
scalaSerialize(myList, s, name="myList", verbose=TRUE)
scalaSerialize(myList, s, verbose=TRUE)

identical(scalaUnserializeList(scalaSerialize(myList, s)),myList)




scalaSerialize(mtcars, s, name="MTCars", verbose=TRUE)
scalaSerialize(mtcars, s, name="MTCars", verbose=TRUE)





identical(scalaUnserialize(scalaSerialize(s, myList)),myList)

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



